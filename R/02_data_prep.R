# 02_data_prep.R -- Data Transformation and Variable Derivation
#
# All data sourced from grave_d_data2026 via 01_load_data.R.
# Monadic data is derived from the dyadic master by collapsing to
#   country-year (Side A only).
#
# This script is idempotent: if dyad_ready and monadic_ready already exist
# in the global environment, the script skips re-loading and re-preparation.

# Guard: skip if data already prepared (avoids redundant 3.9 GB reloads)
if (exists("dyad_ready") && exists("monadic_ready")) {
  message("[02] dyad_ready and monadic_ready already exist. Skipping data prep.")
} else {

# Load required scripts
  source(here::here("R", "00_packages.R"))
  source(here::here("R", "01_load_data.R"))

# 1. Load Data ----
dyad_df <- load_dyad_data()

# 2. Dyadic Preparation (H1-H9) ----
prep_dyad <- function(df) {
  df %>%
    mutate(
      # Outcome variables
      mid_initiated = as.integer(hihosta >= 2),
      targets_democracy = as.integer(v2x_libdem_b >= 0.5),

      # Ideology Gap (2025 H3 Component)
      ideology_gap = abs(v2exl_legitideol_a - v2exl_legitideol_b),
      large_ideology_gap = as.integer(ideology_gap > median(ideology_gap, na.rm = TRUE)),

      # Legitimation Mix components
      legit_total = v2exl_legitideol_a + v2exl_legitperf_a + v2exl_legitlead_a,
      legit_ratio = v2exl_legitideol_a / (legit_total + 0.01),

      # Control Variable standardization (using cinc_a/cinc_b from GRAVE-D)
      log_cinc_a = log(cinc_a + 0.0001),
      log_cinc_b = log(cinc_b + 0.0001),
      cinc_ratio = cinc_a / (cinc_b + 0.0001),

      # Time periods
      cold_war = as.integer(year >= 1947 & year <= 1991)
    ) %>%
    # Handle Peace Years (porting logic from 2024 data_code.qmd)
    arrange(dyad, year) %>%
    group_by(dyad) %>%
    mutate(
      conflict_year = if_else(mid_initiated == 1, year, NA_integer_),
      last_conflict = cummax(if_else(is.na(conflict_year), 0, conflict_year)),
      peace_years   = if_else(last_conflict == 0, 35, year - last_conflict),
      t  = peace_years,
      t2 = t^2,
      t3 = t^3
    ) %>%
                # After the mutate block that creates t/t2/t3
                mutate(
                        t_scaled = t / 100,          # Carter & Signorino themselves recommend this for long panels
                        t2_scaled = t_scaled^2,
                        t3_scaled = t_scaled^3
                        ) %>%
    ungroup()
}

dyad_ready <- prep_dyad(dyad_df)

# 3. Monadic Preparation (derived from dyadic master) ----
prep_monadic <- function(dyad_df) {
  dyad_df %>%
    group_by(COWcode_a, year) %>%
    summarise(
      # Conflict: did this country initiate any MID this year?
      first_use_force   = as.integer(any(hihosta >= 4, na.rm = TRUE)),
      mid_initiated     = as.integer(any(hihosta >= 2, na.rm = TRUE)),
      targets_democracy = as.integer(any(v2x_libdem_b >= 0.5, na.rm = TRUE)),

      # Conflict counts for survival models (H9)
      n_mids          = sum(hihosta >= 2, na.rm = TRUE),
      n_vs_democracy  = sum(hihosta >= 2 & v2x_libdem_b >= 0.5, na.rm = TRUE),

      # Side A attributes (take first non-NA, all should be same within country-year)
      v2exl_legitideol_a = first(v2exl_legitideol_a),
      v2exl_legitperf_a  = first(v2exl_legitperf_a),
      v2exl_legitlead_a  = first(v2exl_legitlead_a),
      v2x_libdem_a       = first(v2x_libdem_a),
      cinc_a             = first(cinc_a),

      # GRAVE-D variables
      sidea_revisionist_domestic    = first(sidea_revisionist_domestic),
      sidea_dynamic_leader          = first(sidea_dynamic_leader),
      sidea_winning_coalition_size  = first(sidea_winning_coalition_size),
      sidea_military_support        = first(sidea_military_support),

      # Survival / leader variables from GRAVE-D
      tenure              = first(tenure),
      entry               = first(entry),
      irregulartransition = first(irregulartransition),
      posttenurefate      = first(posttenurefate),

      # Economic / population controls
      flgdpen = first(flgdpen),
      tpop    = first(tpop),
      epop_a  = first(epop_a),
      .groups = "drop"
    ) %>%
    rename(COWcode = COWcode_a) %>%
    mutate(
      # Legitimation mix (monadic)
      legit_ratio = v2exl_legitideol_a /
        (v2exl_legitideol_a + v2exl_legitperf_a + v2exl_legitlead_a + 0.01),

      # Survival variables (H9)
      tenure_years    = tenure,
      irregular_entry = as.integer(irregulartransition == 1),
      survived_to_end = as.integer(!is.na(posttenurefate) & posttenurefate <= 1),

      # Conflict counts for H9
      conflicts_vs_ideo_targets  = n_mids,
      conflicts_vs_democracy     = n_vs_democracy,
      conflicts_vs_ideo_opponent = n_mids,

      # Averaged controls for H9
      avg_ideological_legit = v2exl_legitideol_a,
      avg_autocracy_level   = 1 - v2x_libdem_a,
      log_avg_gdp = log(flgdpen + 1),
      log_avg_pop = log(ifelse(!is.na(epop_a), epop_a,
                               ifelse(!is.na(tpop), tpop, NA)) + 1),

      # Controls
      log_cinc = log(cinc_a + 0.0001),
      cold_war = as.integer(year >= 1947 & year <= 1991),

      # M1/M2 mediation variables
      log_tenure      = log(tenure + 1),
      avg_other_legit = v2exl_legitperf_a + v2exl_legitlead_a
    ) %>%
    # Peace years logic
    arrange(COWcode, year) %>%
    group_by(COWcode) %>%
    mutate(
      conflict_year = if_else(mid_initiated == 1, year, NA_integer_),
      last_conflict = cummax(if_else(is.na(conflict_year), 0L, conflict_year)),
      peace_years   = if_else(last_conflict == 0L, 35L, as.integer(year - last_conflict)),
      t  = peace_years,
      t2 = t^2,
      t3 = t^3
    ) %>%
                # After the mutate block that creates t/t2/t3
                mutate(
                        t_scaled = t / 100,          # Carter & Signorino themselves recommend this for long panels
                        t2_scaled = t_scaled^2,
                        t3_scaled = t_scaled^3
                        ) %>%
    ungroup()
}

monadic_ready <- prep_monadic(dyad_df)

# 4. Save Ready Data (Internal Use) ----
dir.create("ready_data", showWarnings = FALSE)
saveRDS(dyad_ready,    "ready_data/dyad_ready.rds")
saveRDS(monadic_ready, "ready_data/monadic_ready.rds")
message("Data preparation complete. Ready files saved to ready_data/")

# Also cache to results/ for Quarto rendering (avoids reloading full pipeline)
dir.create(here::here("results"), showWarnings = FALSE)
saveRDS(dyad_ready,    here::here("results", "dyad_ready.rds"))
saveRDS(monadic_ready, here::here("results", "monadic_ready.rds"))

# 5. Memory Cleanup (pre-subset) ----
rm(dyad_df, prep_dyad, prep_monadic)
gc()

message(sprintf("[02] Cleanup done. dyad_ready: %s, monadic_ready: %s",
                format(object.size(dyad_ready), units = "MB"),
                format(object.size(monadic_ready), units = "MB")))

} # end guard

# ==============================================================================
# 6. Subset to model-used variables only ----
# Reduces memory footprint by dropping columns not referenced in any formula.
# Identifying columns (dyad, year, country codes) are retained for merging.
# ==============================================================================


# --- dyad_ready: keep IDs + all variables used across 03-08 scripts -----------
dyad_keep <- c(
        # Identifiers
        "dyad", "year", "COWcode_a", "COWcode_b", "ccode1", "ccode2",
        "statea", "stateb",
        # Outcomes
        "mid_initiated", "targets_democracy",
        # H1 IVs (leader ideology composite + sub-types)
        "sidea_revisionist_domestic",
        "sidea_religious_revisionist_domestic",
        "sidea_socialist_revisionist_domestic",
        "sidea_nationalist_revisionist_domestic",
        "sidea_reactionary_revisionist_domestic",
        "sidea_separatist_revisionist_domestic",
        # H3/H4 IVs (support group variables)
        "sidea_religious_support", "sidea_party_elite_support",
        "sidea_rural_worker_support", "sidea_military_support",
        "sidea_ethnic_racial_support",
        # H5/H6 IVs (legitimation mix)
        "legit_ratio", "legit_total",
        "v2exl_legitideol_a", "v2exl_legitperf_a", "v2exl_legitlead_a",
        # H8 IVs (moderation)
        "sidea_dynamic_leader",
        # Controls
        "cinc_a", "log_cinc_a", "log_cinc_b",
        "sidea_winning_coalition_size",
        "cold_war",
        # Temporal dependence (Carter-Signorino polynomial - scaled versions)
        "t_scaled", "t2_scaled", "t3_scaled"
        # Optional: keep unscaled if you still reference them somewhere
        # "t", "t2", "t3"   # ← comment out or remove if fully switching to scaled
)

# Keep only columns that actually exist (some ID columns may vary by dataset)
dyad_keep <- intersect(dyad_keep, names(dyad_ready))
dyad_before <- ncol(dyad_ready)
dyad_ready <- dyad_ready[, dyad_keep, drop = FALSE]
message(sprintf("[02] dyad_ready subset: %d -> %d columns (%s)",
                dyad_before, ncol(dyad_ready),
                format(object.size(dyad_ready), units = "MB")))

# --- monadic_ready: keep IDs + all variables used in 05_m1_m2 / 06_h9 --------
monadic_keep <- c(
  # Identifiers
  "COWcode", "year",
  # Treatment / conflict variables
  "conflicts_vs_ideo_targets", "conflicts_vs_democracy",
  "conflicts_vs_ideo_opponent",
  # Mediators
  "avg_ideological_legit", "legit_ratio", "avg_other_legit",
  # Outcomes
  "log_tenure", "tenure_years", "survived_to_end",
  # Controls
  "avg_autocracy_level", "log_avg_gdp", "log_avg_pop",
  "irregular_entry"
)
monadic_keep   <- intersect(monadic_keep, names(monadic_ready))
monadic_before <- ncol(monadic_ready)
monadic_ready  <- monadic_ready[, monadic_keep, drop = FALSE]
message(sprintf("[02] monadic_ready subset: %d -> %d columns (%s)",
                monadic_before, ncol(monadic_ready),
                format(object.size(monadic_ready), units = "MB")))

# ==============================================================================
# 7. Final environment cleanup ----
# Remove only temporary objects created by this script (not the whole environment).
# ==============================================================================
rm(list = intersect(ls(), c("grave_d","dyad_df", "prep_dyad", "prep_monadic", "dyad_keep", "dyad_before", "monadic_keep", "monadic_before")))
gc()
message("[02_data_prep.R] Done. dyad_ready and monadic_ready are in the environment.")
