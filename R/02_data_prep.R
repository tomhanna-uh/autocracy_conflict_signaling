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
source("R/00_packages.R")
source("R/01_load_data.R")

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
      peace_years = if_else(last_conflict == 0, 35, year - last_conflict),
      t  = peace_years,
      t2 = t^2,
      t3 = t^3
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
      n_mids            = sum(hihosta >= 2, na.rm = TRUE),
      n_vs_democracy    = sum(hihosta >= 2 & v2x_libdem_b >= 0.5, na.rm = TRUE),

      # Side A attributes (take first non-NA, all should be same within country-year)
      v2exl_legitideol_a = first(v2exl_legitideol_a),
      v2exl_legitperf_a  = first(v2exl_legitperf_a),
      v2exl_legitlead_a  = first(v2exl_legitlead_a),
      v2x_libdem_a       = first(v2x_libdem_a),
      cinc_a             = first(cinc_a),

      # GRAVE-D variables
      sidea_revisionist_domestic    = first(sidea_revisionist_domestic),
      sidea_dynamic_leader          = first(sidea_dynamic_leader),
      sidea_winning_coalition_size   = first(sidea_winning_coalition_size),
      sidea_military_support        = first(sidea_military_support),

      # Survival / leader variables from GRAVE-D
      tenure              = first(tenure),
      entry               = first(entry),
      irregulartransition = first(irregulartransition),
      posttenurefate      = first(posttenurefate),

      # Economic / population controls
      flgdpen  = first(flgdpen),
      tpop     = first(tpop),
      epop_a   = first(epop_a),

      .groups = "drop"
    ) %>%
    rename(COWcode = COWcode_a) %>%
    mutate(
      # Legitimation mix (monadic)
      legit_ratio = v2exl_legitideol_a / (v2exl_legitideol_a + v2exl_legitperf_a + v2exl_legitlead_a + 0.01),

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
      log_avg_gdp           = log(flgdpen + 1),
      log_avg_pop           = log(ifelse(!is.na(epop_a), epop_a,
                                         ifelse(!is.na(tpop), tpop, NA)) + 1),

      # Controls
      log_cinc  = log(cinc_a + 0.0001),
      cold_war  = as.integer(year >= 1947 & year <= 1991)
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
    ungroup()
}

monadic_ready <- prep_monadic(dyad_df)

# 4. Save Ready Data (Internal Use) ----
dir.create("ready_data", showWarnings = FALSE)
saveRDS(dyad_ready, "ready_data/dyad_ready.rds")
saveRDS(monadic_ready, "ready_data/monadic_ready.rds")
message("Data preparation complete. Ready files saved to ready_data/")

# 5. Memory Cleanup ----
rm(dyad_df, prep_dyad, prep_monadic)
gc()
message(sprintf("[02] Cleanup done. dyad_ready: %s, monadic_ready: %s",
                format(object.size(dyad_ready), units = "MB"),
                format(object.size(monadic_ready), units = "MB")))

} # end guard
