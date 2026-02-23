# 02_data_prep.R — Data Transformation and Variable Derivation

# Load required scripts
source("R/00_packages.R")
source("R/01_load_data.R")

# 1. Load Data ----
data_list <- load_all_data()
dyad_df   <- data_list$dyad
monadic_df <- data_list$monadic

# 2. Dyadic Preparation (H1–H9) ----
# Note: If validation warned about missing GRAVE columns, merge them here
# from consolidated_autocracies_2.csv if needed.

prep_dyad <- function(df) {
  df %>%
    mutate(
      # Outcome variables
      mid_initiated    = as.integer(hihosta >= 2),
      targets_democracy = as.integer(v2x_libdem_b >= 0.5),

      # Ideology Gap (2025 H3 Component)
      ideology_gap      = abs(v2exl_legitideol_a - v2exl_legitideol_b),
      large_ideology_gap = as.integer(ideology_gap > median(ideology_gap, na.rm = TRUE)),

      # Legitimation Mix components
      legit_total = v2exl_legitideol_a + v2exl_legitperf_a + v2exl_legitlead_a,
      legit_ratio = v2exl_legitideol_a / (legit_total + 0.01),

      # Control Variable standardization
      log_cinc_a  = log(sidea_national_military_capabilities + 0.0001),
      log_cinc_b  = log(sideb_national_military_capabilities + 0.0001),
      cinc_ratio  = sidea_national_military_capabilities / (sideb_national_military_capabilities + 0.0001),

      # Time periods
      cold_war    = as.integer(year >= 1947 & year <= 1991)
    ) %>%
    # Handle Peace Years (porting logic from 2024 data_code.qmd)
    arrange(dyad, year) %>%
    group_by(dyad) %>%
    mutate(
      conflict_year = if_else(mid_initiated == 1, year, NA_integer_),
      last_conflict = cummax(if_else(is.na(conflict_year), 0, conflict_year)),
      peace_years   = if_else(last_conflict == 0, 35, year - last_conflict), # assume 35 for left-censored
      t  = peace_years,
      t2 = t^2,
      t3 = t^3
    ) %>%
    ungroup()
}

dyad_ready <- prep_dyad(dyad_df)

# 3. Monadic Preparation (Tier 1 Baseline) ----
prep_monadic <- function(df) {
  df %>%
    mutate(
      # Standardize naming from m_conflict_data.csv
      first_use_force    = as.integer(fuf == 1),
      targets_democracy  = as.integer(first_use_of_force_targeting_democracy == 1),

      # Legitimation mix (monadic)
      legit_ratio = v2exl_legitideol_a / (v2exl_legitideol_a + v2exl_legitperf_a + v2exl_legitlead_a + 0.01),

      # Controls
      log_cinc  = log(national_military_capabilities + 0.0001),
      cold_war  = as.integer(year >= 1947 & year <= 1991)
    ) %>%
    # Peace years logic
    arrange(COWcode, year) %>%
    group_by(COWcode) %>%
    mutate(
      peace_years = if_else(is.infinite(peace_years), 31, peace_years),
      t  = peace_years,
      t2 = t^2,
      t3 = t^3
    ) %>%
    ungroup()
}

monadic_ready <- prep_monadic(monadic_df)

# 4. Save Ready Data (Internal Use) ----
# Using .rds preserves factors and attributes
dir.create("ready_data", showWarnings = FALSE)
saveRDS(dyad_ready,    "ready_data/dyad_ready.rds")
saveRDS(monadic_ready, "ready_data/monadic_ready.rds")

message("Data preparation complete. Ready files saved to ready_data/")
