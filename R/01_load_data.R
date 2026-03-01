# =============================================================================
# 01_load_data.R
# Data loading functions for autocracy_conflict_signaling
#
# Defines two functions:
#   load_dyad_data()   -- dyadic MID data (main analysis dataset)
#   load_monadic_data() -- monadic first-use-of-force data
#
# Both functions validate expected columns, filter to autocracies post-1946,
# and return a clean tibble. Source this file at the top of analysis scripts:
#   source(here::here("R", "01_load_data.R"))
# =============================================================================

library(here)
library(tidyverse)

source("R/check_paths.R")

grave_d_path <- check_grave_d_path("GRAVE_D_Master.csv")

grave_d <- readr::read_csv(grave_d_path)


# -----------------------------------------------------------------------------
# Expected column sets
# Used for validation; stops early with a clear error if columns are missing.
# Update these vectors if the master dataset changes.
# -----------------------------------------------------------------------------

.DYAD_REQUIRED_COLS <- c(
  # Identifiers
  "COWcode_a", "COWcode_b", "year",

  # Democracy scores (V-Dem)
  "v2x_libdem_a", "v2x_libdem_b",

  # Conflict outcome
  "hihosta",

  # V-Dem legitimation variables (Tier 1-2 IVs)
  "v2exl_legitideol_a",
  "v2exl_legitlead_a",
  "v2exl_legitperf_a",

  # Capabilities (CINC scores from NMC)
  "cinc_a",
  "cinc_b"
)

.DYAD_GRAVE_COLS <- c(
  # GRAVE-D leadership ideology (Tier 1 IVs for H1/H2)
  "sidea_revisionist_domestic",

  # GRAVE-D support group variables (Tier 1 IVs for H3/H4)
  "sidea_religious_support",
  "sidea_party_elite_support",
  "sidea_rural_worker_support",
  "sidea_ethnic_racial_support",
  "sidea_military_support",

  "sidea_nationalist_revisionist_domestic",
  "sidea_socialist_revisionist_domestic",
  "sidea_religious_revisionist_domestic",
  "sidea_reactionary_revisionist_domestic",
  "sidea_separatist_revisionist_domestic",

  # GRAVE-D dynamic leadership (H8 moderator)
  "sidea_dynamic_leader",

  # Selectorate theory
  "sidea_winning_coalition_size",

  # Derived variables from 05_build_master.R
  "mid_initiated",
  "fuf_initiator",
  "targets_democracy",
  "cold_war",
  "rev_potential_a",
  "rev_potential_b",
  "revisionism_distance"
)

.MONADIC_REQUIRED_COLS <- c(
  "COWcode", "year",
  "first_use_of_force",
  "cinc",
  "winning_coalition_size",
  "military_support",
  "cold_war"
)

# -----------------------------------------------------------------------------
# Internal helper: column validation
# -----------------------------------------------------------------------------

.check_cols <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    warning(
      sprintf(
        "[%s] Missing columns (may need data merge in 02_data_prep.R):\n  %s",
        label,
        paste(missing, collapse = "\n  ")
      )
    )
  } else {
    message(sprintf("[%s] All required columns present.", label))
  }
}

# -----------------------------------------------------------------------------
# load_dyad_data()
# Loads the master dyadic dataset.
#
# Arguments:
#   filepath   -- path to CSV; defaults to data/GRAVE_D_Master.csv
#   year_min   -- earliest year to retain (default 1946)
#   autoc_max  -- V-Dem liberal democracy ceiling for Side A autocracy filter
#                 (default 0.5; rows where v2x_libdem_a >= autoc_max are dropped)
#   warn_grave -- if TRUE (default), warns when GRAVE-D columns are absent
#                 (needed for H3/H4/H7/H8 but may not be in all data versions)
#
# Returns: tibble, filtered and lightly validated
# -----------------------------------------------------------------------------

load_dyad_data <- function(
    filepath  = here("data", "GRAVE_D_Master.csv"),
    year_min  = 1946L,
    autoc_max = 0.5,
    warn_grave = TRUE
) {
  if (!file.exists(filepath)) {
    stop(
      sprintf(
        "[load_dyad_data] Data file not found:\n  %s\n",
        "Place GRAVE_D_Master.csv in the data/ directory."
      )
    )
  }

  message("[load_dyad_data] Reading data...")
  raw <- read_csv(filepath, show_col_types = FALSE)

  # Validate core columns
  .check_cols(raw, .DYAD_REQUIRED_COLS, "load_dyad_data: core")

  # Optionally warn on GRAVE-D columns (needed for H3/H4/H7/H8)
  if (warn_grave) {
    .check_cols(raw, .DYAD_GRAVE_COLS, "load_dyad_data: GRAVE-D")
  }

  # Filter: post-1946, Side A autocracies only
  out <- raw |>
    filter(year >= year_min) |>
    filter(v2x_libdem_a < autoc_max)

  message(sprintf(
    "[load_dyad_data] Loaded %d rows x %d cols (years %d-%d).",
    nrow(out), ncol(out),
    min(out$year, na.rm = TRUE),
    max(out$year, na.rm = TRUE)
  ))

  out
}

# -----------------------------------------------------------------------------
# load_monadic_data()
# Loads the monadic first-use-of-force dataset.
#
# Arguments:
#   filepath -- path to CSV; defaults to data/m_conflict_autocracies.csv
#   year_min -- earliest year to retain (default 1946)
#
# Returns: tibble
# -----------------------------------------------------------------------------

load_monadic_data <- function(
    filepath = here("data", "m_conflict_autocracies.csv"),
    year_min = 1946L
) {
  if (!file.exists(filepath)) {
    stop(
      sprintf(
        "[load_monadic_data] Data file not found:\n  %s\n",
        "Place m_conflict_autocracies.csv in the data/ directory."
      )
    )
  }

  message("[load_monadic_data] Reading data...")
  raw <- read_csv(filepath, show_col_types = FALSE)

  .check_cols(raw, .MONADIC_REQUIRED_COLS, "load_monadic_data")

  out <- raw |> filter(year >= year_min)

  message(sprintf(
    "[load_monadic_data] Loaded %d rows x %d cols (years %d-%d).",
    nrow(out), ncol(out),
    min(out$year, na.rm = TRUE),
    max(out$year, na.rm = TRUE)
  ))

  out
}

# -----------------------------------------------------------------------------
# Convenience loader: runs both and returns a named list
# Use when a script needs both datasets:
#   datasets <- load_all_data()
#   dyad_raw <- datasets$dyad
#   monad_raw <- datasets$monadic
# -----------------------------------------------------------------------------

load_all_data <- function(...) {
  list(
    dyad    = load_dyad_data(...),
    monadic = load_monadic_data()
  )
}

message("[01_load_data.R] Functions defined: load_dyad_data(), load_monadic_data(), load_all_data()")
