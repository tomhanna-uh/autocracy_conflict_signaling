# ==============================================================================
# 09_reporting_tables.R — Consolidated Results Reporting
# Purpose: Generates publication-quality regression tables (stargazer) and
#          summary statistics for the entire project.
# Tier 4: Reporting and Visualization
# ==============================================================================

# Load required scripts
source("R/00_packages.R")

# Note: This script assumes all previous model scripts have been run and their
# .rds outputs are available in the results/ folder.

# ==============================================================================
# 1. Utility Functions ----
# ==============================================================================

#' Load all results objects from results/ folder
#' @return Named list of model lists
load_all_results <- function() {
  list(
    h1 = readRDS("results/h1_logit_models.rds"),
    h2 = readRDS("results/h2_logit_models.rds"),
    h3 = readRDS("results/h3_logit_models.rds"),
    h4 = readRDS("results/h4_logit_models.rds"),
    h5_h6 = readRDS("results/h5_h6_models.rds"),
    h7 = readRDS("results/h7_mediation.rds"),
    h8 = readRDS("results/h8_init_models.rds"),
    h9 = readRDS("results/h9_survival_models.rds")
  )
}

# ==============================================================================
# 2. Main Reporting Functions ----
# ==============================================================================

#' Generate Tier 1 (H1-H4) Summary Tables
#' @param res Results list from load_all_results()
report_tier1_logit <- function(res) {

  dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

  # --- Table 1: H1 (Initiation) & H2 (Targeting) Main Results ---
  # Matches the sequential specification logic in R/03_h1_h2_logit.R
  stargazer(
    res$h1$h1_full, res$h2$h2_full,
    type = "text",
    title = "Tier 1: Leader Ideology and Conflict Behavior (H1 & H2)",
    dep.var.labels = c("MID Initiated", "Target is Democracy"),
    covariate.labels = c(
      "Ideological Legitimation Share",
      "Log CINC (A)", "Log CINC (B)",
      "Winning Coalition Size", "Cold War"
    ),
    star.cutoffs = c(0.05, 0.01, 0.001),
    out = "results/tables/tier1_h1_h2_main.html"
  )

  # --- Table 2: H3 (Initiation) & H4 (Targeting) Main Results ---
  stargazer(
    res$h3$h3_full, res$h4$h4_full,
    type = "text",
    title = "Tier 1: Support Group Ideology and Conflict Behavior (H3 & H4)",
    dep.var.labels = c("MID Initiated", "Target is Democracy"),
    star.cutoffs = c(0.05, 0.01, 0.001),
    out = "results/tables/tier1_h3_h4_main.html"
  )
}

#' Generate Tier 2 (H5-H6) Summary Tables
#' @param res Results list
report_tier2_legit <- function(res) {

  # H5/H6 Legitimation Mix models (logit and hurdle)
  stargazer(
    res$h5_h6$h5_logit, res$h5_h6$h6_logit,
    type = "text",
    title = "Tier 2: Legitimation Mix and Conflict (H5 & H6)",
    dep.var.labels = c("MID Initiated", "Target is Democracy"),
    out = "results/tables/tier2_legit_mix.html"
  )
}

#' Generate Tier 3 (H7-H9) Summary Tables
#' @param res Results list
report_tier3_complex <- function(res) {

  # --- H8: Moderation (Messianic Autocrat Test) ---
  stargazer(
    res$h8$h8_init_int, res$h8$h8_target_int,
    type = "text",
    title = "Tier 3: Moderation by Dynamic Leadership (H8)",
    dep.var.labels = c("MID Initiated", "Target is Democracy"),
    out = "results/tables/tier3_h8_moderation.html"
  )

  # --- H9: Survival (Cox PH) ---
  stargazer(
    res$h9$cox_h9_ratio, res$h9$cox_h9_int_ratio,
    type = "text",
    title = "Tier 3: Leader Survival and Conflict (H9)",
    dep.var.labels = "Hazard (Exit from Office)",
    out = "results/tables/tier3_h9_survival.html"
  )
}

# ==============================================================================
# 3. Execution ----
# ==============================================================================

# Note: Wrapped in try() because some .rds files might not exist yet during
# development of the individual model scripts.
try({
  all_res <- load_all_results()
  report_tier1_logit(all_res)
  report_tier2_legit(all_res)
  report_tier3_complex(all_res)
  message("[09_reporting_tables.R] All reporting tables generated in results/tables/")
}, silent = FALSE)
