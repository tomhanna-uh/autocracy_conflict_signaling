# ==============================================================================
# run_all.R -- Master Pipeline Script
# Runs all analysis scripts in order.
# Usage: source("R/run_all.R") from the project root,
#        or setwd() to project root first.
# ==============================================================================

library(here)

# ------------------------------------------------------------------------------
# Helper: run a single script with timing and error handling
# ------------------------------------------------------------------------------
run_step <- function(script_path, label) {
  message(sprintf("\n=== [%s] %s ===", format(Sys.time(), "%H:%M:%S"), label))
  t0 <- proc.time()
  tryCatch(
    source(here::here(script_path), local = FALSE),
    error = function(e) {
      message(sprintf("  *** FAILED: %s", e$message))
      stop(e)
    }
  )
  elapsed <- (proc.time() - t0)["elapsed"]
  message(sprintf("  Done in %.1f s", elapsed))
}

# ==============================================================================
# Pipeline
# ==============================================================================

# --- Stage 0: Setup & Data ---
run_step("R/00_packages.R",        "00_packages.R — load/install packages")
run_step("R/01_load_data.R",       "01_load_data.R — load raw data")
run_step("R/02_data_prep.R",       "02_data_prep.R — construct analysis variables")

# --- Stage 1: Tier 1 — Simple Logistic Regression ---
run_step("R/03_h1_h2_logit.R",     "03_h1_h2_logit.R — H1 & H2: leader ideology")
run_step("R/04_h3_h4_logit.R",     "04_h3_h4_logit.R — H3 & H4: support group ideology")

# --- Stage 2: Tier 2 — Legitimation Mix ---
run_step("R/05_h5_h6_legitmix.R",  "05_h5_h6_legitmix.R — H5 & H6: legitimation mix")
run_step("R/05_m1_m2_mediation.R", "05_m1_m2_mediation.R — M1 & M2: mediation models")

# --- Stage 3: Tier 3 — Complex Models ---
run_step("R/06_h9_survival.R",     "06_h9_survival.R — H9: leader survival (Cox PH)")
run_step("R/07_h7_mediation.R",    "07_h7_mediation.R — H7: SEM/lavaan mediation")
run_step("R/08_h8_moderation.R",   "08_h8_moderation.R — H8: dynamic leader moderation")

# --- Stage 4: Reporting ---
run_step("R/09_reporting_tables.R", "09_reporting_tables.R — consolidated tables")

message("\n=== Pipeline complete ===")
