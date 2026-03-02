# ==============================================================================
# run_all.R
# Master pipeline: run all analysis scripts in order, then render the
# Quarto book to HTML, PDF, and DOCX.
#
# Usage (from RStudio or terminal at project root):
#   source("run_all.R")          # in R
#   Rscript run_all.R            # from terminal
#
# Prerequisites:
#   - Place GRAVE_D_Master_with_Leaders.csv in data/
#   - Install all packages with source("R/00_packages.R") first
#   - Quarto CLI must be installed (https://quarto.org)
# ==============================================================================

library(here)

# ------------------------------------------------------------------------------
# Helper: run a script and report success/failure
# ------------------------------------------------------------------------------
run_step <- function(script, label = script) {
  message("\n", strrep("=", 70))
  message("RUNNING: ", label)
  message(strrep("-", 70))
  tryCatch(
    source(here(script)),
    error = function(e) {
      message("ERROR in ", label, ": ", conditionMessage(e))
      stop("Pipeline stopped at: ", label, call. = FALSE)
    }
  )
  message("DONE:    ", label)
}

# ==============================================================================
# STAGE 1 — Install / Load Packages
# ==============================================================================
run_step("R/00_packages.R", "00_packages.R — load all libraries")

# ==============================================================================
# STAGE 2 — Load and Prepare Data
# ==============================================================================
run_step("R/01_load_data.R",  "01_load_data.R  — load raw dataset")
run_step("R/02_data_prep.R",  "02_data_prep.R  — recode, derive, merge variables")

# ==============================================================================
# STAGE 3 — Tier 1 Models: Simple Logistic Regression (H1–H4)
# ==============================================================================
run_step("R/03_h1_h2_logit.R", "03_h1_h2_logit.R — H1 & H2: leader ideology")
run_step("R/04_h3_h4_logit.R", "04_h3_h4_logit.R — H3 & H4: support groups")

# ==============================================================================
# STAGE 4 — Tier 2 Models: Legitimation Mix (H5–H6)
# ==============================================================================
run_step("R/05_h5_h6_legitmix.R", "05_h5_h6_legitmix.R — H5 & H6: legitimation mix")


# ==============================================================================
# STAGE 4b — M1/M2 Mediation and Survival Models
# ==============================================================================
run_step("R/05_m1_m2_mediation.R", "05_m1_m2_mediation.R — M1 & M2: mediation and survival")
# ==============================================================================
# STAGE 5 — Tier 3 Models: Mediation, Moderation, Survival (H7–H9)
# Note: 06_h9_survival.R runs before 07 and 08 so the Cox models are
#       available if 07/08 reference the survival data object.
# ==============================================================================
run_step("R/06_h9_survival.R",  "06_h9_survival.R  — H9: Cox PH survival models")
run_step("R/07_h7_mediation.R", "07_h7_mediation.R — H7: support group mediation")
run_step("R/08_h8_moderation.R","08_h8_moderation.R — H8: dynamic leader moderation")

# ==============================================================================
# STAGE 6 — Consolidated Reporting Tables
# ==============================================================================
run_step("R/09_reporting_tables.R", "09_reporting_tables.R — generate all output tables")

# ==============================================================================
# STAGE 7 — Render the Quarto Book
# All chapter .qmd files read pre-saved .rds objects from results/, so no
# R models are re-estimated during rendering (freeze: auto is set in _quarto.yml).
# ==============================================================================
message("\n", strrep("=", 70))
message("RENDERING: Quarto book (HTML + PDF + DOCX)")
message(strrep("-", 70))

quarto_status <- system(
  paste("quarto render", shQuote(here("docs"))),
  intern  = FALSE,
  wait    = TRUE
)

if (quarto_status != 0L) {
  stop(
    "quarto render exited with status ", quarto_status,
    ".\nCheck the output above for errors.",
    call. = FALSE
  )
}

message(strrep("=", 70))
message("PIPELINE COMPLETE")
message("Output written to: ", here("docs", "_book"))
message(strrep("=", 70))
