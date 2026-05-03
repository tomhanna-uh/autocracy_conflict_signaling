# =============================================================================
# 09_reporting_tables.R
# =============================================================================
# Single script to produce all Tier 1-3 tables in ONE .tex file
# Follows all project coding rules: here::i_am, comments, clean env, stargazer preferred,
# friendly English labels, no full models saved, etc.
# Run AFTER all modeling scripts (03_h1_h2_logit.R through 08_h8_moderation.R)
# Output: results/tables/all_tables_combined.tex (single file, ready for \input{} in manuscript)

here::i_am("R/09_reporting_tables_3.R")
setwd(here::here())

# Force clean load (Rule 3)
rm(list = ls())

## -- Setup -------------------------------------------------------------------

source(here::here("R", "00_packages.R"))
suppressPackageStartupMessages({
        library(here)
        library(stargazer)
        library(modelsummary)
        library(broom)
        library(dplyr)
        library(tibble)
        library(knitr)
        library(kableExtra)
})

results_dir <- here::here("results")
tables_dir  <- here::here("results", "tables")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

## -- Helper: load RDS safely -------------------------------------------------

load_rds <- function(name) {
        path <- file.path(results_dir, paste0(name, ".rds"))
        if (file.exists(path)) {
                readRDS(path)
        } else {
                warning("RDS not found: ", path)
                NULL
        }
}

## -- Shared covariate label maps ---------------------------------------------
# Adjust display names here to match your actual variable names.

covar_labels_logit <- c(
        # Key predictors
        "sidea_revisionist_domestic"       = "Revisionist Leadership Ideology",
        "sidea_party_elite_support"        = "Party/Elite Support",
        "sidea_religious_support"          = "Religious Support",
        "sidea_ethnic_racial_support"      = "Ethnic/Racial Support",
        "sidea_military_support"           = "Military Support",
        "sidea_rural_worker_support"       = "Rural/Worker Support",
        "v2exl_legitideol_a"               = "Ideological Legitimation",
        "legit_ideol_ratio"                = "Legitimation Ratio (Ideo/Perf)",
        "legit_ideol_ratio_norm"           = "Legitimation Ratio (norm.)",
        "v2exl_legitlead_a"                = "Personalist Legitimation",
        "v2exl_legitperf_a"                = "Performance Legitimation",
        "sidea_dynamic_leader"             = "Dynamic Leadership",
        # Controls
        "cinc_a"                           = "Capabilities (CINC)",
        "cinc_a_log"                       = "Log Capabilities (CINC)",
        "sidea_winning_coalition_size"     = "Winning Coalition Size",
        "cold_war"                         = "Cold War",
        "war_on_terror"                    = "War on Terror",
        "log_capdist"                      = "Log Capital Distance",
        "ln_capital_dist_km"               = "Log Capital Distance (km)",
        "v2x_libdem_b"                     = "Target Liberal Democracy",
        "targets_democracy"                = "Target is Democracy",
        "autocracy_a"                      = "Sender is Autocracy",
        "(Intercept)"                      = "Constant"
)


# =============================================================================
# LOAD STRIPPED MODELS (Rule 4 - only stripped RDS <20% size)
# =============================================================================
# Assumes modeling scripts have already run and saved stripped models to results/models/
# If models not saved, source the modeling scripts here (but only if they exist)

model_dir <- here("results/models")
if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)

# Example loading (uncomment and adjust after running modeling scripts)
# h1_baseline <- readRDS(here(model_dir, "h1_baseline_stripped.rds"))
# h1_full     <- readRDS(here(model_dir, "h1_full_stripped.rds"))
# ... repeat for all models

# For now, script assumes models are in global environment or loaded above.
# If running standalone, source the relevant modeling scripts:
# source(here("R/models/03_h1_h2_logit.R"))  # etc. — only if needed and models not pre-saved

# =============================================================================
# TABLE 1: H1 + H2 (Leadership Ideology — Initiation + Democracy Targeting)
# One table, max 5 columns, basic + full controls for converging model
# =============================================================================
# Stargazer with friendly English labels (Rule 9)
# Columns: (1) H1 Baseline, (2) H1 Full, (3) H2 Baseline, (4) H2 Full

stargazer(
  h1_baseline, h1_full, h2_baseline, h2_full,
  type = "latex",
  title = "H1 and H2: Revisionist Leadership Ideology, Conflict Initiation, and Democracy Targeting",
  label = "tab:h1_h2",
  column.labels = c("H1: Initiation (Basic)", "H1: Initiation (Full)", 
                    "H2: Targeting (Basic)", "H2: Targeting (Full)"),
  dep.var.labels = c("MID Initiated", "Targets Democracy"),
  covariate.labels = c(
    "Revisionist Leadership Ideology (Side A)",
    "Log CINC (Side A)",
    "Winning Coalition Size (Side A)",
    "Cold War Period",
    "Peace Years Spline (t)",
    "Peace Years Spline (t2)",
    "Peace Years Spline (t3)"
  ),
  omit = c("Constant"),
  omit.stat = c("ll", "aic", "bic"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Standard errors in parentheses. *** p < 0.001, ** p < 0.01, * p < 0.05. Full models exclude non-converging temporal terms for rare-event stability.",
  out = here("results/tables/h1_h2_combined.tex")
)

# =============================================================================
# TABLE 2: H3 + H4 (Support Group Ideology — Initiation + Democracy Targeting)
# One table, max 5 columns, basic + full for converging model
# =============================================================================
stargazer(
  h3_religious, h3_full, h4_religious, h4_full,
  type = "latex",
  title = "H3 and H4: Ideologically Defined Support Groups, Conflict Initiation, and Democracy Targeting",
  label = "tab:h3_h4",
  column.labels = c("H3: Religious Support (Basic)", "H3: Full Support Groups (Full)",
                    "H4: Religious Support (Basic)", "H4: Full Support Groups (Full)"),
  dep.var.labels = c("MID Initiated", "Targets Democracy"),
  covariate.labels = c(
    "Religious Support (Side A)",
    "Party Elite Support (Side A)",
    "Military Support (Side A)",
    "Rural Worker Support (Side A)",
    "Ethnic/Racial Support (Side A)",
    "Log CINC (Side A)",
    "Winning Coalition Size (Side A)"
  ),
  omit = c("Constant"),
  omit.stat = c("ll", "aic", "bic"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Standard errors in parentheses. *** p < 0.001, ** p < 0.01, * p < 0.05. Models restricted to converging specifications; temporal terms dropped for rare-event stability.",
  out = here("results/tables/h3_h4_combined.tex")
)

# =============================================================================
# TABLE 3: H5 + H6 (Legitimation Mix — Initiation + Democracy Targeting)
# One table, max 5 columns, basic + full for converging model
# =============================================================================
stargazer(
  h5_baseline, h5_full, h6_baseline, h6_full,
  type = "latex",
  title = "H5 and H6: Ideological Legitimation Ratio, Conflict Initiation, and Democracy Targeting",
  label = "tab:h5_h6",
  column.labels = c("H5: Initiation (Basic)", "H5: Initiation (Full)",
                    "H6: Targeting (Basic)", "H6: Targeting (Full)"),
  dep.var.labels = c("MID Initiated", "Targets Democracy"),
  covariate.labels = c(
    "Ideological Legitimation Ratio",
    "Performance Legitimation (V-Dem)",
    "Personalist Legitimation (V-Dem)",
    "Log CINC (Side A)",
    "Winning Coalition Size (Side A)"
  ),
  omit = c("Constant"),
  omit.stat = c("ll", "aic", "bic"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Standard errors in parentheses. *** p < 0.001, ** p < 0.01, * p < 0.05. Hurdle models (zero-inflated) available in supplementary materials for robustness.",
  out = here("results/tables/h5_h6_combined.tex")
)

# =============================================================================
# TABLE 4: H7 — Mediation (Support Groups) — Separate as requested
# Uses mediation package output or custom stargazer for ACME/ADE
# =============================================================================
# For mediation, stargazer on the mediation object or manual table
# Assuming med_h7 object from mediation::mediate()

stargazer(
  med_h7_religious, med_h7_party, med_h7_military,
  type = "latex",
  title = "H7: Mediation Analysis — Effect of Revisionist Ideology on Democracy Targeting via Support Groups",
  label = "tab:h7_mediation",
  column.labels = c("Religious Support Mediator", "Party Elite Mediator", "Military Support Mediator"),
  dep.var.labels = c("ACME (Indirect)", "ADE (Direct)", "Total Effect", "Prop. Mediated"),
  covariate.labels = c(
    "Average Causal Mediation Effect (ACME)",
    "Average Direct Effect (ADE)",
    "Total Effect",
    "Proportion Mediated"
  ),
  omit = c("Constant"),
  omit.stat = c("ll", "aic", "bic"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Bootstrap 1,000 simulations. ACME = Average Causal Mediation Effect. *** p < 0.001, ** p < 0.01, * p < 0.05. Positive ACME indicates ideology increases targeting via the mediator.",
  out = here("results/tables/h7_mediation.tex")
)

# =============================================================================
# TABLE 5: H8 + H9 (Dynamic Leadership Moderation + Survival Mediation)
# One table, max 5 columns
# =============================================================================
stargazer(
  h8_init_main, h8_init_int, h9_cox_direct, h9_cox_interaction,
  type = "latex",
  title = "H8 and H9: Dynamic Leadership Moderation and Leader Survival Benefits of Ideological Conflict",
  label = "tab:h8_h9",
  column.labels = c("H8: Initiation (Main)", "H8: Initiation (Interaction)",
                    "H9: Survival (Direct)", "H9: Survival (Interaction)"),
  dep.var.labels = c("MID Initiated", "Leader Exit Hazard"),
  covariate.labels = c(
    "Ideological Legitimation Ratio",
    "Dynamic/Charismatic Leader (V-Dem)",
    "Winning Coalition Size",
    "Conflict vs. Ideological Targets",
    "Conflict vs. Democracy × Legit Ratio",
    "Log Average GDP per Capita",
    "Irregular Entry"
  ),
  omit = c("Constant"),
  omit.stat = c("ll", "aic", "bic"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Standard errors in parentheses. *** p < 0.001, ** p < 0.01, * p < 0.05. Negative coefficients in H9 indicate reduced hazard (increased survival). LPM used for H8 interaction marginal effects.",
  out = here("results/tables/h8_h9_combined.tex")
)

# =============================================================================
# COMBINE ALL TABLES INTO SINGLE .tex FILE (Rule 4 + user request)
# =============================================================================
output_file <- here("results/tables/all_hypotheses_tables.tex")

# Header for the combined file
cat("% ============================================================\n", file = output_file)
cat("% ALL TABLES — Paper 1 (Ideological Commitment Signaling)\n", file = output_file, append = TRUE)
cat("% Generated by 09_reporting_tables.R — stargazer (journal style)\n", file = output_file, append = TRUE)
cat("% ============================================================\n\n", file = output_file, append = TRUE)

# Append each table (in order: H1-H2, H3-H4, H5-H6, H7, H8-H9)
tables_to_combine <- c(
  here("results/tables/h1_h2_combined.tex"),
  here("results/tables/h3_h4_combined.tex"),
  here("results/tables/h5_h6_combined.tex"),
  here("results/tables/h7_mediation.tex"),
  here("results/tables/h8_h9_combined.tex")
)

for (tbl in tables_to_combine) {
  if (file.exists(tbl)) {
    cat(readLines(tbl), sep = "\n", file = output_file, append = TRUE)
    cat("\n\n% --- End of table ---\n\n", file = output_file, append = TRUE)
  } else {
    warning("Table file not found: ", tbl)
  }
}

message("Single combined .tex file written to: ", output_file)
message("Individual tables also saved in results/tables/ for modular use.")

# Clean up (Rule 3)
rm(list = ls())
gc()

# End of script — ready for \input{results/tables/all_hypotheses_tables.tex} in manuscript