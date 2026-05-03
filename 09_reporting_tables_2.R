# =============================================================================
# 09_reporting_tables.R
# Journal-style regression tables organized by hypothesis tier
# Uses stargazer() where possible, modelsummary() for mediation/SEM
# Run AFTER all model scripts have been sourced (or RDS files saved)
# =============================================================================

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

## =============================================================================
## TABLE 1: H1 & H2 — Tier 1, Baseline Logit
## Ideology → MID initiation (H1); Ideology → targeting democracies (H2)
## Script: 03_h1_h2_logit.R
## Objects: h1_baseline, h1_controls, h1_full, h2_baseline, h2_controls, h2_full
## =============================================================================

cat("Building Table 1: H1 & H2 (Tier 1 baseline logit)...\n")

h1_models <- load_rds("h1_logit_models")
h2_models <- load_rds("h2_logit_models")

# Expected list elements: baseline (minimal), controls, full
h1_minimal  <- h1_models[["baseline"]]
h1_full_m   <- h1_models[["full"]]
h2_minimal  <- h2_models[["baseline"]]
h2_full_m   <- h2_models[["full"]]

if (!any(sapply(list(h1_minimal, h1_full_m, h2_minimal, h2_full_m), is.null))) {
        
        # Build column labels: (1) H1-Min, (2) H1-Full, (3) H2-Min, (4) H2-Full
        stargazer(
                h1_minimal, h1_full_m, h2_minimal, h2_full_m,
                type          = "latex",
                out           = file.path(tables_dir, "table1_h1_h2.tex"),
                title         = "Revisionist Ideology and Conflict Behavior (H1 \\& H2)",
                dep.var.labels = c(
                        "MID Initiation (H1)",
                        "Targets Democracy (H2)"
                ),
                dep.var.caption = "",
                column.labels  = c("Minimal", "Full Controls", "Minimal", "Full Controls"),
                column.separate = c(2, 2),
                covariate.labels = unname(covar_labels_logit[
                        names(covar_labels_logit) %in% unique(
                                c(names(coef(h1_minimal)), names(coef(h1_full_m)),
                                  names(coef(h2_minimal)), names(coef(h2_full_m)))
                        )
                ]),
                omit          = NULL,
                omit.stat     = c("f", "ser"),
                star.cutoffs  = c(0.05, 0.01, 0.001),
                notes         = "Bias-reduced logistic regression. Standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                notes.append  = FALSE,
                no.space      = TRUE,
                float         = TRUE,
                float.env     = "table",
                header        = FALSE
        )
        
        cat("  Saved: table1_h1_h2.tex\n")
}

## =============================================================================
## TABLE 2: H3 & H4 — Tier 1, Rational Autocrat Logit
## Support groups → MID initiation (H3); support groups → targeting (H4)
## Script: 04_h3_h4_logit.R
## Objects: h3_logit_models, h4_logit_models
## =============================================================================

cat("Building Table 2: H3 & H4 (Rational Autocrat logit)...\n")

h3_models <- load_rds("h3_logit_models")
h4_models <- load_rds("h4_logit_models")

h3_minimal <- h3_models[["baseline"]]
h3_full_m  <- h3_models[["full"]]
h4_minimal <- h4_models[["baseline"]]
h4_full_m  <- h4_models[["full"]]

if (!any(sapply(list(h3_minimal, h3_full_m, h4_minimal, h4_full_m), is.null))) {
        
        stargazer(
                h3_minimal, h3_full_m, h4_minimal, h4_full_m,
                type          = "latex",
                out           = file.path(tables_dir, "table2_h3_h4.tex"),
                title         = "Support Groups and Conflict Behavior (H3 \\& H4)",
                dep.var.labels = c(
                        "MID Initiation (H3)",
                        "Targets Democracy (H4)"
                ),
                dep.var.caption = "",
                column.labels  = c("Minimal", "Full Controls", "Minimal", "Full Controls"),
                column.separate = c(2, 2),
                covariate.labels = unname(covar_labels_logit[
                        names(covar_labels_logit) %in% unique(
                                c(names(coef(h3_minimal)), names(coef(h3_full_m)),
                                  names(coef(h4_minimal)), names(coef(h4_full_m)))
                        )
                ]),
                omit.stat     = c("f", "ser"),
                star.cutoffs  = c(0.05, 0.01, 0.001),
                notes         = "Bias-reduced logistic regression. Standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                notes.append  = FALSE,
                no.space      = TRUE,
                float         = TRUE,
                float.env     = "table",
                header        = FALSE
        )
        
        cat("  Saved: table2_h3_h4.tex\n")
}

## =============================================================================
## TABLE 3: H5 & H6 — Tier 2, Legitimation Mix
## Legitimation ratio → initiation (H5); → targeting (H6)
## Script: 05_h5_h6_legitmix.R
## Objects: h5_models, h6_models (may include hurdle components)
## =============================================================================

cat("Building Table 3: H5 & H6 (Legitimation Mix)...\n")

h5_models <- load_rds("h5_models")
h6_models <- load_rds("h6_models")

h5_minimal <- h5_models[["baseline"]]
h5_full_m  <- h5_models[["full"]]
h6_minimal <- h6_models[["baseline"]]
h6_full_m  <- h6_models[["full"]]

if (!any(sapply(list(h5_minimal, h5_full_m, h6_minimal, h6_full_m), is.null))) {
        
        # stargazer works for glm; if hurdle components are separate glm, pass them too
        # Add up to a 5th column for a hurdle component or robustness spec if needed
        h5_hurdle <- h5_models[["hurdle"]]  # NULL if absent
        col_models <- Filter(Negate(is.null), list(h5_minimal, h5_full_m, h6_minimal, h6_full_m, h5_hurdle))
        col_labels <- c("H5 Minimal", "H5 Full", "H6 Minimal", "H6 Full",
                        if (!is.null(h5_hurdle)) "H5 Hurdle" else NULL)
        
        stargazer(
                col_models,
                type          = "latex",
                out           = file.path(tables_dir, "table3_h5_h6.tex"),
                title         = "Legitimation Mix and Conflict Behavior (H5 \\& H6)",
                dep.var.labels = c("Conflict Initiation (H5)", "Targets Democracy (H6)"),
                dep.var.caption = "",
                column.labels  = col_labels,
                omit.stat     = c("f", "ser"),
                star.cutoffs  = c(0.05, 0.01, 0.001),
                notes         = "Bias-reduced logistic regression; hurdle model component where indicated. Standard errors in parentheses.",
                notes.append  = FALSE,
                no.space      = TRUE,
                float         = TRUE,
                float.env     = "table",
                header        = FALSE
        )
        
        cat("  Saved: table3_h5_h6.tex\n")
}

## =============================================================================
## TABLE 4: H7 & H8 — Mediation (SEM) and Moderation
## H7: ideology → support groups → targeting (mediation via lavaan/mediate)
## H8: ideology × dynamic leader → conflict (moderation)
## Scripts: 07_h7_mediation.R, 08_h8_moderation.R
##
## NOTE: mediate/lavaan objects are NOT stargazer-compatible.
##       We build a manual tidy table via modelsummary + custom rows.
## =============================================================================

cat("Building Table 4: H7 & H8 (Mediation/Moderation)...\n")

## -- H8 moderation: standard glm, use stargazer alone if H7 won't merge ------

h8_models <- load_rds("h8_models")
h8_minimal <- h8_models[["baseline"]]
h8_full_m  <- h8_models[["full"]]

## -- H7 mediation: pull ACME/ADE/total from mediate summary ------------------

build_mediation_rows <- function(med_obj, col_label) {
        s <- summary(med_obj)
        tibble(
                term      = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
                estimate  = c(s$d0,      s$z0,      s$tau.coef,    s$n0),
                ci_lo     = c(s$d0.ci[1], s$z0.ci[1], s$tau.ci[1], s$n0.ci[1]),
                ci_hi     = c(s$d0.ci[2], s$z0.ci[2], s$tau.ci[2], s$n0.ci[2]),
                col       = col_label
        )
}

h7_med_main    <- load_rds("h7_mediation_main")
h7_med_robust1 <- load_rds("h7_mediation_robust1")

h7_table <- NULL
if (!is.null(h7_med_main)) {
        rows <- build_mediation_rows(h7_med_main, "H7 Main")
        if (!is.null(h7_med_robust1)) {
                rows2 <- build_mediation_rows(h7_med_robust1, "H7 Robustness")
                rows  <- bind_rows(rows, rows2)
        }
        h7_table <- rows
}

## -- Option A: stargazer for H8 only, modelsummary for H7 separately ---------
## -- Option B: combined custom table (below) ----------------------------------

# H8 table via stargazer (moderation)
if (!any(sapply(list(h8_minimal, h8_full_m), is.null))) {
        stargazer(
                h8_minimal, h8_full_m,
                type          = "latex",
                out           = file.path(tables_dir, "table4b_h8_moderation.tex"),
                title         = "Dynamic Leadership Moderation (H8)",
                dep.var.labels = "Conflict Behavior",
                column.labels  = c("Minimal", "Full Controls"),
                omit.stat     = c("f", "ser"),
                star.cutoffs  = c(0.05, 0.01, 0.001),
                notes         = "Bias-reduced logistic regression with interaction term. Standard errors in parentheses.",
                notes.append  = FALSE,
                no.space      = TRUE,
                float         = TRUE,
                float.env     = "table",
                header        = FALSE
        )
        cat("  Saved: table4b_h8_moderation.tex\n")
}

# H7 table via knitr/kableExtra (mediation effects)
if (!is.null(h7_table)) {
        h7_wide <- h7_table |>
                mutate(
                        result = sprintf("%.3f [%.3f, %.3f]", estimate, ci_lo, ci_hi)
                ) |>
                select(term, col, result) |>
                tidyr::pivot_wider(names_from = col, values_from = result)
        
        kbl_out <- knitr::kable(
                h7_wide,
                format  = "latex",
                booktabs = TRUE,
                caption = "Mediation Analysis: Support Group Mediation of Ideology on Targeting (H7)",
                col.names = c("Quantity", names(h7_wide)[-1]),
                linesep = ""
        ) |>
                kableExtra::kable_styling(latex_options = c("hold_position")) |>
                kableExtra::footnote(
                        general = "Point estimates with 95\\\\% confidence intervals (nonparametric bootstrap). ACME = average causal mediation effect; ADE = average direct effect.",
                        threeparttable = TRUE,
                        escape = FALSE
                )
        
        writeLines(as.character(kbl_out), file.path(tables_dir, "table4a_h7_mediation.tex"))
        cat("  Saved: table4a_h7_mediation.tex\n")
}

## -- Combined H7 + H8 attempt using modelsummary ----------------------------
## modelsummary handles both glm and mediate-like objects if add_rows is used.
## This produces a single tex file; comment out if separate files preferred.

if (!any(sapply(list(h8_minimal, h8_full_m), is.null)) && !is.null(h7_table)) {
        
        # Extra rows for mediation effects appended below H8 model rows
        extra_rows <- h7_table |>
                filter(col == "H7 Main") |>
                mutate(
                        `H7 Main`      = sprintf("%.3f [%.3f, %.3f]", estimate, ci_lo, ci_hi),
                        `H8 Minimal`   = "",
                        `H8 Full`      = ""
                ) |>
                select(term, `H7 Main`, `H8 Minimal`, `H8 Full`) |>
                rename(` ` = term)
        
        attr(extra_rows, "position") <- c(99, 100, 101, 102)  # append at bottom
        
        modelsummary(
                models       = list(
                        "H7 Main"    = h7_med_main,
                        "H8 Minimal" = h8_minimal,
                        "H8 Full"    = h8_full_m
                ),
                output       = file.path(tables_dir, "table4_h7_h8_combined.tex"),
                stars        = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
                fmt          = 3,
                statistic    = "conf.int",
                conf_level   = 0.95,
                add_rows     = extra_rows,
                title        = "Mediation (H7) and Moderation (H8)",
                notes        = list(
                        "H7: mediation effects with 95\\% CI (bootstrap).",
                        "H8: bias-reduced logit with interaction; 95\\% CI in brackets."
                ),
                gof_omit     = "AIC|BIC|Log|RMSE|F|Std",
                escape       = FALSE
        )
        
        cat("  Saved: table4_h7_h8_combined.tex\n")
}

## =============================================================================
## TABLE 5: H9 — Survival (Cox PH)
## Conflict initiation → leader survival (H9)
## Script: 06_h9_survival.R
## Objects: cox_h9_ideology, cox_h9_ratio, cox_h9_int_ideology, cox_h9_int_ratio
## =============================================================================

cat("Building Table 5: H9 (Survival / Cox PH)...\n")

h9_models <- load_rds("h9_survival_models")

cox_ideology     <- h9_models[["cox_h9_ideology"]]
cox_ratio        <- h9_models[["cox_h9_ratio"]]
cox_int_ideology <- h9_models[["cox_h9_int_ideology"]]
cox_int_ratio    <- h9_models[["cox_h9_int_ratio"]]

cox_list <- Filter(Negate(is.null),
                   list(
                           "Ideology"          = cox_ideology,
                           "Ratio"             = cox_ratio,
                           "Ideo × Conflict"   = cox_int_ideology,
                           "Ratio × Conflict"  = cox_int_ratio
                   )
)

if (length(cox_list) > 0) {
        
        # stargazer supports coxph
        stargazer(
                cox_list,
                type          = "latex",
                out           = file.path(tables_dir, "table5_h9_survival.tex"),
                title         = "Leader Survival and Conflict Signaling (H9)",
                dep.var.labels = "Leader Tenure (Hazard)",
                column.labels  = names(cox_list),
                omit.stat     = c("max.rsq", "lr"),
                star.cutoffs  = c(0.05, 0.01, 0.001),
                apply.coef    = exp,          # report hazard ratios
                apply.ci      = exp,
                notes         = "Cox proportional hazard models. Coefficients are hazard ratios (exponentiated). 95\\% CI in brackets.",
                notes.append  = FALSE,
                no.space      = TRUE,
                float         = TRUE,
                float.env     = "table",
                header        = FALSE
        )
        
        cat("  Saved: table5_h9_survival.tex\n")
}

## =============================================================================
## Output summary
## =============================================================================

cat("\n=== Table generation complete ===\n")
cat("Files saved to:", tables_dir, "\n")
cat("  table1_h1_h2.tex           — H1 & H2 (Tier 1 baseline)\n")
cat("  table2_h3_h4.tex           — H3 & H4 (Rational Autocrat)\n")
cat("  table3_h5_h6.tex           — H5 & H6 (Legitimation Mix)\n")
cat("  table4a_h7_mediation.tex   — H7 (Mediation, kableExtra)\n")
cat("  table4b_h8_moderation.tex  — H8 (Moderation, stargazer)\n")
cat("  table4_h7_h8_combined.tex  — H7 + H8 combined (modelsummary)\n")
cat("  table5_h9_survival.tex     — H9 (Cox PH survival)\n")