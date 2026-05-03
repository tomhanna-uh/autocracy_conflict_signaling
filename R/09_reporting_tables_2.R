# =============================================================================
# 09_reporting_tables.R
# Journal-style regression tables organized by hypothesis tier
# RDS names and list element names matched to 99_make_all_plots.R
# =============================================================================

library(here)
source(here::here("R", "00_packages.R"))
suppressPackageStartupMessages({
        library(stargazer)
        library(modelsummary)
        library(broom)
        library(dplyr)
        library(tibble)
        library(tidyr)
        library(knitr)
        library(kableExtra)
})

results_dir <- here::here("results")
tables_dir  <- here::here("results", "tables")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

load_rds <- function(name) {
        path <- file.path(results_dir, paste0(name, ".rds"))
        if (file.exists(path)) {
                readRDS(path)
        } else {
                warning("RDS not found: ", path)
                NULL
        }
}

# Utility: safely extract up to 2 models (minimal/full) from a named list
# Returns a named list; skips NULL entries
pick_models <- function(mod_list, minimal_key, full_key) {
        out <- list()
        if (!is.null(mod_list[[minimal_key]])) out[["Minimal"]]      <- mod_list[[minimal_key]]
        if (!is.null(mod_list[[full_key]]))    out[["Full Controls"]] <- mod_list[[full_key]]
        out
}

## =============================================================================
## TABLE 1: H1 & H2
## RDS: h1_logit_models.rds, h2_logit_models.rds
## List elements (from plots script): iterate over names — first = minimal, last = full
## =============================================================================

cat("Building Table 1: H1 & H2...\n")

h1_models <- load_rds("h1_logit_models")
h2_models <- load_rds("h2_logit_models")

if (!is.null(h1_models) && !is.null(h2_models)) {
        
        # Take first and last model from each list as minimal/full
        h1_nms <- names(h1_models)
        h2_nms <- names(h2_models)
        
        col_models <- Filter(Negate(is.null), list(
                h1_models[[h1_nms[1]]],
                h1_models[[h1_nms[length(h1_nms)]]],
                h2_models[[h2_nms[1]]],
                h2_models[[h2_nms[length(h2_nms)]]]
        ))
        
        col_labels <- c(
                paste0("H1 ", h1_nms[1]),
                paste0("H1 ", h1_nms[length(h1_nms)]),
                paste0("H2 ", h2_nms[1]),
                paste0("H2 ", h2_nms[length(h2_nms)])
        )
        
        stargazer(
                col_models,
                type            = "latex",
                out             = file.path(tables_dir, "table1_h1_h2.tex"),
                title           = "Revisionist Ideology and Conflict Behavior (H1 \\& H2)",
                dep.var.labels  = c("MID Initiation (H1)", "Targets Democracy (H2)"),
                dep.var.caption = "",
                column.labels   = col_labels,
                column.separate = c(2, 2),
                omit.stat       = c("f", "ser"),
                star.cutoffs    = c(0.05, 0.01, 0.001),
                notes           = "Bias-reduced logistic regression. Standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                notes.append    = FALSE,
                no.space        = TRUE,
                float           = TRUE,
                float.env       = "table",
                header          = FALSE
        )
        cat("  Saved: table1_h1_h2.tex\n")
}

## =============================================================================
## TABLE 2: H3 & H4
## RDS: h3_logit_models.rds, h4_logit_models.rds
## =============================================================================

cat("Building Table 2: H3 & H4...\n")

h3_models <- load_rds("h3_logit_models")
h4_models <- load_rds("h4_logit_models")

if (!is.null(h3_models) && !is.null(h4_models)) {
        
        h3_nms <- names(h3_models)
        h4_nms <- names(h4_models)
        
        col_models <- Filter(Negate(is.null), list(
                h3_models[[h3_nms[1]]],
                h3_models[[h3_nms[length(h3_nms)]]],
                h4_models[[h4_nms[1]]],
                h4_models[[h4_nms[length(h4_nms)]]]
        ))
        
        col_labels <- c(
                paste0("H3 ", h3_nms[1]),
                paste0("H3 ", h3_nms[length(h3_nms)]),
                paste0("H4 ", h4_nms[1]),
                paste0("H4 ", h4_nms[length(h4_nms)])
        )
        
        stargazer(
                col_models,
                type            = "latex",
                out             = file.path(tables_dir, "table2_h3_h4.tex"),
                title           = "Support Groups and Conflict Behavior (H3 \\& H4)",
                dep.var.labels  = c("MID Initiation (H3)", "Targets Democracy (H4)"),
                dep.var.caption = "",
                column.labels   = col_labels,
                column.separate = c(2, 2),
                omit.stat       = c("f", "ser"),
                star.cutoffs    = c(0.05, 0.01, 0.001),
                notes           = "Bias-reduced logistic regression. Standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                notes.append    = FALSE,
                no.space        = TRUE,
                float           = TRUE,
                float.env       = "table",
                header          = FALSE
        )
        cat("  Saved: table2_h3_h4.tex\n")
}

## =============================================================================
## TABLE 3: H5 & H6
## RDS: h5_models.rds, h6_models.rds
## =============================================================================
## =============================================================================
## TABLE 3: H5 & H6
## RDS: h5_logit_models.rds, h5_hurdle_models.rds, h6_logit_models.rds
## =============================================================================

cat("Building Table 3: H5 & H6...\n")

h5_models        <- load_rds("h5_logit_models")
h5_hurdle_models <- load_rds("h5_hurdle_models")
h6_models        <- load_rds("h6_logit_models")

if (!is.null(h5_models) && !is.null(h6_models)) {
        
        h5_nms <- names(h5_models)
        h6_nms <- names(h6_models)
        
        # H5 minimal, H5 full, H5 hurdle (if exists), H6 minimal, H6 full — max 5 cols
        h5_hurdle_col <- if (!is.null(h5_hurdle_models)) h5_hurdle_models[[names(h5_hurdle_models)[1]]] else NULL
        
        col_models <- Filter(Negate(is.null), list(
                h5_models[[h5_nms[1]]],
                h5_models[[h5_nms[length(h5_nms)]]],
                h5_hurdle_col,
                h6_models[[h6_nms[1]]],
                h6_models[[h6_nms[length(h6_nms)]]]
        ))
        
        col_labels <- c(
                paste0("H5 ", h5_nms[1]),
                paste0("H5 ", h5_nms[length(h5_nms)]),
                if (!is.null(h5_hurdle_col)) "H5 Hurdle" else NULL,
                paste0("H6 ", h6_nms[1]),
                paste0("H6 ", h6_nms[length(h6_nms)])
        )
        
        stargazer(
                col_models,
                type            = "latex",
                out             = file.path(tables_dir, "table3_h5_h6.tex"),
                title           = "Legitimation Mix and Conflict Behavior (H5 \\& H6)",
                dep.var.labels  = c("Conflict Initiation (H5)", "Targets Democracy (H6)"),
                dep.var.caption = "",
                column.labels   = col_labels,
                column.separate = c(3, 2),
                omit            = "t_scaled",          # drop empty spline/penalty term
                omit.labels     = "Time spline",
                omit.stat       = c("f", "ser"),
                star.cutoffs    = c(0.05, 0.01, 0.001),
                notes           = "Bias-reduced logistic regression. Standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                notes.append    = FALSE,
                no.space        = TRUE,
                float           = TRUE,
                float.env       = "table",
                header          = FALSE
        )
        cat("  Saved: table3_h5_h6.tex\n")
}

## =============================================================================
## TABLE 4a: H7 — Mediation
## RDS: h7_mediation.rds (single mediate object or named list)
##      h7_components.rds (underlying mediator/outcome regression models)
##      m1_mediation.rds, m1_robustness.rds, m2_mediation.rds
## =============================================================================

# Component regression models — Baron-Kenny a/b paths
# NOTE: models are stripped (no $model/$data) so vcov() fails;
# coefficients and summary() are intact and used directly.
h7_components <- load_rds("h7_components")

if (!is.null(h7_components) && is.list(h7_components)) {
        
        safe_tidy_stripped <- function(m, model_name) {
                tryCatch({
                        sm     <- summary(m)$coefficients
                        co     <- sm[, "Estimate"]
                        se     <- sm[, "Std. Error"]
                        # p-value column differs between lm and glm
                        pv_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(sm))[1]
                        pv     <- sm[, pv_col]
                        # CI from normal approximation since confint() needs vcov
                        z95    <- qnorm(0.975)
                        tibble(
                                model    = model_name,
                                term     = rownames(sm),
                                estimate = co,
                                std_err  = se,
                                ci_lo    = co - z95 * se,
                                ci_hi    = co + z95 * se,
                                p_value  = pv
                        )
                }, error = function(e) {
                        cat("  Could not extract", model_name, ":", e$message, "\n")
                        NULL
                })
        }
        
        # Only the 6 component models (exclude conflict_data if present)
        model_nms  <- intersect(
                c("a_religious","b_religious","a_party","b_party","a_military","b_military"),
                names(h7_components)
        )
        comp_subset <- h7_components[model_nms]
        
        rows <- purrr::map2(comp_subset, names(comp_subset), safe_tidy_stripped) |>
                purrr::compact() |>
                dplyr::bind_rows()
        
        if (nrow(rows) > 0) {
                
                var_labels <- c(
                        "(Intercept)"                  = "Constant",
                        "sidea_revisionist_domestic"   = "Revisionist Ideology",
                        "sidea_religious_support"      = "Religious Support",
                        "sidea_party_elite_support"    = "Party/Elite Support",
                        "sidea_military_support"       = "Military Support",
                        "cinc_a"                       = "Capabilities (CINC)",
                        "sidea_winning_coalition_size" = "Winning Coalition Size",
                        "cold_war"                     = "Cold War",
                        "t_scaled"                     = "Time (scaled)"
                )
                
                rows <- rows |>
                        dplyr::mutate(
                                stars = dplyr::case_when(
                                        p_value < 0.001 ~ "$^{***}$",
                                        p_value < 0.01  ~ "$^{**}$",
                                        p_value < 0.05  ~ "$^{*}$",
                                        TRUE            ~ ""
                                ),
                                cell       = paste0(sprintf("%.3f", estimate), stars,
                                                    " \\\\ (", sprintf("%.3f", std_err), ")"),
                                term_clean = dplyr::coalesce(var_labels[term], gsub("_", " ", term))
                        )
                
                # Ordered terms for rows (intercept last)
                term_order <- c(
                        "sidea_revisionist_domestic", "sidea_religious_support",
                        "sidea_party_elite_support",  "sidea_military_support",
                        "cinc_a", "sidea_winning_coalition_size", "cold_war", "t_scaled",
                        "(Intercept)"
                )
                term_order <- intersect(term_order, unique(rows$term))
                
                rows <- rows |>
                        dplyr::mutate(term = factor(term, levels = term_order)) |>
                        dplyr::arrange(term)
                
                wide <- rows |>
                        dplyr::select(term_clean, model, cell) |>
                        tidyr::pivot_wider(
                                names_from  = model,
                                values_from = cell,
                                values_fill = ""
                        ) |>
                        # Restore display order after pivot
                        dplyr::mutate(term_clean = factor(term_clean,
                                                          levels = unique(rows$term_clean))) |>
                        dplyr::arrange(term_clean) |>
                        dplyr::mutate(term_clean = as.character(term_clean))
                
                # GOF rows using nobs() and logLik() — safe on stripped models
                gof <- purrr::map2_dfr(comp_subset, names(comp_subset), function(m, nm) {
                        tryCatch(
                                tibble(
                                        model = nm,
                                        N     = formatC(nobs(m), format = "d", big.mark = ","),
                                        logL  = sprintf("%.1f", as.numeric(logLik(m)))
                                ),
                                error = function(e) tibble(model = nm, N = "", logL = "")
                        )
                })
                
                make_gof_row <- function(gof_df, val_col, label) {
                        gof_df |>
                                dplyr::select(model, value = !!val_col) |>
                                tidyr::pivot_wider(names_from = model, values_from = value) |>
                                dplyr::mutate(term_clean = label) |>
                                dplyr::select(names(wide))
                }
                
                final_table <- dplyr::bind_rows(
                        wide,
                        make_gof_row(gof, "N",    "N"),
                        make_gof_row(gof, "logL", "Log Likelihood")
                )
                
                kbl_out <- knitr::kable(
                        final_table,
                        format    = "latex",
                        booktabs  = TRUE,
                        caption   = "Baron-Kenny Component Regressions for H7 Mediation Analysis",
                        col.names = c(" ", model_nms),
                        linesep   = "",
                        escape    = FALSE,
                        align     = c("l", rep("c", length(model_nms)))
                ) |>
                        kableExtra::kable_styling(
                                latex_options = c("hold_position", "scale_down"),
                                font_size     = 9
                        ) |>
                        kableExtra::add_header_above(c(
                                " "          = 1,
                                "Religious"  = 2,
                                "Party"      = 2,
                                "Military"   = 2
                        )) |>
                        kableExtra::add_header_above(c(
                                " "                          = 1,
                                "Path a (IV $\\\\to$ Med)"   = 3,
                                "Path b (Med $\\\\to$ DV)"   = 3
                        )) |>
                        kableExtra::footnote(
                                general        = "Path a: OLS, mediator regressed on IV and controls. Path b: logistic regression, DV regressed on mediator, IV, and controls. Models stripped of fitted values for storage; SEs from summary coefficients, 95\\\\% CIs normal approximation. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                                threeparttable = TRUE,
                                escape         = FALSE
                        )
                
                writeLines(as.character(kbl_out), file.path(tables_dir, "table4a_h7_components.tex"))
                cat("  Saved: table4a_h7_components.tex\n")
        }
}

# Also fix h7_mediation list element names to match actual saved names:
# saved as: med_h7_religious, fit_h7_party, fit_h7_military
h7_mediation <- load_rds("h7_mediation")

med_rows <- list()
if (!is.null(h7_mediation)) {
        med_name_map <- list(
                "H7 Religious" = "med_h7_religious",
                "H7 Party"     = "fit_h7_party",
                "H7 Military"  = "fit_h7_military"
        )
        for (display_nm in names(med_name_map)) {
                key <- med_name_map[[display_nm]]
                obj <- h7_mediation[[key]]
                if (!is.null(obj) && inherits(obj, "mediate")) {
                        med_rows[[display_nm]] <- build_mediation_rows(obj, display_nm)
                }
        }
}

# Add m1/m2 mediation
m1_mediation  <- load_rds("m1_mediation")
m1_robustness <- load_rds("m1_robustness")
m2_mediation  <- load_rds("m2_mediation")

if (!is.null(m1_mediation))
        med_rows[["M1 Main"]] <- build_mediation_rows(m1_mediation, "M1 Main")
if (!is.null(m1_robustness)) {
        if (!is.null(m1_robustness$med_democracy))
                med_rows[["M1 Democracy"]] <- build_mediation_rows(m1_robustness$med_democracy, "M1 Democracy")
        if (!is.null(m1_robustness$med_ideo_gap))
                med_rows[["M1 Ideo Gap"]]  <- build_mediation_rows(m1_robustness$med_ideo_gap,  "M1 Ideo Gap")
}
if (!is.null(m2_mediation))
        med_rows[["M2 Main"]] <- build_mediation_rows(m2_mediation, "M2 Main")

if (length(med_rows) > 0) {
        
        h7_wide <- dplyr::bind_rows(med_rows) |>
                dplyr::mutate(result = sprintf("%.3f [%.3f, %.3f]", estimate, ci_lo, ci_hi)) |>
                dplyr::select(term, col, result) |>
                tidyr::pivot_wider(names_from = col, values_from = result)
        
        kbl_out <- knitr::kable(
                h7_wide,
                format    = "latex",
                booktabs  = TRUE,
                caption   = "Mediation Analysis: Ideology, Support Groups, and Conflict Targeting (H7)",
                col.names = c("Quantity", names(h7_wide)[-1]),
                linesep   = ""
        ) |>
                kableExtra::kable_styling(latex_options = c("hold_position")) |>
                kableExtra::add_header_above(c(
                        " "     = 1,
                        "H7 Baron-Kenny" = length(med_name_map),
                        "M1/M2 Mediation" = length(med_rows) - length(med_name_map)
                )) |>
                kableExtra::footnote(
                        general        = "Point estimates with 95\\\\% confidence intervals (nonparametric bootstrap, 1000 simulations). ACME = average causal mediation effect; ADE = average direct effect.",
                        threeparttable = TRUE,
                        escape         = FALSE
                )
        
        writeLines(as.character(kbl_out), file.path(tables_dir, "table4a_h7_mediation.tex"))
        cat("  Saved: table4a_h7_mediation.tex\n")
}

## =============================================================================
## TABLE 4b: H8 — Moderation
## RDS: h8_init_models.rds (initiation DV), h8_target_models.rds (targeting DV)
## =============================================================================

## =============================================================================
## TABLE 4b: H8 — Moderation (stripped brglm2/cloglog models, manual extraction)
## RDS: h8_init_models.rds  → h8_init_main, h8_init_int, h8_init_lpm_int
##      h8_target_models.rds → h8_target_main, h8_target_int, h8_target_lpm_int
## =============================================================================

## =============================================================================
## TABLE 4b: H8 — Moderation (stripped brglm2/cloglog models, manual extraction)
## RDS: h8_init_models.rds  → h8_init_main, h8_init_int, h8_init_lpm_int
##      h8_target_models.rds → h8_target_main, h8_target_int, h8_target_lpm_int
## =============================================================================

cat("Building Table 4b: H8 moderation...\n")

h8_init   <- load_rds("h8_init_models")
h8_target <- load_rds("h8_target_models")

if (!is.null(h8_init) || !is.null(h8_target)) {
        
        # Reuse safe_tidy_stripped defined in H7 block above
        # Column selection: main (no interaction) + interaction, for each DV
        # Drop LPM columns from main table (those go in appendix via 09b)
        h8_cols <- list(
                "Init: Main"        = h8_init[["h8_init_main"]],
                "Init: Interaction" = h8_init[["h8_init_int"]],
                "Target: Main"      = h8_target[["h8_target_main"]],
                "Target: Interaction" = h8_target[["h8_target_int"]]
        )
        h8_cols <- Filter(Negate(is.null), h8_cols)
        
        if (length(h8_cols) > 0) {
                
                rows <- purrr::map2(h8_cols, names(h8_cols), safe_tidy_stripped) |>
                        purrr::compact() |>
                        dplyr::bind_rows()
                
                if (nrow(rows) > 0) {
                        
                        var_labels_h8 <- c(
                                "(Intercept)"                        = "Constant",
                                "legit_ratio"                        = "Legitimation Ratio",
                                "sidea_dynamic_leader"               = "Dynamic Leadership",
                                "sidea_winning_coalition_size"       = "Winning Coalition Size",
                                "legit_ratio:sidea_dynamic_leader"   = "Legitimation Ratio $\\times$ Dynamic Leadership"
                        )
                        
                        # Term display order
                        term_order_h8 <- c(
                                "legit_ratio",
                                "sidea_dynamic_leader",
                                "legit_ratio:sidea_dynamic_leader",
                                "sidea_winning_coalition_size",
                                "(Intercept)"
                        )
                        term_order_h8 <- intersect(term_order_h8, unique(rows$term))
                        
                        rows <- rows |>
                                dplyr::mutate(
                                        stars = dplyr::case_when(
                                                p_value < 0.001 ~ "$^{***}$",
                                                p_value < 0.01  ~ "$^{**}$",
                                                p_value < 0.05  ~ "$^{*}$",
                                                TRUE            ~ ""
                                        ),
                                        cell       = paste0(sprintf("%.3f", estimate), stars,
                                                            " \\\\ (", sprintf("%.3f", std_err), ")"),
                                        term_clean = dplyr::coalesce(var_labels_h8[term], gsub("_", " ", term)),
                                        term       = factor(term, levels = term_order_h8)
                                ) |>
                                dplyr::arrange(term)
                        
                        wide <- rows |>
                                dplyr::select(term_clean, model, cell) |>
                                tidyr::pivot_wider(
                                        names_from  = model,
                                        values_from = cell,
                                        values_fill = ""
                                ) |>
                                dplyr::mutate(term_clean = factor(term_clean,
                                                                  levels = unique(rows$term_clean))) |>
                                dplyr::arrange(term_clean) |>
                                dplyr::mutate(term_clean = as.character(term_clean))
                        
                        # GOF rows
                        gof_h8 <- purrr::map2_dfr(h8_cols, names(h8_cols), function(m, nm) {
                                tryCatch(
                                        tibble(
                                                model = nm,
                                                N     = formatC(nobs(m), format = "d", big.mark = ","),
                                                logL  = sprintf("%.1f", as.numeric(logLik(m)))
                                        ),
                                        error = function(e) tibble(model = nm, N = "", logL = "")
                                )
                        })
                        
                        make_gof_row_h8 <- function(gof_df, val_col, label) {
                                gof_df |>
                                        dplyr::select(model, value = !!val_col) |>
                                        tidyr::pivot_wider(names_from = model, values_from = value) |>
                                        dplyr::mutate(term_clean = label) |>
                                        dplyr::select(names(wide))
                        }
                        
                        final_h8 <- dplyr::bind_rows(
                                wide,
                                make_gof_row_h8(gof_h8, "N",    "N"),
                                make_gof_row_h8(gof_h8, "logL", "Log Likelihood")
                        )
                        
                        # Column grouping header
                        n_init   <- sum(grepl("^Init",   names(h8_cols)))
                        n_target <- sum(grepl("^Target", names(h8_cols)))
                        
                        kbl_out <- knitr::kable(
                                final_h8,
                                format    = "latex",
                                booktabs  = TRUE,
                                caption   = "Dynamic Leadership Moderation of Ideology Effect (H8)",
                                col.names = c(" ", names(h8_cols)),
                                linesep   = "",
                                escape    = FALSE,
                                align     = c("l", rep("c", length(h8_cols)))
                        ) |>
                                kableExtra::kable_styling(
                                        latex_options = c("hold_position", "scale_down"),
                                        font_size     = 9
                                ) |>
                                kableExtra::add_header_above(c(
                                        " "                   = 1,
                                        "Conflict Initiation" = n_init,
                                        "Targets Democracy"   = n_target
                                )) |>
                                kableExtra::footnote(
                                        general        = paste0(
                                                "Initiation models: complementary log-log (cloglog) link, Cold War period (1960--1989). ",
                                                "Targeting models: bias-reduced logistic regression, conflict dyads only. ",
                                                "All models stripped of fitted values; SEs from summary coefficients, 95\\\\% CIs normal approximation. ",
                                                "$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001."
                                        ),
                                        threeparttable = TRUE,
                                        escape         = FALSE
                                )
                        
                        writeLines(as.character(kbl_out), file.path(tables_dir, "table4b_h8_moderation.tex"))
                        cat("  Saved: table4b_h8_moderation.tex\n")
                }
        }
}


## =============================================================================
## TABLE 5: H9 — Survival (Cox PH)
## RDS: h9_survival.rds (named list of coxph objects)
## =============================================================================

## =============================================================================
## TABLE 5: H9 — Survival (Cox PH)
## =============================================================================

cat("Building Table 5: H9 survival...\n")

h9_models <- load_rds("h9_survival")

if (!is.null(h9_models) && is.list(h9_models)) {
        
        main_cox <- Filter(Negate(is.null), list(
                "Direct"   = h9_models[["cox_h9_direct"]],
                "Ideology" = h9_models[["cox_h9_ideology"]],
                "Ratio"    = h9_models[["cox_h9_ratio"]],
                "Full"     = h9_models[["cox_h9_full"]]
        ))
        
        int_cox <- Filter(Negate(is.null), list(
                "Ideo_x_Conflict"  = h9_models[["cox_h9_int_ideology"]],
                "Ratio_x_Conflict" = h9_models[["cox_h9_int_ratio"]]
        ))
        int_cox_display <- c(
                "Ideo_x_Conflict"  = "Ideo $\\times$ Conflict",
                "Ratio_x_Conflict" = "Ratio $\\times$ Conflict"
        )
        
        # ---------------------------------------------------------------------------
        # Table 5a: main models via stargazer
        # ---------------------------------------------------------------------------
        if (length(main_cox) > 0) {
                stargazer(
                        main_cox,
                        type             = "latex",
                        out              = file.path(tables_dir, "table5a_h9_main.tex"),
                        title            = "Leader Survival and Conflict Signaling (H9)",
                        dep.var.labels   = "Leader Tenure (Hazard Ratio)",
                        dep.var.caption  = "",
                        column.labels    = names(main_cox),
                        apply.coef       = exp,
                        apply.ci         = exp,
                        omit.stat        = c("max.rsq", "lr"),
                        star.cutoffs     = c(0.05, 0.01, 0.001),
                        covariate.labels = c(
                                "Conflicts vs. Ideo. Targets",
                                "Avg. Ideological Legitimation",
                                "Legitimation Ratio",
                                "Avg. Autocracy Level",
                                "Log Avg. GDP",
                                "Log Avg. Population",
                                "Irregular Entry"
                        ),
                        notes        = "Cox proportional hazard models. Coefficients are hazard ratios (exponentiated). Standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                        notes.append = FALSE,
                        no.space     = TRUE,
                        float        = TRUE,
                        float.env    = "table",
                        header       = FALSE
                )
                cat("  Saved: table5a_h9_main.tex\n")
        }
        
        # ---------------------------------------------------------------------------
        # Table 5b: interaction models — base R throughout
        # ---------------------------------------------------------------------------
        if (length(int_cox) > 0) {
                
                safe_tidy_cox <- function(m, model_name) {
                        tryCatch({
                                sm    <- summary(m)$coefficients
                                co    <- sm[, "coef"]
                                se    <- sm[, "se(coef)"]
                                pv    <- sm[, "Pr(>|z|)"]
                                hr    <- exp(co)
                                hr_lo <- exp(co - qnorm(0.975) * se)
                                hr_hi <- exp(co + qnorm(0.975) * se)
                                stars <- ifelse(pv < 0.001, "$^{***}$",
                                                ifelse(pv < 0.01,  "$^{**}$",
                                                       ifelse(pv < 0.05,  "$^{*}$", "")))
                                cell  <- paste0(sprintf("%.3f", hr), stars,
                                                " [", sprintf("%.3f", hr_lo),
                                                ", ", sprintf("%.3f", hr_hi), "]")
                                data.frame(
                                        model      = model_name,
                                        term       = rownames(sm),
                                        cell       = cell,
                                        stringsAsFactors = FALSE
                                )
                        }, error = function(e) {
                                cat("  Could not extract", model_name, ":", e$message, "\n")
                                NULL
                        })
                }
                
                rows_list <- mapply(safe_tidy_cox,
                                    int_cox, names(int_cox),
                                    SIMPLIFY = FALSE)
                rows_list <- Filter(Negate(is.null), rows_list)
                
                if (length(rows_list) > 0) {
                        
                        rows_int <- do.call(rbind, rows_list)
                        
                        var_labels_h9 <- c(
                                "conflicts_vs_democracy"                       = "Conflicts vs. Democracy",
                                "avg_ideological_legit"                        = "Avg. Ideological Legitimation",
                                "legit_ratio"                                  = "Legitimation Ratio",
                                "conflicts_vs_democracy:avg_ideological_legit" = "Conflicts $\\times$ Ideo. Legit.",
                                "conflicts_vs_democracy:legit_ratio"           = "Conflicts $\\times$ Legit. Ratio",
                                "avg_autocracy_level"                          = "Avg. Autocracy Level",
                                "log_avg_gdp"                                  = "Log Avg. GDP",
                                "log_avg_pop"                                  = "Log Avg. Population",
                                "irregular_entry"                              = "Irregular Entry"
                        )
                        
                        term_order_h9 <- c(
                                "conflicts_vs_democracy",
                                "avg_ideological_legit",
                                "legit_ratio",
                                "conflicts_vs_democracy:avg_ideological_legit",
                                "conflicts_vs_democracy:legit_ratio",
                                "avg_autocracy_level",
                                "log_avg_gdp",
                                "log_avg_pop",
                                "irregular_entry"
                        )
                        term_order_h9 <- intersect(term_order_h9, unique(rows_int$term))
                        
                        rows_int$term_clean <- ifelse(
                                rows_int$term %in% names(var_labels_h9),
                                var_labels_h9[rows_int$term],
                                gsub("_", " ", rows_int$term)
                        )
                        rows_int$term <- factor(rows_int$term, levels = term_order_h9)
                        rows_int <- rows_int[order(rows_int$term), ]
                        
                        # Pivot to wide using base reshape
                        wide_int <- reshape(
                                rows_int[, c("term_clean", "model", "cell")],
                                idvar     = "term_clean",
                                timevar   = "model",
                                direction = "wide"
                        )
                        # reshape() prefixes cell. onto column names — strip that
                        names(wide_int) <- sub("^cell\\.", "", names(wide_int))
                        # Enforce column order: term_clean first, then model names
                        model_cols <- names(int_cox)
                        wide_int   <- wide_int[, c("term_clean", model_cols), drop = FALSE]
                        # Restore display order
                        wide_int <- wide_int[match(unique(rows_int$term_clean), wide_int$term_clean), ]
                        rownames(wide_int) <- NULL
                        
                        # GOF rows — base R only
                        get_gof <- function(m, nm) {
                                N_val <- tryCatch(
                                        formatC(m$n, format = "d", big.mark = ","),
                                        error = function(e) ""
                                )
                                logL_val <- tryCatch(
                                        sprintf("%.3f", m$loglik[2]),
                                        error = function(e) ""
                                )
                                wald_val <- tryCatch(
                                        sprintf("%.3f (df = %d)",
                                                summary(m)$waldtest["test"],
                                                summary(m)$waldtest["df"]),
                                        error = function(e) ""
                                )
                                list(model = nm, N = N_val, logL = logL_val, wald = wald_val)
                        }
                        
                        gof_vals <- mapply(get_gof, int_cox, names(int_cox), SIMPLIFY = FALSE)
                        
                        make_gof_row <- function(field, label) {
                                row <- data.frame(term_clean = label, stringsAsFactors = FALSE)
                                for (g in gof_vals) {
                                        v <- g[[field]]
                                        row[[g$model]] <- if (length(v) == 1) v else ""
                                }
                                for (col in setdiff(c("term_clean", model_cols), names(row))) {
                                        row[[col]] <- ""
                                }
                                row[, c("term_clean", model_cols), drop = FALSE]
                        }
                        
                        final_int <- rbind(
                                wide_int,
                                make_gof_row("N",    "N"),
                                make_gof_row("logL", "Log Likelihood"),
                                make_gof_row("wald", "Wald Test")
                        )
                        rownames(final_int) <- NULL
                        
                        kbl_int <- knitr::kable(
                                final_int,
                                format    = "latex",
                                booktabs  = TRUE,
                                caption   = "Leader Survival: Ideology--Conflict Interaction Models (H9)",
                                col.names = c(" ", unname(int_cox_display[model_cols])),
                                linesep   = "",
                                escape    = FALSE,
                                align     = c("l", rep("c", length(model_cols)))
                        ) |>
                                kableExtra::kable_styling(
                                        latex_options = c("hold_position"),
                                        font_size     = 10
                                ) |>
                                kableExtra::footnote(
                                        general        = "Cox proportional hazard models. Coefficients are hazard ratios (exponentiated) with 95\\\\% CI in brackets. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                                        threeparttable = TRUE,
                                        escape         = FALSE
                                )
                        
                        writeLines(as.character(kbl_int), file.path(tables_dir, "table5b_h9_interactions.tex"))
                        cat("  Saved: table5b_h9_interactions.tex\n")
                }
        }
}

## =============================================================================
## TABLE 6: LPM Appendix
## RDS: lpm_appendix_models.rds
## =============================================================================

cat("Building Table 6: LPM appendix...\n")

lpm_models <- load_rds("lpm_appendix_models")

if (!is.null(lpm_models) && is.list(lpm_models)) {
        
        cat("  LPM model elements:\n")
        for (nm in names(lpm_models)) {
                cat("   ", nm, ":", class(lpm_models[[nm]]), "\n")
        }
        
        # Extract coefficients from each model using base R summary()
        safe_tidy_lpm <- function(m, model_name) {
                tryCatch({
                        sm     <- summary(m)$coefficients
                        co     <- sm[, "Estimate"]
                        se     <- sm[, "Std. Error"]
                        pv_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(sm))[1]
                        pv     <- sm[, pv_col]
                        stars  <- ifelse(pv < 0.001, "$^{***}$",
                                         ifelse(pv < 0.01,  "$^{**}$",
                                                ifelse(pv < 0.05,  "$^{*}$", "")))
                        cell   <- paste0(sprintf("%.3f", co), stars,
                                         "\n(", sprintf("%.3f", se), ")")
                        data.frame(
                                model            = model_name,
                                term             = rownames(sm),
                                cell             = cell,
                                stringsAsFactors = FALSE
                        )
                }, error = function(e) {
                        cat("  Could not extract", model_name, ":", e$message, "\n")
                        NULL
                })
        }
        
        # Filter to lm/glm objects only
        lpm_list <- Filter(function(m) inherits(m, c("lm", "glm")), lpm_models)
        
        if (length(lpm_list) == 0) {
                cat("  WARNING: No lm/glm objects found in lpm_appendix_models.\n")
        } else {
                
                rows_list <- mapply(safe_tidy_lpm,
                                    lpm_list, names(lpm_list),
                                    SIMPLIFY = FALSE)
                rows_list <- Filter(Negate(is.null), rows_list)
                
                if (length(rows_list) > 0) {
                        
                        rows_lpm <- do.call(rbind, rows_list)
                        
                        # Drop t_scaled and any other spline/penalty terms with empty cells
                        sparse_terms <- names(which(
                                tapply(rows_lpm$cell, rows_lpm$term, function(x) all(x == ""))
                        ))
                        rows_lpm <- rows_lpm[!rows_lpm$term %in% c("t_scaled", sparse_terms), ]
                        
                        # Human-readable variable labels
                        var_labels_lpm <- c(
                                "(Intercept)"                        = "Constant",
                                "sidea_revisionist_domestic"         = "Revisionist Ideology",
                                "legit_ratio"                        = "Legitimation Ratio",
                                "v2exl_legitideol_a"                 = "Ideological Legitimation",
                                "sidea_dynamic_leader"               = "Dynamic Leadership",
                                "legit_ratio:sidea_dynamic_leader"   = "Legit. Ratio $\\times$ Dyn. Leadership",
                                "sidea_religious_support"            = "Religious Support",
                                "sidea_party_elite_support"          = "Party/Elite Support",
                                "sidea_military_support"             = "Military Support",
                                "cinc_a"                             = "Capabilities (CINC)",
                                "sidea_winning_coalition_size"       = "Winning Coalition Size",
                                "cold_war"                           = "Cold War",
                                "targets_democracy"                  = "Targets Democracy",
                                "mid_initiated"                      = "MID Initiated"
                        )
                        
                        rows_lpm$term_clean <- ifelse(
                                rows_lpm$term %in% names(var_labels_lpm),
                                var_labels_lpm[rows_lpm$term],
                                gsub("_", " ", rows_lpm$term)
                        )
                        
                        # Put intercept last
                        non_intercept <- unique(rows_lpm$term[rows_lpm$term != "(Intercept)"])
                        term_order    <- c(non_intercept, "(Intercept)")
                        term_order    <- intersect(term_order, unique(rows_lpm$term))
                        rows_lpm$term <- factor(rows_lpm$term, levels = term_order)
                        rows_lpm      <- rows_lpm[order(rows_lpm$term), ]
                        
                        # Pivot wide with base reshape()
                        wide_lpm <- reshape(
                                rows_lpm[, c("term_clean", "model", "cell")],
                                idvar     = "term_clean",
                                timevar   = "model",
                                direction = "wide"
                        )
                        names(wide_lpm) <- sub("^cell\\.", "", names(wide_lpm))
                        model_cols_lpm  <- names(lpm_list)
                        # Keep only columns that actually exist after reshape
                        model_cols_lpm  <- intersect(model_cols_lpm, names(wide_lpm))
                        wide_lpm        <- wide_lpm[, c("term_clean", model_cols_lpm), drop = FALSE]
                        wide_lpm        <- wide_lpm[match(unique(rows_lpm$term_clean),
                                                          wide_lpm$term_clean), ]
                        rownames(wide_lpm) <- NULL
                        
                        # Replace NA with ""
                        wide_lpm[is.na(wide_lpm)] <- ""
                        
                        # GOF rows
                        get_gof_lpm <- function(m, nm) {
                                N_val <- tryCatch(
                                        formatC(nobs(m), format = "d", big.mark = ","),
                                        error = function(e) ""
                                )
                                r2_val <- tryCatch(
                                        sprintf("%.3f", summary(m)$r.squared),
                                        error = function(e) ""
                                )
                                list(model = nm, N = N_val, R2 = r2_val)
                        }
                        
                        gof_vals_lpm <- mapply(get_gof_lpm, lpm_list, names(lpm_list),
                                               SIMPLIFY = FALSE)
                        
                        make_gof_row_lpm <- function(field, label) {
                                row <- data.frame(term_clean = label, stringsAsFactors = FALSE)
                                for (g in gof_vals_lpm) {
                                        v <- g[[field]]
                                        row[[g$model]] <- if (length(v) == 1) v else ""
                                }
                                for (col in setdiff(c("term_clean", model_cols_lpm), names(row))) {
                                        row[[col]] <- ""
                                }
                                row[, c("term_clean", model_cols_lpm), drop = FALSE]
                        }
                        
                        final_lpm <- rbind(
                                wide_lpm,
                                make_gof_row_lpm("N",  "N"),
                                make_gof_row_lpm("R2", "R$^{2}$")
                        )
                        rownames(final_lpm) <- NULL
                        
                        # Cap at 5 columns for page width
                        if (length(model_cols_lpm) > 5) {
                                cat("  NOTE: Trimming LPM table to first 5 models for page width.\n")
                                model_cols_lpm <- model_cols_lpm[1:5]
                                final_lpm      <- final_lpm[, c("term_clean", model_cols_lpm), drop = FALSE]
                        }
                        
                        kbl_lpm <- knitr::kable(
                                final_lpm,
                                format    = "latex",
                                booktabs  = TRUE,
                                caption   = "Linear Probability Model Robustness Checks (Appendix)",
                                col.names = c(" ", model_cols_lpm),
                                linesep   = "",
                                escape    = FALSE,
                                align     = c("l", rep("c", length(model_cols_lpm)))
                        ) |>
                                kableExtra::kable_styling(
                                        latex_options = c("hold_position", "scale_down"),
                                        font_size     = 9
                                ) |>
                                kableExtra::footnote(
                                        general        = "Linear probability models. Robust standard errors in parentheses. $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001.",
                                        threeparttable = TRUE,
                                        escape         = FALSE
                                )
                        
                        writeLines(as.character(kbl_lpm), file.path(tables_dir, "table6_lpm_appendix.tex"))
                        cat("  Saved: table6_lpm_appendix.tex\n")
                }
        }
}

## =============================================================================
## OUTPUT SUMMARY
## =============================================================================

cat("\n=== Table generation complete ===\n")
cat("Files saved to:", tables_dir, "\n")
files_written <- list.files(tables_dir, pattern = "\\.tex$")
if (length(files_written) == 0) {
        cat("WARNING: No .tex files found in output directory.\n")
} else {
        cat(paste0("  ", files_written), sep = "\n")
}