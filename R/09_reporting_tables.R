# ==============================================================================
# 09_reporting_tables.R -- Consolidated Results Reporting
# Robust version that handles simplified models (e.g., no temporal terms in full specs,
# occasional NULL models, stripped objects)
# ==============================================================================
source(here::here("R", "00_packages.R"))

# ==============================================================================
# 1. Utility Functions (Robust Version) ----
# ==============================================================================
safe_vcov <- function(model) {
        if (is.null(model)) return(NULL)
        if (inherits(model, "brglmFit") && !is.null(model$Fisher.info)) {
                V <- solve(model$Fisher.info)
                nms <- names(coef(model))
                if (nrow(V) == length(nms)) rownames(V) <- colnames(V) <- nms
                return(V)
        }
        stats::vcov(model)
}

extract_coefs <- function(model, model_name = "") {
        if (is.null(model) || length(coef(model)) == 0) return(NULL)
        tryCatch({
                beta <- coef(model)
                v <- safe_vcov(model)
                se <- if (is.null(v)) rep(NA_real_, length(beta)) else sqrt(diag(v))
                z <- beta / se
                pval <- 2 * pnorm(abs(z), lower.tail = FALSE)
                data.frame(model = model_name, term = names(beta),
                           estimate = beta, std.error = se, p.value = pval,
                           stringsAsFactors = FALSE)
        }, error = function(e) { message("Could not extract from ", model_name, ": ", e$message); NULL })
}

build_model_table <- function(model_list, nice_names = NULL) {
        coef_dfs <- lapply(names(model_list), function(nm) extract_coefs(model_list[[nm]], nm))
        coef_dfs <- coef_dfs[!sapply(coef_dfs, is.null)]
        if (length(coef_dfs) == 0) {
                message("No model coefficients could be extracted for this table.")
                return(NULL)
        }
        
        all_terms <- unique(unlist(lapply(coef_dfs, `[[`, "term")))
        wide <- data.frame(label = all_terms, stringsAsFactors = FALSE)
        
        for (mdl in names(coef_dfs)) {
                df <- coef_dfs[[mdl]]
                wide[[mdl]] <- NA_character_
                idx <- match(wide$label, df$term)
                valid <- !is.na(idx)
                if (any(valid)) {
                        est <- df$estimate[idx[valid]]
                        se <- df$std.error[idx[valid]]
                        p <- df$p.value[idx[valid]]
                        stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
                        wide[[mdl]][valid] <- paste0(sprintf("%.3f", est), stars, " (", sprintf("%.3f", se), ")")
                }
        }
        
        if (!is.null(nice_names)) {
                wide$label <- nice_names[match(wide$label, names(nice_names))]
        }
        wide
}

write_table <- function(tbl, title, filepath) {
        if (is.null(tbl) || nrow(tbl) == 0) {
                message(sprintf("[09] Skipping %s (no valid data)", title))
                return(invisible(NULL))
        }
        cat(sprintf("\n=== %s ===\n", title))
        print(knitr::kable(tbl, format = "simple", row.names = FALSE))
        
        html <- knitr::kable(tbl, format = "html", row.names = FALSE, caption = title)
        writeLines(as.character(html), filepath)
        message(sprintf("[09] Wrote: %s", filepath))
}

# ==============================================================================
# 2. Load Results ----
# ==============================================================================
load_all_results <- function() {
        safe_load <- function(path) if (file.exists(path)) readRDS(path) else NULL
        list(
                h1 = safe_load("results/h1_logit_models.rds"),
                h2 = safe_load("results/h2_logit_models.rds"),
                h3 = safe_load("results/h3_logit_models.rds"),
                h4 = safe_load("results/h4_logit_models.rds"),
                h5 = safe_load("results/h5_logit_models.rds"),
                h6 = safe_load("results/h6_logit_models.rds"),
                h8_init = safe_load("results/h8_init_models.rds"),
                h8_target = safe_load("results/h8_target_models.rds"),
                h9 = safe_load("results/h9_survival.rds")
        )
}

# ==============================================================================
# 3. Reporting Functions ----
# ==============================================================================
report_tier1_logit <- function(res) {
        dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
        
        # H1 Main
        if (!is.null(res$h1)) {
                tbl <- build_model_table(list(
                        "(1) Baseline" = res$h1$h1_baseline,
                        "(2) +Controls" = res$h1$h1_controls,
                        "(3) Simplified Full" = res$h1$h1_full
                ))
                write_table(tbl, "H1: Revisionist Leadership Ideology and Conflict Initiation",
                            "results/tables/tier1_h1_main.html")
        }
        
        # H1 Sub-types
        if (!is.null(res$h1)) {
                tbl <- build_model_table(list(
                        "Relig. (base)" = res$h1$h1_religious_base,
                        "Relig. (+ctrl)" = res$h1$h1_religious,
                        "Social. (base)" = res$h1$h1_socialist_base,
                        "Social. (+ctrl)" = res$h1$h1_socialist,
                        "Nation. (base)" = res$h1$h1_nationalist_base,
                        "Nation. (+ctrl)" = res$h1$h1_nationalist
                ))
                write_table(tbl, "H1: Ideology Sub-types and Conflict Initiation (Simplified Models)",
                            "results/tables/tier1_h1_subtypes.html")
        }
        
        # H2
        if (!is.null(res$h2)) {
                tbl <- build_model_table(list(
                        "(1) Baseline" = res$h2$h2_baseline,
                        "(2) +Controls" = res$h2$h2_controls,
                        "(3) Simplified Full" = res$h2$h2_full
                ))
                write_table(tbl, "H2: Revisionist Leadership Ideology and Democracy Targeting (Initiators Only)",
                            "results/tables/tier1_h2_main.html")
        }
        
        # H3
        if (!is.null(res$h3)) {
                tbl <- build_model_table(list(
                        "(1) Religious" = res$h3$h3_baseline,
                        "(2) Party" = res$h3$h3_party,
                        "(3) Military" = res$h3$h3_military,
                        "(4) Multi" = res$h3$h3_multi,
                        "(5) +Controls" = res$h3$h3_controls,
                        "(6) Simplified Full" = res$h3$h3_full
                ))
                write_table(tbl, "H3: Support Group Ideology and Conflict Initiation",
                            "results/tables/tier1_h3_main.html")
        }
        
        # H4
        if (!is.null(res$h4)) {
                tbl <- build_model_table(list(
                        "(1) Religious" = res$h4$h4_baseline,
                        "(2) Party" = res$h4$h4_party,
                        "(3) Military" = res$h4$h4_military,
                        "(4) Multi" = res$h4$h4_multi,
                        "(5) +Controls" = res$h4$h4_controls,
                        "(6) Simplified Full" = res$h4$h4_full
                ))
                write_table(tbl, "H4: Support Group Ideology and Democracy Targeting (Initiators Only)",
                            "results/tables/tier1_h4_main.html")
        }
}

report_tier2_legit <- function(res) {
        if (!is.null(res$h5) && !is.null(res$h6)) {
                tbl <- build_model_table(list(
                        "H5 Full" = res$h5$h5_full,
                        "H6 Full" = res$h6$h6_full
                ))
                write_table(tbl, "Tier 2: Legitimation Mix and Conflict (H5 & H6)",
                            "results/tables/tier2_legit_mix.html")
        }
}

report_h8_moderation <- function(res) {
        if (!is.null(res$h8_init) && !is.null(res$h8_target)) {
                tbl <- build_model_table(list(
                        "Init (Main)" = res$h8_init$h8_init_main,
                        "Init (Int)" = res$h8_init$h8_init_int,
                        "Target (Main)" = res$h8_target$h8_target_main,
                        "Target (Int)" = res$h8_target$h8_target_int
                ))
                write_table(tbl, "Tier 3: Moderation by Dynamic Leadership (H8)",
                            "results/tables/tier3_h8_moderation.html")
        }
}

report_h7_mediation <- function(res) {
        if (!is.null(res$h7)) {
                message("[09] H7 mediation models loaded (see appendix for detailed results).")
                # Add table if you want (e.g., indirect effects summary)
        }
}

report_h9_survival <- function(res) {
        if (!is.null(res$h9)) {
                tbl <- build_model_table(list(
                        "Direct" = res$h9$cox_h9_direct,
                        "+Ratio" = res$h9$cox_h9_ratio,
                        "Interaction" = res$h9$cox_h9_int_ratio
                ))
                write_table(tbl, "Tier 3: Leader Survival and Conflict (H9)",
                            "results/tables/tier3_h9_survival.html")
        }
}

# ==============================================================================
# 4. Execution ----
# ==============================================================================
try({
        all_res <- load_all_results()
        report_tier1_logit(all_res)
        report_tier2_legit(all_res)
        report_h8_moderation(all_res)
        report_h7_mediation(all_res)
        report_h9_survival(all_res)
        
        message("[09_reporting_tables.R] All reporting tables generated in results/tables/")
        message("Generated tables: H1 main/subtypes, H2, H3, H4, H5/H6, H8, H9")
}, silent = FALSE)