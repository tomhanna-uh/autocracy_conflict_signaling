# 99_make_all_plots.R
# Master script: load saved models / objects and produce coefficient- and effect-plots
# for all main model files:
# 03_h1_h2_logit.R
# 04_h3_h4_logit.R
# 05_h5_h6_legitmix.R
# 05_m1_m2_mediation.R
# 06_h9_survival.R
# 07_h7_mediation.R
# 08_h8_moderation.R
# 09b_lpm_appendix.R

## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

source(here::here("R", "00_packages.R"))   # your package loader
suppressPackageStartupMessages({
        library(broom)
        library(ggplot2)
        library(dplyr)
        library(forcats)
        library(mediation)    # for mediate objects
        library(ggeffects)    # for marginal effects on glm/lm
        library(survival)     # for Cox models
})

results_dir <- here::here("results")
plots_dir   <- here::here("results", "plots")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

## ------------------------------------------------------------------
## Generic helpers (reusable across all model types)
## ------------------------------------------------------------------

coef_plot <- function(model, model_name, conf_level = 0.95) {
        td <- tryCatch(
                {
                        broom::tidy(model, conf.int = TRUE, conf.level = conf_level)
                },
                error = function(e) NULL
        )
        if (is.null(td) || !all(c("estimate", "conf.low", "conf.high") %in% names(td))) {
                return(NULL)
        }
        
        td <- td |>
                filter(!is.na(estimate)) |>
                mutate(
                        term_clean = term |>
                                gsub("_", " ", x = _) |>
                                gsub(":", " × ", x = _)
                )
        
        if (nrow(td) == 0) return(NULL)
        
        ggplot(td, aes(x = estimate, y = fct_rev(term_clean))) +
                geom_point(size = 2) +
                geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
                geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
                labs(
                        title = model_name,
                        x = if (inherits(model, "coxph")) "Log hazard ratio" else "Coefficient",
                        y = NULL
                ) +
                theme_minimal()
}

save_coef_plot <- function(model, model_name) {
        p <- coef_plot(model, model_name)
        if (is.null(p)) return(invisible(NULL))
        
        n_terms <- nrow(broom::tidy(model))
        height  <- max(3, min(8, 0.3 * n_terms + 1))
        
        ggsave(
                filename = file.path(plots_dir, paste0(model_name, "_coef.png")),
                plot     = p,
                width    = 7,
                height   = height,
                dpi      = 300
        )
}

# Simple marginal-effects wrapper for glm/lm
marginal_effects_plot <- function(model, model_name) {
        if (!inherits(model, c("glm", "lm"))) return(NULL)
        
        eff <- tryCatch(
                ggpredict(model),
                error = function(e) NULL
        )
        if (is.null(eff)) return(NULL)
        
        p <- plot(eff) +
                labs(title = paste0(model_name, " – marginal effects")) +
                theme_minimal()
        
        p
}

save_marginal_effects_plot <- function(model, model_name) {
        p <- marginal_effects_plot(model, model_name)
        if (is.null(p)) return(invisible(NULL))
        
        ggsave(
                filename = file.path(plots_dir, paste0(model_name, "_marginal_effects.png")),
                plot     = p,
                width    = 7,
                height   = 4.5,
                dpi      = 300
        )
}

# Mediation summary plot (for mediate objects)
mediation_effect_plot <- function(med_obj, name) {
        s <- summary(med_obj)
        
        df <- tibble(
                quantity   = c("ACME", "ADE", "Total effect"),
                estimate   = c(s$d0, s$z0, s$tau.coef),
                conf.low   = c(s$d0.ci[1], s$z0.ci[1], s$tau.ci[1]),
                conf.high  = c(s$d0.ci[2], s$z0.ci[2], s$tau.ci[2])
        )
        
        ggplot(df, aes(x = quantity, y = estimate)) +
                geom_point(size = 2) +
                geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
                geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
                labs(
                        title = paste0(name, " – mediation effects"),
                        x = NULL,
                        y = "Effect size"
                ) +
                theme_minimal()
}

save_mediation_plot <- function(med_obj, name) {
        p <- mediation_effect_plot(med_obj, name)
        ggsave(
                filename = file.path(plots_dir, paste0(name, "_mediation_effects.png")),
                plot     = p,
                width    = 6,
                height   = 4,
                dpi      = 300
        )
}

## ------------------------------------------------------------------
## 03_h1_h2_logit.R – logit models (H1–H2)
## expects RDS like: h1_logit_models.rds, h2_logit_models.rds (lists of glm)
## ------------------------------------------------------------------

cat("Processing 03_h1_h2_logit models...\n")

for (obj in c("h1_logit_models", "h2_logit_models")) {
        path <- file.path(results_dir, paste0(obj, ".rds"))
        if (!file.exists(path)) {
                cat("  (missing) ", obj, "\n")
                next
        }
        mod_list <- readRDS(path)
        
        if (!is.list(mod_list)) next
        
        for (nm in names(mod_list)) {
                m <- mod_list[[nm]]
                if (is.null(m)) next
                base_name <- paste0(obj, "_", nm)
                save_coef_plot(m, base_name)
                save_marginal_effects_plot(m, base_name)
        }
}

## ------------------------------------------------------------------
## 04_h3_h4_logit.R – additional logit models (H3–H4)
## e.g., h3_logit_models.rds, h4_logit_models.rds
## ------------------------------------------------------------------

cat("Processing 04_h3_h4_logit models...\n")

for (obj in c("h3_logit_models", "h4_logit_models")) {
        path <- file.path(results_dir, paste0(obj, ".rds"))
        if (!file.exists(path)) {
                cat("  (missing) ", obj, "\n")
                next
        }
        mod_list <- readRDS(path)
        if (!is.list(mod_list)) next
        
        for (nm in names(mod_list)) {
                m <- mod_list[[nm]]
                if (is.null(m)) next
                base_name <- paste0(obj, "_", nm)
                save_coef_plot(m, base_name)
                save_marginal_effects_plot(m, base_name)
        }
}

## ------------------------------------------------------------------
## 05_h5_h6_legitmix.R – legitimacy mixture / related models
## adjust object names here to whatever that script saves
## ------------------------------------------------------------------

cat("Processing 05_h5_h6_legitmix models...\n")

for (obj in c("h5__logit_models", "h6_logit_models")) {
        path <- file.path(results_dir, paste0(obj, ".rds"))
        if (!file.exists(path)) {
                cat("  (missing) ", obj, "\n")
                next
        }
        mod_list <- readRDS(path)
        if (!is.list(mod_list)) next
        
        for (nm in names(mod_list)) {
                m <- mod_list[[nm]]
                if (is.null(m)) next
                base_name <- paste0(obj, "_", nm)
                save_coef_plot(m, base_name)
                save_marginal_effects_plot(m, base_name)
        }
}

## ------------------------------------------------------------------
## 05_m1_m2_mediation.R – mediation + Cox survival
## objects: m1_models, m1_mediation, m1_survival, m1_robustness,
##          m2_models, m2_mediation, m2_survival   [page:3]
## ------------------------------------------------------------------

cat("Processing 05_m1_m2_mediation models...\n")

# OLS models
if (file.exists(file.path(results_dir, "m1_models.rds"))) {
        m1_models <- readRDS(file.path(results_dir, "m1_models.rds"))
        
        save_coef_plot(m1_models$mediator_model, "m1_mediator_model")
        save_coef_plot(m1_models$outcome_model,  "m1_outcome_model")
        save_coef_plot(m1_models$direct_model,   "m1_direct_model")
        
        save_marginal_effects_plot(m1_models$mediator_model, "m1_mediator_model")
        save_marginal_effects_plot(m1_models$outcome_model,  "m1_outcome_model")
        save_marginal_effects_plot(m1_models$direct_model,   "m1_direct_model")
}

if (file.exists(file.path(results_dir, "m2_models.rds"))) {
        m2_models <- readRDS(file.path(results_dir, "m2_models.rds"))
        
        save_coef_plot(m2_models$mediator_model,           "m2_mediator_model")
        save_coef_plot(m2_models$outcome_model,            "m2_outcome_model")
        save_coef_plot(m2_models$direct_model,             "m2_direct_model")
        save_coef_plot(m2_models$outcome_model_components, "m2_outcome_components")
        
        save_marginal_effects_plot(m2_models$mediator_model, "m2_mediator_model")
        save_marginal_effects_plot(m2_models$outcome_model,  "m2_outcome_model")
        save_marginal_effects_plot(m2_models$direct_model,   "m2_direct_model")
}

# Cox survival models
if (file.exists(file.path(results_dir, "m1_survival.rds"))) {
        m1_survival <- readRDS(file.path(results_dir, "m1_survival.rds"))
        
        for (nm in c("cox_direct", "cox_mediated", "cox_interaction_1", "cox_interaction_2")) {
                if (!is.null(m1_survival[[nm]])) {
                        save_coef_plot(m1_survival[[nm]], paste0("m1_", nm))
                }
        }
}

if (file.exists(file.path(results_dir, "m2_survival.rds"))) {
        m2_survival <- readRDS(file.path(results_dir, "m2_survival.rds"))
        
        for (nm in c("cox_direct", "cox_mediated", "cox_full")) {
                if (!is.null(m2_survival[[nm]])) {
                        save_coef_plot(m2_survival[[nm]], paste0("m2_", nm))
                }
        }
}

# Mediation objects
if (file.exists(file.path(results_dir, "m1_mediation.rds"))) {
        m1_mediation <- readRDS(file.path(results_dir, "m1_mediation.rds"))
        save_mediation_plot(m1_mediation, "m1_main")
}
if (file.exists(file.path(results_dir, "m1_robustness.rds"))) {
        m1_robustness <- readRDS(file.path(results_dir, "m1_robustness.rds"))
        if (!is.null(m1_robustness$med_democracy)) {
                save_mediation_plot(m1_robustness$med_democracy, "m1_democracy")
        }
        if (!is.null(m1_robustness$med_ideo_gap)) {
                save_mediation_plot(m1_robustness$med_ideo_gap,  "m1_ideo_gap")
        }
}
if (file.exists(file.path(results_dir, "m2_mediation.rds"))) {
        m2_mediation <- readRDS(file.path(results_dir, "m2_mediation.rds"))
        save_mediation_plot(m2_mediation, "m2_main")
}

## ------------------------------------------------------------------
## 06_h9_survival.R – hazard models (H9)
## expected objects: h9_survival_models.rds (list of coxph) or similar
## ------------------------------------------------------------------

cat("Processing 06_h9_survival models...\n")

for (obj in c("h9_survival_models")) {
        path <- file.path(results_dir, paste0(obj, ".rds"))
        if (!file.exists(path)) {
                cat("  (missing) ", obj, "\n")
                next
        }
        mod_list <- readRDS(path)
        if (!is.list(mod_list)) next
        
        for (nm in names(mod_list)) {
                m <- mod_list[[nm]]
                if (is.null(m)) next
                base_name <- paste0(obj, "_", nm)
                save_coef_plot(m, base_name)
        }
}

## ------------------------------------------------------------------
## 07_h7_mediation.R – other mediation models
## expected: h7_mediation_main.rds, h7_mediation_robust.rds, etc.
## ------------------------------------------------------------------

cat("Processing 07_h7_mediation objects...\n")

for (obj in c("h7_mediation_main", "h7_mediation_robust")) {
        path <- file.path(results_dir, paste0(obj, ".rds"))
        if (!file.exists(path)) {
                next
        }
        med_obj <- readRDS(path)
        save_mediation_plot(med_obj, obj)
}

## ------------------------------------------------------------------
## 08_h8_moderation.R – moderation models
## expected: h8_models.rds (list of lm/glm with interactions)
## ------------------------------------------------------------------

cat("Processing 08_h8_moderation models...\n")

if (file.exists(file.path(results_dir, "h8_models.rds"))) {
        h8_models <- readRDS(file.path(results_dir, "h8_models.rds"))
        if (is.list(h8_models)) {
                for (nm in names(h8_models)) {
                        m <- h8_models[[nm]]
                        if (is.null(m)) next
                        base_name <- paste0("h8_", nm)
                        save_coef_plot(m, base_name)
                        save_marginal_effects_plot(m, base_name)
                }
        }
}

## ------------------------------------------------------------------
## 09b_lpm_appendix.R – LPM appendix models
## expected: lpm_models.rds (list of lm)
## ------------------------------------------------------------------

cat("Processing 09b_lpm_appendix models...\n")

if (file.exists(file.path(results_dir, "lpm_models.rds"))) {
        lpm_models <- readRDS(file.path(results_dir, "lpm_models.rds"))
        if (is.list(lpm_models)) {
                for (nm in names(lpm_models)) {
                        m <- lpm_models[[nm]]
                        if (is.null(m)) next
                        base_name <- paste0("lpm_", nm)
                        save_coef_plot(m, base_name)
                        save_marginal_effects_plot(m, base_name)
                }
        }
}

cat("\nAll plotting finished. Files saved to:", plots_dir, "\n")