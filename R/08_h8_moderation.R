# ==============================================================================
# 08_h8_moderation.R -- Ultra-Simplified Moderation Analysis for Hypothesis 8
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

library(marginaleffects)

# ------------------------------------------------------------------------------
# Embedded helpers (no external helpers.R sourcing)
# ------------------------------------------------------------------------------
strip_glm <- function(model) {
        if (is.null(model)) return(NULL)
        model$model <- NULL; model$data <- NULL; model$y <- NULL
        model$linear.predictors <- NULL; model$fitted.values <- NULL
        model$residuals <- NULL; model$weights <- NULL
        model$prior.weights <- NULL; model$effects <- NULL
        attr(model$terms, ".Environment") <- globalenv()
        model
}

safe_lm <- function(formula, data, min_obs = 30) {
        vars <- all.vars(formula)
        for (v in vars) {
                if (!v %in% names(data)) { 
                        warning(sprintf("[safe_lm] '%s' not found. Skipping.", v)); 
                        return(NULL) 
                }
        }
        complete <- complete.cases(data[, vars, drop = FALSE])
        if (sum(complete) < min_obs) { 
                warning("[safe_lm] Insufficient complete cases. Skipping."); 
                return(NULL) 
        }
        fit <- tryCatch(lm(formula, data = data[complete, ]),
                        error = function(e) { 
                                warning(sprintf("[safe_lm] lm failed: %s", e$message)); 
                                NULL 
                        })
        if (!is.null(fit) && (is.null(fit$coefficients) || any(is.na(fit$coefficients)))) {
                warning("[safe_lm] Model singular or NA coefficients. Skipping.")
                return(NULL)
        }
        strip_glm(fit)
}

safe_glm <- function(formula, data, family = binomial(link = "logit"),
                     min_obs = 10, ...) {
        
        vars <- all.vars(formula)
        missing_vars <- vars[!vars %in% names(data)]
        if (length(missing_vars) > 0) {
                message(sprintf("[safe_glm] Missing variable(s): %s. Skipping.", paste(missing_vars, collapse = ", ")))
                return(NULL)
        }
        
        complete <- complete.cases(data[, vars, drop = FALSE])
        n_complete <- sum(complete)
        if (n_complete < min_obs) {
                message(sprintf("[safe_glm] Too few complete cases (%d < %d). Skipping.", n_complete, min_obs))
                return(NULL)
        }
        
        # Try starting values from simple glm
        start_vals <- NULL
        tryCatch({
                simple_fit <- glm(formula, family = family, data = data, maxit = 50)
                if (all(is.finite(coef(simple_fit)))) {
                        start_vals <- coef(simple_fit)
                }
        }, error = function(e) NULL, warning = function(w) NULL)
        
        # Main Firth fit
        fit <- tryCatch(
                glm(formula, family = family, data = data,
                    method = brglm2::brglmFit,
                    start = start_vals,
                    control = brglm2::brglm_control(
                            maxit = 3000,
                            epsilon = 1e-10,
                            slowit = 0.05,
                            response_adjustment = TRUE,
                            type = "AS_mixed"
                    ),
                    ...),
                warning = function(w) { message("[safe_glm] brglm warning: ", w$message); NULL },
                error = function(e) { message("[safe_glm] brglm error: ", e$message); NULL }
        )
        
        if (is.null(fit)) {
                message("[safe_glm] brglmFit failed. Trying ordinary glm...")
                fit <- tryCatch(
                        glm(formula, family = family, data = data, start = start_vals, ...),
                        error = function(e) { message("[safe_glm] Ordinary glm failed: ", e$message); NULL }
                )
        }
        
        if (!is.null(fit) && !fit$converged) {
                message("[safe_glm] Warning: Model did not fully converge.")
        }
        
        return(fit)
}

# ------------------------------------------------------------------------------
# Filter to Cold War years only (1960–1989) and minimal variables
# ------------------------------------------------------------------------------
h8_data <- dyad_ready %>%
        dplyr::filter(year >= 1960 & year <= 1989) %>%
        dplyr::select(
                mid_initiated,
                targets_democracy,
                legit_ratio,
                sidea_dynamic_leader,
                sidea_winning_coalition_size
        )

gc()
message(sprintf("[08] Filtered Cold War data: %d rows x %d cols, %s",
                nrow(h8_data), ncol(h8_data),
                format(object.size(h8_data), units = "MB")))

# ==============================================================================
# 1. Initiation Models (cloglog for rare onset)
# ==============================================================================
estimate_h8_initiation <- function(data) {
        message("[08] Fitting initiation models...")
        
        h8_init_main <- safe_glm(
                mid_initiated ~ legit_ratio + sidea_dynamic_leader +
                        sidea_winning_coalition_size,
                data = data,
                family = binomial(link = "cloglog")
        )
        
        h8_init_int <- safe_glm(
                mid_initiated ~ legit_ratio * sidea_dynamic_leader +
                        sidea_winning_coalition_size,
                data = data,
                family = binomial(link = "cloglog")
        )
        
        h8_init_lpm_int <- safe_lm(
                mid_initiated ~ legit_ratio * sidea_dynamic_leader +
                        sidea_winning_coalition_size,
                data = data
        )
        
        list(h8_init_main = h8_init_main, h8_init_int = h8_init_int,
             h8_init_lpm_int = h8_init_lpm_int)
}

# ==============================================================================
# 2. Targeting Models (logit, filtered to conflict cases)
# ==============================================================================
estimate_h8_targeting <- function(data) {
        conflict_data <- data %>% filter(mid_initiated == 1)
        if (nrow(conflict_data) < 30) {
                warning("[08] Too few conflict obs for targeting. Skipping.")
                return(list(h8_target_main = NULL, h8_target_int = NULL, h8_target_lpm_int = NULL))
        }
        
        message("[08] Fitting targeting models on ", nrow(conflict_data), " cases...")
        
        h8_target_main <- safe_glm(
                targets_democracy ~ legit_ratio + sidea_dynamic_leader +
                        sidea_winning_coalition_size,
                data = conflict_data
        )
        
        h8_target_int <- safe_glm(
                targets_democracy ~ legit_ratio * sidea_dynamic_leader +
                        sidea_winning_coalition_size,
                data = conflict_data
        )
        
        h8_target_lpm_int <- safe_lm(
                targets_democracy ~ legit_ratio * sidea_dynamic_leader +
                        sidea_winning_coalition_size,
                data = conflict_data
        )
        
        list(h8_target_main = h8_target_main, h8_target_int = h8_target_int,
             h8_target_lpm_int = h8_target_lpm_int)
}

# ==============================================================================
# 3. Plots (marginaleffects + ggplot)
# ==============================================================================

plot_h8_interactions <- function(init_models, target_models, data = h8_data) {
        plots <- list()
        tryCatch({
                library(marginaleffects)
                
                # Helper to make range
                make_range <- function(var, n = 100) seq(min(var, na.rm = TRUE), max(var, na.rm = TRUE), length.out = n)
                
                if (!is.null(init_models$h8_init_lpm_int)) {
                        preds1 <- predictions(init_models$h8_init_lpm_int,
                                              variables = "legit_ratio",
                                              newdata = data)  # ← pass data explicitly
                        plots$p1 <- ggplot(preds1, aes(x = legit_ratio, y = estimate,
                                                       color = factor(sidea_dynamic_leader))) +
                                geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
                                labs(title = "H8: Ideology × Dynamic Leadership – Initiation (LPM)",
                                     x = "Legit Ratio", y = "Predicted Probability") +
                                theme_minimal()
                        
                        preds2 <- predictions(init_models$h8_init_lpm_int,
                                              variables = "sidea_dynamic_leader",
                                              newdata = data)
                        plots$p2 <- ggplot(preds2, aes(x = sidea_dynamic_leader, y = estimate,
                                                       color = factor(legit_ratio))) +
                                geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
                                labs(title = "H8: Dynamic Leadership × Ideology – Initiation (LPM)",
                                     x = "Dynamic Leader", y = "Predicted Probability") +
                                theme_minimal()
                }
                
                # Similar for targeting
                if (!is.null(target_models$h8_target_lpm_int)) {
                        preds3 <- predictions(target_models$h8_target_lpm_int,
                                              variables = "legit_ratio",
                                              newdata = data)
                        plots$p3 <- ggplot(preds3, aes(x = legit_ratio, y = estimate,
                                                       color = factor(sidea_dynamic_leader))) +
                                geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
                                labs(title = "H8: Ideology × Dynamic Leadership – Targeting (LPM)",
                                     x = "Legit Ratio", y = "Predicted Probability") +
                                theme_minimal()
                        
                        preds4 <- predictions(target_models$h8_target_lpm_int,
                                              variables = "sidea_dynamic_leader",
                                              newdata = data)
                        plots$p4 <- ggplot(preds4, aes(x = sidea_dynamic_leader, y = estimate,
                                                       color = factor(legit_ratio))) +
                                geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
                                labs(title = "H8: Dynamic Leadership × Ideology – Targeting (LPM)",
                                     x = "Dynamic Leader", y = "Predicted Probability") +
                                theme_minimal()
                }
                
                if (length(plots) > 0) {
                        dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
                        if (!is.null(plots$p1)) ggsave("results/figures/h8_init_ideology_by_dynleader.png", plots$p1, width = 8, height = 5)
                        if (!is.null(plots$p2)) ggsave("results/figures/h8_init_dynleader_by_ideology.png", plots$p2, width = 8, height = 5)
                        if (!is.null(plots$p3)) ggsave("results/figures/h8_target_ideology_by_dynleader.png", plots$p3, width = 8, height = 5)
                        if (!is.null(plots$p4)) ggsave("results/figures/h8_target_dynleader_by_ideology.png", plots$p4, width = 8, height = 5)
                }
        }, error = function(e) warning(sprintf("[08] Plot generation failed: %s", e$message)))
        plots
}

# ==============================================================================
# Execution & Saving
# ==============================================================================
message("[08] Starting initiation models...")
h8_init <- estimate_h8_initiation(h8_data)

message("[08] Starting targeting models...")
h8_target <- estimate_h8_targeting(h8_data)

message("[08] Starting plots...")
# h8_plots <- plot_h8_interactions(h8_init, h8_target, data = h8_data)  # pass data

# preds <- predictions(h8_init$h8_init_int,
#                      variables = "legit_ratio",
#                      newdata = h8_data)
# ggplot(preds, aes(x = legit_ratio, y = estimate, color = factor(sidea_dynamic_leader))) +
#         geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)

# VIF
h8_init_vif   <- lapply(setNames(names(h8_init), names(h8_init)),   
                        function(nm) safe_vif(h8_init[[nm]], nm))
h8_target_vif <- lapply(setNames(names(h8_target), names(h8_target)), 
                        function(nm) safe_vif(h8_target[[nm]], nm))

dir.create("results", showWarnings = FALSE)
saveRDS(h8_init,   "results/h8_init_models.rds")
saveRDS(h8_target, "results/h8_target_models.rds")
saveRDS(list(init = h8_init_vif, target = h8_target_vif), "results/h8_vif.rds")

rm(h8_data, h8_init, h8_target, h8_plots, h8_init_vif, h8_target_vif)
gc()

message("[08_h8_moderation.R] Done. Models saved to results/")