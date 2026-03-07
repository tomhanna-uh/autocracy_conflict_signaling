# ==============================================================================
# 08_h8_moderation.R -- Moderation Analysis for Hypothesis 8
# H8: Moderation -- Dynamic Leadership
# Uses: glm (logit), lm (LPM with interaction), sjPlot, marginaleffects
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))
source("R/helpers.R") 

# ------------------------------------------------------------------------------
# Memory: subset dyad_ready to needed columns
# ------------------------------------------------------------------------------
h8_vars <- c(
  "mid_initiated", "targets_democracy",
  "legit_ratio", "sidea_dynamic_leader",
  "log_cinc_a", "log_cinc_b",
  "sidea_winning_coalition_size", "sidea_military_support",
  "cold_war", "t_scaled", "t2_scaled", "t3_scaled"
)
h8_vars <- intersect(h8_vars, names(dyad_ready))
h8_data <- dyad_ready[, h8_vars, drop = FALSE]
# NOTE: dyad_ready/monadic_ready kept in memory for pipeline efficiency
gc()
message(sprintf("[08] h8_data: %d rows x %d cols, %s",
                nrow(h8_data), ncol(h8_data),
                format(object.size(h8_data), units = "MB")))

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
                if (!v %in% names(data)) { warning(sprintf("[safe_lm] '%s' not found. Skipping.", v)); return(NULL) }
        }
        complete <- complete.cases(data[, vars, drop = FALSE])
        if (sum(complete) < min_obs) { warning("[safe_lm] Insufficient complete cases. Skipping."); return(NULL) }
        fit <- tryCatch(lm(formula, data = data[complete, ]),
                        error = function(e) { warning(sprintf("[safe_lm] lm failed: %s", e$message)); NULL })
        if (!is.null(fit) && is.null(fit$coefficients) || any(is.na(fit$coefficients))) {
                warning("[safe_lm] Model singular or NA coefficients. Skipping.")
                return(NULL)
        }
        strip_glm(fit)
}


# ==============================================================================
# 1. H8 -- Dynamic Leadership and Conflict Initiation ----
# ==============================================================================
estimate_h8_initiation <- function(data) {
  h8_init_main <- safe_glm(
    mid_initiated ~ legit_ratio + sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t_scaled + t2_scaled + t3_scaled + cold_war, data = data)
  h8_init_int <- safe_glm(
    mid_initiated ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t_scaled + t2_scaled + t3_scaled + cold_war, data = data)
  h8_init_lpm_int <- safe_lm(
    mid_initiated ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t_scaled + t2_scaled + t3_scaled + cold_war, data = data)
  list(h8_init_main = h8_init_main, h8_init_int = h8_init_int,
       h8_init_lpm_int = h8_init_lpm_int)
}

# ==============================================================================
# 2. H8 -- Dynamic Leadership and Democracy Targeting ----
# ==============================================================================
estimate_h8_targeting <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) < 30) {
    warning("[08] Too few conflict obs for targeting. Skipping.")
    return(list(h8_target_main = NULL, h8_target_int = NULL, h8_target_lpm_int = NULL))
  }
  h8_target_main <- safe_glm(
    targets_democracy ~ legit_ratio + sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t_scaled + t2_scaled + t3_scaled, data = conflict_data)
  h8_target_int <- safe_glm(
    targets_democracy ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t_scaled + t2_scaled + t3_scaled, data = conflict_data)
  h8_target_lpm_int <- safe_lm(
    targets_democracy ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t_scaled + t2_scaled + t3_scaled, data = conflict_data)
  list(h8_target_main = h8_target_main, h8_target_int = h8_target_int,
       h8_target_lpm_int = h8_target_lpm_int)
}

# ==============================================================================
# 3. Robustness Interaction Models ----
# ==============================================================================
estimate_h8_robustness <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) < 30) return(list(h8_wcoal_int = NULL, h8_milsupp_int = NULL, h8_coldwar_int = NULL))
  h8_wcoal_int <- safe_lm(
    targets_democracy ~ legit_ratio * sidea_winning_coalition_size +
      sidea_dynamic_leader + log_cinc_a + log_cinc_b + cold_war + t_scaled + t2_scaled + t3_scaled + cold_war,
    data = conflict_data)
  h8_milsupp_int <- safe_lm(
    targets_democracy ~ legit_ratio * sidea_military_support +
      sidea_dynamic_leader + log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war + t_scaled + t2_scaled + t3_scaled + cold_war,
    data = conflict_data)
  h8_coldwar_int <- safe_lm(
    targets_democracy ~ legit_ratio * cold_war +
      sidea_dynamic_leader + log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + t_scaled + t2_scaled + t3_scaled + cold_war,
    data = conflict_data)
  list(h8_wcoal_int = h8_wcoal_int, h8_milsupp_int = h8_milsupp_int,
       h8_coldwar_int = h8_coldwar_int)
}

# ==============================================================================
# 4. Predicted Probability Plots (marginaleffects) ----
# ==============================================================================

plot_h8_interactions <- function(init_models, target_models) {
        plots <- list()
        tryCatch({
                if (!is.null(init_models$h8_init_lpm_int) && !is.null(init_models$h8_init_lpm_int$coefficients)) {
                        plots$p1 <- plot_model(init_models$h8_init_lpm_int, type = "pred",
                                               terms = c("legit_ratio", "sidea_dynamic_leader"),
                                               title = "H8: Ideology x Dynamic Leadership -- Initiation (LPM)")
                        plots$p2 <- plot_model(init_models$h8_init_lpm_int, type = "pred",
                                               terms = c("sidea_dynamic_leader", "legit_ratio"),
                                               title = "H8: Dynamic Leadership x Ideology -- Initiation (LPM)")
                } else {
                        message("[08] Skipping init plots - model NULL or invalid.")
                }
                # Similar for target plots
                if (!is.null(target_models$h8_target_lpm_int) && !is.null(target_models$h8_target_lpm_int$coefficients)) {
                        plots$p3 <- plot_model(target_models$h8_target_lpm_int, type = "pred",
                                               terms = c("legit_ratio", "sidea_dynamic_leader"),
                                               title = "H8: Ideology x Dynamic Leadership -- Targeting (LPM)")
                        plots$p4 <- plot_model(target_models$h8_target_lpm_int, type = "pred",
                                               terms = c("sidea_dynamic_leader", "legit_ratio"),
                                               title = "H8: Dynamic Leadership x Ideology -- Targeting (LPM)")
                } else {
                        message("[08] Skipping target plots - model NULL or invalid.")
                }
                # Save only if plots exist
                if (length(plots) > 0) {
                        dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
                        if (!is.null(plots$p1)) ggsave("results/figures/h8_init_ideology_by_dynleader.png", plots$p1, width = 8, height = 5)
                        # ... similarly for others
                }
        }, error = function(e) warning(sprintf("[08] Plot generation failed: %s", e$message)))
        plots
}

# library(marginaleffects)
# # Example for one model
# preds <- predictions(model, variables = "legit_ratio", by = "sidea_dynamic_leader")
# ggplot(preds, aes(x = legit_ratio, y = estimate, color = factor(sidea_dynamic_leader))) +
#         geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)

# ==============================================================================
# 5. Execution and Saving Results ----
# ==============================================================================
h8_init   <- estimate_h8_initiation(h8_data)
h8_target <- estimate_h8_targeting(h8_data)
h8_robust <- estimate_h8_robustness(h8_data)
h8_plots  <- plot_h8_interactions(h8_init, h8_target)

# ==============================================================================
# 6. VIF Diagnostics ----
# ==============================================================================
h8_init_vif <- lapply(setNames(names(h8_init), names(h8_init)),
                      function(nm) safe_vif(h8_init[[nm]], nm))
h8_target_vif <- lapply(setNames(names(h8_target), names(h8_target)),
                        function(nm) safe_vif(h8_target[[nm]], nm))
h8_robust_vif <- lapply(setNames(names(h8_robust), names(h8_robust)),
                        function(nm) safe_vif(h8_robust[[nm]], nm))

dir.create("results", showWarnings = FALSE)
saveRDS(h8_init,   "results/h8_init_models.rds")
saveRDS(h8_target, "results/h8_target_models.rds")
saveRDS(h8_robust, "results/h8_robust_models.rds")
saveRDS(list(init = h8_init_vif, target = h8_target_vif, robust = h8_robust_vif),
        "results/h8_vif.rds")

rm(h8_data, h8_vars, h8_init, h8_target, h8_robust, h8_plots,
   h8_init_vif, h8_target_vif, h8_robust_vif)
gc()
message("[08_h8_moderation.R] Done. Models saved to results/")
