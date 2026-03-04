# ==============================================================================
# 08_h8_moderation.R -- Moderation Analysis for Hypothesis 8
# H8: Moderation -- Dynamic Leadership
# Uses: glm (logit), lm (LPM with interaction), sjPlot, marginaleffects
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

# ------------------------------------------------------------------------------
# Memory: subset dyad_ready to needed columns
# ------------------------------------------------------------------------------
h8_vars <- c(
  "mid_initiated", "targets_democracy",
  "legit_ratio", "sidea_dynamic_leader",
  "log_cinc_a", "log_cinc_b",
  "sidea_winning_coalition_size", "sidea_military_support",
  "cold_war", "t", "t2", "t3"
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

safe_glm <- function(formula, data, family = binomial(link = "logit"), min_obs = 30) {
  vars <- all.vars(formula)
  for (v in vars) {
    if (!v %in% names(data)) { warning(sprintf("[08] '%s' not found. Skipping.", v)); return(NULL) }
    if (all(is.na(data[[v]]))) { warning(sprintf("[08] '%s' all NA. Skipping.", v)); return(NULL) }
  }
  if (sum(complete.cases(data[, vars, drop = FALSE])) < min_obs) {
    warning("[08] Insufficient complete cases. Skipping."); return(NULL)
  }
  if (requireNamespace("brglm2", quietly = TRUE) && identical(family$family, "binomial")) {
    fit <- tryCatch(glm(formula, family = family, data = data, method = brglm2::brglmFit,
                        control = list(maxit = 300, epsilon = 1e-6)),
                    error = function(e) NULL)
    if (!is.null(fit)) return(strip_glm(fit))
  }
  fit <- tryCatch(glm(formula, family = family, data = data, control = glm.control(maxit = 100)),
                  error = function(e) { warning(sprintf("[08] glm failed: %s", e$message)); NULL })
  strip_glm(fit)
}

safe_lm <- function(formula, data, min_obs = 30) {
  vars <- all.vars(formula)
  for (v in vars) {
    if (!v %in% names(data)) { warning(sprintf("[08] '%s' not found. Skipping.", v)); return(NULL) }
  }
  if (sum(complete.cases(data[, vars, drop = FALSE])) < min_obs) {
    warning("[08] Insufficient complete cases. Skipping."); return(NULL)
  }
  fit <- tryCatch(lm(formula, data = data),
                  error = function(e) { warning(sprintf("[08] lm failed: %s", e$message)); NULL })
  strip_glm(fit)  # works for lm too
}

# ==============================================================================
# Helper: safe VIF
# ==============================================================================
safe_vif <- function(model, label = deparse(substitute(model))) {
  tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) v <- v[, "GVIF"]
    message(sprintf("[08] VIF (%s): max = %.2f", label, max(v, na.rm = TRUE)))
    v
  }, error = function(e) {
    message(sprintf("[08] VIF failed for %s: %s", label, e$message))
    NULL
  })
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
      cold_war + t + t2 + t3, data = conflict_data)
  h8_target_int <- safe_glm(
    targets_democracy ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t + t2 + t3, data = conflict_data)
  h8_target_lpm_int <- safe_lm(
    targets_democracy ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + sidea_winning_coalition_size +
      cold_war + t + t2 + t3, data = conflict_data)
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
# 4. Predicted Probability Plots (sjPlot) ----
# ==============================================================================
plot_h8_interactions <- function(init_models, target_models) {
  dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
  plots <- list()
  tryCatch({
    plots$p1 <- plot_model(init_models$h8_init_lpm_int, type = "pred",
                           terms = c("legit_ratio", "sidea_dynamic_leader"),
                           title = "H8: Ideology x Dynamic Leadership -- Initiation (LPM)")
    plots$p2 <- plot_model(init_models$h8_init_lpm_int, type = "pred",
                           terms = c("sidea_dynamic_leader", "legit_ratio"),
                           title = "H8: Dynamic Leadership x Ideology -- Initiation (LPM)")
    plots$p3 <- plot_model(target_models$h8_target_lpm_int, type = "pred",
                           terms = c("legit_ratio", "sidea_dynamic_leader"),
                           title = "H8: Ideology x Dynamic Leadership -- Targeting (LPM)")
    plots$p4 <- plot_model(target_models$h8_target_lpm_int, type = "pred",
                           terms = c("sidea_dynamic_leader", "legit_ratio"),
                           title = "H8: Dynamic Leadership x Ideology -- Targeting (LPM)")
    if (!is.null(plots$p1)) ggplot2::ggsave("results/figures/h8_init_ideology_by_dynleader.png", plots$p1, width = 8, height = 5)
    if (!is.null(plots$p2)) ggplot2::ggsave("results/figures/h8_init_dynleader_by_ideology.png", plots$p2, width = 8, height = 5)
    if (!is.null(plots$p3)) ggplot2::ggsave("results/figures/h8_target_ideology_by_dynleader.png", plots$p3, width = 8, height = 5)
    if (!is.null(plots$p4)) ggplot2::ggsave("results/figures/h8_target_dynleader_by_ideology.png", plots$p4, width = 8, height = 5)
  }, error = function(e) warning(sprintf("[08] Plot generation failed: %s", e$message)))
  plots
}

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
