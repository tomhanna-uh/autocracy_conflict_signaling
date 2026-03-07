# ==============================================================================
# 05_h5_h6_legitmix.R -- Legitimation Mix Analysis for Hypotheses 5 and 6
# H5: Legitimation Mix -- Initiation
# H6: Legitimation Mix -- Targeting
# Tier 2: Logistic + Hurdle Models
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))
source("R/helpers.R")  

# ------------------------------------------------------------------------------
# Memory: subset to needed columns
# ------------------------------------------------------------------------------
h56_vars <- c(
  "mid_initiated", "targets_democracy",
  "legit_ratio", "legit_total",
  "v2exl_legitideol_a", "v2exl_legitperf_a", "v2exl_legitlead_a",
  "sidea_revisionist_domestic",
  "cinc_a", "sidea_winning_coalition_size",
  "t_scaled", "t2_scaled", "t3_scaled", "cold_war"
)
h56_vars <- intersect(h56_vars, names(dyad_ready))
h56_data <- dyad_ready[, h56_vars, drop = FALSE]
# NOTE: dyad_ready/monadic_ready kept in memory for pipeline efficiency
gc()
message(sprintf("[05] h56_data: %d rows x %d cols, %s",
                nrow(h56_data), ncol(h56_data),
                format(object.size(h56_data), units = "MB")))

# Helpers
strip_glm <- function(model) {
  if (is.null(model)) return(NULL)
  model$model <- NULL; model$data <- NULL; model$y <- NULL
  model$linear.predictors <- NULL; model$fitted.values <- NULL
  model$residuals <- NULL; model$weights <- NULL
  model$prior.weights <- NULL; model$effects <- NULL
  attr(model$terms, ".Environment") <- globalenv()
  model
}




# ==============================================================================
# 1. H5: Legitimation Mix -- Initiation ----
# NOTE: targets_democracy is NOT included as a predictor for H5.
#       It is a parallel DV (used in H6), not a control.
# ==============================================================================
estimate_h5_logit <- function(data) {
  h5_baseline   <- safe_glm(mid_initiated ~ legit_ratio, data = data)
  h5_components <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a, data = data)
  h5_controls   <- safe_glm(mid_initiated ~ legit_ratio + cinc_a + sidea_winning_coalition_size, data = data)
  h5_full       <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                              cinc_a + sidea_winning_coalition_size +
                         t_scaled + t2_scaled + t3_scaled + cold_war + cold_war, data = data)
  list(h5_baseline = h5_baseline, h5_components = h5_components,
       h5_controls = h5_controls, h5_full = h5_full)
}

estimate_h5_hurdle <- function(data) {
  hurdle_binary <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                              cinc_a + sidea_winning_coalition_size + 
                        t_scaled + t2_scaled + t3_scaled + cold_war + cold_war, data = data)
  initiators <- data %>% filter(mid_initiated == 1)
  hurdle_count <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                             cinc_a + sidea_winning_coalition_size + cold_war,
                           data = initiators, family = poisson(link = "log"))
  list(hurdle_binary = hurdle_binary, hurdle_count = hurdle_count)
}

# ==============================================================================
# 2. H6: Legitimation Mix -- Targeting ----
# ==============================================================================
estimate_h6_logit <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) == 0) {
    warning("[05] No conflict initiations. H6 skipped.")
    return(list(h6_baseline = NULL, h6_components = NULL,
                h6_controls = NULL, h6_full = NULL, h6_interaction = NULL))
  }
  h6_baseline    <- safe_glm(targets_democracy ~ legit_ratio, data = conflict_data)
  h6_components  <- safe_glm(targets_democracy ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a,
                             data = conflict_data)
  h6_controls    <- safe_glm(targets_democracy ~ legit_ratio + cinc_a + sidea_winning_coalition_size,
                             data = conflict_data)
  h6_full        <- safe_glm(targets_democracy ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                               cinc_a + sidea_winning_coalition_size + t_scaled + cold_war,
                             data = conflict_data)
  h6_interaction <- safe_glm(targets_democracy ~ legit_ratio * sidea_revisionist_domestic +
                               cinc_a + sidea_winning_coalition_size + t_scaled + cold_war,
                             data = conflict_data)
  list(h6_baseline = h6_baseline, h6_components = h6_components,
       h6_controls = h6_controls, h6_full = h6_full, h6_interaction = h6_interaction)
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================
h5_logit_models  <- estimate_h5_logit(h56_data)
h5_hurdle_models <- estimate_h5_hurdle(h56_data)
h6_logit_models  <- estimate_h6_logit(h56_data)

# ==============================================================================
# 4. VIF Diagnostics ----
# ==============================================================================
h5_vif <- lapply(setNames(names(h5_logit_models), names(h5_logit_models)),
                 function(nm) safe_vif(h5_logit_models[[nm]], nm))
h5h_vif <- lapply(setNames(names(h5_hurdle_models), names(h5_hurdle_models)),
                  function(nm) safe_vif(h5_hurdle_models[[nm]], nm))
h6_vif <- lapply(setNames(names(h6_logit_models), names(h6_logit_models)),
                 function(nm) safe_vif(h6_logit_models[[nm]], nm))

dir.create("results", showWarnings = FALSE)
saveRDS(h5_logit_models, "results/h5_logit_models.rds")
saveRDS(h5_hurdle_models, "results/h5_hurdle_models.rds")
saveRDS(h6_logit_models, "results/h6_logit_models.rds")
saveRDS(list(h5 = h5_vif, h5_hurdle = h5h_vif, h6 = h6_vif), "results/h56_vif.rds")

rm(h56_data, h56_vars, h5_logit_models, h5_hurdle_models, h6_logit_models, h5_vif, h5h_vif, h6_vif)
gc()
message("[05_h5_h6_legitmix.R] Done. Models saved to results/")
