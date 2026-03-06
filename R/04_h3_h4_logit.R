# ==============================================================================
# 04_h3_h4_logit.R -- Logit Analysis for Hypotheses 3 and 4
# H3: The Rational Autocrat -- Initiation
#   Support group ideology -> MID initiation
# H4: The Rational Autocrat -- Targeting
#   Support group ideology -> MID targeting of democracies
# Tier 1: Simple Logistic Regression
# ==============================================================================

# Load required scripts
source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))
source("R/helpers.R")  

# ------------------------------------------------------------------------------
# Variable note:
# Support group IVs (GRAVE-D):
#   sidea_religious_support    = religious group regime support
#   sidea_party_elite_support  = party elite regime support
#   sidea_rural_worker_support = rural/worker group regime support
#   sidea_military_support     = military regime support
#   sidea_ethnic_racial_support = ethnic/racial group regime support
# mid_initiated    = binary: hostility level >= 2 (DV for H3)
# targets_democracy = binary: v2x_libdem_b >= 0.5 (DV for H4)
# cinc_a = COW CINC (capabilities control)
# sidea_winning_coalition_size = V-Dem/BdM selectorate control
# All variables constructed in 02_data_prep.R
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Memory: subset dyad_ready to only the columns this script uses.
# ------------------------------------------------------------------------------
h34_vars <- c(
  "mid_initiated", "targets_democracy",
  "sidea_religious_support", "sidea_party_elite_support",
  "sidea_rural_worker_support", "sidea_military_support",
  "sidea_ethnic_racial_support",
  "cinc_a", "sidea_winning_coalition_size",
  "t", "t2", "t3", "cold_war"
)
h34_vars <- intersect(h34_vars, names(dyad_ready))
h34_data <- dyad_ready[, h34_vars, drop = FALSE]
# NOTE: dyad_ready/monadic_ready kept in memory for pipeline efficiency
gc()
message(sprintf("[04] h34_data: %d rows x %d cols, %s",
                nrow(h34_data), ncol(h34_data),
                format(object.size(h34_data), units = "MB")))

# ------------------------------------------------------------------------------
# Reuse helpers from 03 (strip_glm, safe_glm)
# ------------------------------------------------------------------------------
strip_glm <- function(model) {
  if (is.null(model)) return(NULL)
  model$model          <- NULL
  model$data           <- NULL
  model$y              <- NULL
  model$linear.predictors <- NULL
  model$fitted.values  <- NULL
  model$residuals      <- NULL
  model$weights        <- NULL
  model$prior.weights  <- NULL
  model$effects        <- NULL
  attr(model$terms, ".Environment") <- globalenv()
  model
}




# ==============================================================================
# 1. Hypothesis 3: The Rational Autocrat -- Initiation ----
# NOTE: targets_democracy is NOT included as a predictor for H3.
#       It is a parallel DV (used in H4), not a control.
# ==============================================================================
estimate_h3_logit <- function(data) {
  h3_baseline <- safe_glm(mid_initiated ~ sidea_religious_support, data = data)
  h3_party    <- safe_glm(mid_initiated ~ sidea_party_elite_support, data = data)
  h3_military <- safe_glm(mid_initiated ~ sidea_military_support, data = data)

  h3_multi    <- safe_glm(mid_initiated ~ sidea_religious_support +
                            sidea_party_elite_support +
                            sidea_rural_worker_support +
                            sidea_military_support +
                            sidea_ethnic_racial_support, data = data)

  h3_controls <- safe_glm(mid_initiated ~ sidea_religious_support +
                            sidea_party_elite_support +
                            sidea_rural_worker_support +
                            sidea_military_support +
                            sidea_ethnic_racial_support +
                            cinc_a + sidea_winning_coalition_size, data = data)

  h3_full     <- safe_glm(mid_initiated ~ sidea_religious_support +
                            sidea_party_elite_support +
                            sidea_rural_worker_support +
                            sidea_military_support +
                            sidea_ethnic_racial_support +
                            cinc_a +
                            sidea_winning_coalition_size +
                        t_scaled + t2_scaled + t3_scaled + cold_war + cold_war, data = data)

  list(h3_baseline = h3_baseline, h3_party = h3_party,
       h3_military = h3_military, h3_multi = h3_multi,
       h3_controls = h3_controls, h3_full = h3_full)
}

# ==============================================================================
# 2. Hypothesis 4: The Rational Autocrat -- Targeting ----
# ==============================================================================
estimate_h4_logit <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) == 0) {
    warning("[04] No conflict initiations found. H4 models skipped.")
    return(list(h4_baseline = NULL, h4_party = NULL, h4_military = NULL,
                h4_multi = NULL, h4_controls = NULL, h4_full = NULL))
  }
  h4_baseline <- safe_glm(targets_democracy ~ sidea_religious_support, data = conflict_data)
  h4_party    <- safe_glm(targets_democracy ~ sidea_party_elite_support, data = conflict_data)
  h4_military <- safe_glm(targets_democracy ~ sidea_military_support, data = conflict_data)

  h4_multi    <- safe_glm(targets_democracy ~ sidea_religious_support +
                            sidea_party_elite_support +
                            sidea_rural_worker_support +
                            sidea_military_support +
                            sidea_ethnic_racial_support, data = conflict_data)

  h4_controls <- safe_glm(targets_democracy ~ sidea_religious_support +
                            sidea_party_elite_support +
                            sidea_rural_worker_support +
                            sidea_military_support +
                            sidea_ethnic_racial_support +
                            cinc_a + sidea_winning_coalition_size, data = conflict_data)

  h4_full <- safe_glm(targets_democracy ~ sidea_religious_support +
                              sidea_party_elite_support +
                              sidea_rural_worker_support +
                              sidea_military_support +
                              sidea_ethnic_racial_support +
                              cinc_a + sidea_winning_coalition_size +
                              t_scaled + cold_war,   # <-- Change t to t_scaled
                      data = conflict_data)

  list(h4_baseline = h4_baseline, h4_party = h4_party,
       h4_military = h4_military, h4_multi = h4_multi,
       h4_controls = h4_controls, h4_full = h4_full)
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================
h3_models <- estimate_h3_logit(h34_data)
h4_models <- estimate_h4_logit(h34_data)

# ==============================================================================
# 4. VIF Diagnostics ----
# ==============================================================================
h3_vif <- lapply(setNames(names(h3_models), names(h3_models)), function(nm) safe_vif(h3_models[[nm]], nm))
h4_vif <- lapply(setNames(names(h4_models), names(h4_models)), function(nm) safe_vif(h4_models[[nm]], nm))

dir.create("results", showWarnings = FALSE)
saveRDS(h3_models, "results/h3_logit_models.rds")
saveRDS(h4_models, "results/h4_logit_models.rds")
saveRDS(list(h3 = h3_vif, h4 = h4_vif), "results/h34_vif.rds")

# Cleanup
rm(h34_data, h34_vars, h3_models, h4_models, h3_vif, h4_vif)
gc()
message("[04_h3_h4_logit.R] Done. Models saved to results/")
