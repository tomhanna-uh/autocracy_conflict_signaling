# ==============================================================================
# 04_h3_h4_logit.R — Logit Analysis for Hypotheses 3 and 4
# H3: The Rational Autocrat — Initiation
#    Support group ideology → MID initiation
# H4: The Rational Autocrat — Targeting
#    Support group ideology → MID targeting of democracies
# Tier 1: Simple Logistic Regression
# ==============================================================================

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Variable note:
# Support group IVs (GRAVE-D):
#   sidea_religious_support        = religious group regime support
#   sidea_party_elite_support      = party elite regime support
#   sidea_rural_worker_support     = rural/worker group regime support
#   sidea_military_support         = military regime support
#   sidea_ethnic_racial_support    = ethnic/racial group regime support
# mid_initiated   = binary: hostility level >= 2 (DV for H3)
# targets_democracy = binary: v2x_libdem_b >= 0.5 (DV for H4)
# cinc_a = COW CINC (capabilities control)
# sidea_winning_coalition_size = V-Dem/BdM selectorate control
# All variables constructed in 02_data_prep.R
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Hypothesis 3: The Rational Autocrat — Initiation ----
# H3: Autocratic states with higher levels of regime support from groups
#     associated with revisionist ideologies will be more likely to originate
#     a revisionist MID than other autocratic states, all else equal.
# DV:  mid_initiated
# IV:  support group variables (sidea_religious_support, etc.)
# ==============================================================================

#' Estimate H3 Logit Models (Support Groups -> MID Initiation)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list of GLM objects
estimate_h3_logit <- function(data) {

  # h3_baseline: Religious support only
  h3_baseline <- glm(mid_initiated ~ sidea_religious_support,
                     family = binomial(link = "logit"),
                     data = data)

  # h3_party: Party elite support
  h3_party <- glm(mid_initiated ~ sidea_party_elite_support,
                  family = binomial(link = "logit"),
                  data = data)

  # h3_military: Military support
  h3_military <- glm(mid_initiated ~ sidea_military_support,
                     family = binomial(link = "logit"),
                     data = data)

  # h3_multi: All support groups (multi-variable)
  h3_multi <- glm(mid_initiated ~ sidea_religious_support +
                    sidea_party_elite_support +
                    sidea_rural_worker_support +
                    sidea_military_support +
                    sidea_ethnic_racial_support,
                  family = binomial(link = "logit"),
                  data = data)

  # h3_controls: Multi support + capability controls
  h3_controls <- glm(mid_initiated ~ sidea_religious_support +
                      sidea_party_elite_support +
                      sidea_rural_worker_support +
                      sidea_military_support +
                      sidea_ethnic_racial_support +
                      cinc_a +
                      sidea_winning_coalition_size,
                     family = binomial(link = "logit"),
                     data = data)

  # h3_full: Full model with targets + temporal controls
  h3_full <- glm(mid_initiated ~ sidea_religious_support +
                  sidea_party_elite_support +
                  sidea_rural_worker_support +
                  sidea_military_support +
                  sidea_ethnic_racial_support +
                  targets_democracy +
                  cinc_a +
                  sidea_winning_coalition_size +
                  t + t2 + t3 + cold_war,
                 family = binomial(link = "logit"),
                 data = data)

  return(list(
    h3_baseline = h3_baseline,
    h3_party    = h3_party,
    h3_military = h3_military,
    h3_multi    = h3_multi,
    h3_controls = h3_controls,
    h3_full     = h3_full
  ))
}

# ==============================================================================
# 2. Hypothesis 4: The Rational Autocrat — Targeting ----
# H4: Autocratic states with higher levels of regime support from groups
#     associated with revisionist ideologies will be more likely to originate
#     MIDs targeting democracies than other autocratic states, all else equal.
# DV:  targets_democracy (conditional on mid_initiated == 1)
# IV:  support group variables
# ==============================================================================

#' Estimate H4 Logit Models (Support Groups -> Democracy Targeting)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list of GLM objects
estimate_h4_logit <- function(data) {

  # Filter to conflict initiations only
  conflict_data <- data %>% filter(mid_initiated == 1)

  # h4_baseline: Religious support only
  h4_baseline <- glm(targets_democracy ~ sidea_religious_support,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h4_party: Party elite support
  h4_party <- glm(targets_democracy ~ sidea_party_elite_support,
                  family = binomial(link = "logit"),
                  data = conflict_data)

  # h4_military: Military support
  h4_military <- glm(targets_democracy ~ sidea_military_support,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h4_multi: All support groups
  h4_multi <- glm(targets_democracy ~ sidea_religious_support +
                    sidea_party_elite_support +
                    sidea_rural_worker_support +
                    sidea_military_support +
                    sidea_ethnic_racial_support,
                  family = binomial(link = "logit"),
                  data = conflict_data)

  # h4_controls: Multi support + capability controls
  h4_controls <- glm(targets_democracy ~ sidea_religious_support +
                      sidea_party_elite_support +
                      sidea_rural_worker_support +
                      sidea_military_support +
                      sidea_ethnic_racial_support +
                      cinc_a +
                      sidea_winning_coalition_size,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h4_full: Full model with temporal controls
  h4_full <- glm(targets_democracy ~ sidea_religious_support +
                  sidea_party_elite_support +
                  sidea_rural_worker_support +
                  sidea_military_support +
                  sidea_ethnic_racial_support +
                  cinc_a +
                  sidea_winning_coalition_size +
                  t + cold_war,
                 family = binomial(link = "logit"),
                 data = conflict_data)

  return(list(
    h4_baseline = h4_baseline,
    h4_party    = h4_party,
    h4_military = h4_military,
    h4_multi    = h4_multi,
    h4_controls = h4_controls,
    h4_full     = h4_full
  ))
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================

# Run H3 models
h3_models <- estimate_h3_logit(dyad_ready)

# Run H4 models
h4_models <- estimate_h4_logit(dyad_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h3_models, "results/h3_logit_models.rds")
saveRDS(h4_models, "results/h4_logit_models.rds")

message("[04_h3_h4_logit.R] H3 and H4 analysis complete. Models saved to results/")
