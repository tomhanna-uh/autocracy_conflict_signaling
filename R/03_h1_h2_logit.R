# ==============================================================================
# 03_h1_h2_logit.R -- Logit Analysis for Hypotheses 1 and 2
# H1: The Ideological Autocrat -- Initiation
#   Leader ideology (sidea_revisionist_domestic) -> MID initiation
# H2: The Ideological Autocrat -- Targeting
#   Leader ideology -> MID targeting of democracies
# Tier 1: Simple Logistic Regression
# ==============================================================================

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Variable note:
# sidea_revisionist_domestic = composite leader ideology score (GRAVE-D)
#   sub-types: sidea_nationalist/socialist/religious/reactionary/separatist
#              _revisionist_domestic
# mid_initiated   = binary: hostility level >= 2 (DV for H1)
# targets_democracy = binary: v2x_libdem_b >= 0.5 (DV for H2)
# cinc_a = COW CINC (capabilities control)
# sidea_winning_coalition_size = V-Dem/BdM selectorate control
# v2x_libdem_b    = V-Dem liberal democracy score of Side B
# All variables constructed in 02_data_prep.R
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Hypothesis 1: The Ideological Autocrat -- Initiation ----
# H1: Autocratic states with higher levels of revisionist domestic leadership
#     ideology will be more likely to originate a revisionist MID than other
#     autocratic states, all else equal.
# DV:  mid_initiated
# IV:  sidea_revisionist_domestic (composite)
# ==============================================================================

#' Estimate H1 Logit Models (Leader Ideology -> MID Initiation)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list of GLM objects: h1_baseline, h1_controls, h1_full,
#'     h1_religious, h1_socialist, h1_nationalist
estimate_h1_logit <- function(data) {

  # h1_baseline: Leader ideology only
  h1_baseline <- glm(mid_initiated ~ sidea_revisionist_domestic,
                     family = binomial(link = "logit"),
                     data = data)

  # h1_controls: Add capabilities and selectorate controls
  h1_controls <- glm(mid_initiated ~ sidea_revisionist_domestic +
                       cinc_a +
                       sidea_winning_coalition_size,
                     family = binomial(link = "logit"),
                     data = data)

  # h1_full: Add target regime type + temporal controls
  h1_full <- glm(mid_initiated ~ sidea_revisionist_domestic +
                   targets_democracy +
                   cinc_a +
                   sidea_winning_coalition_size +
                   t + t2 + t3 + cold_war,
                 family = binomial(link = "logit"),
                 data = data)

  # Sub-hypothesis models by ideology type
  h1_religious <- glm(mid_initiated ~ sidea_religious_revisionist_domestic +
                        targets_democracy +
                        cinc_a +
                        sidea_winning_coalition_size +
                        t + t2 + t3 + cold_war,
                      family = binomial(link = "logit"),
                      data = data)

  h1_socialist <- glm(mid_initiated ~ sidea_socialist_revisionist_domestic +
                        targets_democracy +
                        cinc_a +
                        sidea_winning_coalition_size +
                        t + t2 + t3 + cold_war,
                      family = binomial(link = "logit"),
                      data = data)

  h1_nationalist <- glm(mid_initiated ~ sidea_nationalist_revisionist_domestic +
                          targets_democracy +
                          cinc_a +
                          sidea_winning_coalition_size +
                          t + t2 + t3 + cold_war,
                        family = binomial(link = "logit"),
                        data = data)

  return(list(
    h1_baseline    = h1_baseline,
    h1_controls    = h1_controls,
    h1_full        = h1_full,
    h1_religious   = h1_religious,
    h1_socialist   = h1_socialist,
    h1_nationalist = h1_nationalist
  ))
}

# ==============================================================================
# 2. Hypothesis 2: The Ideological Autocrat -- Targeting ----
# H2: Autocratic states with higher levels of revisionist domestic leadership
#     ideology will be more likely to originate revisionist MIDs targeting
#     democracies than other autocratic states, all else equal.
# DV:  targets_democracy (conditional on mid_initiated == 1)
# IV:  sidea_revisionist_domestic (composite)
# ==============================================================================

#' Estimate H2 Logit Models (Leader Ideology -> Democracy Targeting)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list of GLM objects: h2_baseline, h2_controls, h2_full,
#'     h2_religious, h2_socialist, h2_nationalist
estimate_h2_logit <- function(data) {

  # Filter to conflict initiations only
  conflict_data <- data %>% filter(mid_initiated == 1)

  # h2_baseline: Leader ideology only
  h2_baseline <- glm(targets_democracy ~ sidea_revisionist_domestic,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h2_controls: Add capabilities and selectorate
  h2_controls <- glm(targets_democracy ~ sidea_revisionist_domestic +
                       cinc_a +
                       sidea_winning_coalition_size,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h2_full: Add temporal controls
  h2_full <- glm(targets_democracy ~ sidea_revisionist_domestic +
                   cinc_a +
                   sidea_winning_coalition_size +
                   t + cold_war,
                 family = binomial(link = "logit"),
                 data = conflict_data)

  # Sub-hypothesis models by ideology type
  h2_religious <- glm(targets_democracy ~ sidea_religious_revisionist_domestic +
                        cinc_a +
                        sidea_winning_coalition_size +
                        t + cold_war,
                      family = binomial(link = "logit"),
                      data = conflict_data)

  h2_socialist <- glm(targets_democracy ~ sidea_socialist_revisionist_domestic +
                        cinc_a +
                        sidea_winning_coalition_size +
                        t + cold_war,
                      family = binomial(link = "logit"),
                      data = conflict_data)

  h2_nationalist <- glm(targets_democracy ~ sidea_nationalist_revisionist_domestic +
                          cinc_a +
                          sidea_winning_coalition_size +
                          t + cold_war,
                        family = binomial(link = "logit"),
                        data = conflict_data)

  return(list(
    h2_baseline    = h2_baseline,
    h2_controls    = h2_controls,
    h2_full        = h2_full,
    h2_religious   = h2_religious,
    h2_socialist   = h2_socialist,
    h2_nationalist = h2_nationalist
  ))
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================

# Run H1 models
h1_models <- estimate_h1_logit(dyad_ready)

# Run H2 models
h2_models <- estimate_h2_logit(dyad_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h1_models, "results/h1_logit_models.rds")
saveRDS(h2_models, "results/h2_logit_models.rds")

message("[03_h1_h2_logit.R] H1 and H2 analysis complete. Models saved to results/")
