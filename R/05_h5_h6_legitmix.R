# ==============================================================================
# 05_h5_h6_legitmix.R — Legitimation Mix Analysis for Hypotheses 5 and 6
# H5: Legitimation Mix — Initiation
#    Relative ideological legitimation dependence → conflict initiation
# H6: Legitimation Mix — Targeting
#    Relative ideological legitimation dependence → democracy targeting
# Tier 2: Logistic + Hurdle Models
# ==============================================================================

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Variable note:
# v2exl_legitideol_a = V-Dem ideological legitimation score (Side A)
# v2exl_legitlead_a  = V-Dem personalist legitimation score (Side A)
# v2exl_legitperf_a  = V-Dem performance legitimation score (Side A)
# legit_ratio        = v2exl_legitideol_a / legit_total (from 02_data_prep.R)
# legit_total        = sum of all three legitimation components
# mid_initiated   = binary: hostility level >= 2 (DV for H5)
# targets_democracy = binary: v2x_libdem_b >= 0.5 (DV for H6)
# All variables constructed in 02_data_prep.R
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Hypothesis 5: Legitimation Mix — Initiation ----
# H5: The relative dependence on ideological legitimation (compared to
#     performance or personalist legitimation) increases the likelihood of
#     conflict initiation, all else equal.
# DV:  mid_initiated
# IV:  legit_ratio (ideological share of legitimation portfolio)
# ==============================================================================

#' Estimate H5 Logit Models (Legitimation Mix -> MID Initiation)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list of GLM objects
estimate_h5_logit <- function(data) {

  # h5_baseline: Legitimation ratio only
  h5_baseline <- glm(mid_initiated ~ legit_ratio,
                     family = binomial(link = "logit"),
                     data = data)

  # h5_components: Add legitimation components (decomposition)
  h5_components <- glm(mid_initiated ~ legit_ratio +
                        v2exl_legitperf_a + v2exl_legitlead_a,
                       family = binomial(link = "logit"),
                       data = data)

  # h5_controls: Add capabilities and selectorate
  h5_controls <- glm(mid_initiated ~ legit_ratio +
                      cinc_a +
                      sidea_winning_coalition_size,
                     family = binomial(link = "logit"),
                     data = data)

  # h5_full: Full model with target regime + temporal controls
  h5_full <- glm(mid_initiated ~ legit_ratio +
                  v2exl_legitperf_a + v2exl_legitlead_a +
                  targets_democracy +
                  cinc_a +
                  sidea_winning_coalition_size +
                  t + t2 + t3 + cold_war,
                 family = binomial(link = "logit"),
                 data = data)

  return(list(
    h5_baseline   = h5_baseline,
    h5_components = h5_components,
    h5_controls   = h5_controls,
    h5_full       = h5_full
  ))
}

#' Estimate H5 Hurdle Models (Two-part: any conflict + conflict count)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list: hurdle_initiation (logit part) and hurdle_count (count part)
estimate_h5_hurdle <- function(data) {

  # Part 1 (binary hurdle): Does legitimation mix predict any conflict?
  hurdle_binary <- glm(mid_initiated ~ legit_ratio +
                        v2exl_legitperf_a + v2exl_legitlead_a +
                        targets_democracy +
                        cinc_a +
                        sidea_winning_coalition_size +
                        t + t2 + t3 + cold_war,
                       family = binomial(link = "logit"),
                       data = data)

  # Part 2 (count): Among conflict initiators, does legitimation mix predict
  # number of MIDs? (uses Poisson; swap for negative binomial if overdispersed)
  initiators <- data %>% filter(mid_initiated == 1)

  hurdle_count <- glm(mid_initiated ~ legit_ratio +
                       v2exl_legitperf_a + v2exl_legitlead_a +
                       cinc_a +
                       sidea_winning_coalition_size +
                       cold_war,
                      family = poisson(link = "log"),
                      data = initiators)

  return(list(
    hurdle_binary = hurdle_binary,
    hurdle_count  = hurdle_count
  ))
}

# ==============================================================================
# 2. Hypothesis 6: Legitimation Mix — Targeting ----
# H6: The relative dependence on ideological legitimation increases the
#     likelihood of targeting democracies, all else equal.
# DV:  targets_democracy (conditional on mid_initiated == 1)
# IV:  legit_ratio
# ==============================================================================

#' Estimate H6 Logit Models (Legitimation Mix -> Democracy Targeting)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A named list of GLM objects
estimate_h6_logit <- function(data) {

  # Filter to conflict initiations only
  conflict_data <- data %>% filter(mid_initiated == 1)

  # h6_baseline: Legitimation ratio only
  h6_baseline <- glm(targets_democracy ~ legit_ratio,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h6_components: Add legitimation components
  h6_components <- glm(targets_democracy ~ legit_ratio +
                        v2exl_legitperf_a + v2exl_legitlead_a,
                       family = binomial(link = "logit"),
                       data = conflict_data)

  # h6_controls: Add capabilities and selectorate
  h6_controls <- glm(targets_democracy ~ legit_ratio +
                      cinc_a +
                      sidea_winning_coalition_size,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # h6_full: Full model with temporal controls
  h6_full <- glm(targets_democracy ~ legit_ratio +
                  v2exl_legitperf_a + v2exl_legitlead_a +
                  cinc_a +
                  sidea_winning_coalition_size +
                  t + cold_war,
                 family = binomial(link = "logit"),
                 data = conflict_data)

  # h6_interaction: Interaction of ideological ratio with leader ideology (test of H1xH5)
  h6_interaction <- glm(targets_democracy ~ legit_ratio * sidea_revisionist_domestic +
                         cinc_a +
                         sidea_winning_coalition_size +
                         t + cold_war,
                        family = binomial(link = "logit"),
                        data = conflict_data)

  return(list(
    h6_baseline    = h6_baseline,
    h6_components  = h6_components,
    h6_controls    = h6_controls,
    h6_full        = h6_full,
    h6_interaction = h6_interaction
  ))
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================

# Run H5 models
h5_logit_models  <- estimate_h5_logit(dyad_ready)
h5_hurdle_models <- estimate_h5_hurdle(dyad_ready)

# Run H6 models
h6_logit_models <- estimate_h6_logit(dyad_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h5_logit_models,  "results/h5_logit_models.rds")
saveRDS(h5_hurdle_models, "results/h5_hurdle_models.rds")
saveRDS(h6_logit_models,  "results/h6_logit_models.rds")

message("[05_h5_h6_legitmix.R] H5 and H6 analysis complete. Models saved to results/")
