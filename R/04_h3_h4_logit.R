# 04_h3_h4_logit.R — Logit Analysis for Hypotheses 3 and 4

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# Variable note:
# legit_ratio     = ideological legitimation share (v2exl_legitideol_a / legit_total)
#                   corresponds to legit_mix_ratio_a in the 2025 source repo
# v2exl_legitperf_a  = performance legitimation component (cf. performance_legit_a)
# v2exl_legitlead_a  = leader/personalism legitimation component (cf. leader_personalism_a)
# ideology_gap    = abs difference in ideological legitimation between sides (H3 IV)
# large_ideology_gap = binary: ideology_gap > median (H3 alternative IV)
# All variables constructed in 02_data_prep.R

# 1. Hypothesis 3: Legitimation Mix and Conflict Initiation ----
# H3: Autocracies with a higher ideological legitimation share (relative to
#     performance and personalist strategies) are more likely to initiate conflict.
# DV: mid_initiated
# Core IV: legit_ratio

#' Estimate H3 Logit Models
#' @param data Prepared dyadic data (dyad_ready)
#' @return A list of GLM objects
estimate_h3_logit <- function(data) {
  # Model 1: Legitimation ratio only (baseline)
  m1 <- glm(mid_initiated ~ legit_ratio,
            family = binomial(link = "logit"),
            data = data)

  # Model 2: Add legitimation component breakdown
  m2 <- glm(mid_initiated ~ legit_ratio +
              v2exl_legitperf_a + v2exl_legitlead_a,
            family = binomial(link = "logit"),
            data = data)

  # Model 3: Add target regime type
  m3 <- glm(mid_initiated ~ legit_ratio +
              targets_democracy,
            family = binomial(link = "logit"),
            data = data)

  # Model 4: Add capabilities and temporal controls
  m4 <- glm(mid_initiated ~ legit_ratio +
              targets_democracy +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3,
            family = binomial(link = "logit"),
            data = data)

  # Model 5: Full model with legitimation components
  m5 <- glm(mid_initiated ~ legit_ratio +
              v2exl_legitperf_a + v2exl_legitlead_a +
              targets_democracy +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3 + cold_war,
            family = binomial(link = "logit"),
            data = data)

  # Model 6: Replace legit_ratio with ideology_gap (alternative H3 operationalization)
  m6 <- glm(mid_initiated ~ ideology_gap +
              targets_democracy +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3,
            family = binomial(link = "logit"),
            data = data)

  return(list(
    logit1 = m1,
    logit2 = m2,
    logit3 = m3,
    logit4 = m4,
    logit5 = m5,
    logit6 = m6
  ))
}

# 2. Hypothesis 4: Legitimation Mix and Democracy Targeting ----
# H4: Autocracies with a higher ideological legitimation share are more likely
#     to target democracies (conditional on conflict initiation).
# DV: targets_democracy (among mid_initiated == 1)
# Core IV: legit_ratio

#' Estimate H4 Logit Models (filtered: initiators only)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A list of GLM objects
estimate_h4_logit <- function(data) {
  # Filter to conflict initiations only
  conflict_data <- data %>% filter(mid_initiated == 1)

  # Model 1: Legitimation ratio only (baseline)
  m1 <- glm(targets_democracy ~ legit_ratio,
            family = binomial(link = "logit"),
            data = conflict_data)

  # Model 2: Add legitimation component breakdown
  m2 <- glm(targets_democracy ~ legit_ratio +
              v2exl_legitperf_a + v2exl_legitlead_a,
            family = binomial(link = "logit"),
            data = conflict_data)

  # Model 3: Add capabilities
  m3 <- glm(targets_democracy ~ legit_ratio +
              log_cinc_a + log_cinc_b + cinc_ratio,
            family = binomial(link = "logit"),
            data = conflict_data)

  # Model 4: Full model with temporal controls
  m4 <- glm(targets_democracy ~ legit_ratio +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + cold_war,
            family = binomial(link = "logit"),
            data = conflict_data)

  # Model 5: Full model with legitimation components
  m5 <- glm(targets_democracy ~ legit_ratio +
              v2exl_legitperf_a + v2exl_legitlead_a +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + cold_war,
            family = binomial(link = "logit"),
            data = conflict_data)

  # Model 6: Ideology gap alternative operationalization
  m6 <- glm(targets_democracy ~ ideology_gap +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + cold_war,
            family = binomial(link = "logit"),
            data = conflict_data)

  # Model 7: Interaction (legit_ratio * ideology_gap)
  m7 <- glm(targets_democracy ~ legit_ratio * ideology_gap +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + cold_war,
            family = binomial(link = "logit"),
            data = conflict_data)

  return(list(
    logit1 = m1,
    logit2 = m2,
    logit3 = m3,
    logit4 = m4,
    logit5 = m5,
    logit6 = m6,
    logit7 = m7
  ))
}

# 3. H4 Unfiltered: Full dyad (conflict vs. democracy without initiation filter) ----
# Robustness check: does H4 pattern hold without conditioning on initiation?
# DV: targets_democracy (all dyad-years)

#' Estimate H4 Unfiltered Logit Models (all dyad-years)
#' @param data Prepared dyadic data (dyad_ready)
#' @return A list of GLM objects
estimate_h4_unfiltered_logit <- function(data) {
  # Model 1: Legitimation ratio only
  m1 <- glm(targets_democracy ~ legit_ratio,
            family = binomial(link = "logit"),
            data = data)

  # Model 2: Add legitimation components
  m2 <- glm(targets_democracy ~ legit_ratio +
              v2exl_legitperf_a + v2exl_legitlead_a,
            family = binomial(link = "logit"),
            data = data)

  # Model 3: Add capabilities
  m3 <- glm(targets_democracy ~ legit_ratio +
              log_cinc_a + log_cinc_b + cinc_ratio,
            family = binomial(link = "logit"),
            data = data)

  # Model 4: Full model with temporal controls
  m4 <- glm(targets_democracy ~ legit_ratio +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3 + cold_war,
            family = binomial(link = "logit"),
            data = data)

  # Model 5: Full model with legitimation components
  m5 <- glm(targets_democracy ~ legit_ratio +
              v2exl_legitperf_a + v2exl_legitlead_a +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3 + cold_war,
            family = binomial(link = "logit"),
            data = data)

  # Model 6: Ideology gap alternative
  m6 <- glm(targets_democracy ~ ideology_gap +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3 + cold_war,
            family = binomial(link = "logit"),
            data = data)

  # Model 7: Interaction (legit_ratio * ideology_gap)
  m7 <- glm(targets_democracy ~ legit_ratio * ideology_gap +
              log_cinc_a + log_cinc_b + cinc_ratio +
              t + t2 + t3 + cold_war,
            family = binomial(link = "logit"),
            data = data)

  return(list(
    logit1 = m1,
    logit2 = m2,
    logit3 = m3,
    logit4 = m4,
    logit5 = m5,
    logit6 = m6,
    logit7 = m7
  ))
}

# 4. Execution and Saving Results ----

# Run H3 Models
h3_models <- estimate_h3_logit(dyad_ready)

# Run H4 Models (filtered: initiators only)
h4_models <- estimate_h4_logit(dyad_ready)

# Run H4 Unfiltered Models (robustness check)
h4_unfiltered_models <- estimate_h4_unfiltered_logit(dyad_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h3_models,            "results/h3_logit_models.rds")
saveRDS(h4_models,            "results/h4_logit_models.rds")
saveRDS(h4_unfiltered_models, "results/h4_unfiltered_logit_models.rds")

message("[04_h3_h4_logit.R] Analysis complete. Models saved to results/")
