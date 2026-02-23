# ==============================================================================
# 06_h9_survival.R — Cox Proportional Hazard Leader Survival Models
# H9: Survival Mediation
#   Conflict initiation against ideological targets increases leader survival
#   by enhancing domestic ideological legitimation.
# Tier 3: Mediation, Moderation, and Survival
# ==============================================================================

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Variable note (monadic leader-level data: monadic_ready):
# conflicts_vs_ideo_targets  = count of MIDs initiated against ideological targets
# conflicts_vs_democracy     = count of MIDs initiated against democracies
# conflicts_vs_ideo_opponent = count of MIDs against ideologically distant opponents
# avg_ideological_legit      = leader-period mean of v2exl_legitideol_a
# legit_ratio                = leader-period mean of ideological legitimation share
# avg_autocracy_level        = leader-period mean autocracy score
# log_avg_gdp                = log of leader-period mean GDP
# log_avg_pop                = log of leader-period mean population
# irregular_entry            = 1 if leader came to power irregularly
# tenure_years               = leader tenure length in years
# survived_to_end            = 1 if leader was still in power at data end (censored)
# All variables sourced from monadic_ready (built in 02_data_prep.R)
# ------------------------------------------------------------------------------
# Model naming convention (per README):
#   cox_h9_ideology, cox_h9_ratio, cox_h9_int_ideology, cox_h9_int_ratio
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Hypothesis 9: Survival Mediation ----
# H9: Conflict initiation against ideological targets increases leader survival
#     by enhancing domestic ideological legitimation.
# Event:   leader removal (survived_to_end == 0)
# Outcome: Surv(tenure_years, event)
# ==============================================================================

#' Estimate Cox PH Survival Models for H9
#' @param data Leader-level monadic data (monadic_ready)
#' @return Named list of coxph objects
estimate_h9_survival <- function(data) {

  # Create survival data with event indicator
  surv_data <- data %>%
    mutate(event = as.integer(survived_to_end == 0))

  # cox_h9_ideology: Conflict vs ideological targets + avg ideological legitimation
  # Tests whether ideological conflict -> legitimation -> survival
  cox_h9_ideology <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_ideological_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop +
      irregular_entry,
    data = surv_data
  )

  # cox_h9_ratio: Conflict vs ideological targets + legitimation ratio
  # Tests whether ideological conflict -> legitimation mix -> survival
  cox_h9_ratio <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop +
      irregular_entry,
    data = surv_data
  )

  # cox_h9_int_ideology: Interaction of conflict vs democracies * ideological legitimation
  # Tests whether the survival benefit depends on ideological legitimation level
  cox_h9_int_ideology <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_democracy * avg_ideological_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop +
      irregular_entry,
    data = surv_data
  )

  # cox_h9_int_ratio: Interaction of conflict vs democracies * legitimation ratio
  cox_h9_int_ratio <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_democracy * legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop +
      irregular_entry,
    data = surv_data
  )

  # cox_h9_direct: Direct effect only (without mediator) for comparison
  cox_h9_direct <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_autocracy_level + log_avg_gdp + log_avg_pop +
      irregular_entry,
    data = surv_data
  )

  # cox_h9_full: Full model with both legitimation measures
  cox_h9_full <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_ideological_legit + legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop +
      irregular_entry,
    data = surv_data
  )

  return(list(
    cox_h9_ideology     = cox_h9_ideology,
    cox_h9_ratio        = cox_h9_ratio,
    cox_h9_int_ideology = cox_h9_int_ideology,
    cox_h9_int_ratio    = cox_h9_int_ratio,
    cox_h9_direct       = cox_h9_direct,
    cox_h9_full         = cox_h9_full,
    surv_data           = surv_data
  ))
}

# ==============================================================================
# 2. Execution and Saving Results ----
# ==============================================================================

# Run H9 survival models
h9_survival <- estimate_h9_survival(monadic_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h9_survival, "results/h9_survival.rds")

message("[06_h9_survival.R] H9 survival analysis complete. Models saved to results/")
