# 05_m1_m2_mediation.R — Mediation and Survival Analysis (M1 and M2)

# Load required scripts
source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

# Variable note (monadic leader-level data: monadic_ready):
# avg_ideological_legit     = leader-period mean of v2exl_legitideol_a (M1 mediator)
# legit_ratio               = leader-period mean legit_ratio from 02_data_prep.R (M2 mediator)
#                             corresponds to legit_mix_ratio in the 2025 source repo
# conflicts_vs_ideo_targets = count of conflicts initiated against ideological targets
# conflicts_vs_democracy    = count of conflicts initiated against democracies
# conflicts_vs_ideo_opponent= count of conflicts against ideologically distant opponents
# avg_autocracy_level       = leader-period mean autocracy score
# log_avg_gdp               = log of leader-period mean GDP
# log_avg_pop               = log of leader-period mean population
# irregular_entry           = 1 if leader came to power irregularly
# log_tenure                = log of total tenure length in years
# tenure_years              = tenure length in years (for survival models)
# survived_to_end           = 1 if leader was still in power at data end (censored)
# avg_other_legit           = leader-period mean of non-ideological legitimation (M2)
# All variables sourced from monadic_ready (built in 02_data_prep.R)

# =============================================================================
# M1: Ideological Legitimation as Mediator
# Q: Does conflict against ideological targets increase ideological legitimation,
#    which in turn extends leader tenure?
# Treatment: conflicts_vs_ideo_targets
# Mediator:  avg_ideological_legit
# Outcome:   log_tenure / tenure_years
# =============================================================================

#' Estimate M1 Component Models
#' @param data Leader-level monadic data (monadic_ready)
#' @return Named list: mediator_model, outcome_model, direct_model
estimate_m1_models <- function(data) {
  mediator_model <- lm(avg_ideological_legit ~ conflicts_vs_ideo_targets +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  outcome_model <- lm(log_tenure ~ conflicts_vs_ideo_targets + avg_ideological_legit +
                        avg_autocracy_level + log_avg_gdp + log_avg_pop +
                        irregular_entry,
                      data = data)
  direct_model <- lm(log_tenure ~ conflicts_vs_ideo_targets +
                       avg_autocracy_level + log_avg_gdp + log_avg_pop +
                       irregular_entry,
                     data = data)
  return(list(
    mediator_model = mediator_model,
    outcome_model  = outcome_model,
    direct_model   = direct_model
  ))
}

#' Run Formal Causal Mediation Analysis for M1
#' @param models List from estimate_m1_models()
#' @param sims Bootstrap simulations (default 1000)
#' @return mediate() object
run_m1_mediation <- function(models, sims = 1000) {
  set.seed(12345)
  mediate(
    models$mediator_model,
    models$outcome_model,
    treat    = "conflicts_vs_ideo_targets",
    mediator = "avg_ideological_legit",
    boot     = TRUE,
    sims     = sims
  )
}

#' Estimate M1 Survival (Cox) Models
#' @param data Leader-level monadic data (monadic_ready)
#' @return Named list of coxph objects plus survival data
estimate_m1_survival <- function(data) {
  leader_surv <- data %>%
    mutate(event = as.integer(survived_to_end == 0))
  cox_direct <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  cox_mediated <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets + avg_ideological_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  cox_interaction_1 <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_democracy * avg_ideological_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  cox_interaction_2 <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_democracy * legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  return(list(
    cox_direct        = cox_direct,
    cox_mediated      = cox_mediated,
    cox_interaction_1 = cox_interaction_1,
    cox_interaction_2 = cox_interaction_2,
    leader_surv_data  = leader_surv
  ))
}

#' Run M1 Robustness Mediation Models
#' @param data Leader-level monadic data (monadic_ready)
#' @param sims Bootstrap simulations (default 500)
#' @return Named list of mediate() objects
run_m1_robustness <- function(data, sims = 500) {
  set.seed(12345)
  med_democracy <- mediate(
    lm(avg_ideological_legit ~ conflicts_vs_democracy +
         avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
       data = data),
    lm(log_tenure ~ conflicts_vs_democracy + avg_ideological_legit +
         avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
       data = data),
    treat = "conflicts_vs_democracy", mediator = "avg_ideological_legit",
    boot = TRUE, sims = sims)
  med_ideo_gap <- mediate(
    lm(avg_ideological_legit ~ conflicts_vs_ideo_opponent +
         avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
       data = data),
    lm(log_tenure ~ conflicts_vs_ideo_opponent + avg_ideological_legit +
         avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
       data = data),
    treat = "conflicts_vs_ideo_opponent", mediator = "avg_ideological_legit",
    boot = TRUE, sims = sims)
  return(list(med_democracy = med_democracy, med_ideo_gap = med_ideo_gap))
}

# =============================================================================
# M2: Legitimation Ratio (Mix) as Mediator
# Q: Does conflict against ideological targets increase the ideological SHARE
#    of legitimation (legit_ratio), which in turn extends tenure?
# Treatment: conflicts_vs_ideo_targets
# Mediator:  legit_ratio  (= legit_mix_ratio in the 2025 source repo)
# Outcome:   log_tenure / tenure_years
# =============================================================================

#' Estimate M2 Component Models
#' @param data Leader-level monadic data (monadic_ready)
#' @return Named list: mediator_model, outcome_model, direct_model,
#'         outcome_model_components
estimate_m2_models <- function(data) {
  mediator_model <- lm(legit_ratio ~ conflicts_vs_ideo_targets +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  outcome_model <- lm(log_tenure ~ conflicts_vs_ideo_targets + legit_ratio +
                        avg_autocracy_level + log_avg_gdp + log_avg_pop +
                        irregular_entry,
                      data = data)
  direct_model <- lm(log_tenure ~ conflicts_vs_ideo_targets +
                       avg_autocracy_level + log_avg_gdp + log_avg_pop +
                       irregular_entry,
                     data = data)
  # Model D: test whether ratio adds beyond absolute legitimation levels
  outcome_model_components <- lm(
    log_tenure ~ conflicts_vs_ideo_targets + legit_ratio +
      avg_ideological_legit + avg_other_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = data)
  return(list(
    mediator_model           = mediator_model,
    outcome_model            = outcome_model,
    direct_model             = direct_model,
    outcome_model_components = outcome_model_components
  ))
}

#' Run Formal Causal Mediation Analysis for M2
#' @param models List from estimate_m2_models()
#' @param sims Bootstrap simulations (default 1000)
#' @return mediate() object
run_m2_mediation <- function(models, sims = 1000) {
  set.seed(12345)
  mediate(
    models$mediator_model,
    models$outcome_model,
    treat    = "conflicts_vs_ideo_targets",
    mediator = "legit_ratio",
    boot     = TRUE,
    sims     = sims
  )
}

#' Estimate M2 Survival (Cox) Models
#' @param data Leader-level monadic data (monadic_ready)
#' @return Named list of coxph objects plus survival data
estimate_m2_survival <- function(data) {
  leader_surv <- data %>%
    mutate(event = as.integer(survived_to_end == 0))
  cox_direct <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  cox_mediated <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets + legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  # Full: ratio + absolute components (uniqueness test)
  cox_full <- coxph(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      legit_ratio + avg_ideological_legit + avg_other_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = leader_surv)
  return(list(
    cox_direct       = cox_direct,
    cox_mediated     = cox_mediated,
    cox_full         = cox_full,
    leader_surv_data = leader_surv
  ))
}

# =============================================================================
# Execution
# =============================================================================

# M1
m1_models     <- estimate_m1_models(monadic_ready)
m1_mediation  <- run_m1_mediation(m1_models)
m1_survival   <- estimate_m1_survival(monadic_ready)
m1_robustness <- run_m1_robustness(monadic_ready)

# M2
m2_models     <- estimate_m2_models(monadic_ready)
m2_mediation  <- run_m2_mediation(m2_models)
m2_survival   <- estimate_m2_survival(monadic_ready)

# Save results
dir.create("results", showWarnings = FALSE)
saveRDS(m1_models,     "results/m1_models.rds")
saveRDS(m1_mediation,  "results/m1_mediation.rds")
saveRDS(m1_survival,   "results/m1_survival.rds")
saveRDS(m1_robustness, "results/m1_robustness.rds")
saveRDS(m2_models,     "results/m2_models.rds")
saveRDS(m2_mediation,  "results/m2_mediation.rds")
saveRDS(m2_survival,   "results/m2_survival.rds")

message("[05_m1_m2_mediation.R] Analysis complete. Models saved to results/")
