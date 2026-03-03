# 05_m1_m2_mediation.R — Mediation and Survival Analysis (M1 and M2)

# Load required scripts
source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

# Variable note (monadic leader-level data: monadic_ready):
# avg_ideological_legit    = leader-period mean of v2exl_legitideol_a (M1 mediator)
# legit_ratio              = leader-period mean legit_ratio from 02_data_prep.R (M2 mediator)
#                            corresponds to legit_mix_ratio in the 2025 source repo
# conflicts_vs_ideo_targets = count of conflicts initiated against ideological targets
# conflicts_vs_democracy   = count of conflicts initiated against democracies
# conflicts_vs_ideo_opponent= count of conflicts against ideologically distant opponents
# avg_autocracy_level      = leader-period mean autocracy score
# log_avg_gdp              = log of leader-period mean GDP
# log_avg_pop              = log of leader-period mean population
# irregular_entry          = 1 if leader came to power irregularly
# log_tenure               = log of total tenure length in years
# tenure_years             = tenure length in years (for survival models)
# survived_to_end          = 1 if leader was still in power at data end (censored)
# avg_other_legit          = leader-period mean of non-ideological legitimation (M2)
# All variables sourced from monadic_ready (built in 02_data_prep.R)

# ==============================================================================
# Helper: safe VIF
# ==============================================================================
safe_vif <- function(model, label = deparse(substitute(model))) {
  tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) v <- v[, "GVIF"]
    message(sprintf("[05_m1_m2] VIF (%s): max = %.2f", label, max(v, na.rm = TRUE)))
    v
  }, error = function(e) {
    message(sprintf("[05_m1_m2] VIF failed for %s: %s", label, e$message))
    NULL
  })
}

# =============================================================================
# M1: Ideological Legitimation as Mediator
# =============================================================================

estimate_m1_models <- function(data) {
  mediator_model <- lm(avg_ideological_legit ~ conflicts_vs_ideo_targets +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  outcome_model  <- lm(log_tenure ~ conflicts_vs_ideo_targets + avg_ideological_legit +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  direct_model   <- lm(log_tenure ~ conflicts_vs_ideo_targets +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  return(list(
    mediator_model = mediator_model,
    outcome_model  = outcome_model,
    direct_model   = direct_model
  ))
}

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
# =============================================================================

estimate_m2_models <- function(data) {
  mediator_model <- lm(legit_ratio ~ conflicts_vs_ideo_targets +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  outcome_model  <- lm(log_tenure ~ conflicts_vs_ideo_targets + legit_ratio +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
  direct_model   <- lm(log_tenure ~ conflicts_vs_ideo_targets +
                         avg_autocracy_level + log_avg_gdp + log_avg_pop +
                         irregular_entry,
                       data = data)
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
m1_models     <- estimate_m1_models(monadic_ready)
m1_mediation  <- run_m1_mediation(m1_models)
m1_survival   <- estimate_m1_survival(monadic_ready)
m1_robustness <- run_m1_robustness(monadic_ready)

m2_models    <- estimate_m2_models(monadic_ready)
m2_mediation <- run_m2_mediation(m2_models)
m2_survival  <- estimate_m2_survival(monadic_ready)

# ==============================================================================
# VIF Diagnostics
# ==============================================================================
m1_vif <- list(
  mediator = safe_vif(m1_models$mediator_model, "m1_mediator"),
  outcome  = safe_vif(m1_models$outcome_model,  "m1_outcome"),
  direct   = safe_vif(m1_models$direct_model,   "m1_direct")
)
m2_vif <- list(
  mediator   = safe_vif(m2_models$mediator_model,           "m2_mediator"),
  outcome    = safe_vif(m2_models$outcome_model,            "m2_outcome"),
  direct     = safe_vif(m2_models$direct_model,             "m2_direct"),
  components = safe_vif(m2_models$outcome_model_components, "m2_components")
)

# Save results
dir.create("results", showWarnings = FALSE)
saveRDS(m1_models,     "results/m1_models.rds")
saveRDS(m1_mediation,  "results/m1_mediation.rds")
saveRDS(m1_survival,   "results/m1_survival.rds")
saveRDS(m1_robustness, "results/m1_robustness.rds")
saveRDS(m2_models,     "results/m2_models.rds")
saveRDS(m2_mediation,  "results/m2_mediation.rds")
saveRDS(m2_survival,   "results/m2_survival.rds")
saveRDS(list(m1 = m1_vif, m2 = m2_vif), "results/m12_vif.rds")

message("[05_m1_m2_mediation.R] Analysis complete. Models saved to results/")
