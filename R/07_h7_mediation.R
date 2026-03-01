# ==============================================================================
# 07_h7_mediation.R — Mediation Analysis for Hypothesis 7
# H7: Mediation — Support Groups
#    The effect of revisionist leadership ideology on targeting democracies
#    is mediated by the leader's dependence on ideologically associated
#    support groups.
# Tier 3: Mediation, Moderation, and Survival
# Uses: mediation package (Baron-Kenny + bootstrap) and lavaan (SEM)
# ==============================================================================

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Variable note (dyadic data: dyad_ready):
# Treatment (X): sidea_revisionist_domestic (leader ideology)
# Mediator  (M): Support group variables (religious, party, military, etc.)
#                sidea_religious_support, sidea_party_elite_support,
#                sidea_rural_worker_support, sidea_military_support,
#                sidea_ethnic_racial_support
# Outcome   (Y): targets_democracy (DV)
# Controls:      cinc_a, sidea_winning_coalition_size,
#                t, cold_war
# Model naming convention: med_h7_religious, fit_h7_socialist, fit_h7_party
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Baron-Kenny Mediation (mediation package) ----
# ==============================================================================

#' Estimate H7 Mediation Models (Baron-Kenny framework)
#' @param data Prepared dyadic data (dyad_ready)
#' @return Named list of component lm/glm models for each support group mediator
estimate_h7_component_models <- function(data) {

  # Filter to MID initiators for targeting outcome
  conflict_data <- data %>% filter(mid_initiated == 1)

  # --- Religious support as mediator ---
  # Path a: Treatment -> Mediator
  a_religious <- lm(sidea_religious_support ~ sidea_revisionist_domestic +
                     cinc_a +
                     sidea_winning_coalition_size + cold_war,
                    data = conflict_data)

  # Path b + c': Treatment + Mediator -> Outcome
  b_religious <- glm(targets_democracy ~ sidea_revisionist_domestic +
                      sidea_religious_support +
                      cinc_a +
                      sidea_winning_coalition_size +
                      t + cold_war,
                     family = binomial(link = "logit"),
                     data = conflict_data)

  # --- Party elite support as mediator ---
  a_party <- lm(sidea_party_elite_support ~ sidea_revisionist_domestic +
                 cinc_a +
                 sidea_winning_coalition_size + cold_war,
                data = conflict_data)

  b_party <- glm(targets_democracy ~ sidea_revisionist_domestic +
                  sidea_party_elite_support +
                  cinc_a +
                  sidea_winning_coalition_size +
                  t + cold_war,
                 family = binomial(link = "logit"),
                 data = conflict_data)

  # --- Military support as mediator ---
  a_military <- lm(sidea_military_support ~ sidea_revisionist_domestic +
                    cinc_a +
                    sidea_winning_coalition_size + cold_war,
                   data = conflict_data)

  b_military <- glm(targets_democracy ~ sidea_revisionist_domestic +
                     sidea_military_support +
                     cinc_a +
                     sidea_winning_coalition_size +
                     t + cold_war,
                    family = binomial(link = "logit"),
                    data = conflict_data)

  return(list(
    a_religious = a_religious,
    b_religious = b_religious,
    a_party     = a_party,
    b_party     = b_party,
    a_military  = a_military,
    b_military  = b_military
  ))
}

#' Run Formal Mediation Analysis for H7 (bootstrap)
#' @param component_models List from estimate_h7_component_models()
#' @param sims Bootstrap simulations (default 1000)
#' @return Named list of mediate() objects
run_h7_mediation <- function(component_models, sims = 1000) {

  set.seed(12345)

  # med_h7_religious: Religious support as mediator
  med_h7_religious <- mediate(
    component_models$a_religious,
    component_models$b_religious,
    treat    = "sidea_revisionist_domestic",
    mediator = "sidea_religious_support",
    boot     = TRUE,
    sims     = sims
  )

  # fit_h7_party: Party elite support as mediator
  fit_h7_party <- mediate(
    component_models$a_party,
    component_models$b_party,
    treat    = "sidea_revisionist_domestic",
    mediator = "sidea_party_elite_support",
    boot     = TRUE,
    sims     = sims
  )

  # fit_h7_socialist (maps to military support as proxy for organized ideological coalition)
  fit_h7_socialist <- mediate(
    component_models$a_military,
    component_models$b_military,
    treat    = "sidea_revisionist_domestic",
    mediator = "sidea_military_support",
    boot     = TRUE,
    sims     = sims
  )

  return(list(
    med_h7_religious = med_h7_religious,
    fit_h7_party     = fit_h7_party,
    fit_h7_socialist = fit_h7_socialist
  ))
}

# ==============================================================================
# 2. SEM / lavaan Mediation ----
# ==============================================================================

#' Estimate H7 SEM Mediation Models (lavaan)
#' @param data Prepared dyadic data (dyad_ready), conflict initiators only
#' @return Named list of fitted lavaan objects
estimate_h7_sem <- function(data) {

  conflict_data <- data %>% filter(mid_initiated == 1)

  # Model 1: Religious support mediates ideology -> democracy targeting
  sem_religious <- '
    # Mediator model (path a)
    sidea_religious_support ~ a * sidea_revisionist_domestic +
                              cinc_a +
                              sidea_winning_coalition_size

    # Outcome model (paths b and c prime)
    targets_democracy ~ b * sidea_religious_support +
                        cp * sidea_revisionist_domestic +
                        cinc_a +
                        sidea_winning_coalition_size

    # Indirect and total effects
    ab    := a * b
    total := cp + ab
  '

  fit_sem_religious <- lavaan::sem(sem_religious, data = conflict_data)

  # Model 2: Party elite support mediates ideology -> democracy targeting
  sem_party <- '
    sidea_party_elite_support ~ a * sidea_revisionist_domestic +
                                cinc_a +
                                sidea_winning_coalition_size

    targets_democracy ~ b * sidea_party_elite_support +
                        cp * sidea_revisionist_domestic +
                        cinc_a +
                        sidea_winning_coalition_size

    ab    := a * b
    total := cp + ab
  '

  fit_sem_party <- lavaan::sem(sem_party, data = conflict_data)

  return(list(
    fit_sem_religious = fit_sem_religious,
    fit_sem_party     = fit_sem_party
  ))
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================

# Component models
h7_components <- estimate_h7_component_models(dyad_ready)

# Formal mediation (bootstrap)
h7_mediation <- run_h7_mediation(h7_components)

# SEM mediation (lavaan)
h7_sem <- estimate_h7_sem(dyad_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h7_components, "results/h7_components.rds")
saveRDS(h7_mediation,  "results/h7_mediation.rds")
saveRDS(h7_sem,        "results/h7_sem.rds")

message("[07_h7_mediation.R] H7 mediation analysis complete. Models saved to results/")
