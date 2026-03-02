# ==============================================================================
# 07_h7_mediation.R -- Mediation Analysis for Hypothesis 7
# H7: Mediation -- Support Groups
# Uses: mediation package (Baron-Kenny + bootstrap) and lavaan (SEM)
# ==============================================================================

source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Memory: subset dyad_ready to needed columns
# ------------------------------------------------------------------------------
h7_vars <- c(
  "mid_initiated", "targets_democracy",
  "sidea_revisionist_domestic",
  "sidea_religious_support", "sidea_party_elite_support",
  "sidea_rural_worker_support", "sidea_military_support",
  "sidea_ethnic_racial_support",
  "cinc_a", "sidea_winning_coalition_size",
  "t", "cold_war"
)
h7_vars <- intersect(h7_vars, names(dyad_ready))
h7_data <- dyad_ready[, h7_vars, drop = FALSE]
rm(dyad_ready, monadic_ready)
gc()
message(sprintf("[07] h7_data: %d rows x %d cols, %s",
                nrow(h7_data), ncol(h7_data),
                format(object.size(h7_data), units = "MB")))

strip_glm <- function(model) {
  if (is.null(model)) return(NULL)
  model$model <- NULL; model$data <- NULL; model$y <- NULL
  model$linear.predictors <- NULL; model$fitted.values <- NULL
  model$residuals <- NULL; model$weights <- NULL
  model$prior.weights <- NULL; model$effects <- NULL
  if (!is.null(model$qr)) model$qr$qr <- NULL
  attr(model$terms, ".Environment") <- globalenv()
  model
}

# ==============================================================================
# 1. Baron-Kenny Mediation (mediation package) ----
# ==============================================================================
estimate_h7_component_models <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) < 30) {
    warning("[07] Too few conflict obs for mediation. Skipping.")
    return(NULL)
  }
  # Religious support as mediator
  a_religious <- lm(sidea_religious_support ~ sidea_revisionist_domestic +
                      cinc_a + sidea_winning_coalition_size + cold_war,
                    data = conflict_data)
  b_religious <- glm(targets_democracy ~ sidea_revisionist_domestic +
                       sidea_religious_support + cinc_a +
                       sidea_winning_coalition_size + t + cold_war,
                     family = binomial(link = "logit"), data = conflict_data)
  # Party elite support
  a_party <- lm(sidea_party_elite_support ~ sidea_revisionist_domestic +
                  cinc_a + sidea_winning_coalition_size + cold_war,
                data = conflict_data)
  b_party <- glm(targets_democracy ~ sidea_revisionist_domestic +
                   sidea_party_elite_support + cinc_a +
                   sidea_winning_coalition_size + t + cold_war,
                 family = binomial(link = "logit"), data = conflict_data)
  # Military support
  a_military <- lm(sidea_military_support ~ sidea_revisionist_domestic +
                     cinc_a + sidea_winning_coalition_size + cold_war,
                   data = conflict_data)
  b_military <- glm(targets_democracy ~ sidea_revisionist_domestic +
                      sidea_military_support + cinc_a +
                      sidea_winning_coalition_size + t + cold_war,
                    family = binomial(link = "logit"), data = conflict_data)
  list(a_religious = strip_glm(a_religious), b_religious = strip_glm(b_religious),
       a_party = strip_glm(a_party), b_party = strip_glm(b_party),
       a_military = strip_glm(a_military), b_military = strip_glm(b_military))
}

run_h7_mediation <- function(component_models, sims = 1000) {
  if (is.null(component_models)) return(NULL)
  set.seed(12345)
  safe_mediate <- function(model.m, model.y, treat, mediator) {
    tryCatch(mediate(model.m, model.y, treat = treat, mediator = mediator,
                     boot = TRUE, sims = sims),
             error = function(e) { warning(sprintf("[07] mediate failed: %s", e$message)); NULL })
  }
  list(
    med_h7_religious  = safe_mediate(component_models$a_religious, component_models$b_religious,
                                     "sidea_revisionist_domestic", "sidea_religious_support"),
    fit_h7_party      = safe_mediate(component_models$a_party, component_models$b_party,
                                     "sidea_revisionist_domestic", "sidea_party_elite_support"),
    fit_h7_socialist  = safe_mediate(component_models$a_military, component_models$b_military,
                                     "sidea_revisionist_domestic", "sidea_military_support")
  )
}

# ==============================================================================
# 2. SEM / lavaan Mediation ----
# ==============================================================================
estimate_h7_sem <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) < 30) {
    warning("[07] Too few obs for SEM. Skipping.")
    return(NULL)
  }
  sem_religious <- '
    sidea_religious_support ~ a * sidea_revisionist_domestic + cinc_a + sidea_winning_coalition_size
    targets_democracy ~ b * sidea_religious_support + cp * sidea_revisionist_domestic + cinc_a + sidea_winning_coalition_size
    ab := a * b
    total := cp + ab
  '
  sem_party <- '
    sidea_party_elite_support ~ a * sidea_revisionist_domestic + cinc_a + sidea_winning_coalition_size
    targets_democracy ~ b * sidea_party_elite_support + cp * sidea_revisionist_domestic + cinc_a + sidea_winning_coalition_size
    ab := a * b
    total := cp + ab
  '
  fit_sem_religious <- tryCatch(lavaan::sem(sem_religious, data = conflict_data),
                                error = function(e) { warning(sprintf("[07] SEM failed: %s", e$message)); NULL })
  fit_sem_party    <- tryCatch(lavaan::sem(sem_party, data = conflict_data),
                                error = function(e) { warning(sprintf("[07] SEM failed: %s", e$message)); NULL })
  list(fit_sem_religious = fit_sem_religious, fit_sem_party = fit_sem_party)
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================
h7_components <- estimate_h7_component_models(h7_data)
h7_mediation  <- run_h7_mediation(h7_components)
h7_sem        <- estimate_h7_sem(h7_data)

dir.create("results", showWarnings = FALSE)
saveRDS(h7_components, "results/h7_components.rds")
saveRDS(h7_mediation,  "results/h7_mediation.rds")
saveRDS(h7_sem,        "results/h7_sem.rds")

rm(h7_data, h7_vars, h7_components, h7_mediation, h7_sem)
gc()
message("[07_h7_mediation.R] Done. Models saved to results/")
