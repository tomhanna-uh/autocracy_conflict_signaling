# 03_h1_h2_logit.R — Logit Analysis for Hypotheses 1 and 2

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# 1. Hypothesis 1: Conflict Initiation ----
# H1: Higher ideological legitimation increases the probability of conflict initiation.
# DV: mid_initiated
# IV: legit_ratio

#' Estimate H1 Logit Models
#' @param data Prepared dyadic data
#' @return A list of GLM objects
estimate_h1_logit <- function(data) {
  # Model 1: Baseline (Ideology Only)
  m1 <- glm(mid_initiated ~ legit_ratio,
            family = binomial(link = "logit"),
            data = data)
  
  # Model 2: Add Target Regime type
  m2 <- glm(mid_initiated ~ legit_ratio + targets_democracy,
            family = binomial(link = "logit"),
            data = data)
  
  # Model 3: Add Capabilities (CINC)
  m3 <- glm(mid_initiated ~ legit_ratio + targets_democracy + 
              log_cinc_a + log_cinc_b + cinc_ratio,
            family = binomial(link = "logit"),
            data = data)
  
  # Model 4: Full Model with Temporal Controls (Peace Years)
  m4 <- glm(mid_initiated ~ legit_ratio + targets_democracy + 
              log_cinc_a + log_cinc_b + cinc_ratio + 
              t + t2 + t3,
            family = binomial(link = "logit"),
            data = data)
  
  # Model 5: Full Model with Cold War context
  m5 <- glm(mid_initiated ~ legit_ratio + targets_democracy + 
              log_cinc_a + log_cinc_b + cinc_ratio + 
              t + t2 + t3 + cold_war,
            family = binomial(link = "logit"),
            data = data)
  
  return(list(
    logit1 = m1,
    logit2 = m2,
    logit3 = m3,
    logit4 = m4,
    logit5 = m5
  ))
}

# 2. Hypothesis 2: Target Selection ----
# H2: Ideological autocracies are more likely to target democracies.
# DV: targets_democracy (conditional on mid_initiated == 1)
# IV: legit_ratio

#' Estimate H2 Logit Models
#' @param data Prepared dyadic data
#' @return A list of GLM objects
estimate_h2_logit <- function(data) {
  # Filter data to conflict initiations only
  conflict_data <- data %>% filter(mid_initiated == 1)
  
  # Model 1: Baseline
  m1 <- glm(targets_democracy ~ legit_ratio,
            family = binomial(link = "logit"),
            data = conflict_data)
  
  # Model 2: Add Capabilities
  m2 <- glm(targets_democracy ~ legit_ratio + 
              log_cinc_a + log_cinc_b + cinc_ratio,
            family = binomial(link = "logit"),
            data = conflict_data)
  
  # Model 3: Full Model with Temporal Controls
  m3 <- glm(targets_democracy ~ legit_ratio + 
              log_cinc_a + log_cinc_b + cinc_ratio + 
              t + cold_war,
            family = binomial(link = "logit"),
            data = conflict_data)
  
  return(list(
    logit1 = m1,
    logit2 = m2,
    logit3 = m3
  ))
}

# 3. Execution and Saving Results ----

# Run H1 Models
h1_models <- estimate_h1_logit(dyad_ready)

# Run H2 Models
h2_models <- estimate_h2_logit(dyad_ready)

# Save results for reporting scripts
dir.create("results", showWarnings = FALSE)
saveRDS(h1_models, "results/h1_logit_models.rds")
saveRDS(h2_models, "results/h2_logit_models.rds")

message("[03_h1_h2_logit.R] Analysis complete. Models saved to results/")
