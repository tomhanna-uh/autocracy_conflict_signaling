# ==============================================================================
# 08_h8_moderation.R — Moderation Analysis for Hypothesis 8
# H8: Moderation — Dynamic Leadership
# Dynamic personal leadership qualities moderate (amplify) the effect of
# revisionist ideology on conflict behavior (Messianic Autocrat test).
# Tier 3: Mediation, Moderation, and Survival
# Uses: glm (logit), lm (LPM with interaction), sjPlot, marginaleffects
# Source: 2024 repo A1/A2/A3 (alternate hypotheses in
#   major_and_alternative_hypotheses_robustness.qmd)
# ==============================================================================

# Load required scripts
source("R/00_packages.R")
source("R/02_data_prep.R")

# ------------------------------------------------------------------------------
# Variable note (dyadic data: dyad_ready):
# Treatment (X):  legit_ratio (ideological legitimation share, from 02_data_prep.R)
# Moderator (Z):  sidea_dynamic_leader (GRAVE-D dynamic leadership indicator)
# Outcome 1 (Y1): mid_initiated  (conflict initiation)
# Outcome 2 (Y2): targets_democracy (democracy targeting, conditional on initiation)
# Controls:       log_cinc_a, log_cinc_b, sidea_winning_coalition_size,
#                 cold_war, t, t2, t3
# Model naming convention matches 2024 repo:
#   alternate1full / alternate1int  -> H8 initiation (main effect / interaction)
#   alternate2full / alternate2int  -> H8 targeting  (main effect / interaction)
# In new repo these are renamed:
#   h8_init_main, h8_init_int, h8_target_main, h8_target_int
# Plus LPM versions for marginal effects interpretation:
#   h8_init_lpm_int, h8_target_lpm_int
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. H8 — Dynamic Leadership and Conflict Initiation ----
# ==============================================================================

#' Estimate H8 Moderation Models for Conflict Initiation
#'
#' @param data Prepared dyadic data (dyad_ready)
#' @return Named list of glm/lm model objects
estimate_h8_initiation <- function(data) {

  # --- Main effects model: dynamic leader as additive control ---
  # Tests whether dynamic leadership is independently associated with initiation
  # beyond ideology (falsification: if ideology effect disappears, messianic wins)
  h8_init_main <- glm(
    mid_initiated ~ legit_ratio + sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    family = binomial(link = "logit"),
    data = data
  )

  # --- Interaction model (logit): legit_ratio x sidea_dynamic_leader ---
  # Key H8 test: does dynamic leadership amplify the ideology-initiation link?
  h8_init_int <- glm(
    mid_initiated ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    family = binomial(link = "logit"),
    data = data
  )

  # --- LPM interaction model: for marginal effects / predicted probability plots ---
  # Linear probability model mirrors 2024 repo's alternate1int; easier marginal
  # effect interpretation for interaction term visualizations.
  h8_init_lpm_int <- lm(
    mid_initiated ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    data = data
  )

  return(list(
    h8_init_main    = h8_init_main,
    h8_init_int     = h8_init_int,
    h8_init_lpm_int = h8_init_lpm_int
  ))
}

# ==============================================================================
# 2. H8 — Dynamic Leadership and Democracy Targeting ----
# ==============================================================================

#' Estimate H8 Moderation Models for Democracy Targeting
#'
#' @param data Prepared dyadic data (dyad_ready)
#' @return Named list of glm/lm model objects
estimate_h8_targeting <- function(data) {

  # Filter to MID initiators only (targeting conditioned on initiation)
  conflict_data <- data %>% filter(mid_initiated == 1)

  # --- Main effects model ---
  h8_target_main <- glm(
    targets_democracy ~ legit_ratio + sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    family = binomial(link = "logit"),
    data = conflict_data
  )

  # --- Interaction model (logit) ---
  h8_target_int <- glm(
    targets_democracy ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    family = binomial(link = "logit"),
    data = conflict_data
  )

  # --- LPM interaction model ---
  h8_target_lpm_int <- lm(
    targets_democracy ~ legit_ratio * sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    data = conflict_data
  )

  return(list(
    h8_target_main    = h8_target_main,
    h8_target_int     = h8_target_int,
    h8_target_lpm_int = h8_target_lpm_int
  ))
}

# ==============================================================================
# 3. Additional Interaction Robustness Models ----
# (Mirrors 2024 repo interaction checks with coalition size and military support)
# ==============================================================================

#' Estimate H8 Robustness Interaction Models
#'
#' @param data Prepared dyadic data (dyad_ready)
#' @return Named list of lm robustness models
estimate_h8_robustness <- function(data) {

  conflict_data <- data %>% filter(mid_initiated == 1)

  # Winning coalition size x ideology interaction
  h8_wcoal_int <- lm(
    targets_democracy ~ legit_ratio * sidea_winning_coalition_size +
      sidea_dynamic_leader +
      log_cinc_a + log_cinc_b + cold_war +
      t + t2 + t3,
    data = conflict_data
  )

  # Military support x ideology interaction
  h8_milsupp_int <- lm(
    targets_democracy ~ legit_ratio * sidea_military_support +
      sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + cold_war +
      t + t2 + t3,
    data = conflict_data
  )

  # Cold war x ideology interaction
  h8_coldwar_int <- lm(
    targets_democracy ~ legit_ratio * cold_war +
      sidea_dynamic_leader +
      log_cinc_a + log_cinc_b +
      sidea_winning_coalition_size + t + t2 + t3,
    data = conflict_data
  )

  return(list(
    h8_wcoal_int   = h8_wcoal_int,
    h8_milsupp_int = h8_milsupp_int,
    h8_coldwar_int = h8_coldwar_int
  ))
}

# ==============================================================================
# 4. Predicted Probability Plots (sjPlot) ----
# ==============================================================================

#' Generate H8 Interaction Plots
#'
#' @param init_models List from estimate_h8_initiation()
#' @param target_models List from estimate_h8_targeting()
#' @return Saves plots to results/figures/ and returns list of ggplot objects
plot_h8_interactions <- function(init_models, target_models) {

  dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

  # Initiation: ideology effect by dynamic leadership level
  p1 <- plot_model(
    init_models$h8_init_lpm_int,
    type  = "pred",
    terms = c("legit_ratio", "sidea_dynamic_leader"),
    title = "H8: Ideology x Dynamic Leadership — Conflict Initiation\n(LPM predicted probabilities)"
  )

  # Initiation: dynamic leader effect by ideology level
  p2 <- plot_model(
    init_models$h8_init_lpm_int,
    type  = "pred",
    terms = c("sidea_dynamic_leader", "legit_ratio"),
    title = "H8: Dynamic Leadership x Ideology — Conflict Initiation\n(LPM predicted probabilities)"
  )

  # Targeting: ideology effect by dynamic leadership level
  p3 <- plot_model(
    target_models$h8_target_lpm_int,
    type  = "pred",
    terms = c("legit_ratio", "sidea_dynamic_leader"),
    title = "H8: Ideology x Dynamic Leadership — Democracy Targeting\n(LPM predicted probabilities)"
  )

  # Targeting: dynamic leader effect by ideology level
  p4 <- plot_model(
    target_models$h8_target_lpm_int,
    type  = "pred",
    terms = c("sidea_dynamic_leader", "legit_ratio"),
    title = "H8: Dynamic Leadership x Ideology — Democracy Targeting\n(LPM predicted probabilities)"
  )

  # Save plots
  ggplot2::ggsave("results/figures/h8_init_ideology_by_dynleader.png",   p1, width = 8, height = 5)
  ggplot2::ggsave("results/figures/h8_init_dynleader_by_ideology.png",   p2, width = 8, height = 5)
  ggplot2::ggsave("results/figures/h8_target_ideology_by_dynleader.png", p3, width = 8, height = 5)
  ggplot2::ggsave("results/figures/h8_target_dynleader_by_ideology.png", p4, width = 8, height = 5)

  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

# ==============================================================================
# 5. Execution and Saving Results ----
# ==============================================================================

# Initiation models
h8_init    <- estimate_h8_initiation(dyad_ready)

# Targeting models
h8_target  <- estimate_h8_targeting(dyad_ready)

# Robustness interaction models
h8_robust  <- estimate_h8_robustness(dyad_ready)

# Predicted probability plots
h8_plots   <- plot_h8_interactions(h8_init, h8_target)

# Save model objects for reporting
dir.create("results", showWarnings = FALSE)
saveRDS(h8_init,   "results/h8_init_models.rds")
saveRDS(h8_target, "results/h8_target_models.rds")
saveRDS(h8_robust, "results/h8_robust_models.rds")

message("[08_h8_moderation.R] H8 moderation analysis complete. Models saved to results/")
