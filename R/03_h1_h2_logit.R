# ==============================================================================
# 03_h1_h2_logit.R -- Logit Analysis for Hypotheses 1 and 2
# H1: The Ideological Autocrat -- Initiation
#   Leader ideology (sidea_revisionist_domestic) -> MID initiation
# H2: The Ideological Autocrat -- Targeting
#   Leader ideology -> MID targeting of democracies
# Tier 1: Simple Logistic Regression
# ==============================================================================

    # Load required scripts
    source(here::here("R", "00_packages.R"))
    source(here::here("R", "02_data_prep.R"))

# ------------------------------------------------------------------------------
# Variable note:
# sidea_revisionist_domestic = composite leader ideology score (GRAVE-D)
#   sub-types: sidea_nationalist/socialist/religious/reactionary/separatist
#              _revisionist_domestic
# mid_initiated  = binary: hostility level >= 2 (DV for H1)
# targets_democracy = binary: v2x_libdem_b >= 0.5 (DV for H2)
# cinc_a = COW CINC (capabilities control)
# sidea_winning_coalition_size = V-Dem/BdM selectorate control
# v2x_libdem_b = V-Dem liberal democracy score of Side B
# All variables constructed in 02_data_prep.R
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Memory: subset dyad_ready to only the columns this script uses.
# Drops the full ~3.9 GB frame immediately.
# ------------------------------------------------------------------------------
h12_vars <- c(
  "mid_initiated", "targets_democracy",
  "sidea_revisionist_domestic",
  "sidea_religious_revisionist_domestic",
  "sidea_socialist_revisionist_domestic",
  "sidea_nationalist_revisionist_domestic",
  "sidea_reactionary_revisionist_domestic",
  "sidea_separatist_revisionist_domestic",
  "cinc_a", "sidea_winning_coalition_size",
  "t", "t2", "t3", "cold_war"
)
# Keep only columns that actually exist (sub-types may be absent)
h12_vars <- intersect(h12_vars, names(dyad_ready))
h12_data <- dyad_ready[, h12_vars, drop = FALSE]
  # NOTE: dyad_ready/monadic_ready kept in memory for pipeline efficiency
gc()
message(sprintf("[03] h12_data: %d rows x %d cols, %s",
                nrow(h12_data), ncol(h12_data),
                format(object.size(h12_data), units = "MB")))

# ------------------------------------------------------------------------------
# Helper: strip large components from glm objects before saving.
# ------------------------------------------------------------------------------
strip_glm <- function(model) {
  if (is.null(model)) return(NULL)
  model$model          <- NULL
  model$data           <- NULL
  model$y              <- NULL
  model$linear.predictors <- NULL
  model$fitted.values  <- NULL
  model$residuals      <- NULL
  model$weights        <- NULL
  model$prior.weights  <- NULL
  model$effects        <- NULL
  attr(model$terms, ".Environment") <- globalenv()
  model
}

# ------------------------------------------------------------------------------
# Helper: safely fit a logit model.
# ------------------------------------------------------------------------------
safe_glm <- function(formula, data, min_obs = 30) {
  vars <- all.vars(formula)
  for (v in vars) {
    if (!v %in% names(data)) {
      warning(sprintf("[03] Variable '%s' not found in data. Skipping model.", v))
      return(NULL)
    }
    if (all(is.na(data[[v]]))) {
      warning(sprintf("[03] Variable '%s' is entirely NA. Skipping model.", v))
      return(NULL)
    }
  }
  complete <- complete.cases(data[, vars, drop = FALSE])
  n_complete <- sum(complete)
  if (n_complete < min_obs) {
    warning(sprintf(
      "[03] Only %d complete cases for model (need >= %d). Skipping.",
      n_complete, min_obs
    ))
    return(NULL)
  }
  if (requireNamespace("brglm2", quietly = TRUE)) {
    fit <- tryCatch(
      glm(formula, family = binomial(link = "logit"), data = data,
          method = brglm2::brglmFit,
          control = list(maxit = 300, epsilon = 1e-6)),
      error = function(e) {
        warning(sprintf("[03] brglm2 failed: %s. Trying standard glm.", e$message))
        NULL
      }
    )
    if (!is.null(fit)) {
      message(sprintf("[03] Firth logit: %d obs, converged = %s",
                      n_complete, fit$converged))
      return(strip_glm(fit))
    }
  }
  fit <- tryCatch(
    glm(formula, family = binomial(link = "logit"), data = data,
        control = glm.control(maxit = 100)),
    error = function(e) {
      warning(sprintf("[03] glm() failed: %s", e$message))
      NULL
    }
  )
  if (!is.null(fit) && !fit$converged) {
    warning("[03] glm() did not converge even with maxit = 100.")
  }
  strip_glm(fit)
}

# ------------------------------------------------------------------------------
# Helper: safe VIF computation
# ------------------------------------------------------------------------------
safe_vif <- function(model, label = "") {
  if (is.null(model)) return(NULL)
  tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) v <- v[, "GVIF"]
    message(sprintf("[03] VIF (%s): max = %.2f", label, max(v, na.rm = TRUE)))
    v
  }, error = function(e) {
    message(sprintf("[03] VIF failed for %s: %s", label, e$message))
    NULL
  })
}

# ==============================================================================
# 1. Hypothesis 1: The Ideological Autocrat -- Initiation ----
# NOTE: targets_democracy is NOT included as a predictor for H1.
#       It is a parallel DV (used in H2), not a control.
# ==============================================================================
estimate_h1_logit <- function(data) {
  h1_baseline <- safe_glm(mid_initiated ~ sidea_revisionist_domestic,
                          data = data)
  h1_controls <- safe_glm(mid_initiated ~ sidea_revisionist_domestic +
                            cinc_a +
                            sidea_winning_coalition_size,
                          data = data)
  h1_full     <- safe_glm(mid_initiated ~ sidea_revisionist_domestic +
                            cinc_a +
                            sidea_winning_coalition_size +
                            t + t2 + t3 + cold_war,
                          data = data)
  h1_religious <- safe_glm(mid_initiated ~ sidea_religious_revisionist_domestic +
                             cinc_a +
                             sidea_winning_coalition_size +
                             t + t2 + t3 + cold_war,
                           data = data)
  h1_socialist <- safe_glm(mid_initiated ~ sidea_socialist_revisionist_domestic +
                             cinc_a +
                             sidea_winning_coalition_size +
                             t + t2 + t3 + cold_war,
                           data = data)
  h1_nationalist <- safe_glm(mid_initiated ~ sidea_nationalist_revisionist_domestic +
                               cinc_a +
                               sidea_winning_coalition_size +
                               t + t2 + t3 + cold_war,
                             data = data)
  list(
    h1_baseline    = h1_baseline,
    h1_controls    = h1_controls,
    h1_full        = h1_full,
    h1_religious   = h1_religious,
    h1_socialist   = h1_socialist,
    h1_nationalist = h1_nationalist
  )
}

# ==============================================================================
# 2. Hypothesis 2: The Ideological Autocrat -- Targeting ----
# ==============================================================================
estimate_h2_logit <- function(data) {
  conflict_data <- data %>% filter(mid_initiated == 1)
  if (nrow(conflict_data) == 0) {
    warning("[03] No conflict initiations found. H2 models skipped.")
    return(list(
      h2_baseline = NULL, h2_controls = NULL, h2_full = NULL,
      h2_religious = NULL, h2_socialist = NULL, h2_nationalist = NULL
    ))
  }
  h2_baseline <- safe_glm(targets_democracy ~ sidea_revisionist_domestic,
                          data = conflict_data)
  h2_controls <- safe_glm(targets_democracy ~ sidea_revisionist_domestic +
                            cinc_a +
                            sidea_winning_coalition_size,
                          data = conflict_data)
  h2_full     <- safe_glm(targets_democracy ~ sidea_revisionist_domestic +
                            cinc_a +
                            sidea_winning_coalition_size +
                            t + cold_war,
                          data = conflict_data)
  h2_religious <- safe_glm(targets_democracy ~ sidea_religious_revisionist_domestic +
                             cinc_a + sidea_winning_coalition_size +
                             t + cold_war,
                           data = conflict_data)
  h2_socialist <- safe_glm(targets_democracy ~ sidea_socialist_revisionist_domestic +
                             cinc_a + sidea_winning_coalition_size +
                             t + cold_war,
                           data = conflict_data)
  h2_nationalist <- safe_glm(targets_democracy ~ sidea_nationalist_revisionist_domestic +
                               cinc_a + sidea_winning_coalition_size +
                               t + cold_war,
                             data = conflict_data)
  list(
    h2_baseline    = h2_baseline,
    h2_controls    = h2_controls,
    h2_full        = h2_full,
    h2_religious   = h2_religious,
    h2_socialist   = h2_socialist,
    h2_nationalist = h2_nationalist
  )
}

# ==============================================================================
# 3. Execution and Saving Results ----
# ==============================================================================
h1_models <- estimate_h1_logit(h12_data)
h2_models <- estimate_h2_logit(h12_data)

# Report which models succeeded
for (nm in names(h1_models)) {
  status <- if (!is.null(h1_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
  message(sprintf("  H1 %-15s : %s", nm, status))
}
for (nm in names(h2_models)) {
  status <- if (!is.null(h2_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
  message(sprintf("  H2 %-15s : %s", nm, status))
}

# ==============================================================================
# 4. VIF Diagnostics ----
# ==============================================================================
h1_vif <- lapply(setNames(names(h1_models), names(h1_models)), function(nm) safe_vif(h1_models[[nm]], nm))
h2_vif <- lapply(setNames(names(h2_models), names(h2_models)), function(nm) safe_vif(h2_models[[nm]], nm))

# Save results (stripped models = small files)
dir.create("results", showWarnings = FALSE)
saveRDS(h1_models, "results/h1_logit_models.rds")
saveRDS(h2_models, "results/h2_logit_models.rds")
saveRDS(list(h1 = h1_vif, h2 = h2_vif), "results/h12_vif.rds")

# Cleanup: remove local data and model objects
rm(h12_data, h12_vars, h1_models, h2_models, h1_vif, h2_vif)
gc()
message("[03_h1_h2_logit.R] Done. Models saved to results/")
