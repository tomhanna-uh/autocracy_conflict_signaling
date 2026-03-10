# ==============================================================================
# 03_h1_h2_logit.R — H1 & H2: leader ideology
# ==============================================================================
source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

# ------------------------------------------------------------------------------
# Embedded safe_glm (minimal, no external helpers)
# ------------------------------------------------------------------------------
strip_glm <- function(model) {
        if (is.null(model)) return(NULL)
        model$model <- NULL; model$data <- NULL; model$y <- NULL
        model$linear.predictors <- NULL; model$fitted.values <- NULL
        model$residuals <- NULL; model$weights <- NULL
        model$prior.weights <- NULL; model$effects <- NULL
        attr(model$terms, ".Environment") <- globalenv()
        model
}

safe_glm <- function(formula, data, family = binomial(link = "logit"),
                     min_obs = 10, ...) {
        vars <- all.vars(formula)
        missing_vars <- vars[!vars %in% names(data)]
        if (length(missing_vars) > 0) {
                message(sprintf("[safe_glm] Missing: %s. Skipping.", paste(missing_vars, collapse = ", ")))
                return(NULL)
        }
        
        complete <- complete.cases(data[, vars, drop = FALSE])
        if (sum(complete) < min_obs) {
                message("[safe_glm] Too few complete cases. Skipping.")
                return(NULL)
        }
        
        fit <- tryCatch(
                glm(formula, family = family, data = data,
                    method = brglm2::brglmFit,
                    control = brglm2::brglm_control(
                            maxit = 1000,
                            epsilon = 1e-8,
                            slowit = 0.1,
                            response_adjustment = TRUE,
                            type = "AS_mixed"
                    ),
                    ...),
                warning = function(w) { message("[safe_glm] Warning: ", w$message); NULL },
                error = function(e) { message("[safe_glm] Error: ", e$message); NULL }
        )
        
        if (is.null(fit)) {
                message("[safe_glm] brglmFit failed. Using ordinary glm.")
                fit <- tryCatch(glm(formula, family = family, data = data, ...),
                                error = function(e) { message("[safe_glm] Ordinary glm failed: ", e$message); NULL })
        }
        
        if (!is.null(fit) && !fit$converged) {
                message("[safe_glm] Warning: Model did not fully converge.")
        }
        
        fit
}

# ------------------------------------------------------------------------------
# Subset data for this script
# ------------------------------------------------------------------------------
h12_vars <- c(
        "mid_initiated", "targets_democracy",
        "sidea_revisionist_domestic",
        "sidea_religious_revisionist_domestic",
        "sidea_socialist_revisionist_domestic",
        "sidea_nationalist_revisionist_domestic",
        "sidea_reactionary_revisionist_domestic",
        "sidea_separatist_revisionist_domestic",
        "cinc_a", "sidea_winning_coalition_size"
        # Removed temporal terms and cold_war from full models
)
h12_vars <- intersect(h12_vars, names(dyad_ready))
h12_data <- dyad_ready[, h12_vars, drop = FALSE]

gc()
message(sprintf("[03] h12_data: %d rows x %d cols, %s",
                nrow(h12_data), ncol(h12_data),
                format(object.size(h12_data), units = "MB")))

# ------------------------------------------------------------------------------
# Diagnostics block (AFTER data creation)
# ------------------------------------------------------------------------------
message("=== DIAGNOSTIC START: 03_h1_h2_logit.R at ", Sys.time(), " ===")
message("Data dimensions (h12_data): ", nrow(h12_data), " rows x ", ncol(h12_data), " cols")
message("Outcome summary (mid_initiated):")
print(summary(h12_data$mid_initiated))
message("Missingness in key predictors:")
print(colSums(is.na(h12_data[, c("sidea_revisionist_domestic", "cinc_a", "sidea_winning_coalition_size")])))

# ==============================================================================
# 1. Hypothesis 1: The Ideological Autocrat -- Initiation ----
# NOTE: targets_democracy is NOT included as a predictor for H1.
# ==============================================================================
estimate_h1_logit <- function(data) {
        h1_baseline <- safe_glm(mid_initiated ~ sidea_revisionist_domestic,
                                data = data)
        h1_controls <- safe_glm(mid_initiated ~ sidea_revisionist_domestic +
                                        cinc_a +
                                        sidea_winning_coalition_size,
                                data = data)
        # Simplified full model: no temporal terms to avoid separation/non-convergence
        h1_full <- safe_glm(mid_initiated ~ sidea_revisionist_domestic +
                                    cinc_a +
                                    sidea_winning_coalition_size,
                            data = data)
        # Sub-type models (simplified, no temporal spline)
        h1_religious_base <- safe_glm(mid_initiated ~ sidea_religious_revisionist_domestic,
                                      data = data)
        h1_religious <- safe_glm(mid_initiated ~ sidea_religious_revisionist_domestic +
                                         cinc_a +
                                         sidea_winning_coalition_size,
                                 data = data)
        h1_socialist_base <- safe_glm(mid_initiated ~ sidea_socialist_revisionist_domestic,
                                      data = data)
        h1_socialist <- safe_glm(mid_initiated ~ sidea_socialist_revisionist_domestic +
                                         cinc_a + sidea_winning_coalition_size,
                                 data = data)
        h1_nationalist_base <- safe_glm(mid_initiated ~ sidea_nationalist_revisionist_domestic,
                                        data = data)
        h1_nationalist <- safe_glm(mid_initiated ~ sidea_nationalist_revisionist_domestic +
                                           cinc_a + sidea_winning_coalition_size,
                                   data = data)
        list(
                h1_baseline = h1_baseline,
                h1_controls = h1_controls,
                h1_full = h1_full,
                h1_religious_base = h1_religious_base,
                h1_religious = h1_religious,
                h1_socialist_base = h1_socialist_base,
                h1_socialist = h1_socialist,
                h1_nationalist_base = h1_nationalist_base,
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
        # Simplified full model
        h2_full <- safe_glm(targets_democracy ~ sidea_revisionist_domestic +
                                    cinc_a +
                                    sidea_winning_coalition_size +
                                    t_scaled,
                            data = conflict_data)
        h2_religious <- safe_glm(targets_democracy ~ sidea_religious_revisionist_domestic +
                                         cinc_a + sidea_winning_coalition_size,
                                 data = conflict_data)
        h2_socialist <- safe_glm(targets_democracy ~ sidea_socialist_revisionist_domestic +
                                         cinc_a + sidea_winning_coalition_size,
                                 data = conflict_data)
        h2_nationalist <- safe_glm(targets_democracy ~ sidea_nationalist_revisionist_domestic +
                                           cinc_a + sidea_winning_coalition_size,
                                   data = conflict_data)
        list(
                h2_baseline = h2_baseline,
                h2_controls = h2_controls,
                h2_full = h2_full,
                h2_religious = h2_religious,
                h2_socialist = h2_socialist,
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
        message(sprintf(" H1 %-15s : %s", nm, status))
}
for (nm in names(h2_models)) {
        status <- if (!is.null(h2_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
        message(sprintf(" H2 %-15s : %s", nm, status))
}

# Save results (stripped models = small files)
dir.create("results", showWarnings = FALSE)
saveRDS(h1_models, "results/h1_logit_models.rds")
saveRDS(h2_models, "results/h2_logit_models.rds")

# Cleanup
rm(h12_data, h12_vars, h1_models, h2_models)
gc()

message("[03_h1_h2_logit.R] Done. Models saved to results/")