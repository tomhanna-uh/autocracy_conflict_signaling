# ==============================================================================
# 05_h5_h6_legitmix.R — Legitimation Mix Analysis for Hypotheses 5 and 6
# H5: Legitimation Mix -- Initiation
# H6: Legitimation Mix -- Targeting
# Tier 2: Logistic + Hurdle Models
# ==============================================================================
source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

# ------------------------------------------------------------------------------
# Embedded helpers (no external helpers.R)
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
h56_vars <- c(
        "mid_initiated", "targets_democracy",
        "legit_ratio", "legit_total",
        "v2exl_legitideol_a", "v2exl_legitperf_a", "v2exl_legitlead_a",
        "sidea_revisionist_domestic",
        "cinc_a", "sidea_winning_coalition_size",
        "t_scaled", "t2_scaled", "t3_scaled", "cold_war"
)
h56_vars <- intersect(h56_vars, names(dyad_ready))
h56_data <- dyad_ready[, h56_vars, drop = FALSE]

gc()
message(sprintf("[05] h56_data: %d rows x %d cols, %s",
                nrow(h56_data), ncol(h56_data),
                format(object.size(h56_data), units = "MB")))

# ------------------------------------------------------------------------------
# Diagnostics block (AFTER data creation)
# ------------------------------------------------------------------------------
message("=== DIAGNOSTIC START: 05_h5_h6_legitmix.R at ", Sys.time(), " ===")
message("Data dimensions (h56_data): ", nrow(h56_data), " rows x ", ncol(h56_data), " cols")
message("Outcome summary (mid_initiated):")
print(summary(h56_data$mid_initiated))
message("Missingness in key predictors:")
print(colSums(is.na(h56_data[, c("legit_ratio", "v2exl_legitideol_a", "v2exl_legitperf_a", "v2exl_legitlead_a",
                                 "cinc_a", "sidea_winning_coalition_size", "cold_war", "t_scaled")])))

# ------------------------------------------------------------------------------
# Model timing tracker
# ------------------------------------------------------------------------------
model_times <- list()
start_all <- Sys.time()

# ==============================================================================
# 1. H5: Legitimation Mix -- Initiation
# ==============================================================================
estimate_h5_logit <- function(data) {
        h5_baseline <- safe_glm(mid_initiated ~ legit_ratio, data = data)
        h5_components <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a, data = data)
        h5_controls <- safe_glm(mid_initiated ~ legit_ratio + cinc_a + sidea_winning_coalition_size, data = data)
        # Simplified full model — no temporal term
        h5_full <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                                    cinc_a + sidea_winning_coalition_size,
                            data = data)
        list(h5_baseline = h5_baseline, h5_components = h5_components,
             h5_controls = h5_controls, h5_full = h5_full)
}

estimate_h5_hurdle <- function(data) {
        hurdle_binary <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                                          cinc_a + sidea_winning_coalition_size,
                                  data = data)
        initiators <- data %>% filter(mid_initiated == 1)
        if (nrow(initiators) == 0) {
                warning("[05] No initiators found for hurdle count. Skipping.")
                hurdle_count <- NULL
        } else {
                hurdle_count <- safe_glm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                                                 cinc_a + sidea_winning_coalition_size,
                                         data = initiators, family = poisson(link = "log"))
        }
        list(hurdle_binary = hurdle_binary, hurdle_count = hurdle_count)
}
# ==============================================================================
# 2. H6: Legitimation Mix -- Targeting
# ==============================================================================
estimate_h6_logit <- function(data) {
        conflict_data <- data %>% filter(mid_initiated == 1)
        if (nrow(conflict_data) == 0) {
                warning("[05] No conflict initiations. H6 skipped.")
                return(list(h6_baseline = NULL, h6_components = NULL,
                            h6_controls = NULL, h6_full = NULL, h6_interaction = NULL))
        }
        
        start_model <- Sys.time()
        message("[05] Fitting h6_baseline at ", start_model)
        h6_baseline <- safe_glm(targets_democracy ~ legit_ratio, data = conflict_data)
        model_times[["h6_baseline"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h6_baseline)) message("  Converged: ", h6_baseline$converged, " | Iter: ", h6_baseline$iter)
        
        start_model <- Sys.time()
        message("[05] Fitting h6_components at ", start_model)
        h6_components <- safe_glm(targets_democracy ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a,
                                  data = conflict_data)
        model_times[["h6_components"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h6_components)) message("  Converged: ", h6_components$converged, " | Iter: ", h6_components$iter)
        
        start_model <- Sys.time()
        message("[05] Fitting h6_controls at ", start_model)
        h6_controls <- safe_glm(targets_democracy ~ legit_ratio + cinc_a + sidea_winning_coalition_size,
                                data = conflict_data)
        model_times[["h6_controls"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h6_controls)) message("  Converged: ", h6_controls$converged, " | Iter: ", h6_controls$iter)
        
        start_model <- Sys.time()
        message("[05] Fitting h6_full at ", start_model)
        h6_full <- safe_glm(targets_democracy ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                                    cinc_a + sidea_winning_coalition_size + t_scaled,
                            data = conflict_data)
        model_times[["h6_full"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h6_full)) message("  Converged: ", h6_full$converged, " | Iter: ", h6_full$iter)
        
        start_model <- Sys.time()
        message("[05] Fitting h6_interaction at ", start_model)
        h6_interaction <- safe_glm(targets_democracy ~ legit_ratio * sidea_revisionist_domestic +
                                           cinc_a + sidea_winning_coalition_size + t_scaled,
                                   data = conflict_data)
        model_times[["h6_interaction"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h6_interaction)) message("  Converged: ", h6_interaction$converged, " | Iter: ", h6_interaction$iter)
        
        list(h6_baseline = h6_baseline, h6_components = h6_components,
             h6_controls = h6_controls, h6_full = h6_full, h6_interaction = h6_interaction)
}

# ==============================================================================
# Execution and Saving Results
# ==============================================================================
h5_logit_models <- estimate_h5_logit(h56_data)
h5_hurdle_models <- estimate_h5_hurdle(h56_data)
h6_logit_models <- estimate_h6_logit(h56_data)

# Report which models succeeded
for (nm in names(h5_logit_models)) {
        status <- if (!is.null(h5_logit_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
        message(sprintf(" H5 %-15s : %s", nm, status))
}
for (nm in names(h5_hurdle_models)) {
        status <- if (!is.null(h5_hurdle_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
        message(sprintf(" H5 Hurdle %-10s : %s", nm, status))
}
for (nm in names(h6_logit_models)) {
        status <- if (!is.null(h6_logit_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
        message(sprintf(" H6 %-15s : %s", nm, status))
}

# Print model timing summary
message("\n=== Model Timing Summary ===")
for (nm in names(model_times)) {
        t <- model_times[[nm]]
        message(sprintf("%-15s: %s → %s (%.1f seconds)", nm, format(t$start, "%H:%M:%S"), 
                        format(t$end, "%H:%M:%S"), as.numeric(t$duration)))
}

# Save results
dir.create("results", showWarnings = FALSE)
saveRDS(h5_logit_models, "results/h5_logit_models.rds")
saveRDS(h5_hurdle_models, "results/h5_hurdle_models.rds")
saveRDS(h6_logit_models, "results/h6_logit_models.rds")

# Cleanup
rm(h56_data, h56_vars, h5_logit_models, h5_hurdle_models, h6_logit_models, model_times)
gc()

message("[05_h5_h6_legitmix.R] Done. Models saved to results/")