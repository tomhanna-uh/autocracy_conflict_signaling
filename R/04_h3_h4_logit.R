# ==============================================================================
# 04_h3_h4_logit.R — H3 & H4: support groups
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
h34_vars <- c(
        "mid_initiated", "targets_democracy",
        "sidea_religious_support", "sidea_party_elite_support",
        "sidea_rural_worker_support", "sidea_military_support",
        "sidea_ethnic_racial_support",
        "cinc_a", "sidea_winning_coalition_size",
        "t_scaled", "t2_scaled", "t3_scaled", "cold_war"
)
h34_vars <- intersect(h34_vars, names(dyad_ready))
h34_data <- dyad_ready[, h34_vars, drop = FALSE]

gc()
message(sprintf("[04] h34_data: %d rows x %d cols, %s",
                nrow(h34_data), ncol(h34_data),
                format(object.size(h34_data), units = "MB")))

# ------------------------------------------------------------------------------
# Diagnostics block (AFTER data creation)
# ------------------------------------------------------------------------------
message("=== DIAGNOSTIC START: 04_h3_h4_logit.R at ", Sys.time(), " ===")
message("Data dimensions (h34_data): ", nrow(h34_data), " rows x ", ncol(h34_data), " cols")
message("Outcome summary (mid_initiated):")
print(summary(h34_data$mid_initiated))
message("Missingness in key predictors:")
print(colSums(is.na(h34_data[, c("sidea_religious_support", "sidea_party_elite_support",
                                 "sidea_rural_worker_support", "sidea_military_support",
                                 "sidea_ethnic_racial_support", "cinc_a",
                                 "sidea_winning_coalition_size", "cold_war", "t_scaled")])))

# ------------------------------------------------------------------------------
# Model timing tracker
# ------------------------------------------------------------------------------
model_times <- list()
start_all <- Sys.time()

# ==============================================================================
# 1. H3: Support Group Ideology and Conflict Initiation
# ==============================================================================
estimate_h3_logit <- function(data) {
        start_model <- Sys.time()
        message("[04] Fitting h3_baseline at ", start_model)
        h3_baseline <- safe_glm(mid_initiated ~ sidea_religious_support, data = data)
        model_times[["h3_baseline"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h3_baseline)) message("  Converged: ", h3_baseline$converged, " | Iter: ", h3_baseline$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h3_party at ", start_model)
        h3_party <- safe_glm(mid_initiated ~ sidea_party_elite_support, data = data)
        model_times[["h3_party"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h3_party)) message("  Converged: ", h3_party$converged, " | Iter: ", h3_party$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h3_military at ", start_model)
        h3_military <- safe_glm(mid_initiated ~ sidea_military_support, data = data)
        model_times[["h3_military"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h3_military)) message("  Converged: ", h3_military$converged, " | Iter: ", h3_military$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h3_multi at ", start_model)
        h3_multi <- safe_glm(mid_initiated ~ sidea_religious_support +
                                     sidea_party_elite_support +
                                     sidea_rural_worker_support +
                                     sidea_military_support +
                                     sidea_ethnic_racial_support, data = data)
        model_times[["h3_multi"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h3_multi)) message("  Converged: ", h3_multi$converged, " | Iter: ", h3_multi$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h3_controls at ", start_model)
        h3_controls <- safe_glm(mid_initiated ~ sidea_religious_support +
                                        sidea_party_elite_support +
                                        sidea_rural_worker_support +
                                        sidea_ethnic_racial_support +
                                        cinc_a + sidea_winning_coalition_size, data = data)
        model_times[["h3_controls"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h3_controls)) message("  Converged: ", h3_controls$converged, " | Iter: ", h3_controls$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h3_full at ", start_model)
        h3_full <- safe_glm(mid_initiated ~ sidea_religious_support +
                                    sidea_party_elite_support +
                                    sidea_rural_worker_support +
                                    sidea_ethnic_racial_support +
                                    cinc_a + sidea_winning_coalition_size,
                            data = data)  # no t_scaled/t2/t3/cold_war
        model_times[["h3_full"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h3_full)) message("  Converged: ", h3_full$converged, " | Iter: ", h3_full$iter)
        
        list(h3_baseline = h3_baseline, h3_party = h3_party, h3_military = h3_military,
             h3_multi = h3_multi, h3_controls = h3_controls, h3_full = h3_full)
}

# ==============================================================================
# 2. H4: Support Group Ideology and Democracy Targeting
# ==============================================================================
estimate_h4_logit <- function(data) {
        conflict_data <- data %>% filter(mid_initiated == 1)
        if (nrow(conflict_data) == 0) {
                warning("[04] No conflict initiations found. H4 models skipped.")
                return(list(
                        h4_baseline = NULL, h4_party = NULL, h4_military = NULL,
                        h4_multi = NULL, h4_controls = NULL, h4_full = NULL
                ))
        }
        
        start_model <- Sys.time()
        message("[04] Fitting h4_baseline at ", start_model)
        h4_baseline <- safe_glm(targets_democracy ~ sidea_religious_support,
                                data = conflict_data)
        model_times[["h4_baseline"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h4_baseline)) message("  Converged: ", h4_baseline$converged, " | Iter: ", h4_baseline$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h4_party at ", start_model)
        h4_party <- safe_glm(targets_democracy ~ sidea_party_elite_support,
                             data = conflict_data)
        model_times[["h4_party"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h4_party)) message("  Converged: ", h4_party$converged, " | Iter: ", h4_party$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h4_military at ", start_model)
        h4_military <- safe_glm(targets_democracy ~ sidea_military_support,
                                data = conflict_data)
        model_times[["h4_military"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h4_military)) message("  Converged: ", h4_military$converged, " | Iter: ", h4_military$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h4_multi at ", start_model)
        h4_multi <- safe_glm(targets_democracy ~ sidea_religious_support +
                                     sidea_party_elite_support +
                                     sidea_rural_worker_support +
                                     sidea_military_support +
                                     sidea_ethnic_racial_support,
                             data = conflict_data)
        model_times[["h4_multi"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h4_multi)) message("  Converged: ", h4_multi$converged, " | Iter: ", h4_multi$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h4_controls at ", start_model)
        h4_controls <- safe_glm(targets_democracy ~ sidea_religious_support +
                                        sidea_party_elite_support +
                                        sidea_rural_worker_support +
                                        sidea_ethnic_racial_support +
                                        cinc_a + sidea_winning_coalition_size,
                                data = conflict_data)
        model_times[["h4_controls"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h4_controls)) message("  Converged: ", h4_controls$converged, " | Iter: ", h4_controls$iter)
        
        start_model <- Sys.time()
        message("[04] Fitting h4_full at ", start_model)
        h4_full <- safe_glm(targets_democracy ~ sidea_religious_support +
                                    sidea_party_elite_support +
                                    sidea_rural_worker_support +
                                    sidea_military_support +
                                    sidea_ethnic_racial_support +
                                    cinc_a + sidea_winning_coalition_size +
                                    t_scaled + t2_scaled + t3_scaled + cold_war,
                            data = conflict_data)
        model_times[["h4_full"]] <- list(start = start_model, end = Sys.time(), duration = Sys.time() - start_model)
        if (!is.null(h4_full)) message("  Converged: ", h4_full$converged, " | Iter: ", h4_full$iter)
        
        list(h4_baseline = h4_baseline, h4_party = h4_party, h4_military = h4_military,
             h4_multi = h4_multi, h4_controls = h4_controls, h4_full = h4_full)
}

# ==============================================================================
# Execution and Saving Results
# ==============================================================================
h3_models <- estimate_h3_logit(h34_data)
h4_models <- estimate_h4_logit(h34_data)

# Report which models succeeded
for (nm in names(h3_models)) {
        status <- if (!is.null(h3_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
        message(sprintf(" H3 %-15s : %s", nm, status))
}
for (nm in names(h4_models)) {
        status <- if (!is.null(h4_models[[nm]])) "OK" else "SKIPPED (insufficient data)"
        message(sprintf(" H4 %-15s : %s", nm, status))
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
saveRDS(h3_models, "results/h3_logit_models.rds")
saveRDS(h4_models, "results/h4_logit_models.rds")

# Cleanup
rm(h34_data, h34_vars, h3_models, h4_models, model_times)
gc()

message("[04_h3_h4_logit.R] Done. Models saved to results/")