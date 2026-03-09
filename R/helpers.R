safe_vif <- function(model, label = "", type = "predictor") {
        
        # Early exit if model is NULL or invalid
        if (is.null(model)) {
                message(sprintf("[VIF] %s: Model is NULL. Skipping VIF.", label))
                return(NULL)
        }
        
        # Check if the model even has enough structure for VIF
        if (length(coef(model)) < 2) {
                message(sprintf("[VIF] %s: Model has too few coefficients. Skipping VIF.", label))
                return(NULL)
        }
        
        # Try to compute vcov first (fails before vif in many bad cases)
        vcov_ok <- try(vcov(model), silent = TRUE)
        if (inherits(vcov_ok, "try-error")) {
                message(sprintf(
                        "[VIF] %s: Cannot compute variance-covariance matrix (%s). Skipping VIF.",
                        label, as.character(vcov_ok)
                ))
                return(NULL)
        }
        
        # Now try vif
        vif_res <- tryCatch(
                {
                        v <- car::vif(model, type = type)
                        # Handle the case where vif returns a matrix (factors with >2 levels)
                        if (is.matrix(v)) {
                                v <- v[, "GVIF^(1/(2*df))"]  # or keep your original "GVIF" if preferred
                        }
                        # Report max VIF for quick diagnostics
                        max_vif <- max(v, na.rm = TRUE)
                        message(sprintf("[VIF] %s: max = %.2f", label, max_vif))
                        v
                },
                
                warning = function(w) {
                        message(sprintf("[VIF] %s: Warning during VIF - %s", label, w$message))
                        car::vif(model, type = type)  # return partial result
                },
                
                error = function(e) {
                        message(sprintf(
                                "[VIF] %s: VIF calculation failed - %s (possible perfect collinearity or aliased terms)",
                                label, e$message
                        ))
                        NULL
                }
        )
        
        return(vif_res)
}

#' Safe GLM wrapper with Firth bias reduction, robust convergence, and graceful failure
#'
#' @param formula Formula for the model
#' @param data Data frame
#' @param family Family object (default: binomial("logit"))
#' @param min_obs Minimum number of complete observations required
#' @param ... Additional arguments passed to glm()
#'
#' @return Fitted model object (brglm or glm) or NULL if it fails/skips
safe_glm <- function(formula, data, family = binomial(link = "logit"),
                     min_obs = 10, ...) {
        
        # 1. Check required variables exist
        vars <- all.vars(formula)
        missing_vars <- vars[!vars %in% names(data)]
        if (length(missing_vars) > 0) {
                message(sprintf("[safe_glm] Missing variable(s): %s. Skipping.", paste(missing_vars, collapse = ", ")))
                return(NULL)
        }
        
        # 2. Check sufficient complete cases
        complete <- complete.cases(data[, vars, drop = FALSE])
        n_complete <- sum(complete)
        if (n_complete < min_obs) {
                message(sprintf("[safe_glm] Too few complete cases (%d < %d). Skipping.", n_complete, min_obs))
                return(NULL)
        }
        
        # 3. Try to get good starting values (non-blocking)
        start_vals <- NULL
        tryCatch({
                simple_fit <- glm(formula, family = family, data = data, maxit = 50, quiet = TRUE)
                if (all(is.finite(coef(simple_fit)))) {
                        start_vals <- coef(simple_fit)
                        message("[safe_glm] Using starting values from initial glm.")
                }
        }, error = function(e) NULL, warning = function(w) NULL)
        
        # 4. Main Firth fit (only pass start if valid)
        fit <- tryCatch({
                glm_args <- list(
                        formula = formula,
                        family = family,
                        data = data,
                        method = brglm2::brglmFit,
                        control = brglm2::brglm_control(
                                maxit = 1000,
                                epsilon = 1e-10,
                                slowit = 0.01,
                                response_adjustment = TRUE,
                                type = "AS_mean"          # your preferred type
                        ),
                        ...
                )
                
                if (!is.null(start_vals)) {
                        glm_args$start <- start_vals
                }
                
                do.call(glm, glm_args)
        }, warning = function(w) {
                message("[safe_glm] brglmFit warning: ", w$message)
                NULL
        }, error = function(e) {
                message("[safe_glm] brglmFit error: ", e$message)
                if (grepl("missing|default", e$message) && !is.null(start_vals)) {
                        message("[safe_glm] Retrying without starting values...")
                        glm_args$start <- NULL
                        do.call(glm, glm_args)
                } else {
                        NULL
                }
        })
        
        # 5. Fallback to ordinary glm if Firth failed
        if (is.null(fit)) {
                message("[safe_glm] brglmFit failed. Trying ordinary glm...")
                fit <- tryCatch(
                        glm(formula, family = family, data = data, start = start_vals, ...),
                        error = function(e) {
                                message("[safe_glm] Ordinary glm also failed: ", e$message)
                                NULL
                        }
                )
        }
        
        # 6. Convergence check
        if (!is.null(fit) && !fit$converged) {
                message("[safe_glm] Warning: Model did not fully converge.")
        }
        
        return(fit)
}