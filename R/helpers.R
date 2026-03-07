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

#' Safe GLM wrapper with Firth bias reduction (brglm2), checks for separation/rare events,
#' graceful failure, and fallback to ordinary glm
#'
#' @param formula Formula for the model
#' @param data Data frame
#' @param family Family object (default: binomial("logit"))
#' @param min_obs Minimum number of complete observations required (default 10 for rare events)
#' @param ... Additional arguments passed to glm()
#'
#' @return Fitted model object (brglm or glm) or NULL if it fails/skips
safe_glm <- function(formula, data, family = binomial(link = "logit"),
                     min_obs = 10, ...) {
        
        # 1. Check if all variables in formula exist in data
        vars <- all.vars(formula)
        missing_vars <- vars[!vars %in% names(data)]
        if (length(missing_vars) > 0) {
                message(sprintf("[safe_glm] Missing variable(s) in data: %s. Skipping model.",
                                paste(missing_vars, collapse = ", ")))
                return(NULL)
        }
        
        # 2. Check sufficient complete cases
        complete_rows <- complete.cases(data[, vars, drop = FALSE])
        n_complete <- sum(complete_rows)
        if (n_complete < min_obs) {
                message(sprintf("[safe_glm] Too few complete cases (%d < %d required). Skipping model.",
                                n_complete, min_obs))
                return(NULL)
        }
        
        # 3. Try Firth bias-reduced logistic regression first (handles separation best)
        fit <- tryCatch(
                glm(formula, family = family, data = data,
                    method = brglm2::brglmFit,
                    control = brglm2::brglm_control(
                            maxit = 2000,            # increase iterations
                            epsilon = 1e-10,
                            slowit = 0.1,            # slow acceleration if oscillating
                            response_adjustment = TRUE,
                            type = "AS_mean"         # alternative mean bias reduction (often more stable)
                    )),
                warning = function(w) { message("[safe_glm] brglm warning: ", w$message); NULL },
                error = function(e) { message("[safe_glm] brglm error: ", e$message); NULL }
        )
        # 4. Fallback to standard glm if Firth didn't work
        if (is.null(fit)) {
                fit <- tryCatch(
                        glm(formula, family = family, data = data, ...),
                        error = function(e) {
                                message("[safe_glm] Ordinary glm also failed: ", e$message, ". Model skipped.")
                                NULL
                        }
                )
        }
        
        # 5. Final check: warn if converged=FALSE (common with strong separation)
        if (!is.null(fit) && !is.null(fit$converged) && !fit$converged) {
                message("[safe_glm] Warning: Model did not fully converge. Coefficients may be unstable/large.")
        }
        
        return(fit)
}