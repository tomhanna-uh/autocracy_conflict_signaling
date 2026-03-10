# ==============================================================================
# 09b_lpm_appendix.R — Appendix: Linear Probability Model Robustness Checks
# Fits full models as LPM with clustered SEs on dyad for H1–H6
# ==============================================================================
source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))
source(here::here("R", "09_reporting_tables.R"))  # ← This line loads build_model_table() & extract_coefs

library(sandwich)   # for vcovCL
library(lmtest)     # for coeftest

# ------------------------------------------------------------------------------
# Load data (same as main scripts)
# ------------------------------------------------------------------------------
h_data <- dyad_ready  # full data

# ------------------------------------------------------------------------------
# LPM function with clustered SEs
# ------------------------------------------------------------------------------
safe_lpm <- function(formula, data, cluster_var = "dyad", min_obs = 30) {
        vars <- all.vars(formula)
        missing_vars <- vars[!vars %in% names(data)]
        if (length(missing_vars) > 0) {
                message(sprintf("[safe_lpm] Missing: %s. Skipping.", paste(missing_vars, collapse = ", ")))
                return(NULL)
        }
        
        complete <- complete.cases(data[, vars, drop = FALSE])
        if (sum(complete) < min_obs) {
                message("[safe_lpm] Too few complete cases. Skipping.")
                return(NULL)
        }
        
        fit <- tryCatch(lm(formula, data = data[complete, ]),
                        error = function(e) { message("[safe_lpm] lm failed: ", e$message); NULL })
        
        if (!is.null(fit)) {
                # Add clustered SEs
                vcov_cl <- vcovCL(fit, cluster = data[complete, cluster_var])
                fit_robust <- coeftest(fit, vcov = vcov_cl)
                attr(fit, "robust_summary") <- fit_robust
                attr(fit, "robust_vcov") <- vcov_cl
        }
        
        fit
}

# ------------------------------------------------------------------------------
# Fit LPM versions of full models (for appendix)
# ------------------------------------------------------------------------------
lpm_h1_full <- safe_lpm(mid_initiated ~ sidea_revisionist_domestic +
                                cinc_a + sidea_winning_coalition_size +
                                t_scaled + t2_scaled + t3_scaled + cold_war,
                        data = h_data)

lpm_h2_full <- safe_lpm(targets_democracy ~ sidea_revisionist_domestic +
                                cinc_a + sidea_winning_coalition_size +
                                t_scaled + cold_war,
                        data = h_data %>% filter(mid_initiated == 1))

lpm_h3_full <- safe_lpm(mid_initiated ~ sidea_religious_support +
                                sidea_party_elite_support +
                                sidea_rural_worker_support +
                                sidea_military_support +
                                sidea_ethnic_racial_support +
                                cinc_a + sidea_winning_coalition_size +
                                t_scaled + t2_scaled + t3_scaled + cold_war,
                        data = h_data)

lpm_h4_full <- safe_lpm(targets_democracy ~ sidea_religious_support +
                                sidea_party_elite_support +
                                sidea_rural_worker_support +
                                sidea_military_support +
                                sidea_ethnic_racial_support +
                                cinc_a + sidea_winning_coalition_size +
                                t_scaled + t2_scaled + t3_scaled + cold_war,
                        data = h_data %>% filter(mid_initiated == 1))

lpm_h5_full <- safe_lpm(mid_initiated ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                                cinc_a + sidea_winning_coalition_size +
                                t_scaled + t2_scaled + t3_scaled + cold_war,
                        data = h_data)

lpm_h6_full <- safe_lpm(targets_democracy ~ legit_ratio + v2exl_legitperf_a + v2exl_legitlead_a +
                                cinc_a + sidea_winning_coalition_size + t_scaled,
                        data = h_data %>% filter(mid_initiated == 1))

# Save appendix models
saveRDS(list(
        h1_full = lpm_h1_full,
        h2_full = lpm_h2_full,
        h3_full = lpm_h3_full,
        h4_full = lpm_h4_full,
        h5_full = lpm_h5_full,
        h6_full = lpm_h6_full
), "results/lpm_appendix_models.rds")

message("[09b_lpm_appendix.R] Done. LPM models saved to results/lpm_appendix_models.rds")