# ==============================================================================
# 06_h9_survival.R -- Cox Proportional Hazard Leader Survival Models
# H9: Survival Mediation
# ==============================================================================

source(here::here("R", "00_packages.R"))
source(here::here("R", "02_data_prep.R"))

# ------------------------------------------------------------------------------
# Memory: subset monadic_ready to needed columns, drop large frames
# ------------------------------------------------------------------------------
h9_vars <- c(
  "conflicts_vs_ideo_targets", "conflicts_vs_democracy",
  "conflicts_vs_ideo_opponent",
  "avg_ideological_legit", "legit_ratio",
  "avg_autocracy_level", "log_avg_gdp", "log_avg_pop",
  "irregular_entry", "tenure_years", "survived_to_end"
)

# Check which vars are missing before subsetting
h9_missing <- setdiff(h9_vars, names(monadic_ready))
if (length(h9_missing) > 0) {
  warning(sprintf("[06] Missing columns in monadic_ready: %s",
                  paste(h9_missing, collapse = ", ")))
}

h9_vars <- intersect(h9_vars, names(monadic_ready))
h9_data <- monadic_ready[, h9_vars, drop = FALSE]
# NOTE: dyad_ready/monadic_ready kept in memory for pipeline efficiency
gc()

message(sprintf("[06] h9_data: %d rows x %d cols, %s",
                nrow(h9_data), ncol(h9_data),
                format(object.size(h9_data), units = "MB")))

# ==============================================================================
# 1. H9: Survival Mediation ----
# ==============================================================================

estimate_h9_survival <- function(data) {

  # Validate required columns
  required <- c("survived_to_end", "tenure_years")
  for (v in required) {
    if (!v %in% names(data)) {
      warning(sprintf("[06] Required column '%s' missing. H9 models skipped.", v))
      return(list(
        cox_h9_ideology = NULL, cox_h9_ratio = NULL,
        cox_h9_int_ideology = NULL, cox_h9_int_ratio = NULL,
        cox_h9_direct = NULL, cox_h9_full = NULL
      ))
    }
  }

  surv_data <- data %>%
    filter(!is.na(survived_to_end) & !is.na(tenure_years) & tenure_years > 0) %>%
    mutate(event = as.integer(survived_to_end == 0))

  message(sprintf("[06] Survival data: %d rows, %d events",
                  nrow(surv_data), sum(surv_data$event, na.rm = TRUE)))

  if (nrow(surv_data) < 30) {
    warning("[06] Too few survival observations. H9 models skipped.")
    return(list(
      cox_h9_ideology = NULL, cox_h9_ratio = NULL,
      cox_h9_int_ideology = NULL, cox_h9_int_ratio = NULL,
      cox_h9_direct = NULL, cox_h9_full = NULL
    ))
  }

  safe_cox <- function(formula, data) {
    vars <- all.vars(formula)
    for (v in vars) {
      if (!v %in% names(data)) { warning(sprintf("[06] '%s' missing. Skipping.", v)); return(NULL) }
      if (all(is.na(data[[v]]))) { warning(sprintf("[06] '%s' all NA. Skipping.", v)); return(NULL) }
    }
    tryCatch(coxph(formula, data = data),
             error = function(e) { warning(sprintf("[06] coxph failed: %s", e$message)); NULL })
  }

  cox_h9_ideology <- safe_cox(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets + avg_ideological_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = surv_data)

  cox_h9_ratio <- safe_cox(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets + legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = surv_data)

  cox_h9_int_ideology <- safe_cox(
    Surv(tenure_years, event) ~ conflicts_vs_democracy * avg_ideological_legit +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = surv_data)

  cox_h9_int_ratio <- safe_cox(
    Surv(tenure_years, event) ~ conflicts_vs_democracy * legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = surv_data)

  cox_h9_direct <- safe_cox(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = surv_data)

  cox_h9_full <- safe_cox(
    Surv(tenure_years, event) ~ conflicts_vs_ideo_targets +
      avg_ideological_legit + legit_ratio +
      avg_autocracy_level + log_avg_gdp + log_avg_pop + irregular_entry,
    data = surv_data)

  list(
    cox_h9_ideology     = cox_h9_ideology,
    cox_h9_ratio        = cox_h9_ratio,
    cox_h9_int_ideology = cox_h9_int_ideology,
    cox_h9_int_ratio    = cox_h9_int_ratio,
    cox_h9_direct       = cox_h9_direct,
    cox_h9_full         = cox_h9_full
  )
}

# ==============================================================================
# 2. Execution and Saving Results ----
# ==============================================================================

h9_survival <- estimate_h9_survival(h9_data)

# Report which models succeeded
for (nm in names(h9_survival)) {
  status <- if (!is.null(h9_survival[[nm]])) "OK" else "SKIPPED"
  message(sprintf("  H9 %-25s : %s", nm, status))
}

dir.create("results", showWarnings = FALSE)
saveRDS(h9_survival, "results/h9_survival.rds")

rm(h9_data, h9_vars, h9_survival)
gc()
message("[06_h9_survival.R] Done. Models saved to results/")
