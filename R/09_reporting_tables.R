# ==============================================================================
# 09_reporting_tables.R -- Consolidated Results Reporting
# Purpose: Generates publication-quality regression tables and
#          summary statistics for the entire project.
# Tier 4: Reporting and Visualization
#
# NOTE: This script avoids stargazer() and summary() on brglmFit objects
#       because summary.brglmFit crashes with "mu.eta(eta): REAL() can only
#       be applied to 'numeric', not 'NULL'" when model components are stripped.
#       Instead we use extract_coefs() + safe_vcov() to build tables directly.
# ==============================================================================

# Load required scripts
source(here::here("R", "00_packages.R"))

# ==============================================================================
# 1. Utility Functions ----
# ==============================================================================

#' Safe vcov that bypasses summary.brglmFit (which crashes on mu.eta)
safe_vcov <- function(model) {
  if (inherits(model, "brglmFit") && !is.null(model$Fisher.info)) {
    V <- solve(model$Fisher.info)
    nms <- names(coef(model))
    if (nrow(V) == length(nms)) {
      rownames(V) <- colnames(V) <- nms
    }
    return(V)
  }
  if (inherits(model, "glm") && !is.null(model$qr)) {
    p <- model$rank
    p1 <- seq_len(p)
    R <- qr.R(model$qr)[p1, p1, drop = FALSE]
    Rinv <- backsolve(R, diag(p))
    dispersion <- if (model$family$family %in% c("binomial", "poisson")) 1 else {
      sum(model$weights * model$residuals^2) / model$df.residual
    }
    V <- dispersion * tcrossprod(Rinv)
    rownames(V) <- colnames(V) <- names(coef(model))[p1]
    return(V)
  }
  stats::vcov(model)
}

#' Universal coefficient extractor (avoids summary() which crashes on brglmFit)
extract_coefs <- function(model, model_name) {
  if (is.null(model)) return(NULL)
  tryCatch({
    if (inherits(model, "logistf")) {
      data.frame(model = model_name, term = names(coef(model)),
                 estimate = coef(model), std.error = sqrt(diag(vcov(model))),
                 p.value = model$prob, stringsAsFactors = FALSE, row.names = NULL)
    } else if (inherits(model, "coxph")) {
      s <- summary(model)
      data.frame(model = model_name, term = rownames(s$coefficients),
                 estimate = s$coefficients[, "coef"], std.error = s$coefficients[, "se(coef)"],
                 p.value = s$coefficients[, "Pr(>|z|)"], stringsAsFactors = FALSE, row.names = NULL)
    } else {
      beta <- coef(model)
      v <- tryCatch(safe_vcov(model), error = function(e) NULL)
      se <- if (is.null(v)) rep(NA_real_, length(beta)) else sqrt(diag(v))
      z <- beta / se
      pval <- 2 * pnorm(abs(z), lower.tail = FALSE)
      data.frame(model = model_name, term = names(beta),
                 estimate = beta, std.error = se, p.value = pval,
                 stringsAsFactors = FALSE, row.names = NULL)
    }
  }, error = function(e) { message("Could not extract from ", model_name, ": ", e$message); NULL })
}

#' Build a wide-format model comparison table
build_model_table <- function(model_list, coef_labels = NULL) {
  all_coefs <- do.call(rbind, Filter(Negate(is.null),
    Map(extract_coefs, model_list, names(model_list))))
  if (is.null(all_coefs) || nrow(all_coefs) == 0) {
    message("No model coefficients could be extracted.")
    return(invisible(NULL))
  }
  if (!is.null(coef_labels)) {
    all_coefs <- all_coefs[all_coefs$term %in% names(coef_labels), ]
    all_coefs$label <- coef_labels[all_coefs$term]
  } else {
    all_coefs$label <- all_coefs$term
  }
  all_coefs$stars <- ifelse(all_coefs$p.value < 0.001, "***",
    ifelse(all_coefs$p.value < 0.01, "**",
    ifelse(all_coefs$p.value < 0.05, "*", "")))
  all_coefs$cell <- paste0(sprintf("%.3f", all_coefs$estimate),
    all_coefs$stars, " (", sprintf("%.3f", all_coefs$std.error), ")")
  wide <- reshape(all_coefs[, c("label", "model", "cell")],
    idvar = "label", timevar = "model", direction = "wide")
  names(wide) <- gsub("cell\\.", "", names(wide))
  wide
}

#' Write a table to text and HTML
write_table <- function(tbl, title, filepath) {
  if (is.null(tbl)) { message(sprintf("[09] Skipping %s (no data)", title)); return(invisible(NULL)) }
  # Text output
  cat(sprintf("\n=== %s ===\n", title))
  print(knitr::kable(tbl, row.names = FALSE, format = "simple"))
  # HTML output
  html <- knitr::kable(tbl, row.names = FALSE, format = "html", caption = title)
  writeLines(as.character(html), filepath)
  message(sprintf("[09] Wrote: %s", filepath))
}

#' Load all results objects from results/ folder
#' @return Named list of model lists
load_all_results <- function() {
  safe_load <- function(path) {
    if (file.exists(path)) readRDS(path) else { warning(sprintf("[09] Missing: %s", path)); NULL }
  }
  list(
    h1 = safe_load("results/h1_logit_models.rds"),
    h2 = safe_load("results/h2_logit_models.rds"),
    h3 = safe_load("results/h3_logit_models.rds"),
    h4 = safe_load("results/h4_logit_models.rds"),
    h5_logit = safe_load("results/h5_logit_models.rds"),
    h5_hurdle = safe_load("results/h5_hurdle_models.rds"),
    h6_logit = safe_load("results/h6_logit_models.rds"),
    h7 = safe_load("results/h7_mediation.rds"),
    h8_init = safe_load("results/h8_init_models.rds"),
    h8_target = safe_load("results/h8_target_models.rds"),
    h9 = safe_load("results/h9_survival.rds")
  )
}

# ==============================================================================
# 2. Main Reporting Functions ----
# ==============================================================================

#' Generate Tier 1 (H1-H4) Summary Tables
report_tier1_logit <- function(res) {
  dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

  # --- Table 1: H1 Main Results ---
  if (!is.null(res$h1)) {
    tbl <- build_model_table(list(
      "H1 Baseline" = res$h1$h1_baseline,
      "H1 Controls" = res$h1$h1_controls,
      "H1 Full"     = res$h1$h1_full
    ))
    write_table(tbl, "Tier 1: Leader Ideology and Conflict Initiation (H1)",
                "results/tables/tier1_h1_main.html")
  }

  # --- Table 1b: H1 Sub-type Results ---
  if (!is.null(res$h1)) {
    tbl <- build_model_table(list(
      "Religious (base)"    = res$h1$h1_religious_base,
      "Religious (+ctrl)"   = res$h1$h1_religious,
      "Socialist (base)"    = res$h1$h1_socialist_base,
      "Socialist (+ctrl)"   = res$h1$h1_socialist,
      "Nationalist (base)"  = res$h1$h1_nationalist_base,
      "Nationalist (+ctrl)" = res$h1$h1_nationalist
    ))
    write_table(tbl, "H1: Ideology Sub-types and Conflict Initiation",
                "results/tables/tier1_h1_subtypes.html")
  }

  # --- Table 2: H2 Main Results ---
  if (!is.null(res$h2)) {
    tbl <- build_model_table(list(
      "H2 Baseline" = res$h2$h2_baseline,
      "H2 Controls" = res$h2$h2_controls,
      "H2 Full"     = res$h2$h2_full
    ))
    write_table(tbl, "Tier 1: Leader Ideology and Democracy Targeting (H2)",
                "results/tables/tier1_h2_main.html")
  }

  # --- Table 3: H3 & H4 Support Group Results ---
  if (!is.null(res$h3) && !is.null(res$h4)) {
    tbl <- build_model_table(list(
      "H3 Full" = res$h3$h3_full,
      "H4 Full" = res$h4$h4_full
    ))
    write_table(tbl, "Tier 1: Support Group Ideology (H3 & H4)",
                "results/tables/tier1_h3_h4_main.html")
  }
}

#' Generate Tier 2 (H5-H6) Summary Tables
report_tier2_legit <- function(res) {
  if (!is.null(res$h5_logit) && !is.null(res$h6_logit)) {
    tbl <- build_model_table(list(
      "H5 Full" = res$h5_logit$h5_full,
      "H6 Full" = res$h6_logit$h6_full
    ))
    write_table(tbl, "Tier 2: Legitimation Mix and Conflict (H5 & H6)",
                "results/tables/tier2_legit_mix.html")
  }
}

#' Generate Tier 3 (H7-H9) Summary Tables
report_tier3_complex <- function(res) {
  # --- H8: Moderation ---
  if (!is.null(res$h8_init) && !is.null(res$h8_target)) {
    tbl <- build_model_table(list(
      "Init (Int)"   = res$h8_init$h8_init_int,
      "Target (Int)" = res$h8_target$h8_target_int
    ))
    write_table(tbl, "Tier 3: Moderation by Dynamic Leadership (H8)",
                "results/tables/tier3_h8_moderation.html")
  }

  # --- H9: Survival (Cox PH) ---
  if (!is.null(res$h9)) {
    tbl <- build_model_table(list(
      "Direct"      = res$h9$cox_h9_direct,
      "+Ratio"      = res$h9$cox_h9_ratio,
      "Interaction" = res$h9$cox_h9_int_ratio
    ))
    write_table(tbl, "Tier 3: Leader Survival and Conflict (H9)",
                "results/tables/tier3_h9_survival.html")
  }
}

# ==============================================================================
# 3. Execution ----
# ==============================================================================
try({
  all_res <- load_all_results()
  report_tier1_logit(all_res)
  report_tier2_legit(all_res)
  report_tier3_complex(all_res)
  message("[09_reporting_tables.R] All reporting tables generated in results/tables/")
}, silent = FALSE)
