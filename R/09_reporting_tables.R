# ==============================================================================
# 09_reporting_tables.R -- Consolidated Results Reporting
# Robust version that handles simplified models (e.g., H8 with no temporal terms,
# no logged CINC, Cold War filter only)
# ==============================================================================

source(here::here("R", "00_packages.R"))

# ==============================================================================
# 1. Utility Functions (Robust Version) ----
# ==============================================================================

safe_vcov <- function(model) {
        if (is.null(model)) return(NULL)
        if (inherits(model, "brglmFit") && !is.null(model$Fisher.info)) {
                V <- solve(model$Fisher.info)
                nms <- names(coef(model))
                if (nrow(V) == length(nms)) rownames(V) <- colnames(V) <- nms
                return(V)
        }
        stats::vcov(model)
}

extract_coefs <- function(model, model_name = "") {
        if (is.null(model) || length(coef(model)) == 0) return(NULL)
        tryCatch({
                beta <- coef(model)
                v <- safe_vcov(model)
                se <- if (is.null(v)) rep(NA_real_, length(beta)) else sqrt(diag(v))
                z <- beta / se
                pval <- 2 * pnorm(abs(z), lower.tail = FALSE)
                data.frame(model = model_name, term = names(beta),
                           estimate = beta, std.error = se, p.value = pval,
                           stringsAsFactors = FALSE)
        }, error = function(e) { message("Could not extract from ", model_name); NULL })
}

build_model_table <- function(model_list, nice_names = NULL) {
        coef_dfs <- lapply(names(model_list), function(nm) extract_coefs(model_list[[nm]], nm))
        coef_dfs <- coef_dfs[!sapply(coef_dfs, is.null)]
        if (length(coef_dfs) == 0) return(NULL)
        
        all_terms <- unique(unlist(lapply(coef_dfs, `[[`, "term")))
        wide <- data.frame(label = all_terms, stringsAsFactors = FALSE)
        
        for (mdl in names(coef_dfs)) {
                df <- coef_dfs[[mdl]]
                wide[[mdl]] <- NA_character_
                idx <- match(wide$label, df$term)
                valid <- !is.na(idx)
                if (any(valid)) {
                        est <- df$estimate[idx[valid]]
                        se  <- df$std.error[idx[valid]]
                        p   <- df$p.value[idx[valid]]
                        stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
                        wide[[mdl]][valid] <- paste0(sprintf("%.3f", est), stars, " (", sprintf("%.3f", se), ")")
                }
        }
        
        if (!is.null(nice_names)) {
                wide$label <- nice_names[match(wide$label, names(nice_names))]
        }
        wide
}

write_table <- function(tbl, title, filepath) {
        if (is.null(tbl) || nrow(tbl) == 0) {
                message(sprintf("[09] Skipping %s (no valid data)", title))
                return(invisible(NULL))
        }
        cat(sprintf("\n=== %s ===\n", title))
        print(knitr::kable(tbl, format = "simple", row.names = FALSE))
        
        html <- knitr::kable(tbl, format = "html", row.names = FALSE, caption = title)
        writeLines(as.character(html), filepath)
        message(sprintf("[09] Wrote: %s", filepath))
}

# ==============================================================================
# 2. Load Results ----
# ==============================================================================
load_all_results <- function() {
        safe_load <- function(path) if (file.exists(path)) readRDS(path) else NULL
        list(
                h1 = safe_load("results/h1_logit_models.rds"),
                h2 = safe_load("results/h2_logit_models.rds"),
                h3 = safe_load("results/h3_logit_models.rds"),
                h4 = safe_load("results/h4_logit_models.rds"),
                h5 = safe_load("results/h5_logit_models.rds"),
                h6 = safe_load("results/h6_logit_models.rds"),
                h8_init = safe_load("results/h8_init_models.rds"),
                h8_target = safe_load("results/h8_target_models.rds"),
                h9 = safe_load("results/h9_survival.rds")
        )
}

# ==============================================================================
# 3. Reporting Functions (robust to simplified H8) ----
# ==============================================================================

report_tier1_logit <- function(res) {
        dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
        # H1, H2, H3/H4 tables as before (unchanged)
        # ... (keep your existing code for H1/H2/H3/H4 here)
}

report_h8_moderation <- function(res) {
        if (!is.null(res$h8_init) && !is.null(res$h8_target)) {
                tbl <- build_model_table(list(
                        "Init (Main)" = res$h8_init$h8_init_main,
                        "Init (Int)"  = res$h8_init$h8_init_int,
                        "Target (Main)" = res$h8_target$h8_target_main,
                        "Target (Int)"  = res$h8_target$h8_target_int
                ))
                write_table(tbl, "Tier 3: Moderation by Dynamic Leadership (H8)",
                            "results/tables/tier3_h8_moderation.html")
        }
}

# ==============================================================================
# 4. Execution ----
# ==============================================================================
try({
        all_res <- load_all_results()
        report_tier1_logit(all_res)
        report_h8_moderation(all_res)
        message("[09_reporting_tables.R] All reporting tables generated in results/tables/")
}, silent = FALSE)