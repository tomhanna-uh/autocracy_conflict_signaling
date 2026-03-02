# =============================================================================
# 00_packages.R
# Load and (if needed) install all packages for the project.
# Source this file at the top of every other script:
#   source(here::here("R", "00_packages.R"))
# =============================================================================

# Helper: install if not present, then load
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# -----------------------------------------------------------------------------
# Core data wrangling
# -----------------------------------------------------------------------------
load_pkg("tidyverse")   # dplyr, tidyr, ggplot2, readr, stringr, forcats, purrr
load_pkg("here")        # here() for project-relative paths
load_pkg("janitor")     # clean_names(), tabyl()
load_pkg("labelled")    # value labels from haven/V-Dem files
load_pkg("haven")       # read_dta() for Stata files if needed

# -----------------------------------------------------------------------------
# Estimation: Tier 1 - Logistic regression
# -----------------------------------------------------------------------------
load_pkg("bife")        # Fixed-effects binary choice (incidental parameters)
load_pkg("alpaca")      # Fast FE logit (large N panels)
load_pkg("pglm")        # Panel GLM
load_pkg("plm")         # Panel linear models / Hausman tests
load_pkg("sandwich")    # Robust SEs (HC, cluster)
load_pkg("lmtest")      # coeftest() for clustered SEs
load_pkg("brglm2")    # Firth penalized logit (handles separation)
load_pkg("detectseparation") # detect_separation() diagnostic

# -----------------------------------------------------------------------------
# Estimation: Tier 2 - Hurdle models
# -----------------------------------------------------------------------------
load_pkg("pscl")        # hurdle() and zeroinfl() for two-stage count/binary

# -----------------------------------------------------------------------------
# Estimation: Tier 3 - Survival, mediation, moderation
# -----------------------------------------------------------------------------
load_pkg("survival")    # Surv(), coxph(), survfit()
load_pkg("survminer")   # ggsurvplot() for Kaplan-Meier curves
load_pkg("mediation")   # mediate() for causal mediation analysis
load_pkg("lavaan")      # SEM / path models for H7
load_pkg("ggdag")       # DAG visualizations

# Note: Zelig is archived on CRAN; mediation() is the preferred replacement.
# If you need Zelig for legacy replication, install from GitHub:
#   devtools::install_github("IQSS/Zelig")

# -----------------------------------------------------------------------------
# Marginal effects and post-estimation
# -----------------------------------------------------------------------------
load_pkg("marginaleffects") # avg_slopes(), plot_predictions() - modern margins
load_pkg("margins")         # margins() for legacy comparisons with 2025 code
load_pkg("broom")           # tidy(), glance(), augment()
load_pkg("performance")     # model fit: r2(), check_model()
load_pkg("parameters")      # model_parameters() with CIs

# -----------------------------------------------------------------------------
# Output tables
# -----------------------------------------------------------------------------
load_pkg("stargazer")       # LaTeX/HTML regression tables (matches prior repos)
load_pkg("modelsummary")    # Modern alternative; use for HTML/Word output
load_pkg("knitr")           # kable() for simple tables in Quarto
load_pkg("kableExtra")      # kable styling

# -----------------------------------------------------------------------------
# Visualization
# -----------------------------------------------------------------------------
load_pkg("ggplot2")     # (already loaded via tidyverse; explicit for clarity)
load_pkg("ggrepel")     # Non-overlapping labels in plots
load_pkg("patchwork")   # Combining ggplot panels
load_pkg("sjPlot")      # plot_model() for interaction/marginal effects plots
load_pkg("ggeffects")   # ggpredict() predicted probability plots

# -----------------------------------------------------------------------------
# Reproducibility
# -----------------------------------------------------------------------------
load_pkg("renv")        # Package version lockfile (run renv::snapshot() after setup)

# Suppress scientific notation globally
options(scipen = 999)

# Set default ggplot theme
theme_set(theme_minimal(base_size = 12))

message("[00_packages.R] All packages loaded successfully.")
