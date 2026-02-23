# R/ — Analysis Scripts

This directory contains all R scripts for the *Autocracy, Conflict, and Ideological Signaling* project. Scripts are numbered to reflect their execution order and grouped by analytical tier (simple → complex). Each file section below documents the design decisions behind that file.

---

## Execution Order

```
source("R/00_packages.R")   # Always first
source("R/01_load_data.R")  # Always second
source("R/02_data_prep.R")  # Always third
# Then any model script in 03–09 order
```

All model scripts assume `00_packages.R` and `01_load_data.R` have been sourced in the same session. `02_data_prep.R` must also be sourced before any model script.

---

## File Descriptions and Design Rationale

---

### `00_packages.R` — Package Management

**Purpose:** Central package loading for the entire project. Every other script assumes this has been sourced; no script calls `library()` directly.

**Key design decisions:**

- **`load_pkg()` helper** — installs the package if missing, then loads silently. Means any collaborator or reviewer can clone and run without a separate install step or manual `install.packages()` calls.
- **`bife` + `alpaca`** — both kept since the 2024 repo uses both for panel fixed-effects logit; they address the incidental parameters problem in time-series cross-sectional (TSCS) conflict data.
- **`pscl`** — added for `hurdle()` models in H5/H6 (Tier 2). Not present in either prior repo.
- **`Zelig`** — flagged as archived with a GitHub install fallback comment. `mediation` package is used as the primary tool for H7/H9 instead.
- **`marginaleffects`** — added as the modern replacement for `margins()`. Both are loaded: `marginaleffects` for new model scripts, `margins` for backward compatibility with 2025 repo code.
- **`modelsummary`** — loaded alongside `stargazer`. `stargazer` matches the prior RPubs output format; `modelsummary` is preferred for Word/HTML journal submission outputs.
- **`ggeffects` + `sjPlot`** — both kept for predicted probability plots and interaction visualizations.
- **Global options at bottom** — `scipen = 999` (matches 2024 repo convention) and `theme_set(theme_minimal())` so all ggplot2 outputs share a consistent base style without per-script theming.

---

### `01_load_data.R` — Data Loading and Validation

**Purpose:** Single source of truth for reading raw data files into R. All model scripts call the functions defined here rather than reading CSVs directly, so file path changes only need to be made in one place.

**Key design decisions:**

- **Three exported functions:** `load_dyad_data()`, `load_monadic_data()`, and `load_all_data()`. Scripts needing only the main dyadic dataset call `load_dyad_data()`; scripts needing both call `load_all_data()` for a named list.
- **`GRAVE_D_Master_with_Leaders.csv` as master dataset** — treated as the primary file per the reconciliation decision. The 2025 repo used this file; the 2024 repo used separate dyadic and monadic files. This unifies data input.
### `02_data_prep.R` — Data Transformation and Variable Derivation

**Purpose:** Unified cleaning and variable construction for both dyadic and monadic datasets. Standardizes variable names across the 2024 and 2025 source files and generates the complex time-dependent variables (peace years) required for conflict modeling.

**Key design decisions:**

- **Unification of 2024/2025 Variable Naming** — Standardizes outcome variables like `mid_initiated` (hihosta >= 2) and `targets_democracy` (libdem >= 0.5) to ensure H1–H9 run on identical data structures.
- **Legitimation Mix Derivation** — Constructs the `legit_ratio` (ideological legitimation / total legitimation) from V-Dem components. This is the core IV for H5, H6, and H9.
- **Standardized Control Variables** — Implements consistent log-transformations for CINC scores and standardizes the `cold_war` dummy (1947–1991).
- **Peace Years Logic (Beck, Katz, and Tucker)** — Ports the data.table-based peace years logic from the 2024 `data_code.qmd`. Specifically, it handles left-censoring by assuming 35 years for regimes with no prior conflict and generates the cubic splines (`t`, `t2`, `t3`) required for binary time-series cross-sectional (BTSCS) models.
- **RDS Storage** — Saves the cleaned objects as `.rds` in a `ready_data/` folder (Git-ignored). This is preferred over CSV because it preserves factor levels and R-specific data attributes, ensuring `03–09` scripts don't need to re-type or re-factor columns.
- **Functional Approach** — Uses `prep_dyad()` and `prep_monadic()` wrappers. This allows the preparation logic to be re-run or modified centrally without affecting the data loading or model scripts.

---
- **Two validation column vectors (`.DYAD_REQUIRED_COLS` and `.DYAD_GRAVE_COLS`)** — intentionally separated. Core V-Dem and conflict variables `stop()` hard if missing. GRAVE-D support group columns issue a `warning()` instead, because it is still an open question whether `GRAVE_D_Master_with_Leaders.csv` contains those columns. The warning output on first run will identify exactly which columns need to be merged in `02_data_prep.R`.
- **`warn_grave = TRUE` argument** — lets you silence GRAVE-D warnings after confirming the merge is complete, without altering validation logic.
- **`autoc_max = 0.5` parameter** — matches the V-Dem autocracy threshold used in the 2025 repo's `methodology_results.qmd`. Parameterized so the cutoff can be adjusted for robustness checks without editing function internals.
- **`here::here()` for all paths** — consistent with the 2025 repo convention. Prevents broken paths when the project is opened from different working directories.

---

### `03_h1_h2_logit.R` — Logistic Regression for H1 and H2

**Purpose:** Estimates the primary logistic regression models for Hypothesis 1 (conflict initiation) and Hypothesis 2 (target selection). Produces sequentially-specified GLM model lists saved as `.rds` objects for use by downstream reporting scripts.

**Key design decisions:**

- **Two-function architecture** — `estimate_h1_logit()` and `estimate_h2_logit()` are defined as separate named functions rather than a single omnibus script. This allows them to be sourced individually in testing contexts and makes the hypothesis-level boundaries explicit.
- **H1: `mid_initiated` as DV** — uses the dyadic conflict initiation indicator (hihosta >= 2) constructed in `02_data_prep.R`. Five sequentially-specified models progress from ideology only → add target regime type → add CINC capabilities → add peace years cubic splines → add Cold War dummy. This mirrors the progressive specification logic in the 2025 repo's `h1_analysis.R`.
- **H2: `targets_democracy` conditional on initiation** — H2 asks whether initiators disproportionately target democracies. The H2 function filters `dyad_ready` to `mid_initiated == 1` rows before modeling, matching the conditionality of the original hypothesis.
- **`legit_ratio` as core IV** — replaces the `revisionist_ideology_a` variable used in the 2025 repo with the unified `legit_ratio` (ideological legitimation share) derived in `02_data_prep.R`. This is the standardized IV for the new project.
- **CINC controls: `log_cinc_a`, `log_cinc_b`, `cinc_ratio`** — all three are included to capture both side-specific capability levels and relative capability balance, consistent with standard conflict onset models.
- **Peace years as BTSCS control** — `t`, `t2`, `t3` cubic splines from `02_data_prep.R` are added in H1 Models 4–5 to correct for temporal autocorrelation in binary time-series cross-sectional data (Beck, Katz, and Tucker 1998).
- **Results saved to `results/`** — models are serialized as `.rds` to `results/h1_logit_models.rds` and `results/h2_logit_models.rds`. This folder is Git-ignored and must exist before running; `dir.create("results", showWarnings = FALSE)` handles this automatically.

---

_This README is updated as each new script is added to the `R/` directory._

*This README is updated as each new script is added to the `R/` directory.*
