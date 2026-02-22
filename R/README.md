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
- **Two validation column vectors (`.DYAD_REQUIRED_COLS` and `.DYAD_GRAVE_COLS`)** — intentionally separated. Core V-Dem and conflict variables `stop()` hard if missing. GRAVE-D support group columns issue a `warning()` instead, because it is still an open question whether `GRAVE_D_Master_with_Leaders.csv` contains those columns. The warning output on first run will identify exactly which columns need to be merged in `02_data_prep.R`.
- **`warn_grave = TRUE` argument** — lets you silence GRAVE-D warnings after confirming the merge is complete, without altering validation logic.
- **`autoc_max = 0.5` parameter** — matches the V-Dem autocracy threshold used in the 2025 repo's `methodology_results.qmd`. Parameterized so the cutoff can be adjusted for robustness checks without editing function internals.
- **`here::here()` for all paths** — consistent with the 2025 repo convention. Prevents broken paths when the project is opened from different working directories.

---

*This README is updated as each new script is added to the `R/` directory.*
