# R/ Directory README

This file (`R/README.md`) documents the R scripts in the `R/` folder of the **autocracy_conflict_signaling** project.  
It explains the purpose of each script, the overall workflow, key conventions, and important lessons learned from debugging and optimization.

## Overview

The `R/` directory contains all analysis code for the project. Scripts are numbered to indicate logical execution order (Tier 1 → Tier 3 → reporting).  

All scripts use `here::here()` for path safety and rely on `dyad_ready` and `monadic_ready` objects created by `02_data_prep.R`.

### Execution Order (Recommended)

```text
00_packages.R        → load/install all packages
01_load_data.R       → define loading function
02_data_prep.R       → recode, derive variables, subset, save ready data
03_h1_h2_logit.R     → Tier 1: Leader ideology (H1 & H2)
04_h3_h4_logit.R     → Tier 1: Support groups (H3 & H4)
05_h5_h6_legitmix.R  → Tier 2: Legitimation mix (H5 & H6)
06_h9_survival.R     → Tier 3: Leader survival (H9 – Cox PH)
07_h7_mediation.R    → Tier 3: Support group mediation (H7 – lavaan/SEM)
08_h8_moderation.R   → Tier 3: Dynamic leader moderation (H8)
09_reporting_tables.R → Generate consolidated tables for Quarto

### Then Render the Book

In terminal:

cd docs
quarto render


| Script                  | Purpose                                                                 | Main Models / Outputs                          | Key Dependencies / Notes |
|-------------------------|-------------------------------------------------------------------------|------------------------------------------------|--------------------------|
| `00_packages.R`         | Install/load all required packages in one place                        | —                                              | Run first in every session |
| `01_load_data.R`        | Define `load_dyad_data()` function; validation helpers                 | —                                              | Sourced by `02_data_prep.R` |
| `02_data_prep.R`        | Load raw GRAVE-D, recode/derive variables, create scaled Carter-Signorino terms, subset to analysis columns, save `.rds` | `dyad_ready.rds`, `monadic_ready.rds`          | Critical — must run fresh for scaled terms |
| `03_h1_h2_logit.R`      | H1 & H2: Leader ideology → MID initiation & democracy targeting       | Logit models (baseline, controls, full, subtypes) | Uses `t_scaled + t2_scaled + t3_scaled` |
| `04_h3_h4_logit.R`      | H3 & H4: Support group variables → MID initiation & targeting         | Logit models by support group type             | Uses scaled temporal terms |
| `05_h5_h6_legitmix.R`   | H5 & H6: Legitimation mix (ratio) → initiation & targeting            | Logit + hurdle models                          | Scaled terms required |
| `06_h9_survival.R`      | H9: Leader survival (Cox PH) after conflict                           | Cox models (ideology, ratio, interactions)     | Uses monadic_ready |
| `07_h7_mediation.R`     | H7: Mediation via support groups (lavaan/SEM)                         | Path models                                    | Careful with formula variable names |
| `08_h8_moderation.R`    | H8: Dynamic leadership moderation of ideology effect                  | Logit + LPM with interactions                  | Scaled terms; robust plotting |
| `09_reporting_tables.R` | Consolidated stargazer/modelsummary tables for Quarto chapters        | HTML tables in `results/tables/`               | Reads saved `.rds` models |


## Important Conventions & Lessons Learned
### Temporal Dependence (Carter-Signorino)

Original problem: Unscaled t + t2 + t3 caused complete/quasi-separation, huge coefficients (8+ digits), and non-convergence.
Solution: Added scaled versions in 02_data_prep.R:

t_scaled   = t / 100
t2_scaled  = t_scaled^2
t3_scaled  = t_scaled^3

All model formulas now use t_scaled + t2_scaled + t3_scaled (or linear t_scaled).
Lesson: Always scale long-duration polynomials in BTSCS models. Unscaled t^3 explodes numerically.

### Narrowing dyad_ready

Original: Full 478 columns → large memory footprint (~4 GB).
Solution: Subset to ~31–34 columns via dyad_keep vector in 02_data_prep.R.
Key: Must include "t_scaled", "t2_scaled", "t3_scaled" in dyad_keep or models skip with missing-variable errors.
Lesson: Define a single authoritative dyad_keep vector and verify it includes all needed vars before modeling.

### Model Non-Convergence & Separation

Original: Ordinary glm() failed on rare events (MID onset ~0.1%).
Solution: brglm2::brglmFit (Firth bias reduction) as default in safe_glm.
Added aggressive controls: maxit = 2000, epsilon = 1e-10, slowit = 0.1, response_adjustment = TRUE.
Lesson: Rare binary outcomes almost always need penalized estimation. Check convergence with fit$converged and separation with detectseparation::detect_separation().

### Variable Name Collisions

Original: Formulas with + t + ... caused invalid type (closure) for variable 't' because t() is base R transpose.
Solution: Switched exclusively to t_scaled (and removed unscaled from dyad_keep where possible).
Lesson: Avoid base R function names (t, c, I, F, T) as variable names. Prefix derived variables (e.g., peace_t_scaled).

### VIF & Post-Estimation Failures

Original: VIF crashed on singular models; plot_model() failed on NULL/invalid fits.
Solution:
safe_vif() with early vcov checks and type = "terms" for glm/coxph.
Conditional checks before plotting (!is.null(model) && !is.null(model$coefficients)).

Lesson: Wrap all post-estimation (VIF, plots, predictions) in safe wrappers with NULL handling.

### Data Prep Skipping Pitfalls

Original: if (exists("dyad_ready")) skipped prep → used stale version without scaled terms.
Solution: Added force-remove at top of 02_data_prep.R:R

rm(list = c("dyad_ready", "monadic_ready"), envir = .GlobalEnv)

Lesson: Make prep scripts force-refresh during development. Consider a force_refresh = TRUE argument for production.

### Memory & Performance

- Original: Full 478-column dyad_ready used excessive RAM.
- Solution: Narrowed to 31–34 columns; stripped model internals before saving RDS.
- Lesson: Subset aggressively to analysis-relevant columns early; strip large model components ($data, $fitted.values, etc.) before RDS save.

### General Advice for Future Projects

- Define all model-relevant columns in one place (dyad_keep) and verify inclusion.
- Use scaled/orthogonal temporal terms from the beginning.
- Default to penalized methods for rare events.
- Build safe_ wrappers early for estimation, diagnostics, and plotting.
- Avoid base function names as variables.
- Test each script standalone with fresh prep before full pipeline.

This README should be kept up-to-date as the project evolves.
