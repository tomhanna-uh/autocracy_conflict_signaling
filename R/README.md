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


Script,Purpose,Main Models / Outputs,Key Dependencies / Notes
00_packages.R,Install/load all required packages in one place,—,Run first in every session
01_load_data.R,Define load_dyad_data() function; validation helpers,—,Sourced by 02_data_prep.R
02_data_prep.R,"Load raw GRAVE-D, recode/derive variables, create scaled Carter-Signorino terms, subset to analysis columns, save .rds","dyad_ready.rds, monadic_ready.rds",Critical — must run fresh for scaled terms
03_h1_h2_logit.R,H1 & H2: Leader ideology → MID initiation & democracy targeting,"Logit models (baseline, controls, full, subtypes)",Uses t_scaled + t2_scaled + t3_scaled
04_h3_h4_logit.R,H3 & H4: Support group variables → MID initiation & targeting,Logit models by support group type,Uses scaled temporal terms
05_h5_h6_legitmix.R,H5 & H6: Legitimation mix (ratio) → initiation & targeting,Logit + hurdle models,Scaled terms required
06_h9_survival.R,H9: Leader survival (Cox PH) after conflict,"Cox models (ideology, ratio, interactions)",Uses monadic_ready
07_h7_mediation.R,H7: Mediation via support groups (lavaan/SEM),Path models,Careful with formula variable names
08_h8_moderation.R,H8: Dynamic leadership moderation of ideology effect,Logit + LPM with interactions,Scaled terms; robust plotting
09_reporting_tables.R,Consolidated stargazer/modelsummary tables for Quarto chapters,HTML tables in results/tables/,Reads saved .rds models

