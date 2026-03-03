# Data Points Used in Model Formulas

This document lists every dataframe (or derived subset) referenced by model formulas
in the `R/` analysis scripts, along with the variables used from each.

All data originates from `grave_d_data2026` loaded in `01_load_data.R` and
transformed in `02_data_prep.R`.

---

## 1. `dyad_ready` (dyadic directed-dyad-year data)

Source: `02_data_prep.R` via `prep_dyad(dyad_df)`

Used directly by: `03_h1_h2_logit.R`, `04_h3_h4_logit.R`, `05_h5_h6_legitmix.R`,
`07_h7_mediation.R`, `08_h8_moderation.R`

### Outcome Variables

| Variable | Description | Used in |
|---|---|---|
| `mid_initiated` | Binary: hostility level >= 2 (MID initiation) | H1, H3, H5, H8 (DV); H2, H4, H6, H7 (filter) |
| `targets_democracy` | Binary: v2x_libdem_b >= 0.5 (targets a democracy) | H2, H4, H6, H7, H8 (DV) |

### Main Independent Variables

| Variable | Description | Used in |
|---|---|---|
| `sidea_revisionist_domestic` | Composite leader ideology score (GRAVE-D) | H1, H6, H7 |
| `sidea_religious_revisionist_domestic` | Religious ideology sub-type | H1 |
| `sidea_socialist_revisionist_domestic` | Socialist ideology sub-type | H1 |
| `sidea_nationalist_revisionist_domestic` | Nationalist ideology sub-type | H1 |
| `sidea_reactionary_revisionist_domestic` | Reactionary ideology sub-type | H1 |
| `sidea_separatist_revisionist_domestic` | Separatist ideology sub-type | H1 |
| `sidea_religious_support` | Religious group regime support (GRAVE-D) | H3, H4, H7 |
| `sidea_party_elite_support` | Party elite regime support (GRAVE-D) | H3, H4, H7 |
| `sidea_rural_worker_support` | Rural/worker group regime support (GRAVE-D) | H3, H4 |
| `sidea_military_support` | Military regime support (GRAVE-D) | H3, H4, H7, H8 |
| `sidea_ethnic_racial_support` | Ethnic/racial group regime support (GRAVE-D) | H3, H4 |
| `legit_ratio` | v2exl_legitideol_a / (legit_total + 0.01) | H5, H6, H8 |
| `legit_total` | Sum of legitimation components | H5 (subset var) |
| `v2exl_legitideol_a` | V-Dem ideological legitimation (Side A) | H5, H6 |
| `v2exl_legitperf_a` | V-Dem performance legitimation (Side A) | H5, H6 |
| `v2exl_legitlead_a` | V-Dem leader personalist legitimation (Side A) | H5, H6 |
| `sidea_dynamic_leader` | Dynamic leader indicator (GRAVE-D) | H8 |
| `ideology_gap` | abs(v2exl_legitideol_a - v2exl_legitideol_b) | (derived, used in data prep) |
| `large_ideology_gap` | Binary: ideology_gap > median | (derived, used in data prep) |

### Control Variables

| Variable | Description | Used in |
|---|---|---|
| `cinc_a` | COW CINC score (Side A capabilities) | H1, H2, H3, H4, H5, H6, H7 |
| `log_cinc_a` | log(cinc_a + 0.0001) | H8 |
| `log_cinc_b` | log(cinc_b + 0.0001) | H8 |
| `sidea_winning_coalition_size` | V-Dem/BdM selectorate control | H1, H2, H3, H4, H5, H6, H7, H8 |
| `cold_war` | Binary: year in [1947, 1991] | H1, H2, H3, H4, H5, H6, H7, H8 |
| `t` | Peace years (time since last conflict) | H1, H2, H3, H4, H5, H6, H8 |
| `t2` | t^2 (peace years squared) | H1, H3, H5, H8 |
| `t3` | t^3 (peace years cubed) | H1, H3, H5, H8 |

---

## 2. `monadic_ready` (monadic country-year data)

Source: `02_data_prep.R` via `prep_monadic(dyad_df)`

Used directly by: `05_m1_m2_mediation.R`, `06_h9_survival.R`

### Outcome / Dependent Variables

| Variable | Description | Used in |
|---|---|---|
| `log_tenure` | log(tenure + 1) | M1, M2 (outcome) |
| `tenure_years` | Tenure length in years | M1, M2, H9 (survival time) |
| `survived_to_end` | Binary: leader still in power at data end | M1, M2, H9 (censoring) |

### Treatment / Independent Variables

| Variable | Description | Used in |
|---|---|---|
| `conflicts_vs_ideo_targets` | Count of MIDs initiated (proxy for ideological targets) | M1, M2, H9 |
| `conflicts_vs_democracy` | Count of MIDs against democracies | M1 (robustness), H9 |
| `conflicts_vs_ideo_opponent` | Count of MIDs against ideologically distant opponents | M1 (robustness) |

### Mediator Variables

| Variable | Description | Used in |
|---|---|---|
| `avg_ideological_legit` | Leader-period mean v2exl_legitideol_a | M1 (mediator), H9 |
| `legit_ratio` | Ideological share of legitimation | M2 (mediator), H9 |
| `avg_other_legit` | v2exl_legitperf_a + v2exl_legitlead_a | M2 |

### Control Variables

| Variable | Description | Used in |
|---|---|---|
| `avg_autocracy_level` | 1 - v2x_libdem_a | M1, M2, H9 |
| `log_avg_gdp` | log(flgdpen + 1) | M1, M2, H9 |
| `log_avg_pop` | log(epop_a or tpop + 1) | M1, M2, H9 |
| `irregular_entry` | Binary: irregulartransition == 1 | M1, M2, H9 |

---

## 3. Derived Subsets (created inside model scripts)

These are not separate dataframes stored on disk; they are filtered subsets
created within model estimation functions.

### `h12_data` (script: `03_h1_h2_logit.R`)
- Source: `dyad_ready[, h12_vars]`
- Variables: `mid_initiated`, `targets_democracy`, `sidea_revisionist_domestic`,
  `sidea_religious_revisionist_domestic`, `sidea_socialist_revisionist_domestic`,
  `sidea_nationalist_revisionist_domestic`, `sidea_reactionary_revisionist_domestic`,
  `sidea_separatist_revisionist_domestic`, `cinc_a`, `sidea_winning_coalition_size`,
  `t`, `t2`, `t3`, `cold_war`
- H2 uses `conflict_data <- h12_data %>% filter(mid_initiated == 1)`

### `h34_data` (script: `04_h3_h4_logit.R`)
- Source: `dyad_ready[, h34_vars]`
- Variables: `mid_initiated`, `targets_democracy`, `sidea_religious_support`,
  `sidea_party_elite_support`, `sidea_rural_worker_support`, `sidea_military_support`,
  `sidea_ethnic_racial_support`, `cinc_a`, `sidea_winning_coalition_size`,
  `t`, `t2`, `t3`, `cold_war`
- H4 uses `conflict_data <- h34_data %>% filter(mid_initiated == 1)`

### `h56_data` (script: `05_h5_h6_legitmix.R`)
- Source: `dyad_ready[, h56_vars]`
- Variables: `mid_initiated`, `targets_democracy`, `legit_ratio`, `legit_total`,
  `v2exl_legitideol_a`, `v2exl_legitperf_a`, `v2exl_legitlead_a`,
  `sidea_revisionist_domestic`, `cinc_a`, `sidea_winning_coalition_size`,
  `t`, `t2`, `t3`, `cold_war`
- H6 uses `conflict_data <- h56_data %>% filter(mid_initiated == 1)`
- H5 hurdle uses `initiators <- h56_data %>% filter(mid_initiated == 1)`

### `h7_data` (script: `07_h7_mediation.R`)
- Source: `dyad_ready[, h7_vars]`
- Variables: `mid_initiated`, `targets_democracy`, `sidea_revisionist_domestic`,
  `sidea_religious_support`, `sidea_party_elite_support`,
  `sidea_rural_worker_support`, `sidea_military_support`,
  `sidea_ethnic_racial_support`, `cinc_a`, `sidea_winning_coalition_size`,
  `t`, `cold_war`
- All H7 models use `conflict_data <- h7_data %>% filter(mid_initiated == 1)`

### `h8_data` (script: `08_h8_moderation.R`)
- Source: `dyad_ready[, h8_vars]`
- Variables: `mid_initiated`, `targets_democracy`, `legit_ratio`,
  `sidea_dynamic_leader`, `log_cinc_a`, `log_cinc_b`,
  `sidea_winning_coalition_size`, `sidea_military_support`,
  `cold_war`, `t`, `t2`, `t3`
- H8 targeting/robustness use `conflict_data <- h8_data %>% filter(mid_initiated == 1)`

### `h9_data` (script: `06_h9_survival.R`)
- Source: `monadic_ready[, h9_vars]`
- Variables: `conflicts_vs_ideo_targets`, `conflicts_vs_democracy`,
  `conflicts_vs_ideo_opponent`, `avg_ideological_legit`, `legit_ratio`,
  `avg_autocracy_level`, `log_avg_gdp`, `log_avg_pop`,
  `irregular_entry`, `tenure_years`, `survived_to_end`
- All H9 Cox models create `surv_data` with derived `event = as.integer(survived_to_end == 0)`

### `monadic_ready` used directly (script: `05_m1_m2_mediation.R`)
- M1/M2 models use `monadic_ready` directly
- M1 survival creates `leader_surv` with derived `event = as.integer(survived_to_end == 0)`

---

## 4. Summary: All Unique Variables in Model Formulas

### Dependent Variables
- `mid_initiated` (dyadic, binary)
- `targets_democracy` (dyadic, binary)
- `log_tenure` (monadic, continuous)
- `tenure_years` (monadic, survival time)
- `survived_to_end` / `event` (monadic, censoring indicator)
- `avg_ideological_legit` (monadic, M1 mediator model DV)
- `legit_ratio` (monadic, M2 mediator model DV)
- `sidea_religious_support` (dyadic, H7 mediator path a DV)
- `sidea_party_elite_support` (dyadic, H7 mediator path a DV)
- `sidea_military_support` (dyadic, H7 mediator path a DV)

### Independent / Predictor Variables (on RHS of formulas)
- `sidea_revisionist_domestic`
- `sidea_religious_revisionist_domestic`
- `sidea_socialist_revisionist_domestic`
- `sidea_nationalist_revisionist_domestic`
- `sidea_religious_support`
- `sidea_party_elite_support`
- `sidea_rural_worker_support`
- `sidea_military_support`
- `sidea_ethnic_racial_support`
- `legit_ratio`
- `v2exl_legitideol_a`
- `v2exl_legitperf_a`
- `v2exl_legitlead_a`
- `sidea_dynamic_leader`
- `conflicts_vs_ideo_targets`
- `conflicts_vs_democracy`
- `conflicts_vs_ideo_opponent`
- `avg_ideological_legit`
- `avg_other_legit`
- `cinc_a`
- `log_cinc_a`
- `log_cinc_b`
- `sidea_winning_coalition_size`
- `avg_autocracy_level`
- `log_avg_gdp`
- `log_avg_pop`
- `irregular_entry`
- `cold_war`
- `t`
- `t2`
- `t3`
