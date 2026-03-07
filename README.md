# Autocracy, Conflict, and Ideological Signaling

**Author:** Tom Hanna
**ORCID:** 0000-0002-8054-0335
**Affiliation:** University of Houston, Department of Political Science
**License:** [CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/)
**Copyright:** Tom Hanna, 2020–2026

---

## Abstract

Why do radical revisionist domestic ideologies result in revisionist international demands? Why are dictatorships led by extremists more prone to targeting democracies in interstate conflict? While the pattern of extremism and conflict is not much questioned, the causal mechanism is not thoroughly explained. A common assumption is that ideological leaders pursue revisionist goals because of personal normative preferences and that they are able to use conflict because the domestic agenda is entirely theirs to set. This conflicts with the assumption that the primary motive of leaders is to remain in power. The costly pursuit of extraterritorial ambitions detracts from the material resources available to pay off support coalitions and otherwise keep the dictator in power.

I argue that ideological dictators use revisionist demands to send signals of ideological commitment to their most important domestic supporters — the **Rational Autocrat** hypothesis. This project tests this argument against the alternative **Messianic Autocrat** hypothesis (that personal dynamic leadership explains the pattern) through an analysis of dyadic MIDs and a monadic analysis of the first use of violent force. It further examines how the *mix* of legitimation strategies (ideological vs. performance vs. personalist) shapes both conflict initiation and target selection, and whether conflict behavior enhances leader survival through renewed domestic legitimation.

---

## Theoretical Framework

Three ideal types structure the theory:

| Type | Core Claim | Key Mechanism |
|---|---|---|
| **The Ideological Autocrat** | Leadership ideology drives revisionist conflict | Direct preference expression |
| **The Rational Autocrat** | Support group ideology drives conflict as costly signal | Coalition loyalty maintenance |
| **The Messianic Autocrat** | Dynamic personal leadership drives conflict | Leader charisma / personal drive |

The Rational Autocrat argument is the primary theoretical contribution. The Ideological and Messianic Autocrat claims are treated as competing hypotheses.

---

## Hypotheses

All hypotheses are listed in order from simplest to most complex model tier.

### Tier 1 — Simple Logistic Regression

| Label | Substantive Name | Statement | Old Source |
|---|---|---|---|
| **H1** | The Ideological Autocrat — Initiation | Autocratic states with higher levels of revisionist domestic leadership ideology will be more likely to originate a revisionist MID than other autocratic states, all else equal. | 2024 H2a; 2025 H1 |
| **H2** | The Ideological Autocrat — Targeting | Autocratic states with higher levels of revisionist domestic leadership ideology will be more likely to originate revisionist MIDs targeting democracies than other autocratic states, all else equal. | 2024 H2b; 2025 H2 |
| **H3** | The Rational Autocrat — Initiation | Autocratic states with higher levels of regime support from groups associated with revisionist ideologies will be more likely to originate a revisionist MID than other autocratic states, all else equal. | 2024 H1a |
| **H4** | The Rational Autocrat — Targeting | Autocratic states with higher levels of regime support from groups associated with revisionist ideologies will be more likely to originate MIDs targeting democracies than other autocratic states, all else equal. | 2024 H1b |

### Tier 2 — Legitimation Mix (Logistic + Hurdle Models)

| Label | Substantive Name | Statement | Old Source |
|---|---|---|---|
| **H5** | Legitimation Mix — Initiation | The relative dependence on ideological legitimation (compared to performance or personalist legitimation) increases the likelihood of conflict initiation, all else equal. | 2025 H3 |
| **H6** | Legitimation Mix — Targeting | The relative dependence on ideological legitimation increases the likelihood of targeting democracies, all else equal. | 2025 H4 |

### Tier 3 — Mediation, Moderation, and Survival

| Label | Substantive Name | Statement | Old Source |
|---|---|---|---|
| **H7** | Mediation — Support Groups | The effect of revisionist leadership ideology on targeting democracies is mediated by the leader's dependence on ideologically associated support groups. | 2024 H3 / SEM |
| **H8** | Moderation — Dynamic Leadership | Dynamic personal leadership qualities moderate (amplify) the effect of revisionist ideology on conflict behavior (Messianic Autocrat test). | 2024 A1/A2/A3 |
| **H9** | Survival Mediation | Conflict initiation against ideological targets increases leader survival by enhancing domestic ideological legitimation. | 2025 M1/M2 |

---

## Repository Structure

```
autocracy_conflict_signaling/
├── README.md
├── autocracy_conflict_signaling.Rproj
├── .gitignore
├── data/                          # gitignored — place data files here
│   └── GRAVE_D_Master_with_Leaders.csv
├── R/
│   ├── 00_packages.R              # All library() calls in one place
│   ├── 01_load_data.R             # Data loading function, sourced by all scripts
│   ├── 02_data_prep.R             # Recodes, variable derivations, merges
│   ├── 03_h1_h2_logit.R           # H1 & H2: Leader ideology → MID/targeting
│   ├── 04_h3_h4_logit.R           # H3 & H4: Support groups → MID/targeting
│   ├── 05_h5_h6_legitmix.R        # H5 & H6: Legitimation mix (logit + hurdle)
│   ├── 06_h9_survival.R           # H9: Cox PH leader survival models
│   ├── 07_h7_mediation.R          # H7: SEM/lavaan support group mediation
│   ├── 08_h8_moderation.R         # H8: Dynamic leader moderation
│   └── 09_reporting_tables.R      # Consolidated stargazer/modelsummary output
└── docs/
    ├── _quarto.yml                # Quarto project config
    ├── theory.qmd                 # Theory chapter
    ├── data_methods.qmd           # Data, variables, model specifications
    ├── results_h1_h4.qmd          # Tier 1 results (simple logit)
    ├── results_h5_h6.qmd          # Tier 2 results (legitimation mix)
    ├── results_h7_h9.qmd          # Tier 3 results (mediation, moderation, survival)
    └── appendix.qmd               # Robustness checks, descriptive statistics
```

---

## Data

The primary dataset is **GRAVE_D_Master_with_Leaders.csv**, which integrates:

- **GRAVE-D** dyadic MID data with GRAVE-D leadership ideology and support group variables
- **V-Dem** legitimation variables: `v2exl_legitideol`, `v2exl_legitlead`, `v2exl_legitperf`
- **Correlates of War** national material capabilities (CINC scores)
- Leader tenure and entry/exit data

Data files are **gitignored** and must be placed in `data/` before running any analysis scripts. See `R/01_load_data.R` for expected column names and filtering logic.

### Key Variables

| Variable | Source | Role |
|---|---|---|
| `sidea_revisionist_domestic` | GRAVE-D | Leadership ideology (composite) |
| `sidea_nationalist/socialist/religious/reactionary/separatist_revisionist_domestic` | GRAVE-D | Ideology by type |
| `sidea_religious/party_elite/rural_worker/military/ethnic_racial_support` | GRAVE-D | Support group variables |
| `v2exl_legitideol_a` | V-Dem | Ideological legitimation |
| `v2exl_legitlead_a` | V-Dem | Personalist legitimation |
| `v2exl_legitperf_a` | V-Dem | Performance legitimation |
| `sidea_dynamic_leader` | GRAVE-D | Dynamic leadership (Messianic Autocrat proxy) |
| `cinc_a` | COW CINC | Capabilities control |
| `sidea_winning_coalition_size` | V-Dem/BdM | Selectorate theory control |

---

## Running the Analysis

All scripts use `here::here()` for path management. To run the full analysis:

```r
# 1. Install/load all packages
source("R/00_packages.R")

# 2. Load and prep data
source("R/01_load_data.R")
source("R/02_data_prep.R")

# 3. Run models in order (Tier 1 → Tier 3)
source("R/03_h1_h2_logit.R")
source("R/04_h3_h4_logit.R")
source("R/05_h5_h6_legitmix.R")
source("R/06_h9_survival.R")
source("R/07_h7_mediation.R")
source("R/08_h8_moderation.R")

# 4. Generate all output tables
source("R/09_reporting_tables.R")
```

To render the full manuscript:

```bash
cd docs
quarto render
```

---

## Naming Conventions

For consistency across scripts, Quarto documents, and output tables:

- **Model objects:** `h1_baseline`, `h1_controls`, `h1_full`; sub-hypotheses by ideology type: `h1_religious`, `h1_socialist`, `h1_nationalist`
- **Mediation objects:** `med_h7_religious`, `fit_h7_socialist`, `fit_h7_party`
- **Cox models:** `cox_h9_ideology`, `cox_h9_ratio`, `cox_h9_int_ideology`, `cox_h9_int_ratio`
- **Hypothesis labels in prose:** Always bold, e.g., **H1**, **H7**

---

## Related Repositories

- [2024_Research_Conflict_Ideology](https://github.com/tomhanna-uh/2024_Research_Conflict_Ideology) — Prior version with GRAVE-D support group and ideology models (Selectorate Theory framing)
- [2025_grave_d_conflict](https://github.com/tomhanna-uh/2025_grave_d_conflict) — Prior version with V-Dem legitimation mix and survival analysis
- [NAGS_Signaling](https://github.com/tomhanna-uh/nags_signaling) - Papers 2 and 3 on this topic which focus on support for foreign Non-State Armed Groups as signaling to domestic support and opposition groups. 

---

Note that the references section is based on the citations in this limited draft version only. 

---

## Citation

Hanna, Tom. *Autocracy, Conflict, and Ideological Signaling*. Working manuscript, University of Houston, 2025.

---

## License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).
