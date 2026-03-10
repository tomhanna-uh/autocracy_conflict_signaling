# Load if needed
h7_mediation <- readRDS("results/h7_mediation.rds")

# Summary for each mediation model (indirect, direct, total effects + p-values)
summary(h7_mediation$med_h7_religious)   # Religious support mediation
summary(h7_mediation$fit_h7_party)       # Party elite
summary(h7_mediation$fit_h7_military)    # Military

# Or loop over all for a clean table
for (nm in names(h7_mediation)) {
        cat("\n=== ", nm, " ===\n")
        print(summary(h7_mediation[[nm]]))
}

# Load if needed
h7_sem <- readRDS("results/h7_sem.rds")

# Detailed summary for each SEM model (path coefficients, fit indices)
summary(h7_sem$fit_sem_religious, fit.measures = TRUE, standardized = TRUE)
summary(h7_sem$fit_sem_party, fit.measures = TRUE, standardized = TRUE)
# Add more if there are additional fits

# Key fit indices (CFI, TLI, RMSEA, SRMR)
fitMeasures(h7_sem$fit_sem_religious, c("cfi", "tli", "rmsea", "srmr"))
fitMeasures(h7_sem$fit_sem_party, c("cfi", "tli", "rmsea", "srmr"))

# Standardized path coefficients (easier to interpret)
standardizedSolution(h7_sem$fit_sem_religious)
standardizedSolution(h7_sem$fit_sem_party)



# In QMD chunk
med_summary <- function(med) {
        s <- summary(med)
        data.frame(
                Group = deparse(substitute(med)),
                ACME = s$d.avg,
                ACME_CI = paste0("[", round(s$d.avg.ci[1], 3), ", ", round(s$d.avg.ci[2], 3), "]"),
                ACME_p = s$d.avg.p,
                ADE = s$z.avg,
                ADE_CI = paste0("[", round(s$z.avg.ci[1], 3), ", ", round(s$z.avg.ci[2], 3), "]"),
                ADE_p = s$z.avg.p,
                Total = s$n.avg,
                Total_CI = paste0("[", round(s$n.avg.ci[1], 3), ", ", round(s$n.avg.ci[2], 3), "]"),
                Total_p = s$n.avg.p,
                Prop_Mediated = s$n.avg / s$d.avg
        )
}

med_table <- rbind(
        med_summary(h7_mediation$med_h7_religious),
        med_summary(h7_mediation$fit_h7_party),
        med_summary(h7_mediation$fit_h7_military)
)

knitr::kable(med_table, digits = 3, caption = "Mediation Effects (Nonparametric Bootstrap)")



# Manuscript Wording:Mediation analysis (Imai et al. 2010) shows that religious support groups significantly mediate the effect of revisionist ideology on conflict (ACME = 0.0087, p < 0.001), while party elite support suppresses it (ACME = -0.0056, p < 0.001). Military support shows no mediation (ACME ≈ 0, p = 0.956). SEM fits were perfect (CFI/TLI = 1.0, RMSEA/SRMR = 0.0), with strong paths from ideology to religious/party elite support and from those groups to conflict outcomes.

library(tidySEM)

# Religious
graph_sem(model = h7_sem$fit_sem_religious,
          layout = get_layout(h7_sem$fit_sem_religious),
          edges = get_edges(h7_sem$fit_sem_religious),
          nodes = get_nodes(h7_sem$fit_sem_religious))

ggsave("results/figures/sem_religious_tidy.png", width = 10, height = 8)

# Party
graph_sem(model = h7_sem$fit_sem_party)
ggsave("results/figures/sem_party_tidy.png", width = 10, height = 8)