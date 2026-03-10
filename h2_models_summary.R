h3_models <- readRDS("results/h3_logit_models.rds")
summary(h3_models$h3_full)  # or coef(h3_models$h3_full)


h4_models <- readRDS("results/h4_logit_models.rds")
summary(h4_models$h4_full)