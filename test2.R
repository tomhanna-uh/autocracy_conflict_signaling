# Load the H3 models
h3_models <- readRDS("results/h3_logit_models.rds")

# Now you can summarize any of them
summary(h3_models$h3_full)

# Or look at coefficients only (safer if summary crashes)
coef(h3_models$h3_full)

# # Or tidy version (from broom)
# library(broom)
# tidy(h3_models$h3_full)

# Check convergence status
h3_models$h3_full$converged   # TRUE/FALSE
h3_models$h3_full$iter        # Number of iterations used



# Load the H3 models
h4_models <- readRDS("results/h4_logit_models.rds")

# Now you can summarize any of them
summary(h4_models$h4_full)

# Or look at coefficients only (safer if summary crashes)
coef(h4_models$h4_full)

# # Or tidy version (from broom)
# library(broom)
# tidy(h3_models$h3_full)

# Check convergence status
h4_models$h4_full$converged   # TRUE/FALSE
h4_models$h4_full$iter        # Number of iterations used