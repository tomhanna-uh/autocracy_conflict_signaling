# Load the H3 models
h3_models <- readRDS("results/h3_logit_models.rds")

# Now you can summarize any of them
summary(h3_models$h3_full)

# Or look at coefficients only (safer if summary crashes)
coef(h3_models$h3_full)

# Or tidy version (from broom)
library(broom)
tidy(h3_models$h3_full)

# Check convergence status
h3_models$h3_full$converged   # TRUE/FALSE
h3_models$h3_full$iter        # Number of iterations used