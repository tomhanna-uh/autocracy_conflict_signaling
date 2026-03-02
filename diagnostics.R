# Check size of one model vs the data
h1 <- readRDS("results/h1_logit_models.rds")
cat("Full list:", format(object.size(h1), units = "MB"), "\n")
cat("Just h1_full:", format(object.size(h1$h1_full), units = "MB"), "\n")
cat("dyad_ready:", format(object.size(dyad_ready), units = "MB"), "\n")

# The model frame is the culprit:
cat("Model frame:", format(object.size(h1$h1_full$model), units = "MB"), "\n")
