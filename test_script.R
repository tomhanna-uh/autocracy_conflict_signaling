# test really simple models



my_data <- na.omit(dyad_ready[, c("COWcode_a","dyad", "year", "mid_initiated", "sidea_revisionist_domestic", "cinc_a", "t")])

my_data <- data.frame(my_data)

rm(list = c("dyad_ready","grave_d","monadic_ready"))

gc()

model_test1 <- glm(mid_initiated ~ sidea_revisionist_domestic, 
                   data = my_data, family = binomial(link = "logit"))

stargazer(model_test1)

summary(model_test1) 


