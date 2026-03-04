library(detectseparation)
library(brglm2)

# Example — run on whichever model is failing
model <- safe_glm(mid_initiated ~ sidea_religious_revisionist_domestic +
                          cinc_a + sidea_winning_coalition_size +
                          t_scaled + t2_scaled + t3_scaled + cold_war,
                  data = h12_data)

detect_separation(model)
check_infinite_estimates(model)