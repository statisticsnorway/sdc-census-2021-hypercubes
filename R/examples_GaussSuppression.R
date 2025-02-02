#  Two GaussSuppression examples dealing with the first group of hypercubes.


library(GaussSuppression)

load("data/data_1000000.RData")

# As generated by  hyper_formula(1, remove_geo_n = TRUE)
hyper_formula_1 <- ~sex * (age_l + age_m + age_h) * (lms_l + lms_h) + 
  sex * (age_l + age_m + age_h) * (hst_l + hst_m + hst_h) + 
  sex * (age_l + age_m + age_h) * (fst_l + fst_m + fst_h) + 
  sex * (lms_l + lms_h) * (hst_l + hst_m + hst_h)


a1 <- SuppressSmallCounts(data_1000000, 
                          maxN = 3, 
                          protectZeros = FALSE, 
                          formula = hyper_formula_1, 
                          extend0 = FALSE)

b1 <- SuppressDominantCells(data_1000000, 
                            dominanceVar = "value", 
                            pPercent = 5, 
                            formula = hyper_formula_1, 
                            contributorVar = "id")

