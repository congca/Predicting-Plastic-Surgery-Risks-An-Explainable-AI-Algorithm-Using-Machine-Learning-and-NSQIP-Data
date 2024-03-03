# This script does hot-deck imputation of the processed data from the NSQIP 2016, 2017, and 2018 datasets
# data_processing_puf16.R, data_processing_puf17.R, and data_processing_puf18.R must be run prior
# to running the script for the preimpute_pred_puf16, preimpute_pred_puf17, preimpute_pred_puf18,
# preimpute_outcomes_puf16, preimpute_outcomes_puf17, and preimpute_outcomes_puf18 objects.
# This file generates latex code for a table of outcomes: outcomes.tex


library(simputation)
# library(dplyr)

### Import Data ###
load("pred_puf16.Rda")
load("pred_puf17.Rda")
load("pred_puf18.Rda")


pred_puf16 <- impute_shd(pred_puf16, . ~ .)
pred_puf17 <- impute_shd(pred_puf17, . ~ .)
pred_puf18 <- impute_shd(pred_puf18, . ~ .)


save(pred_puf16, file = "pred_puf16.Rda")
save(pred_puf17, file = "pred_puf17.Rda")
save(pred_puf18, file =  "pred_puf18.Rda")
