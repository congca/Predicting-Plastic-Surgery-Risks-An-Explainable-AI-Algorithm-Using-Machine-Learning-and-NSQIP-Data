# This script creates logit forest models for the NSQIP 2016 dataset
# using variables selected by the corresponding fuzzy forest
# data_processing_puf16.R, outcome_grouping_puf16.R, and ff.R must be run prior to 
# running the script for the pred_puf16, grouped_outcomes_puf16, and ff objects.
# This code is adapted from https://github.com/sysilviakim/turnout2016


library(dplyr)
library(caret)

# Load data
load("pred_puf16.Rda")
load("grouped_outcomes_puf16.Rda")

# Creates models for outcomes and saves the models to file
# "y_cardiac", "y_renal", "y_dead", not enough cases for model
outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_SSI", "y_uti", "y_thromb", "y_readmit", "y_reop", "y_discharge_care", "y_sepsis")
for (outcome in outcome_names){
  
  # Selects plastic surgery data for specified outcome
  train <- mutate(pred_puf16, y_var = as.factor(grouped_outcomes_puf16[[outcome]]))
  levels(train$y_var) <- c("no_outcome", "outcome")
  plastic_train = filter(train, CPT_plastic == 1)
  
  load(paste0( outcome, "_ff.Rda"))
  plastic_train = select(plastic_train, plastic.ff$feature_list[["feature_name"]], y_var)
  
  repseeds <- function(folds = 10, from = 1e+04, seed = 123) {
    set.seed(seed)
    ## (n_repeats * nresampling) + 1
    seeds <- vector(mode = "list", length = folds + 1)
    for (i in 1:folds)
      seeds[[i]] <- sample.int(n = from, from)
    seeds[[folds + 1]] <- sample.int(n = from, 1)
    return(seeds)
  }
  
  tc <- trainControl(
    method = "cv",
    number = 10,
    summaryFunction = twoClassSummary, ## Provides ROC summary stats
    allowParallel = TRUE,
    verboseIter = FALSE,
    seeds = repseeds(), ## Reproducible seeds
    classProbs = TRUE
  )
  
  logit.time <- system.time(
    plastic.logit <-
      train(
        y_var ~ .,
        metric = "ROC",
        method = "glm",
        family = "binomial",
        trControl = tc,
        data = plastic_train
      )
  )
  save(
    list = c("logit.time", "plastic.logit"),
    file = paste0("./data/", outcome, "_logit.Rda")
  )
}

