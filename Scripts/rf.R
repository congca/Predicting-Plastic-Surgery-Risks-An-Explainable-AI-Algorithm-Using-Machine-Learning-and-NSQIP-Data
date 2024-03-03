# This script creates random forest models for the NSQIP 2016 dataset
# data_processing_puf16.R and outcome_grouping_puf16.R must be run prior to 
# running the script for the pred_puf16 and grouped_outcomes_puf16 objects.
# This code is adapted from https://github.com/sysilviakim/turnout2016
# Kyle McGraw, August 2019

library(caret)
library(dplyr)
library(doParallel)


# Setup clusters
cl <- parallel::detectCores() / 2
mc <- makeCluster(cl)
registerDoParallel(mc)

# Load data
load("pred_puf16.Rda")
load("grouped_outcomes_puf16.Rda")

# Creates models for outcomes and saves the models to file
outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_cardiac", "y_SSI", "y_uti", "y_thromb", "y_renal", "y_readmit", "y_reop", "y_dead", "y_discharge_care", "y_sepsis")
for (i in outcome_names){
  
  # Selects plastic surgery data for specified outcome
  train <- mutate(pred_puf16, y_var = as.factor(grouped_outcomes_puf16[[i]]))
  levels(train$y_var) <- c("no_outcome", "outcome")
  plastic_train = filter(train, CPT_plastic == 1)
  
  mtrys <-
    c(
      floor(sqrt(ncol(pred_puf16))),
      2, 
      floor((2 + (ncol(pred_puf16) - 1)) / 2),
      ncol(pred_puf16)
    )
  
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
  
  # Run random forest separately for each mtry and store the output ============
  rf.time.cv <- system.time(
    plastic.rf <- 
      train(
        y_var ~ .,
        metric = "ROC",
        method = "rf",
        importance = T,
        proximity = F,
        ntree = 1000,
        tuneGrid = data.frame(.mtry = expand.grid(.mtry = mtrys)),
        trControl = tc,
        data = plastic_train
      )
  )
  best_mtry <- plastic.rf$bestTune$mtry
  
  save(
    list = c(
      "rf.time.cv", "mtrys", "tc", "plastic.rf", "best_mtry"
    ),
    file = paste0("./data/", i, "_rf.Rda")
    )
  
}
stopCluster(mc)

