# This script creates fuzzy forest models for the NSQIP 2016 dataset
# data_processing_puf16.R and outcome_grouping_puf16.R must be run prior to 
# running the script for the pred_puf16 and grouped_outcomes_puf16 objects.
# This code is adapted from https://github.com/sysilviakim/turnout2016
# Kyle McGraw, August 2019

library(caret)
library(dplyr)
library(doParallel)
library(ROCR)

# 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))
# BiocManager::available()
# BiocManager::install(version = "3.18")
# BiocManager::install("Biobase")
# BiocManager::install("WGCNA",force = TRUE)
# BiocManager::install("GO.db")
# BiocManager::valid("GO.db")
library(WGCNA)


 
library(fuzzyforest)

# Load data
load("pred_puf16.Rda")
load("grouped_outcomes_puf16.Rda")


ff_params <- function(controls, fxn) {
  screen_params <-
    screen_control(
      drop_fraction = controls$drop_fraction[fxn],
      keep_fraction = controls$keep_fraction[fxn],
      min_ntree = controls$min_ntree[fxn],
      ntree_factor = controls$ntree_factor[fxn],
      mtry_factor = controls$mtry_factor[fxn]
    )
  select_params <-
    select_control(
      drop_fraction = controls$drop_fraction[fxn],
      number_selected = controls$number_selected[fxn],
      min_ntree = controls$min_ntree[fxn],
      ntree_factor = controls$ntree_factor[fxn],
      mtry_factor = controls$mtry_factor[fxn]
    )
  return(list(screen_params = screen_params, select_params = select_params))
}

# Creates models for outcomes and saves the models to file
# "y_cardiac", "y_renal", "y_dead", not enough cases for model
outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_SSI", "y_uti", "y_thromb", "y_readmit", "y_reop", "y_discharge_care", "y_sepsis")
for (outcome in outcome_names){
  
  # Setup clusters
#  cl <- parallel::detectCores() / 2
  cl <- 4
  mc <- makeCluster(cl)
  registerDoParallel(mc)
  
  # Selects plastic surgery data for specified outcome
  train <- mutate(pred_puf16, y_var = as.factor(grouped_outcomes_puf16[[outcome]]))
  levels(train$y_var) <- c("no_outcome", "outcome")
  plastic_train = filter(train, CPT_plastic == 1)
  
  
  # Analysis of scale free topology for soft-thresholding ======================
  p <- pickSoftThreshold(plastic_train)
  x <- NULL
  if (!(is.na(p$powerEstimate))) x <- p$powerEstimate
  power_vec <- unique(c(4, 6), x)
  
  # Hyperparameters ============================================================
  ## We will later cross validate these choices
  ## With default minModuleSize, you get only the gray and turquoise module.
  controls <-
    expand.grid(
      mtry_factor = c(1),
      min_ntree = c(1000),
      ntree_factor = c(1),
      nodesize = c(1),
      final_ntree = c(1000),        ## Default is 500
      number_selected = c(20),      ## Default is 5
      power = power_vec,            ## Default is 6
      drop_fraction = c(0.5, 0.25), ## Default is 0.25
      minModuleSize = c(3),         ## Default is min(20, ncol(X) / 2).
      keep_fraction = c(0.25),      ## Default is 0.05
      nThreads = c(cl),
      TOMType = c("unsigned")
    )
  
  # Covariates and dependent variable ==========================================
  X_basic <- select(plastic_train, -y_var)
  y_basic <- plastic_train$y_var
  class(X_basic) <- "data.frame"
  set.seed(123)
  cv_index <- createFolds(y_basic, k = 10) ## List of indices
  
  # Fuzzy forest loop ==========================================================
  cv_perf_wff <- cv_perf_ff <- list()
  for (i in seq(1, nrow(controls))) {
    output <- list()
    ## Parameter controls
    param_list <- ff_params(controls, i)
    ## 10-fold CV
    for (j in 1:10) {
      X <- X_basic[-unlist(cv_index[j]), ]
      y <- y_basic[-unlist(cv_index[j])]
      X_test <- X_basic[unlist(cv_index[j]), ]
      y_test <- y_basic[unlist(cv_index[j])]
      module <- blockwiseModules(
        X,
        power = controls$power[i],
        minModuleSize = controls$minModuleSize[i],
        nThreads = controls$nThreads[i]
      )
      # print(paste0("Module calculated: spec. ", i, ", ", j, "-th iter."))
      model.ff <-
        ff(
          X, y,
          module_membership = module$colors,
          num_processors = cl,
          screen_params = param_list$screen_params,
          select_params = param_list$select_params,
          final_ntree = controls$final_ntree[i]
        )
      # print(paste0("ff complete: spec. ", i, ", ", j, "-th iter."))
      ## Metric is ROC (i.e., AUC) 
      ff_predict <- predict(model.ff$final_rf, newdata = X_test, type = "prob")
      ff_rocr <- prediction(ff_predict[, 2], y_test)
      ff_auc <- performance(ff_rocr, "auc")
      ## Store output 
      output[[j]] <-
        list(
          X = X, y = y, X_test = X_test, y_test = y_test,
          model.ff = model.ff, module = module, param_list = param_list,
          ff_predict = ff_predict, ff_rocr = ff_rocr, ff_auc = ff_auc  
        )
      gc(reset = T)
    }
    ## Average performance?
    cv_perf_ff[[i]] <-
      unlist(lapply(output, function(x) x$ff_auc@y.values[[1]]))
    # print(cv_perf_ff[[i]])
  }
  
  # Final choice of models =====================================================
  final_choice_ff <-
    which(unlist(lapply(cv_perf_ff, mean)) ==
            max(unlist(lapply(cv_perf_ff, mean))))
  ## In the rare case that there are 2+ specifications with same max level
  if (length(final_choice_ff) > 1) final_choice_ff = max(final_choice_ff)
  
  ff_X <- select(plastic_train, -y_var)
  ff_controls <- controls[final_choice_ff, ]
  param_list <- ff_params(controls, final_choice_ff)
  
  module <- blockwiseModules(
    ff_X,
    power = ff_controls$power,
    minModuleSize = ff_controls$minModuleSize,
    nThreads = ff_controls$nThreads
  )
  
  ff.time <- system.time(
    plastic.ff <-
      ff(ff_X, plastic_train$y_var,
         module_membership = module$colors,
         num_processors = cl,
         screen_params = param_list$screen_params,
         select_params = param_list$select_params,
         final_ntree = ff_controls$final_ntree
      )
  )
  
  save(
    list = c(
      "ff.time", "plastic.ff", "controls", "cv_index", "cv_perf_ff", "final_choice_ff"
    ),
    file = paste0("", i, "_ff.Rda")
    
  )
  registerDoSEQ()
}
stopCluster(mc)

