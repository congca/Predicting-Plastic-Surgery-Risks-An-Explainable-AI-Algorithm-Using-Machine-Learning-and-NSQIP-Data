# This script creates uses the NSQIP 2017 dataset to test the models
# data_processing_puf17.R, outcome_grouping_puf17.R, rf.r, ff.R, and logit.R
# must be run prior to running the script for the pred_puf17, 
# grouped_outcomes_puf17, rf, ff, and logit objects.
# This file generates a table of AUC values for each models as well as graphs of
# the ROC curves for each model
# Kyle McGraw, August 2019

library(dplyr)
library(xtable)
library(ROCR)
library(fuzzyforest)
library(gridExtra)

# Load data
load("pred_puf17.Rda")
load("grouped_outcomes_puf17.Rda")

# Changes age and BMI to categories
NSQIP_pred17 <- mutate(pred_puf17,
                       age_under_65 = if_else(patient_age < 65, 1, 0, missing=0),
                       age_65_to_74 = if_else(patient_age >= 65 & patient_age <= 74, 1, 0, missing=0),
                       age_75_to_84 = if_else(patient_age >= 75 & patient_age <= 84, 1, 0, missing=0),
                       age_85_plus = if_else(patient_age >= 85, 1, 0, missing=0),
                       
                       BMI_underweight = if_else(BMI < 18.5, 1, 0, missing=0),
                       BMI_normal = if_else(BMI >= 18.5 & BMI < 25, 1, 0, missing=0),
                       BMI_overweight = if_else(BMI >= 25 & BMI < 30, 1, 0, missing=0),
                       BMI_obese_1 = if_else(BMI >= 30 & BMI < 35, 1, 0, missing=0),
                       BMI_obese_2 = if_else(BMI >= 35 & BMI < 40, 1, 0, missing=0),
                       BMI_obese_3 = if_else(BMI >= 40, 1, 0, missing=0),
)

# Selects predictors
NSQIP_frame <- select(NSQIP_pred17, CPT_plastic, female, male, cpt, age_under_65, age_65_to_74, age_75_to_84, age_85_plus, BMI_underweight, BMI_normal, 
                      BMI_overweight, BMI_obese_1, BMI_obese_2, BMI_obese_3, diabetes_no, diabetes_insulin, diabetes_noninsulin, 
                      smoke_yes, smoke_no, dyspnea_rest, dyspnea_moderate, dyspnea_no, functional_hs_independent, functional_hs_partially, 
                      functional_hs_dependent, functional_hs_unknown, ventilator_dependent, ventilator_independent, history_COPD, 
                      history_noCOPD, ascites_n, ascites_y, CHF_y, CHF_n, Hyper_med_y, Hyper_med_n, Renal_fail_y, Renal_fail_n, 
                      Dialysis_y, Dialysis_n, Diss_cancer_y, Diss_cancer_n, Chronic_steroid_y, Chronic_steroid_n, Sepsis_none, 
                      Sepsis_sepsis, Sepsis_shock, Sepsis_sirs, Emergency_y, Emergency_n, ASA_no, ASA_mild, ASA_severe, ASA_life, 
                      ASA_moribund, ASA_none)

# Setup AUC table
aucs <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("rf", "ff", "logit", "NSQIPlogit")
colnames(aucs) <- x

# Creates ROC curves and AUC values for each model for each outcome
# "y_cardiac", "y_renal", "y_dead", not enough cases for model
outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_SSI", "y_uti", "y_thromb", "y_readmit", "y_reop", "y_discharge_care", "y_sepsis")
# , "Cardiac Complication", "Renal Failure", "Death"
outcome_labels <- c("Serious Complication", "Any Complication", "Pneumonia", "Surgical Site Infection", "Urinary Tract Infection", "Venous Thromboembolism", "Readmission", "Return to OR", "Discharge to Nursing or Rehab Facility", "Sepsis")

for (i in 1:length(outcome_names)){
  outcome = outcome_names[i]
  outcome_label = outcome_labels[i]
  # Selects plastic surgery data for specified outcome
  test <- mutate(pred_puf17, y_var = as.factor(grouped_outcomes_puf17[[outcome]]))
  levels(test$y_var) <- c("no_outcome", "outcome")
  plastic_test = filter(test, CPT_plastic == 1)
  test_2 <- mutate(NSQIP_frame, y_var = as.factor(grouped_outcomes_puf17[[outcome]]))
  levels(test_2$y_var) <- c("no_outcome", "outcome")
  plastic_test_2 = filter(test_2, CPT_plastic == 1)

  
  load(outcome, "_rf.Rda")
  rf_predict <- predict(plastic.rf, newdata = select(plastic_test, -y_var), type = "prob")
  rf_rocr <- prediction(rf_predict[, 2], plastic_test$y_var)
  rf_auc <- performance(rf_rocr, "auc")
  rf_perf <- performance(rf_rocr, "tpr", "fpr")
  
  load( outcome, "_ff.Rda")
  ff_predict <- predict(plastic.ff$final_rf, newdata = select(plastic_test, -y_var), type = "prob")
  ff_rocr <- prediction(list(ff_predict[, 2]), plastic_test$y_var)
  ff_auc <- performance(ff_rocr, "auc")
  ff_perf <- performance(ff_rocr, "tpr", "fpr")
  
  load( outcome, "_logit.Rda")
  logit_predict <- predict(plastic.logit, newdata = select(plastic_test, -y_var), type = "prob")
  logit_rocr <- prediction(logit_predict[, 2], plastic_test$y_var)
  logit_auc <- performance(logit_rocr, "auc")
  logit_perf <- performance(logit_rocr, "tpr", "fpr")
  
  load( outcome, "_NSQIPlogit.Rda")
  NSQIPlogit_predict <- predict(plastic.NSQIPlogit, newdata = select(plastic_test_2, -y_var), type = "prob")
  NSQIPlogit_rocr <- prediction(NSQIPlogit_predict[, 2], plastic_test_2$y_var)
  NSQIPlogit_auc <- performance(NSQIPlogit_rocr, "auc")
  NSQIPlogit_perf <- performance(NSQIPlogit_rocr, "tpr", "fpr")
  
  aucs <- add_row(aucs, "rf" = rf_auc@y.values[[1]], "ff" = ff_auc@y.values[[1]], "logit" = logit_auc@y.values[[1]], "NSQIPlogit" = NSQIPlogit_auc@y.values[[1]])

  pdf(paste0("./figures/", "ROC_", outcome, ".pdf"))
  plot(NSQIPlogit_perf, main = outcome_label, col="orange")
  plot(rf_perf, add = TRUE, col="red")
  plot(ff_perf, add = TRUE, col="blue")
  plot(logit_perf, add = TRUE, col="green")
  legend("bottomright", c("NSQIP GLM", "Random Forest", "Fuzzy Forest", "Fuzzy Forest GLM"), lty=1,
         col = c("orange", "red", "blue", "green"), bty="n", inset=c(0.15,0.15))
  dev.off()
  
  print(data.frame(summary(plastic.logit)$coefficients[,1]))
  print(plastic.ff$feature_list[, 1:2])
  
  pdf( outcome, "_features.pdf")
  if (nrow(data.frame(summary(plastic.logit)$coefficients[,1])) != nrow(plastic.ff$feature_list[, 1:2])) {
    X <- data.frame(summary(plastic.logit)$coefficients[-1,1], plastic.ff$feature_list)
  } else {
    X <- data.frame(summary(plastic.logit)$coefficients[-1,1], filter(plastic.ff$feature_list, feature_name %in% row.names(summary(plastic.logit)$coefficients))[, 2])
  }
  colnames(X) <- c("GLM Coefficient", "FF Variable Importance")
  grid.table(X)
  dev.off()
}

# Saves table to latex
row.names(aucs) <- outcome_labels
print(xtable(aucs, caption = "AUC by Model and Outcome for Plastic Surgery", type = "latex"), file =  "plastics_AUC.tex")
