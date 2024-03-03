# This script runs simple models on the 2018 dataset
# data_processing_puf16.R, data_processing_puf17.R, data_processing_puf18.R, outcome_grouping_puf16.R,
# outcome_grouping_puf17.R, and outcome_grouping_puf18.R must all be run prior to running the script for the 
# pred_puf16, pred_puf17, pred_puf18, grouped_outcomes_puf16, grouped_outcomes_puf17, and grouped_outcomes_puf18 objects.
# This script prints out summaries of the models created
# Kyle McGraw, July 2019


### Import Data ###
load("pred_puf18.Rda")
load("grouped_outcomes_puf18.Rda")


#### Logit Models ####

# Gender, age, and race as predictors for death or any outcome

# Import predictors and outcome
male <- pred_puf18$male
age <- pred_puf18$patient_age
white <- pred_puf18$race_white
asian <- pred_puf18$race_asian
black <- pred_puf18$race_black
nativeam <- pred_puf18$race_nativeam
aip <- pred_puf18$race_aip
dead <- grouped_outcomes_puf18$y_dead
any <- grouped_outcomes_puf18$y_any

# Combine into data frame and run model for death
y_dead <- data.frame(male, age, white, asian, black, nativeam, aip, dead)
is_dead <- glm(dead ~ male + age + asian + white + black + nativeam + aip, data = y_dead, family = "binomial")
summary(is_dead)

# Combine into data frame and run model for any outcome
y_any <- data.frame(male, age, white, asian, black, nativeam, aip, any)
any_outcome <- glm(any ~ male + age + asian + white + black + nativeam + aip, data = y_any, family = "binomial")
summary(any_outcome)

