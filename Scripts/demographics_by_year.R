# This script creates a table of demographics for the NSQIP 2016, 2017, and 2018 datasets
# data_processing_puf16.R, data_processing_puf17.R, and data_processing_puf18.R must be run prior
# to running the script for the pred_puf16, pred_puf17, and pred_puf18 objects.
# This file generates latex code for a table of demographics: demographics.tex


library(dplyr)
library(tibble)
library(xtable)

### Import Data ###
load("pred_puf16.Rda")
load("pred_puf17.Rda")
load("pred_puf18.Rda")


#### Demographics by Year ####

# Creates an empty data frame with years as columns
demo <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(demo) <- x

# Recodes ages into categories to include in demographics
new_pred16 <- mutate(pred_puf16,
                     age_18_to_24 = if_else(patient_age >= 18 & patient_age <= 24, 1, 0, missing=0),
                     age_25_to_35 = if_else(patient_age >= 25 & patient_age <= 35, 1, 0, missing=0),
                     age_36_to_50 = if_else(patient_age >= 36 & patient_age <= 50, 1, 0, missing=0),
                     age_51_to_65 = if_else(patient_age >= 51 & patient_age <= 65, 1, 0, missing=0),
                     age_66_plus = if_else(patient_age >= 66, 1, 0, missing=0),
)
new_pred17 <- mutate(pred_puf17,
                     age_18_to_24 = if_else(patient_age >= 18 & patient_age <= 24, 1, 0, missing=0),
                     age_25_to_35 = if_else(patient_age >= 25 & patient_age <= 35, 1, 0, missing=0),
                     age_36_to_50 = if_else(patient_age >= 36 & patient_age <= 50, 1, 0, missing=0),
                     age_51_to_65 = if_else(patient_age >= 51 & patient_age <= 65, 1, 0, missing=0),
                     age_66_plus = if_else(patient_age >= 66, 1, 0, missing=0),
)
new_pred18 <- mutate(pred_puf18,
                     age_18_to_24 = if_else(patient_age >= 18 & patient_age <= 24, 1, 0, missing=0),
                     age_25_to_35 = if_else(patient_age >= 25 & patient_age <= 35, 1, 0, missing=0),
                     age_36_to_50 = if_else(patient_age >= 36 & patient_age <= 50, 1, 0, missing=0),
                     age_51_to_65 = if_else(patient_age >= 51 & patient_age <= 65, 1, 0, missing=0),
                     age_66_plus = if_else(patient_age >= 66, 1, 0, missing=0),
)

# Generates total observations for each year for percentages
total16 <- nrow(new_pred16)
total17 <- nrow(new_pred17)
total18 <- nrow(new_pred18)

# List of variable names to be used and labels for each variable in the table
demo_names <- c("male", "female", "age_18_to_24", "age_25_to_35", "age_36_to_50", "age_51_to_65", "age_66_plus", "race_asian", "race_black", "race_nativeam", "race_aip", "race_white", "race_unknown", "hispanic_y", "hispanic_n", "hispanic_u")
demo_labels <- c("Male", "% Male", "Female", "% Female", "Age 18-24", "% Age 18-24", "Age 25-35", "% Age 25-35", "Age 36-50", "% Age 36-50", "Age 51-65", "% Age 51-65", "Age 66+", "% Age 66+", "Race Asian", "% Race Asian", "Race Black", "% Race Black", "Race Native American", "% Race Native American", "Race Pacific Islander", "% Race Pacific Islander", "Race White", "% Race White", "Race Unknown", "% Race Unknown", "Hispanic", "% Hispanic", "Not Hispanic", "% Not Hispanic", "Hispanic Unknown", "% Hispanic Unknown")

# Adds each variable as a number and as a percentage
for (i in demo_names){
  tble16 <- table(new_pred16[[i]])
  tble17 <- table(new_pred17[[i]])
  tble18 <- table(new_pred18[[i]])
  demo <- add_row(demo, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
  demo <- add_row(demo, "2016" = tble16[2]/total16*100, "2017" = tble17[2]/total17*100, "2018" = tble18[2]/total18*100)
}

# Adds row names
row.names(demo) <- demo_labels

# Export latex code to file
print(xtable(demo, caption = "Demographics by Year", type = "latex"), file = "demographics.tex")


#### Demographics by Year for Plastic Surgery ####

# Creates an empty data frame with years as columns
plastic <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(plastic) <- x

plastics16 <- filter(new_pred16, CPT_plastic == 1)
plastics17 <- filter(new_pred17, CPT_plastic == 1)
plastics18 <- filter(new_pred18, CPT_plastic == 1)

for (i in demo_names){
  tble16 <- table(plastics16[[i]])
  tble17 <- table(plastics17[[i]])
  tble18 <- table(plastics18[[i]])
  plastic <- add_row(plastic, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
}

# Adds row names
plastic_labels <- c("Male", "Female", "Age 18-24", "Age 25-35", "Age 36-50", "Age 51-65", "Age 66+", "Race Asian", "Race Black", "Race Native American", "Race Pacific Islander", "Race White", "Race Unknown", "Hispanic", "Not Hispanic", "Hispanic Unknown")
row.names(plastic) <- plastic_labels

# Export latex code to file
print(xtable(plastic, caption = "Demographics by Year for Plastic Surgery", type = "latex"), file = "plastics_demographics.tex")
