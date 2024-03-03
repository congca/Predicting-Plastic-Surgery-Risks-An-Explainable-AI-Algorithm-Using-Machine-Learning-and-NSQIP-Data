# This script creates a table of outcomes for the NSQIP 2016, 2017, and 2018 datasets
# outcome_grouping_puf16.R, outcome_grouping_puf17.R, and outcome_grouping_puf18.R must be run prior
# to running the script for the grouped_outcomes_puf16, grouped_outcomes_puf17, and grouped_outcomes_puf18 objects.
# This file generates latex code for a table of outcomes: outcomes.tex
# Kyle McGraw, July 2019

library(dplyr)
library(xtable)
library(tibble)

### Import Data ###
load("grouped_outcomes_puf16.Rda")
load("grouped_outcomes_puf17.Rda")
load("grouped_outcomes_puf18.Rda")


#### Outcomes by Year ####

# Creates an empty data frame with years as columns
outcomes <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(outcomes) <- x

# List of variable names to be used and labels for each variable in the table
outcome_names <- c("y_discharge", "y_dead", "y_reop", "y_readmit", "y_sup_ssi", "y_deep_ssi", "y_organ_ssi", "y_wound_disruption", "y_pneumonia", "y_unplanned_intubation", "y_emb", "y_vent", "y_PRF", "y_ARF", "y_uti", "y_stroke", "y_cpr", "y_mi", "y_trans", "y_thromb", "y_sepsis", "y_sepshock", "y_cdiff")
outcome_labels <- c("Discharged", "Dead", "Reoperation", "Readmission", "Superficial Incisional SS", "Deep Incisional SSI", "Organ SSI", "Wound Disruption", "Pneumonia", "Unplanned Intubation", "Pulmonary Embolism", "Ventilator", "Progressive Renal Failure", "Acute Renal Failure", "Urinary Tract Infection", "Stroke", "Cardiac Arrest Requiring CPR", "Myocardial Infarction", "Bleeding Transfusions", "DVT/Thrombophlebitis", "Sepsis", "Septic Shock", "Clostridium Difficile Colitis")

# Adds each variable to the table
for (i in outcome_names){
  tble16 <- table(grouped_outcomes_puf16[[i]])
  tble17 <- table(grouped_outcomes_puf17[[i]])
  tble18 <- table(grouped_outcomes_puf18[[i]])
  outcomes <- add_row(outcomes, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
}

# Adds row names
row.names(outcomes) <- outcome_labels

# Export latex code to file
print(xtable(outcomes, caption = "Outcomes by Year", type = "latex"), file =  "outcomes.tex")


### Import Data ###
load("pred_puf16.Rda")
load("pred_puf17.Rda")
load("pred_puf18.Rda")


#### Outcomes by Year for Plastic Surgery ####

total16 <- mutate(grouped_outcomes_puf16, CPT_plastic = pred_puf16[["CPT_plastic"]])
total17 <- mutate(grouped_outcomes_puf17, CPT_plastic = pred_puf17[["CPT_plastic"]])
total18 <- mutate(grouped_outcomes_puf18, CPT_plastic = pred_puf18[["CPT_plastic"]])
plastics16 <- filter(total16, CPT_plastic == 1)
plastics17 <- filter(total17, CPT_plastic == 1)
plastics18 <- filter(total18, CPT_plastic == 1)

# Creates an empty data frame with years as columns
plastic <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(plastic) <- x


outcome_names <- c("y_serious", "y_any", "y_pneumonia", "y_SSI", "y_uti", "y_thromb", "y_readmit", "y_reop", "y_discharge_care", "y_sepsis", "y_cardiac", "y_renal", "y_dead")
for (i in outcome_names){
  tble16 <- table(plastics16[[i]])
  tble17 <- table(plastics17[[i]])
  tble18 <- table(plastics18[[i]])
  plastic <- add_row(plastic, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
}

# Adds row names
outcome_labels <- c("Serious Complication", "Any Complication", "Pneumonia", "Surgical Site Infection", "Urinary Tract Infection", "Venous Thromboembolism", "Readmission", "Return to OR", "Discharge to Nursing or Rehab Facility", "Sepsis", "Cardiac Complication", "Renal Failure", "Death")
row.names(plastic) <- outcome_labels

# Export latex code to file
print(xtable(plastic, caption = "Outcomes by Year for Plastic Surgery", type = "latex"), file =  "plastics_outcomes.tex")
