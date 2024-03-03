# This script creates a table of the number of a surgery type for the NSQIP 2016, 2017, and 2018 datasets
# data_processing_puf16.R, data_processing_puf17.R, and data_processing_puf18.R must be run prior
# to running the script for the pred_puf16, pred_puf17, and pred_puf18 objects.
# This file generates latex code for a table of the number of a surgery type: surgery.tex
# Kyle McGraw, July 2019

library(dplyr)
library(tibble)
library(xtable)
library(descr)

### Import Data ###
load("pred_puf16.Rda")
load("pred_puf17.Rda")
load("pred_puf18.Rda")


#### Surgery by Year ####
CPT_Codes_2016 <- select(pred_puf16, cpt)
CPT_Codes_2017 <- select(pred_puf17, cpt)
CPT_Codes_2018 <- select(pred_puf18, cpt)

table = data.frame(sort(table(CPT_Codes_2016), decreasing = TRUE)[1:20], sort(table(CPT_Codes_2017), decreasing = TRUE)[1:20], sort(table(CPT_Codes_2018), decreasing = TRUE)[1:20])

# Export latex code to file
print(xtable(table, caption = "Most Common CPTs by Year", type = "latex"), file = "cpts.tex")


#### Plastic Surgery by Year ####

# Creates an empty data frame with years as columns
surg <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("2016", "2017", "2018")
colnames(surg) <- x

# Adds a category to look for overlap in surgery categories
surg_pred16 <- mutate(pred_puf16,
                     plastics_both = if_else(CPT_plastic == 1 & surgery_plastic == 1, 1, 0, missing=0),
)
surg_pred17 <- mutate(pred_puf17,
                     plastics_both = if_else(CPT_plastic == 1 & surgery_plastic == 1, 1, 0, missing=0),
)
surg_pred18 <- mutate(pred_puf18,
                     plastics_both = if_else(CPT_plastic == 1 & surgery_plastic == 1, 1, 0, missing=0),
)

# Generates total observations for each year for percentages
total16 <- nrow(surg_pred16)
total17 <- nrow(surg_pred17)
total18 <- nrow(surg_pred18)

# List of variable names to be used and labels for each variable in the table
surg_names <- c("CPT_plastic", "surgery_plastic", "plastics_both")
surg_labels <- c("CPT Plastic", "Surgery Specialty Plastic", "Both CPT and Surgery Specialty Plastic")

# Adds each variable as a number and as a percentage
for (i in surg_names){
  tble16 <- table(surg_pred16[[i]])
  tble17 <- table(surg_pred17[[i]])
  tble18 <- table(surg_pred18[[i]])
  surg <- add_row(surg, "2016" = tble16[2], "2017" = tble17[2], "2018" = tble18[2])
  # surg <- add_row(surg, "2016" = tble16[2]/total16*100, "2017" = tble17[2]/total17*100, "2018" = tble18[2]/total18*100)
}

# Adds row names
row.names(surg) <- surg_labels

# Export latex code to file
print(xtable(surg, caption = "Plastic Surgery", type = "latex"), file = "surgery.tex")

# test out cross tab
print(xtable(CrossTable(pred_puf16[["CPT_plastic"]], pred_puf16[["surgery_plastic"]]), caption = "2016 Cross Table", type = "latex"), file ="surgery16.tex")
print(xtable(CrossTable(pred_puf16[["CPT_plastic"]], pred_puf16[["surgery_plastic"]]), caption = "2017 Cross Table", type = "latex"), file =  "surgery17.tex")
print(xtable(CrossTable(pred_puf16[["CPT_plastic"]], pred_puf16[["surgery_plastic"]]), caption = "2018 Cross Table", type = "latex"), file = "surgery18.tex")

pre16 <- read.csv("acs_nsqip_puf16.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)

surgeryplastic16_yescpts <- filter(pred_puf16, surgery_plastic == 1)
print(xtable(t(data.frame(sort(unique(surgeryplastic16_yescpts[["cpt"]])))), caption = "CPTs in Surgery Specialty Plastic", type = "latex"), file =  "specialtyplastic.tex")


surgeryplastic16_nocpts <- filter(pred_puf16, surgery_plastic == 1, CPT_plastic == 0)
print(xtable(t(data.frame(sort(unique(surgeryplastic16_nocpts[["cpt"]])))), caption = "CPTs in Surgery Specialty Plastic (without given CPTs)", type = "latex"), file =  "specialtyplasticnocpts.tex")

plastic16 <- filter(pre16, CPT == 19318 | CPT == 19324 | CPT == 19325 |
                      CPT == 19340 | CPT == 19342 | CPT == 19357 |
                      CPT == 19361 | CPT == 19364 | CPT == 19366 |
                      CPT == 19367 | CPT == 19368 | CPT == 19369 |
                      CPT == 19370 | CPT == 19371 | CPT == 19380)

print(xtable(table(plastic16[["SURGSPEC"]]), caption = "Surgery Specialties of Plastic Surgery CPTs", type = "latex"), file = "CPTplastic.tex")

