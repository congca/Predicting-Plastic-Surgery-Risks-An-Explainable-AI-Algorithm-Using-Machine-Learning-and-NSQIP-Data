# This script runs some tests on the data from the NSQIP 2018 dataset
# data_processing_puf18.R must be run prior to running the script for the pred_puf18 and outcomes_puf18 objects.
# This file generates latex code for tables of variables pre and post processing:
# postPredictors18.tex, postOutcomes18.tex, pre18.tex
# This file generates two data tables for zero control tests:
# pred_sum_testing_puf18, outcome_sum_testing_puf18


library(dplyr)
library(xtable)

### Import Data ###
load("pred_puf18.Rda")
load("outcomes_puf18.Rda")
pre18 <- read.csv("acs_nsqip_puf18.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)


#### Frequency Distributions ####
# Generated latex tables to check that frequencies are similar before and after recoding
# Only uses categories that have less than 15 options to only include multiple choice categories

# Creates new empty file for all three sets of tables
file.create( "postPredictors18.tex")
file.create("postOutcomes18.tex")
file.create( "pre18.tex")

# Recoded predictors
# Appends each variable to file as an additional table
for (i in colnames(pred_puf18)){
  temp <- table(pred_puf18[[i]])
  if (nrow(temp) < 15) {
    # Get latex code of table without printing to console
    temp_table <- capture.output(print(xtable(temp, caption = i, type = "latex")))
    
    # Edits the latex code to import nicely into latex
    temp_table <- gsub("\\\\begin\\{table\\}\\[ht\\]", "\\\\bigskip\\\\bigskip", temp_table)
    temp_table <- gsub("\\\\end\\{table\\}", "", temp_table)
    temp_table <- gsub("\\\\caption\\{", "\\\\captionof\\{table\\}\\{", temp_table)
    temp_table <- gsub("_", "-", temp_table)
    
    # Append latex code to file
    write(temp_table, file =  "postPredictors18.tex", append = TRUE)
  }
}

# Recoded outcomes
# Appends each variable to file as an additional table
for (i in colnames(outcomes_puf18)){
  temp <- table(outcomes_puf18[[i]])
  if (nrow(temp) < 15) {
    # Get latex code of table without printing to console
    temp_table <- capture.output(print(xtable(temp, caption = i, type = "latex")))
    
    # Edits the latex code to import nicely into latex
    temp_table <- gsub("\\\\begin\\{table\\}\\[ht\\]", "\\\\bigskip\\\\bigskip", temp_table)
    temp_table <- gsub("\\\\end\\{table\\}", "", temp_table)
    temp_table <- gsub("\\\\caption\\{", "\\\\captionof\\{table\\}\\{", temp_table)
    temp_table <- gsub("_", "-", temp_table)
    
    # Append latex code to file
    write(temp_table, file = "postOutcomes18.tex", append = TRUE)
  }
}

# Predictors and outcomes before recoding
# Appends each variable to file as an additional table
for (i in colnames(pre18)){
  temp <- table(pre18[[i]])
  if (nrow(temp) < 15) {
    # Get latex code of table without printing to console
    temp_table <- capture.output(print(xtable(temp, caption = i, type = "latex")))
    
    # Edits the latex code to import nicely into latex
    temp_table <- gsub("\\\\begin\\{table\\}\\[ht\\]", "\\\\bigskip\\\\bigskip", temp_table)
    temp_table <- gsub("\\\\end\\{table\\}", "", temp_table)
    temp_table <- gsub("\\\\caption\\{", "\\\\captionof\\{table\\}\\{", temp_table)
    temp_table <- gsub("_", "-", temp_table)
    
    # Append latex code to file
    write(temp_table, file =  "pre18.tex", append = TRUE)
  }
}


#### Zero Control Tests ####
# testing that each yes/no answer or category sum to 1

# Recodes each predictor category as sum of different options
pred_sum_testing_puf18 <- transmute(pred_puf18,
                  
                  # Sex
                  sex = female + male,
                  
                  # Race
                  race = race_asian + race_black + race_nativeam + race_aip + 
                    race_white + race_unknown,
                  
                  # Ethnicity Hispanic
                  hispanic = hispanic_y + hispanic_n + hispanic_u,
                  
                  # In/Out-Patient Status
                  status = inpatient + outpatient,
                  
                  # Transfer Status
                  transfer = trans_acute + trans_not + trans_emerg + trans_nurse
                  + trans_other + trans_unknown + trans_acute2 + trans_chronic + trans_chronic_inter,
                  
                  # Principal Anesthesia Technique
                  anesth = anesth_epi + anesth_gen + anesth_local + anesth_iv + anesth_none + anesth_other + anesth_reg + anesth_spine + anesth_unk + anesth_monitor,
                  
                  # Additional Anesthesia Technique(s)
                  anesth_add = anesth_other_gen + anesth_other_epi + anesth_other_spine + anesth_other_reg + anesth_other_local + anesth_other_monitor_iv + anesth_other_other + anesth_other_none + anesth_other_multiple,
                  
                  # Elective Surgery
                  elective = elective_y + elective_n + elective_u,
                  
                  # Diabetes
                  diabetes = diabetes_no + diabetes_insulin + diabetes_noninsulin,
                  
                  # Current Smoker
                  smoke = smoke_yes + smoke_no,
                  
                  # Dyspnea
                  dyspnea = dyspnea_rest + dyspnea_moderate + dyspnea_no,
                  
                  # Functional health status Prior to Surgery
                  functional_hs = functional_hs_independent + functional_hs_partially + functional_hs_dependent + functional_hs_unknown,
                  
                  # Ventilator dependent
                  ventilator = ventilator_dependent + ventilator_independent,
                  
                  # History of severe COPD
                  COPD = history_COPD + history_noCOPD,
                  
                  # Ascites
                  ascites = ascites_y + ascites_n,
                  
                  # Congestive heart failure (CHF) in 30 days before surgery
                  CHF = CHF_y + CHF_n,
                  
                  # Hypertension requiring medication
                  Hyper_med = Hyper_med_y + Hyper_med_n,
                  
                  # Acute renal failure (pre-op)
                  Renal_fail = Renal_fail_y + Renal_fail_n,
                  
                  # Currently on dialysis (pre-op
                  Dialysis = Dialysis_y + Dialysis_n,
                  
                  # Disseminated cance
                  Diss_cancer = Diss_cancer_y + Diss_cancer_n,
                  
                  # Open wound/wound infection
                  Open_wound = Open_wound_y + Open_wound_n,
                  
                  # Steroid use for chronic condition
                  Chronic_steroid = Chronic_steroid_y + Chronic_steroid_n,
                  
                  # >10% loss body weight in last 6 months
                  Weight_loss = Weight_loss_y + Weight_loss_n,
                  
                  # Bleeding disorders
                  Bleeding_dis = Bleeding_dis_y + Bleeding_dis_n,
                  
                  # Preop Transfusion of >= 1 unit of whole/packed RBCs in 72 hours prior to surgery
                  Preop_transfusions = Preop_transfusions_y + Preop_transfusions_n,
                  
                  # Systemic Sepsis within 48 Hours Prior to Surgery
                  Sepsis = Sepsis_none + Sepsis_sepsis + Sepsis_shock + Sepsis_sirs,
                  
                  # Emergency case
                  Emergency = Emergency_y + Emergency_n,
                  
                  # Wound classification
                  Wound = Wound_clean + Wound_clcontn + Wound_contn + Wound_dirty,
                  
                  # ASA classification
                  ASA = ASA_no + ASA_mild + ASA_severe + ASA_life + ASA_moribund + ASA_none,
)

# Recodes each outcome category as sum of different options
outcome_sum_testing_puf18 <- transmute(outcomes_puf18,

                      # Discharge Destination
                      discharge = discharge_unknown + discharge_skilled + discharge_unskilled + discharge_facility + discharge_home + discharge_acute + discharge_rehab + discharge_expired + discharge_hospice + discharge_multi + discharge_ama,
                      
                      # Superficial Incisional SS
                      sup_ssi = sup_ssi_y + sup_ssi_n,
                      sup_ssi_patos = sup_ssi_patos_y + sup_ssi_patos_n,
                      
                      # Deep Incisional SSI
                      deep_ssi = deep_ssi_y + deep_ssi_n,
                      deep_ssi_patos = deep_ssi_patos_y + deep_ssi_patos_n,
                      
                      # Organ/Space SSI
                      organ_ssi = organ_ssi_y + organ_ssi_n,
                      organ_ssi_patos = organ_ssi_patos_y + organ_ssi_patos_n,
                      
                      # Wound Disruption
                      wound_disruption = wound_disruption_y + wound_disruption_n,
                      
                      # Pneumonia
                      pneumonia = pneumonia_y + pneumonia_n,
                      pneumonia_patos = pneumonia_patos_y + pneumonia_patos_n,
                      
                      # Unplanned Intubation
                      unplanned_intubation = unplanned_intubation_y + unplanned_intubation_n,
                      
                      # Pulmonary Embolism
                      emb = emb_y + emb_n,
                      
                      # On Ventilator > 48 Hours
                      vent = vent_y + vent_n,
                      vent_patos = vent_patos_y + vent_patos_n,
                      
                      # Progressive Renal Insufficiency
                      PRF = PRF_y + PRF_n,
                      
                      # Acute Renal Failure
                      ARF = ARF_y + ARF_n,
                      
                      # Urinary Tract Infection
                      uti = uti_y + uti_n,
                      uti_patos = uti_patos_y + uti_patos_n,
                      
                      # Stroke/Cerebral Vascular Accident
                      cva = cva_neuro_def_y + cva_neuro_def_n,
                      
                      # Cardiac Arrest Requiring CPR
                      cpr = cpr_y + cpr_n,
                      
                      # Myocardial Infarction
                      mi = mi_y + mi_n,
                      
                      # Bleeding Transfusions
                      trans = trans_y + trans_n,
                      
                      # DVT/Thrombophlebitis
                      thromb = thromb_y  + thromb_n,
                      
                      # Sepsis
                      sespis = sepsis_y + sepsis_n,
                      sepsis_patos = sepsis_patos_y + sepsis_patos_n,
                      
                      # Septic Shock
                      sepshock = sepshock_y + sepshock_n,
                      sepshock_patos = sepshock_patos_y + sepshock_patos_n,
                      
                      # Unplanned Reoperation
                      return_OR = return_OR_y + return_OR_n,
                      
                      # Still in Hospital > 30 Days 
                      still_in_hosp = still_in_hosp_y + still_in_hosp_n,
                      
                      # Unplanned Reoperations
                      reop1 = reop1_y + reop1_n,
                      related_reop1 = related_reop1_y + related_reop1_n,
                      
                      reop2 = reop2_y + reop2_n,
                      related_reop2 = related_reop2_y + related_reop2_n,
                      
                      reop3plus = reop3plus_y + reop3plus_n,
                      
                      # Hospital Readmission
                      readmit1 = readmit1_y + readmit1_n,
                      unplan_readmit1 = unplan_readmit1_y + unplan_readmit1_n,
                      unplan_readmit1_related = unplan_readmit1_related_y + unplan_readmit1_related_n,
                      
                      readmit2 = readmit2_y + readmit2_n,
                      unplan_readmit2 = unplan_readmit2_y + unplan_readmit2_n,
                      unplan_readmit2_related = unplan_readmit2_related_y + unplan_readmit2_related_n,
                      
                      readmit3 = readmit3_y + readmit3_n,
                      unplan_readmit3 = unplan_readmit3_y + unplan_readmit3_n,
                      unplan_readmit3_related = unplan_readmit3_related_y + unplan_readmit3_related_n,
                      
                      readmit4 = readmit4_y + readmit4_n,
                      unplan_readmit4 = unplan_readmit4_y + unplan_readmit4_n,
                      unplan_readmit4_related = unplan_readmit4_related_y + unplan_readmit4_related_n,
                      
                      readmit5 = readmit5_y + readmit5_n,
                      unplan_readmit5 = unplan_readmit5_y + unplan_readmit5_n,
                      unplan_readmit5_related = unplan_readmit5_related_y + unplan_readmit5_related_n,
                      
                      # Surgical Wound Closure
                      wound = wound_fully_closed + wound_deep_closed + wound_not_closed,
                      
                      #  Clostridium Difficile (C.diff) Colitis
                      cdiff = cdiff_y + cdiff_n,
)


#### Export to CSV ####
save(pred_sum_testing_puf18, file =  "pred_sum_testing_puf18.Rda")
save(outcome_sum_testing_puf18, file =  "outcome_sum_testing_puf18.Rda")
