# This script groups outcomes from the NSQIP 2018 dataset
# data_processing_puf18.R must be run prior to running the script to generate the outcomes_puf18 object.
# The output of this file is grouped_outcomes_puf18 (a data table of grouped outcomes).
# Kyle McGraw, July 2019

library(dplyr)

# Import dataframe
load("outcomes_puf18.Rda")

# Groups outcomes
grouped_outcomes_puf18 <- transmute(outcomes_puf18,
                            
                            # # Discharge Destination
                            # y_discharge_home = discharge_home,
                            # 
                            # # Total operation time
                            # y_optime = optime,
                            # 
                            # # Length of total hospital stay 
                            # y_hosp_stay = total_hosp_stay,
                            
                            # Still in Hospital 
                            y_discharge = if_else(still_in_hosp_y == 0 | days_discharge >= 0 | dicharge_year > 0, 1, 0),
                            
                            # Dead
                            y_dead = if_else(death_year > 0 | days_death >= 0, 1, 0),
                            
                            # Unplanned Reoperation
                            y_reop = if_else(return_OR_y == 1 | reop1_y == 1 | days_reop1 >= 0 | reop2_y == 1 | days_reop2 >= 0 | reop3plus_y == 1, 1, 0),
                            # y_related_reop = if_else(related_reop1_y == 1 | related_reop2_y == 1, 1, 0),
                            
                            # Hospital Readmission
                            y_readmit = if_else(readmit1_y == 1 | unplan_readmit1_y == 1 | days_readmit1 >= 0 | readmit2_y == 1 | unplan_readmit2_y == 1 | 
                                                  days_readmit2 >= 0 | readmit3_y == 1 | unplan_readmit3_y == 1 | days_readmit3 >= 0 | readmit4_y == 1 | 
                                                  unplan_readmit4_y == 1 | days_readmit4 >= 0 | readmit5_y == 1 | unplan_readmit5_y == 1 | 
                                                  days_readmit5 >= 0, 1, 0),
                            # y_related_readmit = if_else(unplan_readmit1_related_y == 1 | unplan_readmit2_related_y == 1 | unplan_readmit3_related_y == 1 | 
                            #                               unplan_readmit4_related_y == 1 | unplan_readmit5_related_y == 1, 1, 0),
                            
                            # Superficial Incisional SSI
                            y_sup_ssi = if_else(sup_ssi_y == 1 | sup_ssi_patos_y == 1 | num_sup_ssi >= 1 | days_sup_ssi >= 0, 1, 0),
                            
                            # Deep Incisional SSI
                            y_deep_ssi = if_else(deep_ssi_y == 1 | deep_ssi_patos_y == 1 | num_deep_ssi >= 1 | days_deep_ssi >= 0, 1, 0),
                            
                            # Organ/Space SSI
                            y_organ_ssi = if_else(organ_ssi_y == 1 | organ_ssi_patos_y == 1 | num_organ_ssi >= 1 | days_organ_ssi >= 0, 1, 0),
                            
                            # Wound Disruption
                            y_wound_disruption = if_else(wound_disruption_y == 1 | num_wound_disruption >= 1 | days_wound_disruption >= 0, 1, 0),
                            
                            # Pneumonia
                            y_pneumonia = if_else(pneumonia_y == 1 | pneumonia_patos_y == 1 | num_pneumonia >= 1 | days_pneumonia >= 0, 1, 0),
                            
                            # Unplanned Intubation
                            y_unplanned_intubation = if_else(unplanned_intubation_y == 1 | num_unplanned_intubation >= 1 | days_unplanned_intubation >= 0, 1, 0),
                            
                            # Pulmonary Embolism
                            y_emb = if_else(emb_y == 1 | num_emb >= 1 | days_emb >= 0, 1, 0),
                            
                            # On Ventilator > 48 Hours
                            y_vent = if_else(vent_y == 1 | vent_patos_y == 1 | num_vent >= 1 | days_vent >= 0, 1, 0),
                            
                            # Progressive Renal Insufficiency
                            y_PRF = if_else(PRF_y == 1 | num_PRF >= 1 | days_PRF >= 0, 1, 0),
                            
                            # Acute Renal Failure
                            y_ARF = if_else(ARF_y == 1 | num_ARF >= 1 | days_ARF >= 0, 1, 0),
                            
                            # Urinary Tract Infection
                            y_uti = if_else(uti_y == 1 | uti_patos_y == 1 | num_uti >= 1 | days_uti >= 0, 1, 0),
                            
                            # Stroke/Cerebral Vascular Accident
                            y_stroke = if_else(cva_neuro_def_y == 1 | num_stroke >= 1 | days_stroke >= 0, 1, 0),
                            
                            # Cardiac Arrest Requiring CPR
                            y_cpr = if_else(cpr_y == 1 | num_cpr >= 1 | days_cpr >= 0, 1, 0),
                            
                            # Myocardial Infarction
                            y_mi = if_else(mi_y == 1 | num_mi >= 1 | days_mi >= 0, 1, 0),
                            
                            # Bleeding Transfusions
                            y_trans = if_else(trans_y == 1 | num_trans >= 1 | days_trans >= 0, 1, 0),
                            
                            # DVT/Thrombophlebitis
                            y_thromb = if_else(thromb_y == 1 | num_thromb >= 1 | days_thromb >= 0, 1, 0),
                            
                            # Sepsis
                            y_sepsis = if_else(sepsis_y == 1 | sepsis_patos_y == 1 | num_sepsis >= 1 | days_sepsis >= 0, 1, 0),
                            
                            # Septic Shock
                            y_sepshock = if_else(sepshock_y == 1 | sepshock_patos_y == 1 | num_sepshock >= 1 | days_sepshock >= 0, 1, 0),
                            
                            #  Clostridium Difficile (C.diff) Colitis
                            y_cdiff = if_else(cdiff_y == 1 | num_cdiff >= 1 | days_cdiff >= 0, 1, 0),
                            
                            # Sum of Outcomes
                            y_all = y_sup_ssi + y_deep_ssi + y_organ_ssi + y_wound_disruption + y_pneumonia + 
                              y_unplanned_intubation + y_emb + y_thromb + y_vent + y_PRF + y_ARF + y_uti + y_stroke + 
                              y_cpr + y_mi + y_reop + y_sepsis,
                            
                            y_any = if_else(y_all > 0, 1, 0),
                            
                            y_serious_sum = y_cpr + y_mi + y_pneumonia + y_PRF + y_ARF + y_emb + y_thromb + y_reop + 
                              y_deep_ssi + y_organ_ssi + y_sepsis + y_unplanned_intubation + y_uti + y_wound_disruption,
                            
                            y_serious = if_else(y_serious_sum > 0, 1, 0),
                            
                            y_cardiac = if_else(y_cpr + y_mi > 0, 1, 0),
                            
                            y_SSI = if_else(y_sup_ssi + y_deep_ssi + y_organ_ssi > 0, 1, 0),
                            
                            y_renal = if_else(y_PRF + y_ARF > 0, 1, 0),
                            
                            y_discharge_care = if_else(discharge_skilled + discharge_unskilled + discharge_rehab + discharge_acute > 0, 1, 0),
)

# Export R Object
save(grouped_outcomes_puf18, file =  "grouped_outcomes_puf18.Rda")
