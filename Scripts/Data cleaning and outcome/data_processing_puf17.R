# This script processes the data from the NSQIP 2017 dataset
# The NSQIP 2017 data set must first be acquired before running the script.
# The output of this file is pred_puf17 (a data table of predictors) and outcomes_puf17 (a data table of outcomes).


library(dplyr)

#### Import Data ####
datatrain_puf17 <- read.csv("acs_nsqip_puf17.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)


#### Pre-processing ####

# change missing values to NA
datatrain_puf17[datatrain_puf17 == ""] = NA
datatrain_puf17[datatrain_puf17 == "NULL"] = NA
datatrain_puf17[datatrain_puf17 == "-99"] = NA

# change answer of No to standard for processing
datatrain_puf17[datatrain_puf17 == "No"] = 0
datatrain_puf17[datatrain_puf17 == "NO"] = 0
datatrain_puf17[datatrain_puf17 == "No Complication"] = 0

# ages larger than 89 recoded as 90
datatrain_puf17[datatrain_puf17 == "90+"] = 90


#### Predictor Processing ####
# NAs are coded as -1
# for categorical predictors, NAs not put in a category

pred_puf17 <- transmute(datatrain_puf17,
                        
                        # Sex
                        female = if_else(SEX == "female", 1, 0, missing=0),
                        male = if_else(SEX == "male", 1, 0, missing=0),
                        
                        # Race
                        race_asian = if_else(RACE_NEW == "Asian", 1, 0,missing=0),
                        race_black = if_else(RACE_NEW == "Black or African American", 1, 0,missing=0),
                        race_nativeam = if_else(RACE_NEW == "American Indian or Alaska Native", 1, 0,missing=0),
                        race_aip = if_else(RACE_NEW == "Native Hawaiian or Pacific Islander", 1, 0,missing=0),
                        race_white = if_else(RACE_NEW == "White", 1, 0,missing=0),
                        race_unknown = if_else(RACE_NEW == "Unknown/Not Reported", 1, 0,missing=0),
                        
                        # Ethnicity Hispanic
                        hispanic_y = if_else(ETHNICITY_HISPANIC == "Yes", 1, 0,missing=0),
                        hispanic_n = if_else(ETHNICITY_HISPANIC == "0", 1, 0,missing=0),
                        hispanic_u = if_else(ETHNICITY_HISPANIC == "Unknown", 1, 0,missing=0),
                        
                        # CPT number
                        cpt = if_else(is.na(CPT), -1, as.numeric(CPT),missing=-1),
                        # With additional codes
                        CPT_plastic = (CPT == 19318 | CPT == 19324 | CPT == 19325 |
                                         CPT == 19340 | CPT == 19342 | CPT == 19357 |
                                         CPT == 19361 | CPT == 19364 | CPT == 19366 |
                                         CPT == 19367 | CPT == 19368 | CPT == 19369 |
                                         CPT == 19370 | CPT == 19371 | CPT == 19380 | 
                                         CPT == 19316 | CPT == 19300 | CPT == 19328 |
                                         CPT == 19350),
                        CPT_plastic_complex = (CPT == 19361 | CPT == 19364 | CPT == 19366 | 
                                                 CPT == 19367 | CPT == 19368 | CPT == 19369),
                        # First Run
                        # CPT_plastic = (CPT == 19318 | CPT == 19324 | CPT == 19325 |
                        #                  CPT == 19340 | CPT == 19342 | CPT == 19357 |
                        #                  CPT == 19361 | CPT == 19364 | CPT == 19366 |
                        #                  CPT == 19367 | CPT == 19368 | CPT == 19369 |
                        #                  CPT == 19370 | CPT == 19371 | CPT == 19380),
                        
                        # In/Out-Patient Status
                        inpatient = if_else(INOUT == "Inpatient", 1, 0,missing=0),
                        outpatient = if_else(INOUT == "Outpatient", 1, 0,missing=0),
                        
                        # Transfer Status
                        trans_acute = if_else(TRANST == "From acute care hospital inpatient", 1, 0,missing=0),
                        trans_not = if_else(TRANST == "Not transferred (admitted from home)", 1, 0,missing=0),
                        trans_emerg = if_else(TRANST == "Outside emergency department", 1, 0,missing=0),
                        trans_nurse = if_else(TRANST == "Nursing home - Chronic care - Intermediate care", 1, 0,missing=0),
                        trans_other = if_else(TRANST == "Transfer from other", 1, 0,missing=0),
                        trans_unknown = if_else(TRANST == "Unknown", 1, 0,missing=0),
                        trans_acute2 = if_else(TRANST == "Acute Care Hospital", 1, 0,missing=0),
                        trans_chronic = if_else(TRANST == "Chronic Care Facility", 1, 0,missing=0),
                        trans_chronic_inter = if_else(TRANST == "Chronic Care - Intermediate care", 1, 0,missing=0),
                        
                        # Age
                        patient_age = if_else(is.na(Age), -1, as.numeric(Age),missing=-1),
                        # Year of Admission
                        admit_year = if_else(is.na(AdmYR), -1, as.numeric(AdmYR),missing=-1),
                        # Year of Operation
                        operation_year = if_else(is.na(OperYR), -1, as.numeric(OperYR),missing=-1),
                        
                        # Principal Anesthesia Technique
                        anesth_epi = if_else(ANESTHES == "Epidural", 1, 0,missing=0),
                        anesth_gen = if_else(ANESTHES == "General", 1, 0,missing=0),
                        anesth_local = if_else(ANESTHES == "Local", 1, 0,missing=0),
                        anesth_iv = if_else(ANESTHES == "MAC/IV Sedation", 1, 0,missing=0),
                        anesth_none = if_else(ANESTHES == "None", 1, 0,missing=0),
                        anesth_other = if_else(ANESTHES == "Other", 1, 0,missing=0),
                        anesth_reg = if_else(ANESTHES == "Regional", 1, 0,missing=0),
                        anesth_spine = if_else(ANESTHES == "Spinal", 1, 0,missing=0),
                        anesth_unk = if_else(ANESTHES == "Unknown", 1, 0,missing=0),
                        anesth_monitor = if_else(ANESTHES == "Monitored Anesthesia Care", 1, 0,missing=0),
                        
                        # Additional Anesthesia Technique(s)
                        anesth_other_gen = if_else(ANESTHES_OTHER == "General", 1, 0,missing=0),
                        anesth_other_epi = if_else(ANESTHES_OTHER == "Epidural", 1, 0,missing=0),
                        anesth_other_spine = if_else(ANESTHES_OTHER == "Spinal", 1, 0,missing=0),
                        anesth_other_reg = if_else(ANESTHES_OTHER == "Regional", 1, 0,missing=0),
                        anesth_other_local = if_else(ANESTHES_OTHER == "Local", 1, 0,missing=0),
                        anesth_other_monitor_iv = if_else(ANESTHES_OTHER == "Monitored Anesthesia Care/IV Sedation", 1, 0,missing=0),
                        anesth_other_other = if_else(ANESTHES_OTHER == "Other", 1, 0,missing=0),
                        anesth_other_multiple = if_else(grepl(",", ANESTHES_OTHER, fixed = TRUE), 1, 0,missing=0),
                        anesth_other_none = if_else(is.na(ANESTHES_OTHER), 1, 0,missing=0),
                        
                        # Surgical Specialty (Plastic Surgery Only)
                        surgery_plastic = if_else(SURGSPEC == "Plastics", 1,0, missing=0),
                        
                        # Elective Surgery
                        elective_y = if_else(ELECTSURG == "Yes", 1, 0,missing=0),
                        elective_n = if_else(ELECTSURG == "0", 1, 0,missing=0),
                        elective_u = if_else(ELECTSURG == "Unknown", 1, 0,missing=0),
                        
                        patient_height = if_else(is.na(HEIGHT), -1, as.numeric(HEIGHT),missing=-1),
                        # Weight in lbs
                        patient_weight = if_else(is.na(WEIGHT), -1, as.numeric(WEIGHT),missing=-1),
                        # BMI calculated from height and weight
                        BMI = if_else(is.na(HEIGHT) | is.na(WEIGHT), -1, round(703*(as.numeric(WEIGHT))/(as.numeric(HEIGHT)^2)),missing=-1),
                        
                        # Diabetes
                        diabetes_no = if_else(DIABETES == "0", 1, 0,missing=0),
                        diabetes_insulin = if_else(DIABETES == "INSULIN", 1, 0,missing=0),
                        diabetes_noninsulin = if_else(DIABETES == "NON-INSULIN", 1, 0,missing=0),
                        
                        # Current Smoker
                        smoke_yes = if_else(SMOKE == "Yes", 1, 0,missing=0),
                        smoke_no = if_else(SMOKE == "0", 1, 0,missing=0),
                        
                        # Dyspnea
                        dyspnea_rest = if_else(DYSPNEA == "AT REST", 1, 0,missing=0),
                        dyspnea_moderate = if_else(DYSPNEA == "MODERATE EXERTION", 1, 0,missing=0),
                        dyspnea_no = if_else(DYSPNEA == "0", 1, 0,missing=0),
                        
                        # Functional health status Prior to Surgery
                        functional_hs_independent = if_else(FNSTATUS2 == "Independent", 1, 0,missing=0),
                        functional_hs_partially = if_else(FNSTATUS2 == "Partially Dependent", 1, 0,missing=0),
                        functional_hs_dependent = if_else(FNSTATUS2 == "Totally Dependent", 1, 0, missing=0),
                        functional_hs_unknown = if_else(FNSTATUS2 == "Unknown", 1, 0,missing=0),
                        
                        # Ventilator dependent
                        ventilator_dependent = if_else(VENTILAT == "Yes", 1, 0, missing=0),
                        ventilator_independent = if_else(VENTILAT == "0", 1, 0,missing=0),
                        
                        # History of severe COPD
                        history_COPD = if_else(HXCOPD =="Yes", 1, 0,missing=0),
                        history_noCOPD = if_else(HXCOPD == "0", 1, 0,missing=0),
                        
                        # Ascites
                        ascites_y = if_else(ASCITES == "Yes", 1, 0,missing=0),
                        ascites_n = if_else(ASCITES == "0", 1, 0,missing=0),
                        
                        # Congestive heart failure (CHF) in 30 days before surgery
                        CHF_y = if_else(HXCHF == "Yes", 1, 0,missing=0),
                        CHF_n = if_else(HXCHF == "0", 1, 0,missing=0),
                        
                        # Hypertension requiring medication
                        Hyper_med_y = if_else(HYPERMED == "Yes", 1, 0 ,missing=0),
                        Hyper_med_n = if_else(HYPERMED == "0", 1, 0, missing=0 ),
                        
                        # Acute renal failure (pre-op)
                        Renal_fail_y = if_else(RENAFAIL == "Yes", 1, 0 ,missing=0),
                        Renal_fail_n = if_else(RENAFAIL == "0", 1, 0 ,missing=0),
                        
                        # Currently on dialysis (pre-op
                        Dialysis_y = if_else(DIALYSIS == "Yes", 1, 0,missing=0),
                        Dialysis_n = if_else(DIALYSIS == "0", 1, 0,missing=0),
                        
                        # Disseminated cance
                        Diss_cancer_y = if_else(DISCANCR == "Yes", 1, 0,missing=0),
                        Diss_cancer_n = if_else(DISCANCR == "0", 1, 0,missing=0),
                        
                        # Open wound/wound infection
                        Open_wound_y = if_else(WNDINF == "Yes", 1, 0,missing=0),
                        Open_wound_n = if_else(WNDINF == "0", 1, 0,missing=0),
                        
                        # Steroid use for chronic condition
                        Chronic_steroid_y = if_else(STEROID == "Yes", 1, 0,missing=0),
                        Chronic_steroid_n = if_else(STEROID == "0", 1, 0,missing=0),
                        
                        # >10% loss body weight in last 6 months
                        Weight_loss_y = if_else(WTLOSS == "Yes", 1, 0,missing=0),
                        Weight_loss_n = if_else(WTLOSS == "0", 1, 0,missing=0),
                        
                        # Bleeding disorders
                        Bleeding_dis_y = if_else(BLEEDDIS == "Yes", 1, 0,missing=0),
                        Bleeding_dis_n = if_else(BLEEDDIS == "0", 1, 0,missing=0),
                        
                        # Preop Transfusion of >= 1 unit of whole/packed RBCs in 72 hours prior to surgery
                        Preop_transfusions_y = if_else(TRANSFUS == "Yes", 1, 0,missing=0),
                        Preop_transfusions_n = if_else(TRANSFUS == "0", 1, 0,missing=0),
                        
                        # Systemic Sepsis within 48 Hours Prior to Surgery
                        Sepsis_none = if_else(PRSEPIS == "None", 1, 0,missing=0),
                        Sepsis_sepsis = if_else(PRSEPIS == "Sepsis", 1, 0,missing=0),
                        Sepsis_shock = if_else(PRSEPIS == "Septic Shock", 1, 0,missing=0),
                        Sepsis_sirs = if_else(PRSEPIS == "SIRS", 1, 0,missing=0),
                        
                        # Days from Preoperative Labs to Operation
                        days_na_preop_lab = if_else(is.na(DPRNA), -1, as.numeric(DPRNA)),
                        days_bun_preop_lab = if_else(is.na(DPRBUN), -1, as.numeric(DPRBUN)),
                        days_creat_preop_lab = if_else(is.na(DPRCREAT), -1, as.numeric(DPRCREAT)),
                        days_album_preop_lab = if_else(is.na(DPRALBUM), -1, as.numeric(DPRALBUM)),
                        days_bili_preop_lab = if_else(is.na(DPRBILI), -1, as.numeric(DPRBILI)),
                        days_sgot_preop_lab = if_else(is.na(DPRSGOT), -1, as.numeric(DPRSGOT)),
                        days_alkph_preop_lab = if_else(is.na(DPRALKPH), -1, as.numeric(DPRALKPH)),
                        days_wbc_preop_lab = if_else(is.na(DPRWBC), -1, as.numeric(DPRWBC)),
                        days_hct_preop_lab = if_else(is.na(DPRHCT), -1, as.numeric(DPRHCT)),
                        days_plate_preop_lab = if_else(is.na(DPRPLATE), -1, as.numeric(DPRPLATE)),
                        days_ptt_preop_lab = if_else(is.na(DPRPTT), -1, as.numeric(DPRPTT)),
                        days_pt_preop_lab = if_else(is.na(DPRPT), -1, as.numeric(DPRPT)),
                        days_inr_preop_lab = if_else(is.na(DPRINR), -1, as.numeric(DPRINR)),
                        
                        # Preoperative Lab Value Information 
                        serum_sodium = if_else(is.na(PRSODM), -1, as.numeric(PRSODM)),
                        BUN = if_else(is.na(PRBUN), -1, as.numeric(PRBUN)),
                        serum_creatinine = if_else(is.na(PRCREAT), -1, as.numeric(PRCREAT)),
                        serum_albumin = if_else(is.na(PRALBUM), -1, as.numeric(PRALBUM)),
                        bilirubin = if_else(is.na(PRBILI), -1, as.numeric(PRBILI)),
                        SGOT = if_else(is.na(PRSGOT), -1, as.numeric(PRSGOT)),
                        alkaline_phos = if_else(is.na(PRALKPH), -1, as.numeric(PRALKPH)),
                        WBC = if_else(is.na(PRWBC), -1, as.numeric(PRWBC)),
                        hematocrit = if_else(is.na(PRHCT), -1, as.numeric(PRHCT)),
                        platlet_count = if_else(is.na(PRPLATE), -1, as.numeric(PRPLATE)),
                        PPT = if_else(is.na(PRPTT), -1, as.numeric(PRPTT)),
                        INR_of_PT = if_else(is.na(PRINR), -1, as.numeric(PRINR)),
                        PT = if_else(is.na(PRPT), -1, as.numeric(PRPT)),
                        
                        # Other Procedures (CPT)
                        other_proc1 = if_else(is.na(OTHERCPT1), 0, 1),
                        other_proc2 = if_else(is.na(OTHERCPT2), 0, 1),
                        other_proc3 = if_else(is.na(OTHERCPT3), 0, 1),
                        other_proc4 = if_else(is.na(OTHERCPT4), 0, 1),
                        other_proc5 = if_else(is.na(OTHERCPT5), 0, 1),
                        other_proc6 = if_else(is.na(OTHERCPT6), 0, 1),
                        other_proc7 = if_else(is.na(OTHERCPT7), 0, 1),
                        other_proc8 = if_else(is.na(OTHERCPT8), 0, 1),
                        other_proc9 = if_else(is.na(OTHERCPT9), 0, 1),
                        other_proc10 = if_else(is.na(OTHERCPT10), 0, 1),
                        other_proc_sum = other_proc1 + other_proc2 + other_proc3 + other_proc4 + other_proc5 + other_proc6 +
                          other_proc7 + other_proc8 + other_proc9 + other_proc10,
                        
                        # Concurrent Procedures (CPT)
                        concurr1 = if_else(is.na(CONCPT1), 0, 1),
                        concurr2 = if_else(is.na(CONCPT2), 0, 1),
                        concurr3 = if_else(is.na(CONCPT3), 0, 1),
                        concurr4 = if_else(is.na(CONCPT4), 0, 1),
                        concurr5 = if_else(is.na(CONCPT5), 0, 1),
                        concurr6 = if_else(is.na(CONCPT6), 0, 1),
                        concurr7 = if_else(is.na(CONCPT7), 0, 1),
                        concurr8 = if_else(is.na(CONCPT8), 0, 1),
                        concurr9 = if_else(is.na(CONCPT9), 0, 1),
                        concurr10 = if_else(is.na(CONCPT10), 0, 1),
                        concurr_sum = concurr1 + concurr2 + concurr3 + concurr4 + concurr5 + concurr6 +
                          concurr7 + concurr8 + concurr9 + concurr10,
                        
                        # Emergency case
                        Emergency_y = if_else(EMERGNCY == "Yes", 1, 0,missing=0),
                        Emergency_n = if_else(EMERGNCY == "0", 1, 0,missing=0),
                        
                        # Wound classification
                        Wound_clean = if_else(WNDCLAS == "1-Clean", 1, 0,missing=0),
                        Wound_clcontn = if_else(WNDCLAS == "2-Clean/Contaminated", 1, 0,missing=0),
                        Wound_contn = if_else(WNDCLAS == "3-Contaminated", 1, 0,missing=0),
                        Wound_dirty = if_else(WNDCLAS == "4-Dirty/Infected", 1, 0,missing=0),
                        
                        # ASA classification
                        ASA_no = if_else(ASACLAS == "1-No Disturb", 1, 0,missing=0),
                        ASA_mild = if_else(ASACLAS == "2-Mild Disturb", 1, 0,missing=0),
                        ASA_severe = if_else(ASACLAS == "3-Severe Disturb", 1, 0,missing=0),
                        ASA_life = if_else(ASACLAS == "4-Life Threat", 1, 0,missing=0),
                        ASA_moribund = if_else(ASACLAS == "5-Moribund", 1, 0,missing=0),
                        ASA_none = if_else(ASACLAS == "None assigned", 1, 0,missing=0),
                        
                        # Estimated Probability of Mortality
                        # mortality = if_else(is.na(MORTPROB), -1, as.numeric(MORTPROB),missing=-1),
                        # Estimated Probability of Morbidity
                        # morbidity = if_else(is.na(MORBPROB), -1, as.numeric(MORBPROB),missing=-1),
                        
                        #  Quarter of Admission
                        admit_quarter = if_else(is.na(AdmQtr), -1, as.numeric(AdmQtr),missing=-1),
                        # Days from Hospital Admission to Operation
                        days_to_opperation = if_else(is.na(HtoODay), -1, as.numeric(HtoODay),missing=-1),
                        
)


#### Outcome Processing ####
# NAs are coded as -1
# for categorical predictors, NAs not put in a category

outcomes_puf17 <- transmute(datatrain_puf17,
                           
                           # Discharge Destination
                           discharge_unknown = if_else(DISCHDEST == "Unknown", 1, 0,missing=0),
                           discharge_skilled = if_else(DISCHDEST == "Skilled Care, Not Home", 1, 0,missing=0),
                           discharge_unskilled = if_else(DISCHDEST == "Unskilled Facility Not Home", 1, 0,missing=0),
                           discharge_facility = if_else(DISCHDEST == "Facility Which was Home", 1, 0,missing=0),
                           discharge_home = if_else(DISCHDEST == "Home", 1, 0,missing=0),
                           discharge_acute = if_else(DISCHDEST == "Separate Acute Care", 1, 0,missing=0),
                           discharge_rehab = if_else(DISCHDEST == "Rehab", 1, 0,missing=0),
                           discharge_expired = if_else(DISCHDEST == "Expired", 1, 0,missing=0),
                           discharge_multi = if_else(DISCHDEST == "Multi - level Senior Community", 1, 0,missing=0),
                           discharge_hospice = if_else(DISCHDEST == "Hospice", 1, 0,missing=0),
                           
                           # Total operation time
                           optime = if_else(is.na(OPTIME), -1, as.numeric(OPTIME),missing=-1),
                           # Hospital discharge Year
                           dicharge_year = if_else(is.na(HDISDT), 0, as.numeric(HDISDT),missing=0),
                           # Year of death
                           death_year = if_else(is.na(YRDEATH), 0, as.numeric(YRDEATH),missing=0),
                           # Length of total hospital stay 
                           total_hosp_stay = if_else(is.na(TOTHLOS), -1, as.numeric(TOTHLOS),missing=-1),
                           
                           # Superficial Incisional SS
                           num_sup_ssi = if_else(is.na(NSUPINFEC), 0, as.numeric(NSUPINFEC),missing=0),
                           sup_ssi_y = if_else(SUPINFEC == "Superficial Incisional SSI", 1, 0,missing=0),
                           sup_ssi_n = if_else(SUPINFEC == "0", 1, 0,missing=0),
                           sup_ssi_patos_y = if_else(SSSIPATOS == "Yes", 1, 0,missing=0),
                           sup_ssi_patos_n = if_else(SSSIPATOS == "0" , 1, 0,missing=0),
                           days_sup_ssi = if_else(is.na(DSUPINFEC), -1, as.numeric(DSUPINFEC),missing=-1),
                           
                           # Deep Incisional SSI
                           num_deep_ssi = if_else(is.na(NWNDINFD), 0, as.numeric(NWNDINFD),missing=0),
                           deep_ssi_y = if_else(WNDINFD == "Deep Incisional SSI", 1, 0,missing=0),
                           deep_ssi_n = if_else(WNDINFD == "0", 1, 0,missing=0),
                           deep_ssi_patos_y = if_else(DSSIPATOS == "Yes", 1, 0,missing=0),
                           deep_ssi_patos_n = if_else(DSSIPATOS == "0", 1, 0,missing=0),
                           days_deep_ssi = if_else(is.na(DWNDINFD), -1, as.numeric(DWNDINFD),missing=-1),
                           
                           # Organ/Space SSI
                           num_organ_ssi = if_else(is.na(NORGSPCSSI), 0, as.numeric(NORGSPCSSI),missing=0),
                           organ_ssi_y = if_else(ORGSPCSSI == "Organ/Space SSI" , 1, 0,missing=0),
                           organ_ssi_n = if_else(ORGSPCSSI == "0"  , 1, 0,missing=0),
                           organ_ssi_patos_y = if_else(OSSIPATOS == "Yes", 1, 0,missing=0),
                           organ_ssi_patos_n = if_else(OSSIPATOS == "0" , 1, 0,missing=0),
                           days_organ_ssi = if_else(is.na(DORGSPCSSI), -1, as.numeric(DORGSPCSSI),missing=-1),
                           
                           # Wound Disruption
                           num_wound_disruption = if_else(is.na(NDEHIS), 0, as.numeric(NDEHIS),missing=0),
                           wound_disruption_y = if_else(DEHIS == "Wound Disruption", 1, 0,missing=0),
                           wound_disruption_n = if_else(DEHIS == "0", 1, 0,missing=0),
                           days_wound_disruption = if_else(is.na(DDEHIS), -1, as.numeric(DDEHIS),missing=-1),
                           
                           # Pneumonia
                           num_pneumonia = if_else(is.na(NOUPNEUMO), 0, as.numeric(NOUPNEUMO),missing=0),
                           pneumonia_y = if_else(OUPNEUMO == "Pneumonia", 1, 0),
                           pneumonia_n = if_else(OUPNEUMO == "0", 1, 0,missing=0),
                           pneumonia_patos_y = if_else(PNAPATOS == "Yes", 1, 0,missing=0),
                           pneumonia_patos_n = if_else(PNAPATOS == "0", 1, 0,missing=0),
                           days_pneumonia = if_else(is.na(DOUPNEUMO), -1, as.numeric(DOUPNEUMO),missing=-1),
                           
                           # Unplanned Intubation
                           num_unplanned_intubation = if_else(is.na(NREINTUB), 0, as.numeric(NREINTUB),missing=0),
                           unplanned_intubation_y = if_else(REINTUB == "Unplanned Intubation", 1, 0,missing=0),
                           unplanned_intubation_n = if_else(REINTUB == "0", 1, 0,missing=0),
                           days_unplanned_intubation = if_else(is.na(DREINTUB), -1, as.numeric(DREINTUB),missing=-1),
                           
                           # Pulmonary Embolism
                           num_emb = if_else(is.na(NPULEMBOL), 0, as.numeric(NPULEMBOL),missing=0),
                           emb_y = if_else(PULEMBOL == "Pulmonary Embolism" , 1, 0,missing=0),
                           emb_n = if_else(PULEMBOL == "0" , 1, 0,missing=0),
                           days_emb = if_else(is.na(DPULEMBOL), -1, as.numeric(DPULEMBOL),missing=-1),
                           
                           # On Ventilator > 48 Hours
                           num_vent = if_else(is.na(NFAILWEAN), 0, as.numeric(NFAILWEAN),missing=0),
                           vent_y = if_else(FAILWEAN == "On Ventilator greater than 48 Hours",1,0,missing=0),
                           vent_n = if_else(FAILWEAN == "0", 1, 0,missing=0),
                           vent_patos_y = if_else(VENTPATOS == "Yes", 1, 0,missing=0 ),
                           vent_patos_n = if_else(VENTPATOS == "0", 1, 0,missing=0 ),
                           days_vent = if_else(is.na(DFAILWEAN), -1, as.numeric(DFAILWEAN),missing=-1),
                           
                           # Progressive Renal Insufficiency
                           num_PRF = if_else(is.na(NRENAINSF), 0, as.numeric(NRENAINSF),missing=0),
                           PRF_y = if_else(RENAINSF == "Progressive Renal Insufficiency", 1, 0,missing=0),
                           PRF_n = if_else(RENAINSF == "0", 1, 0,missing=0),
                           days_PRF = if_else(is.na(DRENAINSF), -1, as.numeric(DRENAINSF),missing=-1),
                           
                           # Acute Renal Failure
                           num_ARF = if_else(is.na(NOPRENAFL), 0, as.numeric(NOPRENAFL),missing=0),     
                           ARF_y = if_else(OPRENAFL == "Acute Renal Failure", 1, 0,missing=0),
                           ARF_n = if_else(OPRENAFL == "0", 1, 0,missing=0),
                           days_ARF = if_else(is.na(DOPRENAFL), -1, as.numeric(DOPRENAFL),missing=-1),
                           
                           # Urinary Tract Infection
                           num_uti = if_else(is.na(NURNINFEC), 0, as.numeric(NURNINFEC),missing=0),
                           uti_y = if_else(URNINFEC == "Urinary Tract Infection", 1, 0,missing=0),
                           uti_n = if_else(URNINFEC == "0", 1, 0,missing=0),
                           uti_patos_y = if_else(UTIPATOS=="Yes", 1, 0,missing=0),
                           uti_patos_n = if_else(UTIPATOS=="0", 1, 0,missing=0),
                           days_uti = if_else(is.na(DURNINFEC), -1, as.numeric(DURNINFEC),missing=-1),
                           
                           # Stroke/Cerebral Vascular Accident
                           num_stroke = if_else(is.na(NCNSCVA), 0, as.numeric(NCNSCVA),missing=0),
                           cva_neuro_def_y = if_else(CNSCVA == "Stroke/CVA", 1, 0,missing=0),
                           cva_neuro_def_n = if_else(CNSCVA == "0", 1, 0,missing=0),
                           days_stroke = if_else(is.na(DCNSCVA), -1, as.numeric(DCNSCVA),missing=-1),
                           
                           # Cardiac Arrest Requiring CPR
                           num_cpr = if_else(is.na(NCDARREST), 0, as.numeric(NCDARREST),missing=0),
                           cpr_y = if_else(CDARREST == "Cardiac Arrest Requiring CPR", 1, 0,missing=0),
                           cpr_n = if_else(CDARREST == "0", 1, 0,missing=0),
                           days_cpr = if_else(is.na(DCDARREST), -1, as.numeric(DCDARREST),missing=-1),
                           
                           # Myocardial Infarction
                           num_mi = if_else(is.na(NCDMI), 0, as.numeric(NCDMI),missing=0),
                           mi_y = if_else(CDMI == "Myocardial Infarction", 1, 0,missing=0),
                           mi_n = if_else(CDMI == "0", 1, 0,missing=0),
                           days_mi = if_else(is.na(DCDMI), -1, as.numeric(DCDMI),missing=-1),
                           
                           # Bleeding Transfusions
                           num_trans = if_else(is.na(NOTHBLEED), 0, as.numeric(NOTHBLEED),missing=0),
                           trans_y = if_else(OTHBLEED == "Transfusions/Intraop/Postop", 1, 0,missing=0),
                           trans_n = if_else(OTHBLEED == "0", 1, 0,missing=0),
                           days_trans = if_else(is.na(DOTHBLEED), -1, as.numeric(DOTHBLEED),missing=-1),
                           
                           # DVT/Thrombophlebitis
                           num_thromb = if_else(is.na(NOTHDVT), 0, as.numeric(NOTHDVT),missing=0),
                           thromb_y = if_else(OTHDVT == "DVT Requiring Therapy", 1, 0,missing=0),
                           thromb_n = if_else(OTHDVT == "0", 1, 0,missing=0),
                           days_thromb = if_else(is.na(DOTHDVT), -1, as.numeric(DOTHDVT),missing=-1),
                           
                           # Sepsis
                           num_sepsis = if_else(is.na(NOTHSYSEP), 0, as.numeric(NOTHSYSEP),missing=0),
                           sepsis_y = if_else(OTHSYSEP == "Sepsis", 1, 0,missing=0),
                           sepsis_n = if_else(OTHSYSEP == "0", 1, 0,missing=0),
                           sepsis_patos_y = if_else(SEPSISPATOS == "Yes", 1,0,missing=0),
                           sepsis_patos_n = if_else(SEPSISPATOS == "0", 1,0,missing=0),
                           days_sepsis = if_else(is.na(DOTHSYSEP), -1, as.numeric(DOTHSYSEP),missing=-1),
                           
                           # Septic Shock
                           num_sepshock = if_else(is.na(NOTHSESHOCK), 0, as.numeric(NOTHSESHOCK),missing=0),
                           sepshock_y = if_else(OTHSESHOCK == "Septic Shock", 1, 0,missing=0),
                           sepshock_n = if_else(OTHSESHOCK == "0", 1, 0,missing=0),
                           sepshock_patos_y = if_else(SEPSHOCKPATOS == "Yes", 1,0,missing=0),
                           sepshock_patos_n = if_else(SEPSHOCKPATOS == "0", 1,0,missing=0),
                           days_sepshock = if_else(is.na(DOTHSESHOCK), -1, as.numeric(DOTHSESHOCK),missing=-1),
                           
                           # Post-op Diagnosis (ICD)
                           postop_ICD9 = if_else(is.na(PODIAG), "-1", PODIAG,missing="-1"),
                           postop_ICD10 = if_else(is.na(PODIAG10), "-1", PODIAG10,missing="-1"),
                           
                           # Unplanned Reoperation
                           return_OR_y = if_else(RETURNOR == "Yes", 1, 0,missing=0),
                           return_OR_n = if_else(RETURNOR == "0", 1, 0,missing=0),
                           
                           # Days from Operation to Death
                           days_death = if_else(is.na(DOpertoD), -1, as.numeric(DOpertoD),missing=-1),
                           
                           # Days from Operation to Discharge
                           days_discharge = if_else(is.na(DOptoDis), -1, as.numeric(DOptoDis),missing=-1),
                           
                           # Still in Hospital > 30 Days 
                           still_in_hosp_y = if_else(STILLINHOSP == "Yes", 1, 0,missing=0),
                           still_in_hosp_n = if_else(STILLINHOSP == "0", 1, 0,missing=0),
                           
                           # Unplanned Reoperations
                           reop1_y = if_else(REOPERATION1 == "Yes", 1, 0,missing=0),
                           reop1_n = if_else(REOPERATION1 != "Yes", 1, 0,missing=0),
                           days_reop1 = if_else(is.na(RETORPODAYS), -1, as.numeric(RETORPODAYS),missing=-1),
                           cpt_reop1 = if_else(is.na(REOPORCPT1), "-1", REOPORCPT1,missing="-1"),
                           related_reop1_y = if_else(RETORRELATED == "Yes", 1, 0,missing=0),
                           related_reop1_n = if_else(RETORRELATED != "Yes", 1, 0,missing=0),
                           reop1_ICD9 = if_else(is.na(REOPORICD91), "-1", REOPORICD91,missing="-1"),
                           reop1_ICD10 = if_else(is.na(REOPOR1ICD101), "-1", REOPOR1ICD101,missing="-1"),
                           
                           reop2_y = if_else(REOPERATION2 == "Yes", 1, 0,missing=0),
                           reop2_n = if_else(REOPERATION2 != "Yes", 1, 0,missing=0),
                           days_reop2 = if_else(is.na(RETOR2PODAYS), -1, as.numeric(RETOR2PODAYS),missing=-1),
                           cpt_reop2 = if_else(is.na(REOPOR2CPT1), "-1", REOPOR2CPT1,missing="-1"),
                           related_reop2_y = if_else(RETOR2RELATED == "Yes", 1, 0,missing=0),
                           related_reop2_n = if_else(RETOR2RELATED != "Yes", 1, 0,missing=0),
                           reop2_ICD9 = if_else(is.na(REOPOR2ICD91), "-1", REOPOR2ICD91,missing="-1"),
                           reop2_ICD10 = if_else(is.na(REOPOR2ICD101), "-1", REOPOR2ICD101,missing="-1"),
                           
                           reop3plus_y = if_else(REOPERATION3 == "Yes", 1, 0,missing=0),
                           reop3plus_n = if_else(REOPERATION3 != "Yes", 1, 0,missing=0),
                           
                           # Hospital Readmission
                           readmit1_y = if_else(READMISSION1 == "Yes", 1, 0,missing=0),
                           readmit1_n = if_else(READMISSION1 != "Yes", 1, 0,missing=0),
                           days_readmit1 = if_else(is.na(READMPODAYS1), -1, as.numeric(READMPODAYS1),missing=-1),
                           unplan_readmit1_y = if_else(UNPLANNEDREADMISSION1 == "Yes", 1, 0,missing=0),
                           unplan_readmit1_n = if_else(UNPLANNEDREADMISSION1 != "Yes", 1, 0,missing=0),
                           unplan_readmit1_related_y = if_else(READMRELATED1 == "Yes", 1, 0,missing=0),
                           unplan_readmit1_related_n = if_else(READMRELATED1 != "Yes", 1, 0,missing=0),
                           
                           readmit2_y = if_else(READMISSION2 == "Yes", 1, 0,missing=0),
                           readmit2_n = if_else(READMISSION2 != "Yes", 1, 0,missing=0),
                           days_readmit2 = if_else(is.na(READMPODAYS2), -1, as.numeric(READMPODAYS2),missing=-1),
                           unplan_readmit2_y = if_else(UNPLANNEDREADMISSION2 == "Yes", 1, 0,missing=0),
                           unplan_readmit2_n = if_else(UNPLANNEDREADMISSION2 != "Yes", 1, 0,missing=0),
                           unplan_readmit2_related_y = if_else(READMRELATED2 == "Yes", 1, 0,missing=0),
                           unplan_readmit2_related_n = if_else(READMRELATED2 != "Yes", 1, 0,missing=0),
                           
                           readmit3_y = if_else(READMISSION3 == "Yes", 1, 0,missing=0),
                           readmit3_n = if_else(READMISSION3 != "Yes", 1, 0,missing=0),
                           days_readmit3 = if_else(is.na(READMPODAYS3), -1, as.numeric(READMPODAYS3),missing=-1),
                           unplan_readmit3_y = if_else(UNPLANNEDREADMISSION3 == "Yes", 1, 0,missing=0),
                           unplan_readmit3_n = if_else(UNPLANNEDREADMISSION3 != "Yes", 1, 0,missing=0),
                           unplan_readmit3_related_y = if_else(READMRELATED3 == "Yes", 1, 0,missing=0),
                           unplan_readmit3_related_n = if_else(READMRELATED3 != "Yes", 1, 0,missing=0),
                           
                           readmit4_y = if_else(READMISSION4 == "Yes", 1, 0,missing=0),
                           readmit4_n = if_else(READMISSION4 != "Yes", 1, 0,missing=0),
                           days_readmit4 = if_else(is.na(READMPODAYS4), -1, as.numeric(READMPODAYS4),missing=-1),
                           unplan_readmit4_y = if_else(UNPLANNEDREADMISSION4 == "Yes", 1, 0,missing=0),
                           unplan_readmit4_n = if_else(UNPLANNEDREADMISSION4 != "Yes", 1, 0,missing=0),
                           unplan_readmit4_related_y = if_else(READMRELATED4 == "Yes", 1, 0,missing=0),
                           unplan_readmit4_related_n = if_else(READMRELATED4 != "Yes", 1, 0,missing=0),
                           
                           readmit5_y = if_else(READMISSION5 == "Yes", 1, 0,missing=0),
                           readmit5_n = if_else(READMISSION5 != "Yes", 1, 0,missing=0),
                           days_readmit5 = if_else(is.na(READMPODAYS5), -1, as.numeric(READMPODAYS5),missing=-1),
                           unplan_readmit5_y = if_else(UNPLANNEDREADMISSION5 == "Yes", 1, 0,missing=0),
                           unplan_readmit5_n = if_else(UNPLANNEDREADMISSION5 != "Yes", 1, 0,missing=0),
                           unplan_readmit5_related_y = if_else(READMRELATED5 == "Yes", 1, 0,missing=0),
                           unplan_readmit5_related_n = if_else(READMRELATED5 != "Yes", 1, 0,missing=0),
                           
                           # Surgical Wound Closure
                           wound_fully_closed = if_else(WOUND_CLOSURE == "All layers of incision (deep and superficial) fully closed", 1, 0,missing=0),
                           wound_deep_closed = if_else(WOUND_CLOSURE == "Only deep layers closed; superficial left open", 1, 0,missing=0),
                           wound_not_closed = if_else(WOUND_CLOSURE == "No layers of incision are surgically closed", 1, 0,missing=0),
                           
                           
                           # Other Post-op Occurrence (ICD)
                           postop_ICD9 = if_else(is.na(PODIAG_OTHER), "-1", PODIAG_OTHER,missing="-1"),
                           postop_ICD10 = if_else(is.na(PODIAG_OTHER10), "-1", PODIAG_OTHER10,missing="-1"),
                           
                           #  Clostridium Difficile (C.diff) Colitis
                           cdiff_y = if_else(OTHCDIFF == "C. diff", 1, 0,missing=0),
                           cdiff_n = if_else(OTHCDIFF == "0", 1, 0,missing=0),
                           num_cdiff = if_else(is.na(NOTHCDIFF), 0, as.numeric(NOTHCDIFF),missing=0),
                           days_cdiff = if_else(is.na(DOTHCDIFF), -1, as.numeric(DOTHCDIFF),missing=-1),
)


#### Export R Objects ####
save(pred_puf17, file = "pred_puf17.Rda")
save(outcomes_puf17, file = "outcomes_puf17.Rda")
