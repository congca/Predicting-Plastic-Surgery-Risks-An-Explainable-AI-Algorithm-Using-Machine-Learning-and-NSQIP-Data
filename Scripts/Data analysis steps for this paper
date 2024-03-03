all: processing tables models
processing: data_processing outcome_grouping
tables: data_quality demographics
models: NSQIP rf ff logit roc

data_processing: data_processing16 data_processing17 data_processing18 imputation
outcome_grouping: outcome_grouping16 outcome_grouping17 outcome_grouping18
data_quality: data_quality16 data_quality17 data_quality18
demographics: demographics_by_year outcomes_by_year surgery_by_year

## Created by make data_processing
preimpute_pred_puf16 = data/preimpute_pred_puf16.Rda
preimpute_pred_puf17 = data/preimpute_pred_puf17.Rda
preimpute_pred_puf18 = data/preimpute_pred_puf18.Rda
pred_puf16 = data/pred_puf16.Rda
pred_puf17 = data/pred_puf17.Rda
pred_puf18 = data/pred_puf18.Rda
outcomes_puf16 = data/outcomes_puf16.Rda
outcomes_puf17 = data/outcomes_puf17.Rda
outcomes_puf18 = data/outcomes_puf18.Rda
outcome_grouping16 = data/grouped_outcomes_puf16.Rda
outcome_grouping17 = data/grouped_outcomes_puf17.Rda
outcome_grouping18 = data/grouped_outcomes_puf18.Rda


## Individual scripts
data_processing16 $(preimpute_pred_puf16) $(outcomes_puf16): data/acs_nsqip_puf16.txt
	Rscript data-processing/data_processing_puf16.R
data_processing17 $(preimpute_pred_puf17) $(outcomes_puf17): data/acs_nsqip_puf17.txt
	Rscript data-processing/data_processing_puf17.R
data_processing18 $(preimpute_pred_puf18) $(outcomes_puf18): data/acs_nsqip_puf18.txt
	Rscript data-processing/data_processing_puf18.R

imputation $(pred_puf16) $(pred_puf17) $(pred_puf18): $(preimpute_pred_puf16) $(preimpute_pred_puf17) $(preimpute_pred_puf18)
	Rscript data-processing/imputation.R

outcome_grouping16 $(outcome_grouping16): $(outcomes_puf16)
	Rscript data-processing/outcome_grouping_puf16.R
outcome_grouping17 $(outcome_grouping17): $(outcomes_puf17)
	Rscript data-processing/outcome_grouping_puf17.R
outcome_grouping18 $(outcome_grouping18): $(outcomes_puf18)
	Rscript data-processing/outcome_grouping_puf18.R

data_quality16: $(pred_puf16) $(outcomes_puf16) data/acs_nsqip_puf16.txt
	Rscript analytics/data_quality_testing_puf16.R
data_quality17: $(pred_puf17) $(outcomes_puf17) data/acs_nsqip_puf17.txt
	Rscript analytics/data_quality_testing_puf17.R
data_quality18: $(pred_puf18) $(outcomes_puf18) data/acs_nsqip_puf18.txt
	Rscript analytics/data_quality_testing_puf18.R

demographics_by_year: $(pred_puf16) $(pred_puf17) $(pred_puf18)
	Rscript analytics/demographics_by_year.R
outcomes_by_year: $(outcome_grouping16) $(outcome_grouping17) $(outcome_grouping18)
	Rscript analytics/outcomes_by_year.R
surgery_by_year: $(pred_puf16) $(pred_puf17) $(pred_puf18)
	Rscript analytics/surgery_by_year.R

simple: $(pred_puf18) $(outcome_grouping18)
	Rscript analytics/simple_models.R
NSQIP: $(pred_puf18) $(outcome_grouping18) $(pred_puf16) $(outcome_grouping16)
	Rscript analytics/NSQIP_model.R
rf: $(pred_puf16) $(outcome_grouping16)
	Rscript analytics/rf.R
ff: $(pred_puf16) $(outcome_grouping16)
	Rscript analytics/ff.R
logit: $(pred_puf16) $(outcome_grouping16)
	Rscript analytics/logit.R
roc: $(pred_puf17) $(outcome_grouping17)
	Rscript analytics/roc.R

## Clean directory of R outputs
clean:
	find . | egrep ".*((\.(Rda|RData|Rout|Rhistory|pdf|tex))|~)$$" | xargs rm
