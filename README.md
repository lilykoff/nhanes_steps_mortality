
See rendered site at https://lilykoff.github.io/nhanes_steps_mortality/

# nhanes_steps_mortality

Purpose: accompanies [Comparing Step Counting Algorithms for High-Resolution Wrist Accelerometry Data in NHANES 2011-2014](https://pubmed.ncbi.nlm.nih.gov/39589008/)

Minute-level data can be downloaded from [Physionet repository](https://physionet.org/content/minute-level-step-count-nhanes/1.0.0/)

Given minute-level step counts from NHANES, run mortality analysis 

## code

- `step_00_download_pa_data.R`:
  - download minute-level physical activity data from Physionet 
- `step_01_download_covariates.R`: 
  - download relevant covariate data from NHANES website, and translate columns
- `step_02_create_analytic_covar_dataset.R`: 
  - create analytic dataset with covariates by aggregating data downloaded from step 1, with nice labels 
- `step_03_create_mortality_dataset.R`: 
  - download mortality data and process from NHANES website
- `step_04_join_demo_mortality.R`:
  - join mortality data and demographic data 
- `step_05a_summarize_pa.R`
  - summarize minute level physical activity data to subject level
- `step_05b_create_inclusion_df.R`
  - create inclusion criteria data frame based on acceleremetry data 
- `step_06_join_demo_pa.R`
  - join demographics and physical activity data
- `step_07<>_run_<>models.R`:
  - `step_07a_run_univariate_models.R`: run univariate Cox PH models (i.e. `mortality ~ variable`) for all variables in the dataset
  - `step_07b_run_sensitivity_univariate.R`: run univariate Cox PH models for individuals with at least one valid day of data (instead of 3)
  - `step_07c_run_agesensitivity_univariate.R`: run univariate Cox PH models for individuals including 80 year olds (instead of 50-79 year olds)
  - `step_07d_run_quartilesensitivity_univariate.R`: run univariate Cox PH models using quartiles of PA variables (instead of continuous)
  - `step_07e_run_stratified_univariate.R`: run univariate Cox PH models stratified by age, mobility, and self-reported health
- `step_08<>_run_multivariate_models<>.R`: 
  - `step_08a_run_multivarate_models.R`: run multivariable models using all individuals with at least 1 valid day of data (instead of at least 3 valid days) 
  - `step_08b_run_sensitivity_multivariate.R`: run multivariate models using all individuals with at least 1 valid day of data (instead of at least 3 valid days) 
  - `step_08c_run_multivariate_cadence_models.R`: run multivariate models with total steps and cadence to investigate added predictive power of cadence
  - `step_08d_run_agesensitivity_multivariate.R`: run multivariate models using all individuals with at least including 80 year olds (instead of 50-79 year olds)
  - `step_08e_run_stratified_multivariate.R`: run multivariate models stratified by age, mobility, and self-reported health
  - `step_09_nonlinear_associations.R`: run models to investigate nonlinear associations between PA and mortality
- `manuscript_figures.R`: generate all figs for manuscript
- `manuscript_tables.R`: generate all tables for manuscript 
- `utils.R`: some helpful functions 
- `create_subjectinfo_file.R`: create file with subject info for physionet submission 
  
## data 

`covariates_accel_mortality_df.rds`: final processed analytical dataset used for manuscript models and analysis 

### accelerometry
 `inclusion_summary.csv.gz`: summary of wear time and other criteria by day and subject 
 
#### minute_level

-   Each file entitled `nhanes_1440_<varname>rds`. `<varname>` is one of: `actisteps`, `adeptsteps`, `oaksteps`, `scrfsteps`, `scsslsteps`, `vssteps`, `vsrevsteps`, `AC`, `log10AC`, `PAXMTSM`, `log10PAXMTSM`, `PAXPREDM`, `PAXFLGSM`
-   Each row in each file is one day for one participant. Each file contains the following columns: - `SEQN`: NHANES participant ID, a character scalar
-   `PAXDAYM`: NHANES day of physical activity measurements for the participant, integer between 1 and 9. Note: days 1 and 9 will not have complete data.
-   `PAXDAYWM`: day of the week, integer between 1 and 7, where 1 corresponds to Sunday, 2 to Monday, ..., and 7 to Saturday.
-   `min_x` for `x = 1, 2, ..., 1440`: the value of `<varname>` for minute `x`. For `actisteps`, `adeptsteps`, `oaksteps`, `scrfsteps`, `scsslsteps`, `vssteps`, `vsrevsteps`, `AC`, `log10AC`, `PAXMTSM`, `log10PAXMTSM`, the values are floats. For `PAXPREDM` they are integers, where `1` = wake wear, `2` = sleep wear, `3` = unknown wear, and `4` = nonwear. For `PAXFLGSM` the values are logical, where `TRUE` corresponds to any wear flags and `FALSE` corresponds to no wear flags

#### summarized 

- `pa_df_subject_level.rds`: subject level physical activity variables. Each row is subject, and the columns are the mean total, mean peak 1 minute, and mean peak 30 minute values across all valid days for that individual. Only individuals with at least 1 valid day are included. 
- `pa_df_day_level.rds`: day-level level PA variable. Each row is subject-day, and columns are the values for PA variables during that day, along with summaries about weartime for that day, including:
  - `wake_min`: total minutes classified as wake wear by NHANES algorithm
  - `sleep_min`: total minutes classified as sleep wear by NHANES algorithm
  - `unknown_min`: total minutes classified as unknown by NHANES algorithm
  - `nonwear_min`: total minutes classified as nonwear by NHANES algorithm
  - `flagged_min`: total minutes with any flags according to NHANES
  - `non_flagged_wear_min`: total minutes classified as wear and not flagged by NHANES algorithm
  - `zero_MIMS_min`: minutes with zero MIMS
  - `include_day`: logical, whether day meets following criteria: at least 1368 minutes classified as wake wear, sleep wear, or unknown and had no data quality flags, at least 420 minutes classified as wake wear, and at least 420 minutes had non-zero MIMS


### demographics 
#### raw
Raw `.XPT` files from [https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/overview.aspx?BeginYear=2013](NHANES website) and mortality data from [https://www.cdc.gov/nchs/data-linkage/mortality-public.htm](NCHS website)
#### processed 
Combined and processed raw data from `.XPT` files and mortality data 


## results 
- `metrics_wtd_100_singlevar.rds`: single variable concordances from 100 times repeated survey-weighted 10-fold cross validation 
- `metrics_wtd_100.rds`: multivariable concordances from 100 times repeated survey-weighted 10-fold cross validation
- `metrics_wtd_100_singlevar_sens.rds`: single variable concordances from 100 times repeated survey-weighted 10-fold cross validation - sensitivity analysis including everyone with at least one valid day 
- `metrics_wtd_100_sens.rds`: multivariable concordances from 100 times repeated survey-weighted 10-fold cross validation - sensitivity analysis including everyone with at least one valid day 
- `metrics_wtd_100_singlevar_80.rds`: single variable concordances from 100 times repeated survey-weighted 10-fold cross validation - sensitivity analysis including 80+ year olds 
- `metrics_wtd_100_80.rds`: multivariable concordances from 100 times repeated survey-weighted 10-fold cross validation - sensitivity analysis including 80+ year olds 

## manuscript
### figures
Figures for manuscript 

## vignettes
- `NHANES_Steps_Mortality_Vignette.qmd`: vignette describing manuscript analyses 
- `Weartime_Vignette.qmd`: vignette on weartime criteria for NHANES data

