# nhanes_steps_mortality

Purpose: accompanies xx manuscript

Given minute-level step counts from NHANES, run mortality analysis 

## code

- `step_01_download_covariates.R`: 
  - download relevant covariate data from NHANES website, and translate columns
- `step_02_create_analytic_covar_dataset.R`: 
  - create analytic dataset with covariates by aggregating data downloaded from step 1, with nice labels 
- `step_03_create_mortality_dataset.R`: 
  - download mortality data and process from NHANES website
- `step_04_join_demo_mortality.R`:
  - join mortality data and demographic data 
- `step_05_summarize_pa.R`
  - summarize minute level physical activity data to subject level, join with covariates
- `step_06<>_run_<>models.R`:
  - run univariate and multivariable mortality prediction models 
- `Analysis.qmd`: final analyses for manuscript 
  
## data 
### demographics 
#### raw
Raw `.XPT` files from [https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/overview.aspx?BeginYear=2013](NHANES website) and mortality data from [https://www.cdc.gov/nchs/data-linkage/mortality-public.htm](NCHS website)
#### processed 
Combined and processed raw data from `.XPT` files and mortality data 

### accelerometry
- `inclusion_summary.csv.gz`: summary of wear time and other criteria by day and subject 
- `nhanes_minute_level_pa.rds`: minute level physical activity data. Each row is a subject (`SEQN`), day (`PAXDAYM`, `PAXDAYWM`), and physical activity variable or wear prediction/wear flag indicator (either a step algorithm, MIMS, AC, log10 MIMS, log10 AC, wear flag, wear prediction), and the columns `min_1`, `min_2`, ..., `min_1440` are the minute level data for that day.
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
