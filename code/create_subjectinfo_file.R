covariates_accel_mortality_df <- readRDS("~/Documents/nhanes_steps_mortality/data/covariates_accel_mortality_df.rds")
subject_info = covariates_accel_mortality_df %>% select(SEQN, data_release_cycle, gender,
                                                        age_in_years_at_screening, full_sample_2_year_interview_weight,
                                                        full_sample_2_year_mec_exam_weight,
                                                        masked_variance_pseudo_psu, masked_variance_pseudo_stratum)
write_csv(subject_info, here::here("data", "accelerometry", "minute_level", "physionet", "subject-info.csv"))
