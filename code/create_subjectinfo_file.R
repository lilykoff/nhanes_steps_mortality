library(tidyverse)

covariates_accel_mortality_df <- readRDS("~/Documents/nhanes_steps_mortality/data/covariates_accel_mortality_df.rds")
subject_info = covariates_accel_mortality_df %>% select(SEQN, data_release_cycle, gender,
                                                        age_in_years_at_screening, full_sample_2_year_interview_weight,
                                                        full_sample_2_year_mec_exam_weight,
                                                        masked_variance_pseudo_psu, masked_variance_pseudo_stratum)
write_csv(subject_info, here::here("data", "accelerometry", "minute_level", "physionet", "subject-info.csv"))

nrow(covariates_accel_mortality_df %>% filter(has_accel))
x = readr::read_csv(here::here("data", "accelerometry", "minute_level", "physionet", "nhanes_1440_PAXFLGSM.csv.xz"))
length(unique(x$SEQN))


covariates_accel_mortality_df %>%
  filter(has_accel) %>%
  group_by(data_release_cycle) %>%
  count()

covariates_accel_mortality_df %>%
  filter(has_accel) %>%
  group_by(gender) %>%
  count()
# > 7505/(7505+7188)

covariates_accel_mortality_df %>%
  filter(has_accel) %>%
  summarize(mage = mean(age_in_years_at_screening, na.rm = TRUE))
# mage sdage
# <dbl> <dbl>
#   1  35.8  23.2
