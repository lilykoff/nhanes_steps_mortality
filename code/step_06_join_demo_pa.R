library(tidyverse)
library(gt)
library(gtsummary)
pa_df = readRDS(here::here("data", "accelerometry", "summarized", "pa_df_subject_level.rds"))
covariates = readRDS(here::here("data", "demographics", "processed", "covariates_mortality_G_H_tidy.rds"))
wt_file = readr::read_csv(here::here("data", "accelerometry", "inclusion_summary.csv.gz"),
                          col_types = cols(SEQN = col_character(),
                                           PAXDAYM = col_integer()))
day_key = wt_file %>%
  group_by(SEQN) %>%
  summarize(num_valid_days = sum(include))


covariates =
  covariates %>%
  left_join(pa_df, by = "SEQN") %>%
  select(-num_valid_days) %>%
  left_join(day_key, by = "SEQN") %>%
  mutate(has_accel = SEQN %in% wt_file$SEQN,
         valid_accel = case_when(is.na(num_valid_days) ~ FALSE,
                                 num_valid_days >= 3 ~ TRUE,
                                 num_valid_days < 3 ~ FALSE),
         inclusion_type = factor(case_when(
           num_valid_days >= 3 ~ ">= 3 valid days",
           num_valid_days == 0 ~ "No valid days",
           num_valid_days < 3 ~ "< 3 valid days",
           TRUE ~ "Didn't receive device"
         )),
         across(c(contains("scrf"), contains("adept")), ~if_else(is.na(.x) & valid_accel, 0, .x)),
         cat_education =
           factor(case_when(
             education_level_adults_20 == "High school graduate/GED or equi" ~ "HS/HS equivalent",
             education_level_adults_20 %in% c("Less than 9th grade", "9-11th grade (Includes 12th grad") ~ "Less than HS",
             is.na(education_level_adults_20) ~ NA_character_,
             TRUE ~ "More than HS"))) %>%
  mutate(across(c(cat_education, race_hispanic_origin, cat_smoke), ~forcats::fct_infreq(.x))) %>%
  mutate(general_health_condition = forcats::fct_relevel(general_health_condition, c("Poor", "Fair", "Good", "Very good", "Excellent")),
         cat_alcohol = forcats::fct_relevel(cat_alcohol, c("Never drinker", "Former drinker", "Moderate drinker", "Heavy drinker", "Missing alcohol")))

saveRDS(covariates, here::here("data", "covariates_accel_mortality_df.rds"))


# make table of exclusion by accelerometer
covariates %>%
  select(
    gender,
    age_in_years_at_screening,
    race_hispanic_origin,
    education_level_adults_20,
    cat_bmi,
    bin_diabetes,
    chf,
    chd,
    stroke,
    cat_alcohol,
    cat_smoke,
    bin_mobilityproblem,
    general_health_condition,
    valid_accel,
    mortstat,
    data_release_cycle
  ) %>%
  mutate(across(
    c(bin_diabetes, chf, chd, stroke, bin_mobilityproblem, mortstat),
    ~ .x == 1
  )) %>%
  tbl_strata(
    strata = data_release_cycle,
    ~ tbl_summary(
      .x,
      by = valid_accel,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 2,
      # label = grade ~ "Tumor Grade",
      missing_text = "(Missing)"
    ) %>%
      add_overall()
  )



