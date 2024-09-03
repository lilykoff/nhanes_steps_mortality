# aggregate pa
library(tidyverse)
library(data.table)
library(gtsummary)
wt_file = readr::read_csv(here::here("data", "accelerometry", "inclusion_summary.csv.gz"))
# pa_df = readr::read_csv(here::here("data", "accelerometry", "nhanes_minute_level_pa.csv.gz"))

if(!file.exists(here::here("data", "accelerometry", "pa_df_day_level.rds"))){
  pa_df = readRDS(here::here("data", "accelerometry", "nhanes_minute_level_pa.rds"))

  pa_dt = as.data.table(pa_df)

  # Perform calculations
  pa_dt_sums <- pa_dt[
    variable != "PAXPREDM" & variable != "PAXFLGSM_BIN",
    .(
      SEQN,
      PAXDAYM, PAXDAYWM, # Include the 'SEQN' column in the result
      variable,   # Include the 'variable' column in the result
      total = rowSums(.SD, na.rm = TRUE),
      peak1 = apply(.SD, 1, max, na.rm = TRUE),
      mean_top_30 = apply(.SD, 1, function(row) {
        top_30_values <- sort(row, decreasing = TRUE)[1:30]
        mean(top_30_values, na.rm = TRUE)
      })
    ),
    .SDcols = patterns("^min_")
  ]

  valid_days =
    wt_file %>%
    filter(include) %>%
    select(SEQN, PAXDAYM)

  pa_df_valid = pa_dt_sums %>%
    right_join(valid_days, by = c("SEQN", "PAXDAYM")) %>%
    group_by(SEQN, variable) %>%
    summarize(mean = mean(total, na.rm = TRUE),
              peak1 = mean(peak1, na.rm = TRUE),
              peak30 = mean(mean_top_30, na.rm = TRUE),
              num_valid_days = n()) %>%
    pivot_wider(names_from = variable, values_from = c(mean, peak1, peak30)) %>%
    ungroup()

  saveRDS(pa_df_valid, here::here("data", "accelerometry", "pa_df_subject_level.rds"))

  # day level data
  pa_df_all =
    pa_dt_sums %>%
    left_join(wt_file %>% select(SEQN, PAXDAYM, W, S, U, N, flag, non_flag_wear, MIMS_0, include), by = c("SEQN", "PAXDAYM"))

  pa_df_all =
    pa_df_all %>%
    rename(mean = total,
           peak30 = mean_top_30,
           wear_min = W,
           sleep_min = S,
           unknown_min = U,
           nonwear_min = N,
           flagged_min = flag,
           non_flagged_wear_min = non_flag_wear,
           zero_MIMS_min = MIMS_0,
           include_day = include) %>%
    pivot_wider(names_from = variable, values_from = c(mean, peak1, peak30))
  saveRDS(pa_df_all, here::here("data", "accelerometry", "pa_df_day_level.rds"))

}

day_key = wt_file %>%
  group_by(SEQN) %>%
  summarize(num_valid_days = sum(include))

pa_df = readRDS(here::here("data", "accelerometry", "pa_df_subject_level.rds"))

covariates = readRDS(here::here("data", "demographics", "processed", "covariates_mortality_G_H_tidy.rds"))

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



