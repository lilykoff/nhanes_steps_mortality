library(tidyverse)
# follow https://github.com/andrew-leroux/rnhanesdata/blob/master/vignettes/create-all-processed-data.Rmd

# function to replace refused or don't know with NAs
replace_NA_bin = function(x){
  ifelse(x %in% c("Refused", "Don't know", "don't know", "Don't Know"), NA, ifelse(x == "Yes", 1, 0))
  # case_when(x %in% c("Refused", "Don't know", "don't know") ~ NA,
  #           x == "Yes", 1,
  #           TRUE ~ 0)
}

replace_NA_cat = function(x){
  ifelse(x %in% c("Refused", "Don't know", "Don't Know", "don't know"), NA, x)
}

# first read in dataset
temp =
  readr::read_csv(here::here("data", "demographics", "processed", "subset_G_H_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_HSQ_HUQ_translated.csv.gz"),
                  col_types = cols('Respondent sequence number' = col_character()))
# readr::read_csv(here::here("lily", "data", "subset_G_H_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_translated.csv.gz"))

# general health condition x is from HSD
# general health condition y is from HUQ
temp_mutated =
  temp %>%
  janitor::clean_names() %>%
  rename(SEQN = respondent_sequence_number) %>%
  mutate(
    # create alcohol categories
    # first change how often drink in last 12 months variable to NA if 777, 999
    how_often_drink_alcohol_over_past_12_mos = ifelse(how_often_drink_alcohol_over_past_12_mos %in% c(777, 999), NA_real_, how_often_drink_alcohol_over_past_12_mos),
    # never drinker if have never had 12 drinks/year or 12 drinks/life
    never_drinker = had_at_least_12_alcohol_drinks_1_yr == "No" & had_at_least_12_alcohol_drinks_lifetime == "No",
    # former drinkers are anyone who indicated that they had at least 12 drinks
    # over the course of any one year or 12 drinks in lifetime, but report drinking no alcohol in the past 12 months
    former_drinker = (had_at_least_12_alcohol_drinks_1_yr == "Yes" & how_often_drink_alcohol_over_past_12_mos == 0)  | (had_at_least_12_alcohol_drinks_1_yr == "No" & had_at_least_12_alcohol_drinks_lifetime == "Yes" & how_often_drink_alcohol_over_past_12_mos == 0),
    # calculate number of weekly drinks, first making 777 and 999 NA
    avg_number_alcoholic_drinks_day_past_12_mos = ifelse(avg_number_alcoholic_drinks_day_past_12_mos %in% c(777, 999), NA, avg_number_alcoholic_drinks_day_past_12_mos),
    # weekly drinks function of alcohol ever, drinks per unit, unit, and frequency
    num_weeklydrinks =
      case_when(
        had_at_least_12_alcohol_drinks_lifetime == "No" ~ 0,
        how_often_drink_alcohol_over_past_12_mos == 0 ~ 0,
        number_days_drink_alcohol_per_wk_mo_yr == "Week" ~ how_often_drink_alcohol_over_past_12_mos * avg_number_alcoholic_drinks_day_past_12_mos,
        number_days_drink_alcohol_per_wk_mo_yr == "Month" ~ how_often_drink_alcohol_over_past_12_mos * (7 / 30) * avg_number_alcoholic_drinks_day_past_12_mos,
        number_days_drink_alcohol_per_wk_mo_yr == "Year" ~ how_often_drink_alcohol_over_past_12_mos * (7 / 365) * avg_number_alcoholic_drinks_day_past_12_mos,
        TRUE ~ NA
      ),
    # based on CDC definitions, categorize people as moderate or heavy
    mod_drinker = between(num_weeklydrinks, 1, 7) & gender == "Female" | between(num_weeklydrinks, 1, 14) &
      gender == "Male",
    heavy_drinker = num_weeklydrinks > 7 & gender == "Female" | num_weeklydrinks > 14 &
      gender == "Male",
    # based on all responses, create cat_alcohol variable
    cat_alcohol = case_when(
      former_drinker == 1 ~ "Former drinker",
      never_drinker == 1 ~ "Never drinker",
      mod_drinker == 1 ~ "Moderate drinker",
      heavy_drinker == 1 ~ "Heavy drinker",
      TRUE ~ "Missing alcohol"
    ),
    # create smoking variable
    # if individual answers no to "Have you smoked at least 100 cigarettes in your entire life": never smoker
    # if answers yes to 100 cigs but no to do you \now smoke, former smoker
    # if answers yes to 100 cigs and "every day" or "some days" to do you now smoke: smoker
    # otherwise missing
    smoked_at_least_100_cigarettes_in_life = case_when(smoked_at_least_100_cigarettes_in_life == "Yes" ~ 1,
                         smoked_at_least_100_cigarettes_in_life == "No" ~ 0,
                         TRUE ~ NA),
    do_you_now_smoke_cigarettes = case_when(
      do_you_now_smoke_cigarettes == "Every day" | do_you_now_smoke_cigarettes == "Some days" ~ 1,
      do_you_now_smoke_cigarettes == "Not at all" ~ 0,
      TRUE ~ NA
    ),
    cat_smoke = case_when(
      smoked_at_least_100_cigarettes_in_life == 0 ~ "Never smoker",
      smoked_at_least_100_cigarettes_in_life == 1 & do_you_now_smoke_cigarettes == 0 ~ "Former smoker",
      smoked_at_least_100_cigarettes_in_life == 1 & do_you_now_smoke_cigarettes == 1 ~ "Current smoker",
      TRUE ~ NA
    ),
    # mobility problem, if any of the following are true:
    # 1. Difficulty walking a quarter mile (PFQ061B)
    # 2. Difficulty climbing 10 stairs (PFQ061C)
    # 3. Use of any special equipment to walk (PFQ054)
    # if younger than  59 and without any work, experience_confusion_memory_problems, or other limitation, they are not asked the difficulty walking questions
    bin_mobilityproblem = case_when(
      # if require equipment to walk - yes
      need_special_equipment_to_walk == "Yes" ~ 1,
      # if <59 and no to limitation questions, no problem
      age_in_years_at_screening <= 59 &
        physical_mental_emotional_limitations == "No" &
        limitations_keeping_you_from_working == "No" & experience_confusion_memory_problems == "No" ~ 0,
      # otherwise if have difficulty walking quarter mile or ten steps, they have problem
      walking_for_a_quarter_mile_difficulty %in% c("Unable to do", "Some difficulty", "Much difficulty", "Do not do this activity") ~ 1,
      walking_up_ten_steps_difficulty %in% c("Unable to do", "Some difficulty", "Much difficulty", "Do not do this activity") ~ 1,
      # otherwise no problem
      walking_for_a_quarter_mile_difficulty == "No difficulty" &
        walking_up_ten_steps_difficulty == "No difficulty" ~ 0,
      # or missing
      TRUE ~ NA
    ),
    # make binary from categorical variables
    bin_diabetes = case_when(
      doctor_told_you_have_diabetes == "Yes" ~ 1,
      doctor_told_you_have_diabetes %in% c("No", "Borderline") ~ 0,
      TRUE ~ NA
    ),
    cat_bmi =
      case_when(
        between(body_mass_index_kg_m_2, 0, 18.5) ~ "Underweight",
        between(body_mass_index_kg_m_2, 18.5, 25) ~ "Normal",
        between(body_mass_index_kg_m_2, 25, 30) ~ "Overweight",
        body_mass_index_kg_m_2 > 30 ~ "Obese",
        TRUE ~ NA
      ),
    general_health_condition_x = replace_NA_cat(general_health_condition_x),
    general_health_condition_y = replace_NA_cat(general_health_condition_y),
    general_health_condition = case_when(
      !is.na(general_health_condition_x) ~ general_health_condition_x,
      is.na(general_health_condition_x) & !is.na(general_health_condition_y) ~ general_health_condition_y,
      TRUE ~ NA_character_
    ),
    general_health_condition = case_when(
      general_health_condition %in% c("Excellent", "Excellent,") ~ "Excellent",
      general_health_condition == "Fair, or" ~ "Fair",
      general_health_condition == "Good," ~ "Good",
      general_health_condition == "Poor?" ~ "Poor",
      general_health_condition == "Very good," ~ "Very good",
      TRUE ~ general_health_condition
    ),
    # add vigorous and moderate work physical activity minutes
    across(c(doctor_ever_said_you_were_overweight, doctor_ever_said_you_had_arthritis,
               ever_told_had_congestive_heart_failure,
               ever_told_you_had_angina_angina_pectoris,
               ever_told_you_had_coronary_heart_disease,
             ever_told_you_had_a_stroke,  ever_told_you_had_heart_attack,
               ever_told_you_had_cancer_or_malignancy, any_physical_activities_past_7_days,
             need_special_equipment_to_walk), replace_NA_bin),
    across(c(education_level_adults_20, marital_status, annual_household_income,
             doctor_told_you_have_diabetes), replace_NA_cat))

temp_mutated =
  temp_mutated %>%
  select(
    SEQN,
    data_release_cycle,
    interview_examination_status,
    gender,
    age_in_years_at_screening,
    age_in_months_at_screening_0_to_24_mos,
    race_hispanic_origin,
    six_month_time_period,
    education_level_adults_20,
    marital_status,
    full_sample_2_year_interview_weight,
    full_sample_2_year_mec_exam_weight,
    masked_variance_pseudo_psu,
    masked_variance_pseudo_stratum,
    annual_household_income,
    weight_kg,
    standing_height_cm,
    body_mass_index_kg_m_2,
    overweight = doctor_ever_said_you_were_overweight,
    diabetes = doctor_told_you_have_diabetes,
    bin_diabetes,
    arthritis = doctor_ever_said_you_had_arthritis,
    chf = ever_told_had_congestive_heart_failure,
    chd = ever_told_you_had_coronary_heart_disease,
    angina = ever_told_you_had_angina_angina_pectoris,
    heartattack = ever_told_you_had_heart_attack,
    stroke = ever_told_you_had_a_stroke,
    cancer = ever_told_you_had_cancer_or_malignancy,
    days_physically_active_at_least_60_min,
    minutes_sedentary_activity,
    minutes_walk_bicycle_for_transportation,
    any_physical_activities_past_7_days,
    need_special_equipment_to_walk,
    cat_alcohol,
    cat_bmi,
    cat_smoke,
    bin_mobilityproblem,
    bin_diabetes,
    general_health_condition,
  ) %>% mutate_if(is.character, factor)


saveRDS(temp_mutated, here::here("data", "demographics", "processed", "subset_G_H_tidy.rds"))

# create example dataset
set.seed(4575)
sample =
  temp_mutated %>%
  mutate(cat_age = cut(age_in_years_at_screening, seq(20,80,10))) %>%
  filter(!is.na(cat_age)) %>%
  group_by(gender, cat_age, data_release_cycle) %>%
  sample_n(size = 1) %>%
  select(SEQN, data_release_cycle, cat_age, age_in_years_at_screening,gender)

readr::write_csv(sample, here::here("data", "demographics","example_subjects.csv"))


# for pax y
library(tidyverse)
# follow https://github.com/andrew-leroux/rnhanesdata/blob/master/vignettes/create-all-processed-data.Rmd

# function to replace refused or don't know with NAs
replace_NA_bin = function(x){
  ifelse(x %in% c("Refused", "Don't know", "don't know", "Don't Know"), NA, ifelse(x == "Yes", 1, 0))
  # case_when(x %in% c("Refused", "Don't know", "don't know") ~ NA,
  #           x == "Yes", 1,
  #           TRUE ~ 0)
}

replace_NA_cat = function(x){
  ifelse(x %in% c("Refused", "Don't know", "Don't Know", "don't know"), NA, x)
}

# first read in dataset
temp =
  readr::read_csv(here::here("data", "demographics", "processed", "subset_Y_DEMO_BMX_DIQ_MCQ_PAQ_PFQ_translated.csv.gz"),
                  col_types = cols('Respondent sequence number' = col_character()))
# readr::read_csv(here::here("lily", "data", "subset_G_H_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_translated.csv.gz"))

# general health condition x is from HSD
# general health condition y is from HUQ
temp_mutated =
  temp %>%
  janitor::clean_names() %>%
  rename(SEQN = respondent_sequence_number) %>%
  mutate(
    cat_bmi =
      case_when(
        between(body_mass_index_kg_m_2, 0, 18.5) ~ "Underweight",
        between(body_mass_index_kg_m_2, 18.5, 25) ~ "Normal",
        between(body_mass_index_kg_m_2, 25, 30) ~ "Overweight",
        body_mass_index_kg_m_2 > 30 ~ "Obese",
        TRUE ~ NA
      ))

temp_mutated =
  temp_mutated %>% mutate_if(is.character, factor) %>%
  mutate(SEQN = as.character(SEQN))


saveRDS(temp_mutated, here::here("data", "demographics", "processed", "subset_Y_tidy.rds"))

