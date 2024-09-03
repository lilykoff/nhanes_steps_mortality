library(tidyverse)
library(tidymodels)
library(future)
library(censored)
library(furrr)

# rm(list = ls())
# 100x cross-validated single variable mortality prediction, by wave

ncores = parallelly::availableCores() - 1

# 100x cross-validated single variable mortality prediction
df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

df_mortality =
  df_all %>%
  filter(num_valid_days > 0) %>% # valid accelerometry
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80) %>%  # age criteria
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          race_hispanic_origin, cat_education,
                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                          general_health_condition, mortstat, permth_exm, mean_PAXMTSM),
                ~!is.na(.x))) %>% # no missing data
  mutate(event_time = permth_exm / 12) # event time in years = person months since exam / 12

df_mortality_win =
  df_mortality %>%
  ungroup() %>%
  mutate(across(c(contains("mean"), contains("peak")), ~DescTools::Winsorize(.x, probs = c(0, 0.99))))


survival_metrics = metric_set(concordance_survival)
survreg_spec = proportional_hazards() %>%
  set_engine("survival") %>%
  set_mode("censored regression")


fit_model = function(var, folds, spec, metrics, mort_df){
  require(tidyverse); require(tidymodels); require(censored)
  # create workflow
  wflow = workflow() %>%
    add_model(spec) %>%
    add_variables(outcomes = mort_surv,
                  predictors = all_of(var)) %>%
    add_case_weights(case_weights_imp)

  # fit model on folds
  res = fit_resamples(
    wflow,
    resamples = folds,
    metrics = metrics,
    control = control_resamples(save_pred = TRUE)
  )

  # get metrics -- for some reason if you just use collect_metrics it doesn't take into account case weights,
  # so we write this function to get concordance
  get_concordance = function(row_num, preds, surv_df){
    preds %>%
      slice(row_num) %>%
      unnest(.predictions) %>%
      select(.pred_time, .row, mort_surv) %>%
      left_join(surv_df %>% select(row_ind, case_weights_imp), by = c(".row" = "row_ind")) %>%
      concordance_survival(truth = mort_surv, estimate = ".pred_time", case_weights = case_weights_imp) %>%
      pull(.estimate)
  }

  # get concordance for each fold
  concordance_vec = map_dbl(.x = 1:nrow(res), .f = get_concordance, preds = res, surv_df = mort_df)
  rm(res)
  # return as tibble
  tibble(concordance = concordance_vec,
         variable = var)
}


demo_vars = c(
  "age_in_years_at_screening",
  "cat_bmi",
  "race_hispanic_origin",
  "bin_diabetes",
  "cat_education",
  "chf",
  "chd",
  "heartattack",
  "stroke",
  "cancer",
  "cat_alcohol",
  "cat_smoke",
  "bin_mobilityproblem",
  "general_health_condition")
pa_vars =
  df_mortality_win %>%
  select(contains("mean"), contains("peak")) %>%
  colnames()

vars = c(demo_vars, pa_vars)

# normalize weights
female_df = df_mortality_win %>%
  filter(gender == "Female") %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))

# create a survival object
surv_female =
  female_df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())

set.seed(4575)
folds_female = vfold_cv(surv_female, v = 10, repeats = 100)
fname = "metrics_wtd_100_singlevar_female_sens.rds"

plan(multisession, workers = ncores)
results =
  furrr::future_map_dfr(
    .x = vars,
    .f = fit_model,
    spec = survreg_spec,
    metrics = survival_metrics,
    folds = folds_female,
    mort_df = surv_female,
    .options = furrr_options(seed = TRUE, globals = TRUE)
  )

if(!dir.exists(here::here("results"))){
  dir.create(here::here("results"))
}
saveRDS(results, here::here("results", fname))

male_df = df_mortality_win %>%
  filter(gender == "Male") %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))

# create a survival object
surv_male =
  male_df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())

set.seed(4575)
folds_male = vfold_cv(surv_male, v = 10, repeats = 100)
fname = "metrics_wtd_100_singlevar_male_sens.rds"

results =
  furrr::future_map_dfr(
    .x = vars,
    .f = fit_model,
    spec = survreg_spec,
    metrics = survival_metrics,
    folds = folds_male,
    mort_df = surv_male,
    .options = furrr_options(seed = TRUE, globals = TRUE)
  )

saveRDS(results, here::here("results", fname))



