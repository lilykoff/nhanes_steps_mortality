library(tidyverse)
library(tidymodels)
library(future)
library(censored)
library(furrr)
source(here::here("code", "utils.R"))

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
                          general_health_condition, mortstat, permth_exm, total_PAXMTSM),
                ~!is.na(.x))) %>% # no missing data
  mutate(event_time = permth_exm / 12) # event time in years = person months since exam / 12

df_mortality_win =
  df_mortality %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~winsorize(.x, quantile(.x, probs = c(0, 0.99))))) %>%
  mutate(across(contains("total"),
                ~ factor(ntile(.x, 4)),
                .names = "{.col}_quartile")) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))

vars = df_mortality_win %>% select(contains("quartile")) %>% colnames()

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


# create a survival object
surv_df =
  df_mortality_win %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())

set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_singlevar_stepsquartile.rds"

plan(multisession, workers = ncores)
results =
  furrr::future_map_dfr(
    .x = vars,
    .f = fit_model,
    spec = survreg_spec,
    metrics = survival_metrics,
    folds = folds,
    mort_df = surv_df,
    .options = furrr_options(seed = TRUE, globals = TRUE)
  )

saveRDS(
  results,
  here::here("results", fname)
)
