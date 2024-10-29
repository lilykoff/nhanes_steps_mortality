library(tidyverse)
library(tidymodels)
library(future)
library(censored)
library(furrr)
source(here::here("code", "utils.R"))

ncores = parallelly::availableCores() - 1

df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

df_mortality =
  df_all %>%
  filter(num_valid_days >= 3) %>% # valid accelerometry
  filter(age_in_years_at_screening >= 50) %>%  # age criteria
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
  mutate(across(c(contains("total"), contains("peak")), ~winsorize(.x, quantile(.x, probs = c(0, 0.99)))))


survival_metrics = metric_set(concordance_survival)

survreg_spec = proportional_hazards() %>%
  set_engine("survival") %>%
  set_mode("censored regression")


# create four workflows: demo only, demo + MIMS, demo + steps, demo + MIMS + steps
# for each model, add case weights

demo_vars = c("age_in_years_at_screening", "cat_bmi", "gender",
              "race_hispanic_origin", "bin_diabetes", "chf", "general_health_condition",
              "chd", "heartattack", "cancer", "stroke", "cat_alcohol", "cat_smoke",
              "bin_mobilityproblem", "cat_education")
fit_model = function(pred_vars, model_name, folds, spec, metrics, mort_df){
  require(tidyverse); require(tidymodels); require(censored)
  wflow =  workflow() %>%
    add_model(spec) %>%
    add_variables(outcomes = mort_surv,
                  predictors = c(all_of(pred_vars), all_of(demo_vars))) %>%
    # predictors = c(all_of(demo_vars), all_of(add_vars))) %>%
    add_case_weights(case_weights_imp)

  res = fit_resamples(
    wflow,
    resamples = folds,
    metrics = metrics,
    control = control_resamples(save_pred = TRUE)
  )

  get_concordance = function(row_num, preds, surv_df){
    preds %>%
      slice(row_num) %>%
      unnest(.predictions) %>%
      select(.pred_time, .row, mort_surv) %>%
      left_join(surv_df %>% select(row_ind, case_weights_imp), by = c(".row" = "row_ind")) %>%
      concordance_survival(truth = mort_surv, estimate = ".pred_time", case_weights = case_weights_imp) %>%
      pull(.estimate)
  }

  concordance_vec = map_dbl(.x = 1:nrow(res), .f = get_concordance, preds = res, surv_df = mort_df)
  rm(res)

  tibble(concordance = concordance_vec,
         model = model_name)
}


add_vars_list = as.list(c(df_mortality_win %>%
                            select(contains("total")) %>% colnames()))

all_list = c(add_vars_list)

preds_list = lapply(all_list, function(x){c(x, demo_vars)})
preds_list_nomob = lapply(all_list, function(x){c(x, demo_vars[!demo_vars %in% c("bin_mobilityproblem")])})
preds_list_nohealth = lapply(all_list, function(x){c(x, demo_vars[!demo_vars %in% c("general_health_condition")])})

model_names =
  map(all_list, .f = function(x){paste(sub(".*total\\_", "", x), collapse = "+")})

df_younger =
  df_mortality_win %>%
  filter(age_in_years_at_screening <= 62)


df_older =
  df_mortality_win %>%
  filter(age_in_years_at_screening > 62 & age_in_years_at_screening < 80)


df_limited =
  df_mortality_win %>%
  filter(bin_mobilityproblem == 1)

df_unlimited =
  df_mortality_win %>%
  filter(bin_mobilityproblem != 1)

df_good =
  df_mortality_win %>%
  filter(general_health_condition %in% c("Excellent", "Good", "Very good"))

df_bad =
  df_mortality_win %>%
  filter(general_health_condition %in% c("Poor", "Fair"))

### younger
df = df_younger %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))
# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())
set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_younger.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list,
    .y = model_names,
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


df = df_older %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))
# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())
set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_older.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list,
    .y = model_names,
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

df = df_limited %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))
# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())
set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_limmob.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list_nomob,
    .y = model_names,
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

df = df_unlimited %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))
# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())
set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_unlmob.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list_nomob,
    .y = model_names,
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

df = df_good %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))
# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())
set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_goodhealth.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list_nohealth,
    .y = model_names,
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

df = df_bad %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))
# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())
set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = "metrics_wtd_100_badhealth.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list_nomob,
    .y = model_names,
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

