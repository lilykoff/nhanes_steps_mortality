library(tidyverse)
library(tidymodels)
library(future)
library(censored)
library(furrr)
ncores = parallelly::availableCores() - 1

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


# create four workflows: demo only, demo + MIMS, demo + steps, demo + MIMS + steps
# for each model, add case weights

demo_vars = c("age_in_years_at_screening", "cat_bmi",
              "race_hispanic_origin", "bin_diabetes", "chf", "general_health_condition",
              "chd", "heartattack", "cancer", "stroke", "cat_alcohol", "cat_smoke",
              "bin_mobilityproblem", "cat_education")
fit_model = function(pred_vars, model_name, folds, spec, metrics, mort_df){
  require(tidyverse); require(tidymodels); require(censored)
  wflow =  workflow() %>%
    add_model(spec) %>%
    add_variables(outcomes = mort_surv,
                  predictors = all_of(pred_vars)) %>%
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
                            select(contains("mean")) %>% colnames()))
step_vars = df_mortality_win %>% select(contains("steps") & contains("mean")) %>% colnames()
step_MIMS_list = lapply(step_vars, function(x){c(x, "mean_PAXMTSM")})
step_lMIMS_list = lapply(step_vars, function(x){c(x, "mean_log10PAXMTSM")})

step_AC_list = lapply(step_vars, function(x){c(x, "mean_AC")})
step_lAC_list = lapply(step_vars, function(x){c(x, "mean_log10AC")})

all_list = c(add_vars_list, step_MIMS_list,step_lMIMS_list,step_lAC_list, step_AC_list)
preds_list = lapply(all_list, function(x){c(x, demo_vars)})
preds_list[[length(preds_list) + 1]] = demo_vars
model_names =
  map(all_list, .f = function(x){paste(sub(".*mean\\_", "", x), collapse = "+")})
model_names[[length(model_names) + 1]] = "Demo only"



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
fname = "metrics_wtd_100_female_sens.rds"


plan(multisession, workers = ncores)
results =
  furrr::future_map2_dfr(
    .x = preds_list,
    .y = model_names,
    .f = fit_model,
    spec = survreg_spec,
    metrics = survival_metrics,
    folds = folds_female,
    mort_df = surv_female,
    .options = furrr_options(seed = TRUE, globals = TRUE)
  )

saveRDS(
  results,
  here::here("results", fname)
)


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
fname = "metrics_wtd_100_male_sens.rds"

results =
  furrr::future_map2_dfr(
    .x = preds_list,
    .y = model_names,
    .f = fit_model,
    spec = survreg_spec,
    metrics = survival_metrics,
    folds = folds_male,
    mort_df = surv_male,
    .options = furrr_options(seed = TRUE, globals = TRUE)
  )

saveRDS(
  results,
  here::here("results", fname)
)
