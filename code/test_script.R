library(tidyverse)
library(tidymodels)
library(furrr)
library(future)
library(censored)
n_cores = parallelly::availableCores() - 1
print(n_cores)
source(here::here("code/utils.R"))

fold = NULL
rm(list = c("fold"))

df = readRDS(here::here("data", "bootstrap_obj.rds"))
xdf = df
ifold = get_fold()
if (!is.na(ifold)) {
  xdf = df[[ifold]]
}




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
  tibble(concordance = mean(concordance_vec),
         variable = var)
}


demo_vars = c(
  "age_in_years_at_screening",
  "cat_bmi",
  "gender",
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
  xdf %>%
  select(contains("total")) %>%
  colnames()

vars = c(demo_vars, pa_vars)

df = xdf %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))

# create a survival object
surv_df =
  df %>%
  mutate(mort_surv = Surv(event_time, mortstat)) %>%
  mutate(case_weights_imp = hardhat::importance_weights(weight_norm)) %>%
  mutate(row_ind = row_number())

set.seed(4575)
folds = vfold_cv(surv_df, v = 10, repeats = 100)
fname = paste0("univariate_fold_", ifold, ".rds")

# plan(multisession, workers = ncores)
plan(future::multisession, workers = n_cores)

results =
  furrr::future_map_dfr(
    .x = vars[1],
    .f = fit_model,
    spec = survreg_spec,
    metrics = survival_metrics,
    folds = folds,
    mort_df = surv_df
    # .options = furrr_options(seed = TRUE, globals = TRUE)
  )
print(results)
# saveRDS(results, here::here("results", "bootstrap", fname))
