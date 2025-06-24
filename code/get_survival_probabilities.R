library(tidyverse)
library(survival)
library(future)
library(furrr)
reweight_accel = function(data,
                          return_unadjusted_wts=FALSE,
                          age_bks = c(0, 18, 30, 40, 50, 60, 70, 80, 85),
                          right=FALSE,
                          demo){
  stopifnot(all(c("SEQN","data_release_cycle","full_sample_2_year_mec_exam_weight","full_sample_2_year_interview_weight") %in% colnames(data)))
  stopifnot(all(data$data_release_cycle %in% c(7,8)))
  if(any(duplicated(data$SEQN))) stop("Data must be in the form of one row per participant")

  ret <- data

  vars_wts <- c("full_sample_2_year_interview_weight_unadj", "full_sample_2_year_mec_exam_weight_unadj",
                "full_sample_2_year_interview_weight_unadj_norm","full_sample_2_year_mec_exam_weight_unadj_norm",
                "wtint4yr_unadj", "wtint4yr_unadj_norm",
                "wtmec4yr_unadj", "wtmec4yr_unadj_norm",
                "full_sample_2_year_interview_weight_adj", "full_sample_2_year_interview_weight_adj_norm",
                "wtint4yr_adj", "wtint4yr_adj_norm",
                "wtmec4yr_adj", "wtmec4yr_adj_norm")

  if(any(vars_wts %in% colnames(data))){
    warning(paste0("Variables:",  paste0(vars_wts[vars_wts %in% colnames(data)],collapse=", ") ," found in data. These have been overwritten."))
  }

  for(i in vars_wts) ret[[i]] <- NULL
  rm(list=c("vars_wts","i"))


  uwave     <- sort(unique(ret$data_release_cycle))
  n_age_bks <- length(age_bks)


  if(return_unadjusted_wts){
    ret$full_sample_2_year_interview_weight_unadj <- ret$full_sample_2_year_interview_weight
    ret$full_sample_2_year_mec_exam_weight_unadj <- ret$full_sample_2_year_mec_exam_weight

    ret$full_sample_2_year_interview_weight_unadj_norm <- ret$full_sample_2_year_interview_weight/mean(ret$full_sample_2_year_interview_weight)
    ret$full_sample_2_year_mec_exam_weight_unadj_norm <- ret$full_sample_2_year_mec_exam_weight/mean(ret$full_sample_2_year_mec_exam_weight)

    ## calculate raw/normalized unadjusted 4-year weights
    if(length(uwave) > 1){
      ret$wtint4yr_unadj      <- ret$full_sample_2_year_interview_weight/2
      ret$wtint4yr_unadj_norm <- ret$wtint4yr_unadj/mean(ret$wtint4yr_unadj)

      ret$wtmec4yr_unadj      <- ret$full_sample_2_year_mec_exam_weight/2
      ret$wtmec4yr_unadj_norm <- ret$wtmec4yr_unadj/mean(ret$wtmec4yr_unadj)
    }
  }



  # data(list=c("Covariate_C","Covariate_D"), envir=environment(), package="rnhanesdata")
  # demo <- rbind(Covariate_C, Covariate_D)

  ## create age categories, 85+ are coded as missing so impute a value >= 85

  demo =
    demo %>%
    mutate(age_mn = if_else(!is.na(age_in_months_at_screening_0_to_24_mos),
                            age_in_months_at_screening_0_to_24_mos / 12,
                            age_in_years_at_screening))

  demo$age_cat_mn <- demo$age_cat_ex <- cut(demo$age_mn, breaks=age_bks, right=right)


  demo$Race2 <- factor(demo$race_hispanic_origin, levels=c("Mexican American", "Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Other Race - Including Multi-Rac"),
                       labels=c("Mexican American", "Other", "Other","Black","Other"))

  full_sample_2_year_mec_exam_weight_adj <- full_sample_2_year_interview_weight_adj <- rep(NA, nrow(ret))
  for(i in seq_along(uwave)){
    for(j in levels(demo$gender)){
      for(k in levels(demo$Race2)){
        for(l in levels(demo$age_cat_mn)){
          inx_int_full <- which(demo$gender == j & demo$Race2 == k & demo$age_cat_mn == l & demo$data_release_cycle==uwave[i])
          inx_mec_full <- which(demo$gender == j & demo$Race2 == k & demo$age_cat_ex == l & demo$data_release_cycle==uwave[i])
          seqn_int     <- demo$SEQN[which(demo$gender == j & demo$Race2 == k & demo$age_cat_mn == l & demo$data_release_cycle==uwave[i])]
          seqn_mec     <- demo$SEQN[which(demo$gender == j & demo$Race2 == k & demo$age_cat_ex == l & demo$data_release_cycle==uwave[i])]

          inx_ret_int <- which(ret$SEQN %in% seqn_int)
          inx_ret_mec <- which(ret$SEQN %in% seqn_mec)


          if(length(inx_ret_int) > 0){
            wt_int_full <- sum(demo$full_sample_2_year_interview_weight[inx_int_full])
            wt_int_ret  <- sum(ret$full_sample_2_year_interview_weight[inx_ret_int])

            full_sample_2_year_interview_weight_adj[inx_ret_int] <- ret$full_sample_2_year_interview_weight[inx_ret_int]*wt_int_full/wt_int_ret
          }

          if(length(inx_ret_mec) > 0){
            wt_mec_full <- sum(demo$full_sample_2_year_mec_exam_weight[inx_mec_full])
            wt_mec_ret  <- sum(ret$full_sample_2_year_mec_exam_weight[inx_ret_mec])

            full_sample_2_year_mec_exam_weight_adj[inx_ret_mec] <- ret$full_sample_2_year_mec_exam_weight[inx_ret_mec]*wt_mec_full/wt_mec_ret
          }
        }
      }
    }

  }



  ret$full_sample_2_year_interview_weight_adj      <- full_sample_2_year_interview_weight_adj
  ret$full_sample_2_year_interview_weight_adj_norm <- ret$full_sample_2_year_interview_weight_adj/mean(ret$full_sample_2_year_interview_weight_adj)

  ret$full_sample_2_year_mec_exam_weight_adj      <- full_sample_2_year_mec_exam_weight_adj
  ret$full_sample_2_year_mec_exam_weight_adj_norm <- ret$full_sample_2_year_mec_exam_weight_adj/mean(ret$full_sample_2_year_mec_exam_weight_adj)

  if(length(uwave) > 1){
    ret$wtint4yr_adj      <- ret$full_sample_2_year_interview_weight_adj/2
    ret$wtint4yr_adj_norm <- ret$wtint4yr_adj/mean(ret$wtint4yr_adj)

    ret$wtmec4yr_adj      <- ret$full_sample_2_year_mec_exam_weight_adj/2
    ret$wtmec4yr_adj_norm <- ret$wtmec4yr_adj/mean(ret$wtmec4yr_adj)
  }

  ret
}



steps_df = read_rds(here::here("data", "covariates_accel_mortality_df.rds"))

df_small =
  steps_df %>%
  select(SEQN, full_sample_2_year_mec_exam_weight, masked_variance_pseudo_psu, data_release_cycle,
         full_sample_2_year_interview_weight, mortstat, permth_exm,
         masked_variance_pseudo_stratum, valid_accel, contains("steps"), gender, age_in_years_at_screening) %>%
  filter(valid_accel) %>%
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80) %>%  # age criteria
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          mortstat, permth_exm),
                ~!is.na(.x))) %>% # no missing data
  mutate(event_time = permth_exm / 12)


joined = reweight_accel(data = df_small,
                        demo = steps_df) %>%
  rename(age = age_in_years_at_screening) %>%
  ungroup() %>%
  mutate(cat_age = cut(age, breaks = c(50,60,70,80), include.lowest = FALSE,
                       right = FALSE)) %>%
  mutate(total_guesssteps = total_scsslsteps / 1.5)

key = tibble(colname = colnames(joined %>% select(contains("steps") & contains("total"))),
             name  = c("Actilife", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL", "Verisense revised", "Verisense",
                       "Best guess"))



data = joined
age = "[60,70)"
sex = "Male"
increase = 500
steps_curr = 5000
algorithm = "total_scsslsteps"
calc_change_in_survival = function(data, age, sex, steps_curr, increase = 500, algorithm, surv_year = 5){
  data = data %>% rename(steps = {{algorithm}})
  model =
    coxph(
      Surv(event_time, mortstat) ~ steps + cat_age + gender,
      weights = wtmec4yr_adj_norm,
      data  = data
    )
  newdata = tibble(
    steps = c(steps_curr, steps_curr + increase),
    cat_age = factor(age, levels = levels(data$cat_age), ordered = TRUE),
    gender = factor(sex, levels = levels(data$gender))
  )
  fit_surv = survfit(model, newdata = newdata)
  res = summary(fit_surv, times = surv_year)
  pct_chg = ((res$surv[2] - res$surv[1]) / res$surv[1]) * 100

  return(tibble(age = age,
                sex = sex,
                steps = steps_curr,
                increase = increase,
                algorithm = algorithm,
                pct_chg_survival = pct_chg))
}


var_df = expand_grid(ages = unique(joined$cat_age),
                     sexes = unique(joined$gender),
                     algos = colnames(joined %>% select(contains("steps") & contains("total"))),
                     steps = seq(0, 30000, by = 10))


plan(multisession, workers = 7)
result_p =
  future_pmap_dfr(.l = list(age = var_df$ages,
                            sex = var_df$sexes,
                            algorithm = var_df$algos,
                            steps_curr = var_df$steps),
                  .f = calc_change_in_survival,
                  data = joined,
                  increase = 500,
                  surv_year = 5)
plan(sequential)
result_p = result_p %>%
  left_join(key, by = c("algorithm" = "colname"))


write_csv(result_p, here::here("steps_survival_all_algorithms_inc10.csv.gz"))
result_p = read_csv(here::here("steps_survival_all_algorithms_inc10.csv.gz"))

result_p %>%
  filter(sex == "Female" & age == "[60,70)" & algorithm == "total_scsslsteps" & steps == 8000)
#### old

calc_change_in_survival_ov = function(data, age, steps_curr, increase = 500, algorithm, surv_year = 5){
  data = data %>% rename(steps = {{algorithm}})
  model =
    coxph(
      Surv(event_time, mortstat) ~ steps + cat_age,
      weights = wtmec4yr_adj_norm,
      data  = data
    )
  newdata = tibble(
    steps = c(steps_curr, steps_curr + increase),
    cat_age = factor(age, levels = levels(data$cat_age), ordered = TRUE)
  )
  fit_surv = survfit(model, newdata = newdata)
  res = summary(fit_surv, times = surv_year)
  pct_chg = ((res$surv[2] - res$surv[1]) / res$surv[1]) * 100

  return(tibble(age = age,
                sex = "Overall",
                steps = steps_curr,
                increase = increase,
                algorithm = algorithm,
                pct_chg_survival = pct_chg))
}


var_df = expand_grid(ages = unique(joined$cat_age),
                     algos = colnames(joined %>% select(contains("steps") & contains("total"))),
                     steps = seq(0, 30000, by = 10))


plan(multisession, workers = 7)
result_p_ov =
  future_pmap_dfr(.l = list(age = var_df$ages,
                            algorithm = var_df$algos,
                            steps_curr = var_df$steps),
                  .f = calc_change_in_survival_ov,
                  data = joined,
                  increase = 500,
                  surv_year = 5)
plan(sequential)
result_p_ov = result_p_ov %>%
  left_join(key, by = c("algorithm" = "colname"))

### join here
write_csv(result_p_ov, here::here("steps_survival_all_algorithms_inc10_overall.csv.gz"))

result_p_ov = read_csv(here::here("steps_survival_all_algorithms_inc10_overall.csv.gz"))
result_p = read_csv(here::here("steps_survival_all_algorithms_inc10.csv.gz"))

# result_p %>%
#   filter(sex == "Female" & age == "[60,70)" & algorithm == "total_scsslsteps" & steps == 8000)


all_results =
  result_p %>%
  bind_rows(result_p_ov)

write_csv(all_results, here::here("steps_survival_all_algorithms_inc10_all.csv.gz"))

all_results %>%
  ggplot(aes(x = steps / 1000, y = pct_chg_survival, color = sex)) +
  # geom_point() +
  geom_line() +
  facet_grid(age ~ name) +
  theme_light() +
  scale_color_brewer(palette = "Dark2", name = "") +
  labs(x = "Baseline Steps (x1000)", y = "% Chg in Survival for 500-step increase") +
  theme(legend.position = "bottom")
