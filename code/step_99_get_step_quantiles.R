## Run this after code/step_06_join_demo_pa.R
library(tidyverse)
library(survey)
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
         full_sample_2_year_interview_weight,
         masked_variance_pseudo_stratum, valid_accel, contains("total"), gender, age_in_years_at_screening) %>%
  filter(valid_accel)


joined = reweight_accel(data = df_small,
                        demo = steps_df) %>%
  rename(age = age_in_years_at_screening) %>%
  ungroup() %>%
  mutate(cat_age = cut(age, breaks = c(0,10, 20,30,40,50,60,70,80, 85), include.lowest = FALSE,
                       right = FALSE))


options(survey.lonely.psu = "adjust")

svyquant_general = function(data, age_tmp, sex, stepsvar, ci = FALSE){
  if(sex %in% c("Male", "Female")) {
    temp =
      data %>%
      filter(cat_age == age_tmp & gender == sex) %>%
      mutate(wt_norm = wtmec4yr_adj_norm / mean(wtmec4yr_adj_norm)) %>%
      rename(steps = {{stepsvar}})
  } else {
    temp =
      data %>%
      filter(cat_age == age_tmp) %>%
      mutate(wt_norm = wtmec4yr_adj_norm / mean(wtmec4yr_adj_norm)) %>%
      rename(steps = {{stepsvar}})
  }

  svy_design =
    survey::svydesign(
      id = ~ masked_variance_pseudo_psu,
      strata = ~ masked_variance_pseudo_stratum,
      weights = ~ wt_norm,
      data = temp,
      nest = TRUE
    )
  run_quantiles = seq(0, 1, 0.001)
  out = svyquantile(~steps, svy_design, quantiles = run_quantiles,
                    ci = ci)
  if (ci) {
    out = out$steps[,1]
  } else {
    out = t(out$steps)
  }
  out = out  %>%
    unname() %>%
    as_tibble() %>%
    mutate(quantile = run_quantiles,
           gender = sex,
           age_cat = age_tmp,
           algorithm = stepsvar)

}

### Getting CDFs ###
run_cdf = function(data) {
  temp =
    data %>%
    mutate(wt_norm = wtmec4yr_adj_norm / mean(wtmec4yr_adj_norm))

  svy_design =
    survey::svydesign(
      id = ~ masked_variance_pseudo_psu,
      strata = ~ masked_variance_pseudo_stratum,
      weights = ~ wt_norm,
      data = temp,
      nest = TRUE
    )
  svycdf(~steps, svy_design)$steps
}


### Reshape to long format and run CDFs ###
long = joined %>%
  select(SEQN, data_release_cycle, cat_age, gender, wtmec4yr_adj_norm,
         masked_variance_pseudo_psu, masked_variance_pseudo_stratum,
         contains("total")) %>%
  pivot_longer(cols = contains("total"),
               names_to = "measure",
               values_to = "steps")

## Run Groups to split the data
long_grouped = long %>%
  group_by(cat_age, gender, measure)
long_grouped_split = long_grouped %>%
  group_split()

# get keys
keys = group_keys(long_grouped)
keys$cdf = purrr::map(long_grouped_split, run_cdf)

## Run Age Groups overall
long_grouped = long %>%
  group_by(cat_age, measure)
long_grouped_split = long_grouped %>%
  group_split()

over_age_keys = group_keys(long_grouped) %>%
  mutate(gender = "Overall")
over_age_keys$cdf = purrr::map(long_grouped_split, run_cdf)
keys = bind_rows(keys, over_age_keys)

## Run Sex Groups overall
long_grouped = long %>%
  group_by(gender, measure)
long_grouped_split = long_grouped %>%
  group_split()

over_sex_keys = group_keys(long_grouped) %>%
  mutate(cat_age = "Overall")
over_sex_keys$cdf = purrr::map(long_grouped_split, run_cdf)

keys = bind_rows(keys, over_sex_keys)
write_rds(keys, here::here("results", "age_sex_cdf.rds"), compress = "xz")


## Run Groups to split the data
long_grouped = long %>%
  group_by(data_release_cycle, cat_age, gender, measure)
long_grouped_split = long_grouped %>%
  group_split()

# get keys
keys = group_keys(long_grouped)
keys$cdf = purrr::map(long_grouped_split, run_cdf)

## Run Age Groups overall
long_grouped = long %>%
  group_by(data_release_cycle, cat_age, measure)
long_grouped_split = long_grouped %>%
  group_split()

over_age_keys = group_keys(long_grouped) %>%
  mutate(gender = "Overall")
over_age_keys$cdf = purrr::map(long_grouped_split, run_cdf)
keys = bind_rows(keys, over_age_keys)

## Run Sex Groups overall
long_grouped = long %>%
  group_by(data_release_cycle, gender, measure)
long_grouped_split = long_grouped %>%
  group_split()

over_sex_keys = group_keys(long_grouped) %>%
  mutate(cat_age = "Overall")
over_sex_keys$cdf = purrr::map(long_grouped_split, run_cdf)

keys = bind_rows(keys, over_sex_keys)


write_rds(keys, here::here("results", "age_sex_cdf_by_wave.rds"), compress = "xz")





var_df = expand_grid(age = unique(as.character(joined$cat_age)),
                     sex = c("Male", "Female", "Overall"),
                     algo = colnames(joined %>% select(contains("total")))
)


result = pmap_dfr(.l = list(age_tmp = var_df$age,
                            sex = var_df$sex,
                            stepsvar = var_df$algo),
                  data = joined,
                  .f = svyquant_general,
                  .progress = TRUE)


write_csv(result, here::here("results", "age_sex_quantiles.csv.gz"))
