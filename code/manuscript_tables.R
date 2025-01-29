library(tidyverse)
library(gtsummary)
library(gt)
library(tidymodels)
library(censored)
library(paletteer)
library(survey)
library(patchwork)
library(Hmisc)
library(data.table)
library(qreport)
library(consort)
conflicted::conflict_prefer("summarize", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# load data
# results from univariate
wt_single = readRDS(here::here("results", "metrics_wtd_100_singlevar.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup()

wt_single_conc = readRDS(here::here("results", "metrics_wtd_100_singlevar_stepsquartile.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  dplyr::summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  dplyr::summarize(concordance_bad = mean(concordance))

# results from multivariable
wt_all = readRDS(here::here("results", "metrics_wtd_100.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup()


# covariate/pa df
df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))


# data used for accelerometry analysis
df_accel =
  df_all %>%
  filter(num_valid_days >= 3 & age_in_years_at_screening >= 18)

# data used for mortality analysis
df_mortality =
  df_all %>%
  filter(num_valid_days >= 3) %>%
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80)


# data used for mortality analysis; Winsorized
df_mortality_win =
  df_mortality %>%
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                                  race_hispanic_origin, cat_education,
                                  cat_bmi, chd, chf, heartattack, stroke, cancer,
                                  bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                                  general_health_condition, mortstat, permth_exm),
                ~!is.na(.x))) %>%
  mutate(event_time = permth_exm / 12) %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~DescTools::Winsorize(.x, quantile(.x, probs = c(0, 0.99)))))

# add indicator for missing covars for consort diagram
df_all_temp =
  df_all %>%
  mutate(missing_covar = if_any(.cols = c(age_in_years_at_screening, gender,
                                          race_hispanic_origin, cat_education,
                                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                                          general_health_condition, mortstat, permth_exm),
                                ~is.na(.x)))

# get numbers alive/died

df_mortality_win %>%
  group_by(mortstat) %>%
  summarize(n = n())
median(df_mortality_win$permth_exm)
mean(df_mortality_win$permth_exm)

######## CONSORT
r2 = data.table(
  id = df_all_temp$SEQN,
  wave = df_all_temp$data_release_cycle,
  age = df_all_temp$age_in_years_at_screening,
  has_accel = df_all_temp$has_accel,
  num_valid_days = df_all_temp$num_valid_days,
  valid_accel = df_all_temp$valid_accel,
  inclusion_type = df_all_temp$inclusion_type,
  missing_covars = df_all_temp$missing_covar,
  mortstat = df_all_temp$mortstat
) %>%
  mutate(
    wave = if_else(wave == 7, "2011-2012", "2013-2014"),
    no_accel = as.factor(if_else(!has_accel, "Did not receive accelerometer", NA_character_)),
    invalid_accel =
           case_when(num_valid_days == 0 ~ "Zero valid days",
                     num_valid_days < 3 ~ "Less than 3 valid days",
                     TRUE ~ NA_character_),
         younger_18 = if_else(valid_accel & age < 18, "Age < 18", NA_character_),
         younger_50 = age < 50,
         older_79 = age > 79,
         accel_analysis = valid_accel & age > 18,
         mort_exc =
           case_when(valid_accel & age < 50 & age > 18 ~ "Younger than 50",
                     valid_accel & age >= 80 ~ "Older than 79",
                     valid_accel & missing_covars ~ "Missing covariates",
                     TRUE ~ NA_character_),
    mort_exc2 =
      case_when(valid_accel & age < 50 & age > 18 ~ "Younger than 50",
                valid_accel & age >= 80 ~ "Older than 79",
                valid_accel & missing_covars ~ "Missing covariates",
                valid_accel & mortstat == 1 ~ "Died by end of follow up",
                TRUE ~ NA_character_),
         mort_analysis = valid_accel & age >= 50 & age < 80 & !missing_covars)



consort_plot(r2,
             orders = c(id      = 'NHANES Study Population',
                        wave = "",
                        no_accel    = "Excluded",
                        has_accel    = 'Received accelerometer',
                        invalid_accel = 'Excluded: insufficient valid wear time',
                        valid_accel = 'Valid accelerometry',
                        younger_18 = "Excluded",
                        accel_analysis = "Accelerometry analysis\nassessed",
                        mort_exc = 'Excluded sequentially',
                        mort_analysis = 'Mortality analysis\nassessed'),
                        # mort_analysis = paste('Mortality analysis\nassessed\nDeaths:', sum(r2$num_died))),  # Add death count),
             side_box = c('no_accel', 'invalid_accel', 'younger_18', 'mort_exc'),
             allocation = 'wave')
             # labels=c('1'='Screening', '3'='Consent', '4'='Randomization', '6'='Follow-up'))


########## table 1
# nice labels for table
labs = c("SEQN", "Data release cycle",
         "Interview Examination Status",
         "Sex", "Age (yrs)", "Age (mos)", "Race/ethnicity",
         "Six month time period",
         "Educ. level adults", "Marital status",
         "2 yr int weight", "2 yr exam weight", "Pseudo PSU",
         "Psueudo stratum",
         "Annual HH income",
         "Weight (kg)", "Height (cm)", "BMI (kg/m2)",
         "Overweight", "Diabetes orig.", "Diabetes", "Arthritis",
         "Coronary Heart Failure", "Congenital Heart Disease", "Angina",
         "Heart attack", "Stroke", "Cancer", colnames(df_all)[29:33],
         "Alcohol use", "BMI Category", "Smoking status", "Mobility problem", "General health condition",
         "Eligbility", "Died by 5 years follow up", "COD", "COD Diabetes", "COD Hypertension",
         "Person-months follow up from interview", "Person-months follow up from exam", "Eligibility category", "COD category",  "AC", "MIMS",
         "Actilife steps", "ADEPT steps",
         "log10 AC", "log10 MIMS", "Oak steps",
         "Sc. RF steps", "Sc. SSL steps",
         "Verisense rev. steps", "Verisense steps",
         "Peak1 AC", "Peak1 MIMS",
         "Peak1 Actilife steps", "Peak1 ADEPT steps",
         "Peak1 log10 AC", "Peak1 log10 MIMS", "Peak1 Oak steps",
         "Peak1 Sc. RF steps", "Peak1 Sc. SSL steps",
         "Peak1 Verisense rev. steps", "Peak1 Verisense steps",
         "Peak30 AC", "Peak30 MIMS",
         "Peak30 Actilife steps", "Peak30 ADEPT steps",
         "Peak30 log10 AC", "Peak30 log10 MIMS", "Peak30 Oak steps",
         "Peak30 Sc. RF steps", "Peak30 Sc. SSL steps",
         "Peak30 Verisense rev. steps", "Peak30 Verisense steps", "No. valid days", "Received accelerometer",
         "Valid accelerometry", "Inclusion category", "Education level")
names(labs) = colnames(df_accel)

df_tab1 =
  df_all %>%
  labelled::set_variable_labels(!!!labs)

# survey weighted table
df_svy =
  df_tab1 %>%
  filter(has_accel) %>%
  filter(valid_accel) %>%
  filter(age_in_years_at_screening >= 18) %>%
  select(
    gender,
    age_in_years_at_screening,
    race_hispanic_origin,
    cat_education,
    cat_bmi,
    bin_diabetes,
    chf,
    chd,
    stroke,
    cat_alcohol,
    cat_smoke,
    bin_mobilityproblem,
    general_health_condition,
    mortstat,
    data_release_cycle,
    masked_variance_pseudo_psu, masked_variance_pseudo_stratum,
    full_sample_2_year_mec_exam_weight
  )  %>%
  mutate(WTMEC4YR = full_sample_2_year_mec_exam_weight/2,
         WTMEC4YR_norm = WTMEC4YR/mean(WTMEC4YR, na.rm = TRUE)) %>%
  select(-full_sample_2_year_mec_exam_weight) %>%
  svydesign(ids = ~masked_variance_pseudo_psu, weights = ~WTMEC4YR_norm,
            strata = ~masked_variance_pseudo_stratum, data=., nest=TRUE)

# survey weighted table
tab = df_svy %>%
  tbl_svysummary(
    by = data_release_cycle,
    include = -c(masked_variance_pseudo_psu, masked_variance_pseudo_stratum, WTMEC4YR,
                 WTMEC4YR_norm),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing",
  ) %>%
  add_overall() %>%
  modify_caption("Demographic Characteristics, All Adults"); tab


# survey weighted table, for latex
tab %>%
  as_gt() %>%
  gtsave(here::here("manuscript", "tables", "table2.docx"))

# unweighted table, for comparison
df_unwt =
  df_tab1 %>%
  filter(has_accel) %>%
  filter(valid_accel) %>%
  select(
    gender,
    age_in_years_at_screening,
    race_hispanic_origin,
    cat_education,
    cat_bmi,
    bin_diabetes,
    chf,
    chd,
    stroke,
    cat_alcohol,
    cat_smoke,
    bin_mobilityproblem,
    general_health_condition,
    mortstat,
    data_release_cycle)

df_unwt %>%
  tbl_summary(
    by = data_release_cycle,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing",
  ) %>%
  add_overall() %>%
  modify_caption("Demographic Characteristics, All Adults")


### table for supplement - only individuals included in mortality analysis
df_tab1 =
  df_all %>%
  labelled::set_variable_labels(!!!labs)
df_svy =
  df_tab1 %>%
  filter(has_accel) %>%
  filter(valid_accel) %>%
  filter(age_in_years_at_screening >= 50 & age_in_years_at_screening < 80) %>%
  select(
    gender,
    age_in_years_at_screening,
    race_hispanic_origin,
    cat_education,
    cat_bmi,
    bin_diabetes,
    chf,
    chd,
    stroke,
    cat_alcohol,
    cat_smoke,
    bin_mobilityproblem,
    general_health_condition,
    mortstat,
    data_release_cycle,
    masked_variance_pseudo_psu, masked_variance_pseudo_stratum,
    full_sample_2_year_mec_exam_weight
  )  %>%
  mutate(WTMEC4YR = full_sample_2_year_mec_exam_weight/2,
         WTMEC4YR_norm = WTMEC4YR/mean(WTMEC4YR, na.rm = TRUE)) %>%
  select(-full_sample_2_year_mec_exam_weight) %>%
  svydesign(ids = ~masked_variance_pseudo_psu, weights = ~WTMEC4YR_norm,
            strata = ~masked_variance_pseudo_stratum, data=., nest=TRUE)

# survey weighted table
tab = df_svy %>%
  tbl_svysummary(
    by = data_release_cycle,
    include = -c(masked_variance_pseudo_psu, masked_variance_pseudo_stratum, WTMEC4YR,
                 WTMEC4YR_norm),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing",
  ); tab

# survey weighted table, for latex
tab %>%
  kableExtra::kbl("latex", booktabs = TRUE)

tab = df_svy %>%
  tbl_svysummary(
    include = -c(masked_variance_pseudo_psu, masked_variance_pseudo_stratum, WTMEC4YR,
                 WTMEC4YR_norm),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing",
  ); tab

tab %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# supplement - non survey weighted mortality characteristics
df_unwt =
  df_tab1 %>%
  filter(has_accel) %>%
  filter(valid_accel) %>%
  filter(age_in_years_at_screening < 80 & age_in_years_at_screening >= 50) %>%
  select(
    gender,
    age_in_years_at_screening,
    race_hispanic_origin,
    cat_education,
    cat_bmi,
    bin_diabetes,
    chf,
    chd,
    stroke,
    cat_alcohol,
    cat_smoke,
    bin_mobilityproblem,
    general_health_condition,
    mortstat,
    data_release_cycle)

tab = df_unwt %>%
  tbl_summary(
    by = data_release_cycle,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing",
  ) %>%
  add_overall() %>%
  modify_caption("Demographic Characteristics, All Adults"); tab
tab %>%
  kableExtra::kbl("latex", booktabs = TRUE)

###### table 2: means/sds of PA variables

df_adult = df_accel %>%
  filter(age_in_years_at_screening >= 18) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2,
         weight_norm = weight / mean(weight)) %>%
  mutate(group = "All adults")

df_old = df_accel %>%
  filter(age_in_years_at_screening >= 50) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2,
         weight_norm = weight / mean(weight)) %>%
  mutate(group = "50+")


df = bind_rows(df_adult, df_old)

df = df %>%
  rename(Verisense = "total_vssteps",
         "Verisense rev" = "total_vsrevsteps",
         Oak = "total_oaksteps",
         ADEPT = "total_adeptsteps",
         SCRF =  "total_scrfsteps",
         SCSSL =  "total_scsslsteps",
         MIMS = "total_PAXMTSM",
         Actilife = "total_actisteps",
         log10MIMS = "total_log10PAXMTSM",
         log10AC = "total_log10AC",
         AC = "total_AC") %>%
  select(!contains("peak"))

df_analysis_svy = survey::svydesign(
  id = ~ masked_variance_pseudo_psu,
  strata = ~ masked_variance_pseudo_stratum,
  weights = ~ weight_norm,
  data = df,
  nest = TRUE
)


tab = df_analysis_svy %>%
  tbl_strata(
    strata = data_release_cycle,
    .tbl_fun =
      ~ .x %>%
      tbl_svysummary(
        .,
        include = c(
          contains("Verisense"),
          contains("AC", ignore.case = F),
          contains("Oak"),
          contains("ADEPT"),
          contains("SCRF"),
          contains("SCSSL"),
          contains("MIMS"),
          contains("actilife")
        ),
        by = group,
        statistic = list(
          all_continuous() ~ "{mean} ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        )
      ),
    .header = "**Wave {strata}**, N = {n}"
  ) %>%
  modify_caption("Physical Activity Mean Totals Stratified by Age and Wave"); tab

# for latex
tab %>%
  kableExtra::kbl("latex", booktabs = TRUE)

tab %>%
  as_gt() %>%
  gtsave(here::here("manuscript", "tables", "table3.docx"))


## unweighted, for comparison
df %>%
  tbl_strata(
    strata = data_release_cycle,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        .,
        include = c(
          contains("Verisense"),
          contains("AC", ignore.case = F),
          contains("Oak"),
          contains("ADEPT"),
          contains("SCRF"),
          contains("SCSSL"),
          contains("MIMS"),
          contains("actilife")
        ),
        by = group,
        statistic = list(
          all_continuous() ~ "{mean} ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        )
      ),
    .header = "**Wave {strata}**, N = {n}"
  ) %>%
  modify_caption("Physical Activity Mean Totals Stratified by Age and Wave - Unweighted")


# diffs

get_pct_diff = function(x, y){
  abs(x-y)/(mean(c(x,y)))*100
}

x = c(11195, 12169, 2342, 2659, 10254, 11794, 9888,11509, 8358, 9144, 8337, 9497, 7532, 9102,
      12435, 13572, 960, 998, 2333927, 2573262, 2878, 2955)
y = c(10850, 11902,2151,2453, 9733,11381, 9502, 11263,8027,8846, 7974,9163,  7122, 8725,12243, 13467,952,994, 2291942,2549846, 2847, 2933)
map2(.x = x, .y = y, .f = get_pct_diff)
get_pct_diff(11195, 12169)

### univariate model summaries
# label variables
vlabs = c(
  "Age (yrs)",
  "Diabetes",
  "Mobility problem",
  "Cancer",
  "Alcohol use",
  "BMI Category",
  "Education category",
  "Smoking status",
  "Congenital Heart Disease",
  "Coronary Heart Failure",
  "Gender",
  "General health condition",
  "Heart attack",
  "Peak1 AC",
  "Peak1 Actilife steps",
  "Peak1 ADEPT steps",
  "Peak1 log10 AC",
  "Peak1 log10 MIMS",
  "Peak1 Oak steps",
  "Peak1 MIMS",
  "Peak1 Stepcount RF steps",
  "Peak1 Stepcount SSL steps",
  "Peak1 Verisense rev. steps",
  "Peak1 Verisense steps",
  "Peak30 AC",
  "Peak30 Actilife steps",
  "Peak30 ADEPT steps",
  "Peak30 log10 AC",
  "Peak30 log10 MIMS",
  "Peak30 Oak steps",
  "Peak30 MIMS",
  "Peak30 Stepcount RF steps",
  "Peak30 Stepcount SSL steps",
  "Peak30 Verisense rev. steps",
  "Peak30 Verisense steps",
  "Race/ethnicity",
  "Stroke",
  "AC",
  "Actilife steps",
  "ADEPT steps",
  "log10 AC",
  "log10 MIMS",
  "Oak steps",
  "MIMS",
  "Stepcount RF steps",
  "Stepcount SSL steps",
  "Verisense rev. steps",
  "Verisense steps"
)

data_summary = wt_single %>%
  group_by(variable) %>%
  summarize(across(concordance, list(
    mean = ~ mean(.x), se = ~ sd(.x) / sqrt(n())
  ))) %>%
  mutate(variable_fac = factor(
    variable,
    labels = vlabs
  )) %>%
  filter(!grepl("peak", variable))

df_means =
  df_mortality_win %>%
  group_by(mortstat) %>%
  summarize(across(c(contains("total"), contains("peak"), contains("age")), ~ mean(.x)),
            across(c(bin_mobilityproblem, cancer, stroke, heartattack, chd, chf, bin_diabetes), ~sum(.x)/n()*100),
            gender = sum(gender == "Female")/n()*100) %>%
  pivot_longer(cols = -mortstat) %>%
  pivot_wider(names_from = mortstat,
              values_from = value,
              names_prefix = 'died_') %>%
  rename(variable = name)

# get the best variable based on concodance
best_var = data_summary %>%
  arrange(desc(concordance_mean)) %>%
  slice(1) %>%
  pull(variable)

best_var_vec = wt_single %>%
  filter(variable == best_var) %>%
  pull(concordance)

t_tests = wt_single %>%
  group_by(variable) %>%
  filter(variable != best_var) %>%
  nest() %>%
  mutate(t_test = map(
    data,
    ~ t.test(
      .x$concordance,
      best_var_vec,
      var.eq = FALSE,
      paired = FALSE,
      alternative = "less"
    )
  ),
  res = map(t_test, tidy)) %>%
  unnest(res) %>%
  ungroup() %>%
  select(variable, p.value)

# join t tests with summary data and make table
tab = data_summary %>%
  left_join(t_tests) %>%
  left_join(df_means) %>%
  arrange(desc(concordance_mean)) %>%
  mutate(
    variable_fac = forcats::fct_reorder(variable_fac, concordance_mean),
    lb = concordance_mean - 1.96 * concordance_se,
    ub = concordance_mean + 1.96 * concordance_se,
    ci = paste0(
      sprintf("%0.3f", round(concordance_mean, 3)),
      " (",
      sprintf("%0.3f", round(lb, 3)),
      ", ",
      sprintf("%0.3f", round(ub, 3)),
      ")"
    )
  ) %>%
  select(variable_fac,
         ci,
         p.value,
         total_alive = died_0,
         total_deceased = died_1) %>%
  mutate(pvalue = style_pvalue(p.value, digits = 3)) %>%
  mutate(pvalue = ifelse(is.na(pvalue), "---", pvalue),
         across(starts_with("mean"), ~ if_else(is.na(.x), "---", as.character(round(
           .x, 0
         ))))) %>%
  rename(
    Variable = variable_fac,
    "Mean (95% CI)" = ci,
    "p-value" = pvalue,
    "Mean value among alive" = total_alive,
    "Mean value among deceased" = total_deceased
  ) %>%
  gt::gt() %>%
  gt::fmt_number(columns = starts_with("mean"), decimals = 0) %>%
  gt::tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = everything(), rows = p.value > 0.05 |
                             is.na(p.value))
  ) %>%
  gt::cols_hide(p.value) %>%
  tab_header(title = "100x 10-fold Cross-Validated Single Variable Concordance", subtitle = "") %>%
  tab_footnote(footnote = "p-value for one sided t-test comparing variable to variable with higest mean concordance",
               locations = cells_column_labels(columns = "p-value")) %>%
  cols_align(columns = everything(), align = "left"); tab

# without p value
tab = data_summary %>%
  left_join(t_tests) %>%
  left_join(df_means) %>%
  arrange(desc(concordance_mean)) %>%
  mutate(
    variable_fac = forcats::fct_reorder(variable_fac, concordance_mean),
    lb = concordance_mean - 1.96 * concordance_se,
    ub = concordance_mean + 1.96 * concordance_se,
    ci = sprintf("%0.3f", round(concordance_mean, 3))) %>%
  select(variable_fac,
         ci,
         total_alive = died_0,
         total_deceased = died_1) %>%
  rename(
    Variable = variable_fac,
    "Mean (95% CI)" = ci,
    "Mean value among alive" = total_alive,
    "Mean value among deceased" = total_deceased
  ) %>%
  gt::gt() %>%
  gt::fmt_number(columns = starts_with("mean"), decimals = 0) %>%
  tab_header(title = "100x 10-fold Cross-Validated Single Variable Concordance", subtitle = "") %>%
  tab_footnote(footnote = "p-value for one sided t-test comparing variable to variable with higest mean concordance",
               locations = cells_column_labels(columns = "p-value")) %>%
  cols_align(columns = everything(), align = "left"); tab

# for latex
tab %>%
  as.data.frame() %>%
  select(-p.value) %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# for latex
tab %>%
  as.data.frame() %>%
  select(-p.value) %>%
  kableExtra::kbl("latex", booktabs = TRUE)



### supplemental tables with sensitivity

wt_single_summ =
  wt_single %>%
  # filter(!grepl("peak", variable)) %>%
  group_by(variable) %>%
  summarize(concordance = mean(concordance))

sens =
  readRDS(here::here("results", "metrics_wtd_100_singlevar_sens.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  group_by(variable) %>%
  summarize(concordance_sens = mean(concordance))

sens80 =
  readRDS(here::here("results", "metrics_wtd_100_singlevar_80.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  group_by(variable) %>%
  summarize(concordance_80 = mean(concordance))

tab =
  wt_single_summ %>%
  left_join(sens, by = "variable") %>%
  left_join(sens80, by = "variable") %>%
  arrange(desc(concordance)) %>%
  mutate(variable_fac = factor(
    variable,
    labels = vlabs
  )) %>%
  filter(!grepl("peak", variable)) %>%
  rename(
    Variable = variable_fac,
    Concordance = concordance,
    "Concordance, >= 1 Valid Day" = concordance_sens,
    "Concordance, inc. 80" = concordance_80
  ) %>%
  select(Variable, contains("concordance")) %>%
  gt::gt() %>%
  gt::fmt_number(columns = contains("concordance"), decimals = 3) %>%
  cols_align(columns = everything(), align = "left"); tab

# for latex
tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)


# include cadence values
data_summary = wt_single %>%
  group_by(variable) %>%
  summarize(across(concordance, list(
    mean = ~ mean(.x), se = ~ sd(.x) / sqrt(n())
  ))) %>%
  mutate(variable_fac = factor(
    variable,
    labels = vlabs
  ))

# get the best variable based on concodance
best_var = data_summary %>%
  arrange(desc(concordance_mean)) %>%
  slice(1) %>%
  pull(variable)

best_var_vec = wt_single %>%
  filter(variable == best_var) %>%
  pull(concordance)

t_tests = wt_single %>%
  group_by(variable) %>%
  filter(variable != best_var) %>%
  nest() %>%
  mutate(t_test = map(
    data,
    ~ t.test(
      .x$concordance,
      best_var_vec,
      var.eq = FALSE,
      paired = FALSE,
      alternative = "less"
    )
  ),
  res = map(t_test, tidy)) %>%
  unnest(res) %>%
  ungroup() %>%
  select(variable, p.value)

# join t tests with summary data and make table
tab = data_summary %>%
  left_join(t_tests) %>%
  left_join(df_means) %>%
  arrange(desc(concordance_mean)) %>%
  mutate(
    variable_fac = forcats::fct_reorder(variable_fac, concordance_mean),
    lb = concordance_mean - 1.96 * concordance_se,
    ub = concordance_mean + 1.96 * concordance_se,
    ci = paste0(
      sprintf("%0.3f", round(concordance_mean, 3)),
      " (",
      sprintf("%0.3f", round(lb, 3)),
      ", ",
      sprintf("%0.3f", round(ub, 3)),
      ")"
    )
  ) %>%
  select(variable_fac,
         ci,
         p.value,
         total_alive = died_0,
         total_deceased = died_1) %>%
  mutate(pvalue = style_pvalue(p.value, digits = 3)) %>%
  mutate(pvalue = ifelse(is.na(pvalue), "---", pvalue),
         across(starts_with("mean"), ~ if_else(is.na(.x), "---", as.character(round(
           .x, 0
         ))))) %>%
  rename(
    Variable = variable_fac,
    "Mean (95% CI)" = ci,
    "p-value" = pvalue,
    "Mean value among alive" = total_alive,
    "Mean value among deceased" = total_deceased
  ) %>%
  gt::gt() %>%
  gt::fmt_number(columns = starts_with("mean"), decimals = 0) %>%
  gt::tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = everything(), rows = p.value > 0.05 |
                             is.na(p.value))
  ) %>%
  gt::cols_hide(p.value) %>%
  tab_header(title = "100x 10-fold Cross-Validated Single Variable Concordance", subtitle = "") %>%
  tab_footnote(footnote = "p-value for one sided t-test comparing variable to variable with higest mean concordance",
               locations = cells_column_labels(columns = "p-value")) %>%
  cols_align(columns = everything(), align = "left"); tab

# for latex
tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# calculate some other stats to manually add to table

df_mortality_win %>%
  group_by(mortstat) %>%
  count(cat_bmi) %>%
  mutate(pct = n / sum(n) * 100)

df_mortality_win %>%
  group_by(mortstat) %>%
  count(general_health_condition) %>%
  mutate(pct = n / sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = mortstat, values_from = pct)

df_mortality_win %>%
  group_by(mortstat) %>%
  count(cat_smoke) %>%
  mutate(pct = n / sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = mortstat, values_from = pct)

df_mortality_win %>%
  group_by(mortstat) %>%
  count(cat_alcohol) %>%
  mutate(pct = n / sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = mortstat, values_from = pct)

df_mortality_win %>%
  group_by(mortstat) %>%
  count(cat_education) %>%
  mutate(pct = n / sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = mortstat, values_from = pct)

df_mortality_win %>%
  group_by(mortstat) %>%
  count(race_hispanic_origin) %>%
  mutate(pct = n / sum(n) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = mortstat, values_from = pct)
### multivariable model summaries
wt_small =
  wt_all %>%
  filter(model %in% c("Demo only", "scrfsteps+PAXMTSM", "scrfsteps", "PAXMTSM"))

summary =
  wt_small %>%
  group_by(model) %>%
  summarize(across(concordance, list(mean = ~mean(.x),
                                     se = ~sd(.x)/sqrt(n())))) %>%
  mutate(model_fac = factor(model, labels = c("Traditional predictors only",
                                              "Traditional predictors+ MIMS",
                                              "Traditional predictors+ SCRF steps",
                                              "Traditional predictors+ SCRF steps + MIMS")))
# get coefficients from steps model
coef_scrf =
  df_mortality_win %>%
  mutate(
    weight = full_sample_2_year_mec_exam_weight / 2,
    weight_norm = weight / mean(weight)) %>%
  coxph(Surv(event_time, mortstat) ~ total_scrfsteps +
          age_in_years_at_screening + cat_education + bin_diabetes + cat_bmi +
          race_hispanic_origin + chf + general_health_condition + chd +
          heartattack + cancer + stroke + cat_alcohol + cat_smoke +
          bin_mobilityproblem, weights = weight_norm, data  = .) %>%
  tidy() %>%
  select(term, estimate, std.error, robust.se, p.value) %>%
  filter(grepl("steps", term)) %>%
  mutate(hr = exp(estimate * 500),
         lb = exp((estimate - 1.96 * std.error) * 500),
         ub = exp((estimate + 1.96 * std.error) * 500),
         model = "scrfsteps")
# coef from MIMS model
coef_mims =
  df_mortality_win %>%
  mutate(
    weight = full_sample_2_year_mec_exam_weight / 2,
    weight_norm = weight / mean(weight)) %>%
  coxph(Surv(event_time, mortstat) ~ total_PAXMTSM + total_scrfsteps +
          age_in_years_at_screening + cat_education + bin_diabetes + cat_bmi +
          race_hispanic_origin + chf + general_health_condition + chd +
          heartattack + cancer + stroke + cat_alcohol + cat_smoke +
          bin_mobilityproblem, weights = weight_norm, data  = .) %>%
  tidy() %>%
  select(term, estimate, std.error, robust.se, p.value) %>%
  filter(grepl("steps", term)) %>%
  mutate(hr = exp(estimate * 500),
         lb = exp((estimate - 1.96 * std.error) * 500),
         ub = exp((estimate + 1.96 * std.error) * 500),
         model = "scrfsteps+PAXMTSM")

coefs = bind_rows(coef_scrf, coef_mims)
# print summary
tab = summary %>%
  left_join(coefs)  %>%
  mutate(conc_lb = concordance_mean - 1.96 * concordance_se,
         conc_ub = concordance_mean + 1.96 * concordance_se,
         conc_ci = paste0(sprintf("%0.3f",round(concordance_mean, 3)),
                          " (", sprintf("%0.3f",round(conc_lb, 3)), ", ", sprintf("%0.3f",round(conc_ub, 3)), ")"),
         hr_ci = case_when(!is.na(hr) ~ paste0(sprintf("%0.3f",round(hr, 3)),
                                               " (", sprintf("%0.3f",round(lb, 3)), ", ", sprintf("%0.3f",round(ub, 3)), ")"),
                           TRUE ~ "---"),
         p.value = format.pval(p.value, digits = 3)) %>%
  select(model_fac, hr_ci,p.value, conc_ci) %>%
  rename(Model = model_fac,
         "Steps HR" = hr_ci,
         "Steps p-value" = p.value,
         "Model concordance" = conc_ci) %>%
  gt::gt() %>%
  # gt::fmt_number(columns = starts_with("mean"), decimals = 0) %>%
  tab_header(title = "Multivariable Model Comparison") %>%
  cols_align(columns = everything(), align = "left"); tab

# for latex
tab %>%
  data.frame() %>%
  kableExtra::kbl(format = "latex")

tab %>%
  gtsave(here::here("manuscript", "tables", "table4.docx"))


### scaled and raw hazard ratios table
# labels df
var_labels =
  tibble(names =
           unique(wt_single$variable),
         labels = c("Age at screening", "Diabetes", "Mobility problem",
                    "Cancer", "Alcohol use",
                    "BMI Category", "Education level", "Smoking status",
                    "CHF", "CHD", "Gender", "Self-reported health", "Heart attack", "Peak1 AC", "Peak1 MIMS", "Peak1 Actilife steps",
                    "Peak1 ADEPT steps", "Peak1 log10 AC", "Peak1 log10 MIMS", "Peak1 Oak steps",
                    "Peak1 Stepcount RF steps", "Peak1 Stepcount SSL steps", "Peak1 Verisense rev. steps",
                    "Peak1 Verisense steps",
                    "Peak30 AC", "Peak30 MIMS", "Peak30 Actilife steps",
                    "Peak30 ADEPT steps", "Peak30 log10 AC", "Peak30 log10 MIMS", "Peak30 Oak steps",
                    "Peak30 Stepcount RF steps", "Peak30 Stepcount SSL steps", "Peak30 Verisense rev. steps",
                    "Peak30 Verisense steps",
                    "Race/ethnicity", "Stroke",
                    "AC", "MIMS", "Actilife steps",
                    "ADEPT steps", "log10 AC", "log10 MIMS", "Oak steps",
                    "Stepcount RF steps", "Stepcount SSL steps", "Verisense rev. steps",
                    "Verisense steps"))

# get PA variables
pa_vars = df_mortality_win %>%
  select(contains("total") & contains("steps")) %>%
  colnames()

# create DF that is scaled steps
df_mortality_win_scaled =
  df_mortality_win %>%
  mutate(across(c(contains("total")), ~scale(.x)))

# get standard deviations
sds =
  df_mortality_win %>%
  summarize(across(c(contains("total")), ~sd(.x)/1000)) %>%
  pivot_longer(cols = contains("total")) %>%
  mutate(value = round(value, digits = 1),
         value = sprintf("%.1f",value)) %>%
  left_join(var_labels, by = c("name" = "names"))

# function to fit model on all data w/ all covariates
fit_model = function(var, df){

  df = df %>%
    mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))



  formula = as.formula(paste0("Surv(event_time, mortstat) ~", var, "+
      age_in_years_at_screening + cat_education + bin_diabetes + cat_bmi +
      race_hispanic_origin +
      gender +
      cat_bmi +
      cat_education +
      chf +
      chd +
      heartattack +
      stroke +
      cancer +
      bin_diabetes +
      cat_alcohol +
      cat_smoke +
      bin_mobilityproblem +
      general_health_condition"))
  coxph(formula, data = df, weights = weight_norm) %>%
    tidy() %>%
    filter(grepl(var, term))

}

steps_res =
  map_dfr(.x = pa_vars,
          .f = fit_model,
          df = df_mortality_win) %>%
  mutate(hr = exp(estimate * 500),
         ci_low = exp(500*(estimate - (1.96 * std.error))),
         ci_high = exp(500*(estimate + (1.96 * std.error))))

steps_res_raw =
  map_dfr(.x = pa_vars,
          .f = fit_model,
          df = df_mortality_win)

steps_res_raw %>%
  mutate(step_inc = log(0.88)/estimate)


steps_res_scaled =
  map_dfr(.x = c(pa_vars, "total_PAXMTSM", "total_AC"),
          .f = fit_model,
          df = df_mortality_win_scaled) %>%
  mutate(hr = exp(estimate),
         ci_low = exp(estimate - (1.96 * std.error)),
         ci_high = exp(estimate + (1.96 * std.error))) %>%
  left_join(var_labels, by = c("term" = "names"))


tab = steps_res %>%
  select(term, hr, p.value, ci_low, ci_high) %>%
  mutate(type = "raw") %>%
  bind_rows(steps_res_scaled %>% select(term, hr, p.value, ci_low, ci_high) %>% mutate(type = "scaled")) %>%
  pivot_wider(names_from = type, values_from = -term) %>%
  filter(grepl("step", term)) %>%
  left_join(sds, by = c("term" = "name")) %>%
  arrange(desc(hr_scaled)) %>%
  mutate(variable_fac = forcats::fct_reorder(term, hr_scaled, mean),
         ci_raw = paste0(sprintf("%0.2f",round(hr_raw, 2)),
                         " (", sprintf("%0.2f",round(ci_low_raw, 2)), ", ", sprintf("%0.2f",round(ci_high_raw, 2)), ")"),
         ci_sc = paste0(sprintf("%0.2f",round(hr_scaled, 2)),
                        " (", sprintf("%0.2f",round(ci_low_scaled, 2)), ", ", sprintf("%0.2f",round(ci_high_scaled, 2)), ")")) %>%
  select("Variable" = variable_fac, "HR, Raw" = ci_raw, "HR, Scaled" = ci_sc, "SD" = value) %>%
  gt::gt() %>%
  cols_align(columns = everything(), align = "left"); tab

tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)

tab %>%
  gtsave(here::here("manuscript", "tables", "table5.docx"))


## supplement multivariable model summaries
wt_all_summ =
  wt_all %>%
  # filter(!grepl("peak", variable)) %>%
  group_by(model) %>%
  summarize(concordance = mean(concordance)) %>%
  filter(!grepl("\\+", model))

sens =
  readRDS(here::here("results", "metrics_wtd_100_sens.rds")) %>%
  filter(!grepl("\\+", model)) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  group_by(model) %>%
  summarize(concordance_sens = mean(concordance))

sens80 =
  readRDS(here::here("results", "metrics_wtd_100_80.rds")) %>%
  filter(!grepl("\\+", model)) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  group_by(model) %>%
  summarize(concordance_sens80 = mean(concordance))

tab =
  wt_all_summ %>%
  left_join(sens, by = "model") %>%
  left_join(sens80, by = "model") %>%
  arrange(desc(concordance)) %>%
  rename(
    Model = model,
    Concordance = concordance,
    "Concordance, >= 1 Valid Day" = concordance_sens,
    "Concordance, inc. 80 yo" = concordance_sens80
  ) %>%
  select(Model, contains("conc")) %>%
  arrange(desc(Concordance)) %>%
  gt::gt() %>%
  gt::fmt_number(columns = contains("concordance"), decimals = 3) %>%
  cols_align(columns = everything(), align = "left"); tab


# for latex
tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# add in hazard ratios assoc. w/ 500 extra steps


steps_res =
  map_dfr(.x = pa_vars,
          .f = fit_model,
          df = df_mortality_win) %>%
  mutate(hr = exp(estimate * 500),
         ci_low = exp(500*(estimate - (1.96 * std.error))),
         ci_high = exp(500*(estimate + (1.96 * std.error))))

df_sens =
  df_all %>%
  filter(num_valid_days > 0) %>%
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80) %>%
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          race_hispanic_origin, cat_education,
                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                          general_health_condition, mortstat, permth_exm),
                ~!is.na(.x))) %>%
  mutate(event_time = permth_exm / 12) %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~DescTools::Winsorize(.x, quantile(.x, probs = c(0, 0.99)))))

df_80 =
  df_all %>%
  filter(num_valid_days >= 3) %>%
  filter(age_in_years_at_screening >= 50) %>%
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          race_hispanic_origin, cat_education,
                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                          general_health_condition, mortstat, permth_exm),
                ~!is.na(.x))) %>%
  mutate(event_time = permth_exm / 12) %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~DescTools::Winsorize(.x, quantile(.x, probs = c(0, 0.99)))))

steps_res_sens =
  map_dfr(.x = pa_vars,
          .f = fit_model,
          df = df_sens) %>%
  mutate(hr = exp(estimate * 500),
         ci_low = exp(500*(estimate - (1.96 * std.error))),
         ci_high = exp(500*(estimate + (1.96 * std.error)))) %>%
  mutate(term = sub(".*total\\_", "", term))



steps_res_80 =
  map_dfr(.x = pa_vars,
          .f = fit_model,
          df = df_80) %>%
  mutate(hr = exp(estimate * 500),
         ci_low = exp(500*(estimate - (1.96 * std.error))),
         ci_high = exp(500*(estimate + (1.96 * std.error)))) %>%
  mutate(term = sub(".*total\\_", "", term))


tab =
  wt_all_summ %>%
  left_join(sens, by = "model") %>%
  left_join(sens80, by = "model") %>%
  left_join(steps_res %>% mutate(term = sub(".*total\\_", "", term)) %>% select(term, hr),
            by = c("model" = "term")) %>%
  left_join(steps_res_sens %>% select(term, hr_sens = hr),
            by = c("model" = "term")) %>%
  left_join(steps_res_80 %>% select(term, hr_80 = hr),
            by = c("model" = "term"))  %>%
  arrange(desc(concordance)) %>%
  select(model, contains("conc"), contains("hr")) %>%
  arrange(desc(concordance)) %>%
  gt::gt() %>%
  gt::fmt_number(columns = c(contains("concordance"), contains("hr")), decimals = 3) %>%
  cols_align(columns = everything(), align = "left"); tab

tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)



###################
# additional sensitivity analysis tables

wt_all = readRDS(here::here("results", "metrics_wtd_100.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup()
wt_old = readRDS(here::here("results", "metrics_wtd_100_older.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarize(concordance_older = mean(concordance))


wt_young = readRDS(here::here("results", "metrics_wtd_100_younger.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarize(concordance_younger = mean(concordance))

wt_lim = readRDS(here::here("results", "metrics_wtd_100_limmob.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarize(concordance_lim = mean(concordance))


wt_unlim = readRDS(here::here("results", "metrics_wtd_100_unlmob.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarize(concordance_unlmob = mean(concordance))

wt_bad = readRDS(here::here("results", "metrics_wtd_100_badhealth.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarize(concordance_bad = mean(concordance))


wt_good = readRDS(here::here("results", "metrics_wtd_100_goodhealth.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarize(concordance_good = mean(concordance))

wt_all_summ =
  wt_all %>%
  group_by(model) %>%
  summarize(concordance = mean(concordance)) %>%
  filter(!grepl("\\+", model))

# covariate/pa df
df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

# get n and # events in each category
df_mortality =
  df_all %>%
  filter(valid_accel) %>% # valid accelerometry
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80) %>%  # age criteria
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          race_hispanic_origin, cat_education,
                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                          general_health_condition, mortstat, permth_exm, total_PAXMTSM),
                ~!is.na(.x)))

summary = df_mortality %>%
  select(mortstat, age_in_years_at_screening, general_health_condition, bin_mobilityproblem) %>%
  mutate(all = TRUE,
         older = age_in_years_at_screening > 62,
         younger = age_in_years_at_screening <= 62,
         goodhealth = general_health_condition %in% c("Excellent", "Very good", "Good"),
         badhealth = !(general_health_condition %in% c("Excellent", "Very good", "Good")),
         mobilitylim = bin_mobilityproblem == 1,
         nolim = bin_mobilityproblem != 1) %>%
  summarize(across(all:nolim, list(n = ~sum(.x),
                                   events = ~sum(mortstat[.x])))) %>%
  pivot_longer(cols = everything()) %>%
  mutate(meas = sub(".*\\_", "", name),
         name = sub("\\_.*", "", name)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  rename(model = meas,
         overall = all)

tab =
  wt_all_summ %>%
  left_join(wt_old, by = "model") %>%
  left_join(wt_young, by = "model") %>%
  left_join(wt_good, by = "model") %>%
  left_join(wt_bad, by = "model") %>%
  left_join(wt_unlim, by = "model") %>%
  left_join(wt_lim, by = "model")  %>%
  arrange(desc(concordance)) %>%
  rename(older = concordance_older,
         younger = concordance_younger,
         goodhealth = concordance_good,
         badhealth = concordance_bad,
         mobilitylim = concordance_lim,
         nolim = concordance_unlmob) %>%
  # rename(
  #   Model = model,
  #   Concordance = concordance,
  #   "Age <=62" = concordance_younger,
  #   "Age >62" = concordance_older,
  #   "Good health" = concordance_good,
  #   "Bad health" = concordance_bad,
  #   "Mobility problem" = concordance_lim,
  #   "No mobility problem" = concordance_unlmob
  # ) %>%
  # select(Model, contains("conc")) %>%
  arrange(desc(concordance)) %>%
  select(model, concordance, younger, older, goodhealth, badhealth, nolim, mobilitylim) %>%
  filter(!is.na(younger)) %>%
  rename(overall = concordance) %>%
  bind_rows(summary) %>%
  gt::gt() %>%
  gt::fmt_number(columns = -model, decimals = 3) %>%
  cols_align(columns = everything(), align = "left"); tab

tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)


# single variable concordance
wt_all = readRDS(here::here("results", "metrics_wtd_100_singlevar.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup()
wt_old = readRDS(here::here("results", "metrics_wtd_100_singlevar_older.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarize(concordance_older = mean(concordance))


wt_young = readRDS(here::here("results", "metrics_wtd_100_singlevar_younger.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarize(concordance_younger = mean(concordance))

wt_lim = readRDS(here::here("results", "metrics_wtd_100_singlevar_limmob.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarize(concordance_lim = mean(concordance))


wt_unlim = readRDS(here::here("results", "metrics_wtd_100_singlevar_unlmob.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarize(concordance_unlmob = mean(concordance))

wt_bad = readRDS(here::here("results", "metrics_wtd_100_singlevar_badhealth.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarize(concordance_bad = mean(concordance))


wt_good = readRDS(here::here("results", "metrics_wtd_100_singlevar_goodhealth.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarize(concordance_good = mean(concordance))

wt_all_summ =
  wt_all %>%
  group_by(variable) %>%
  summarize(concordance = mean(concordance))


tab = wt_all_summ %>%
  left_join(wt_old, by = "variable") %>%
  left_join(wt_young, by = "variable") %>%
  left_join(wt_good, by = "variable") %>%
  left_join(wt_bad, by = "variable") %>%
  left_join(wt_unlim, by = "variable") %>%
  left_join(wt_lim, by = "variable")  %>%
  filter(grepl("step", variable) | grepl("PAXMT", variable) | grepl("AC", variable)) %>%
  filter(!grepl("peak", variable)) %>%
  arrange(desc(concordance)) %>%
  rename(model = variable) %>%
  rename(older = concordance_older,
         younger = concordance_younger,
         goodhealth = concordance_good,
         badhealth = concordance_bad,
         mobilitylim = concordance_lim,
         nolim = concordance_unlmob) %>%
  arrange(desc(concordance)) %>%
  select(model, concordance, younger, older, goodhealth, badhealth, nolim, mobilitylim) %>%
  filter(!is.na(younger)) %>%
  rename(overall = concordance) %>%
  bind_rows(summary) %>%
  gt::gt() %>%
  gt::fmt_number(columns = -model, decimals = 3) %>%
  cols_align(columns = everything(), align = "left"); tab

tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)


quartile_conc = readRDS(here::here("results", "metrics_wtd_100_singlevar_stepsquartile.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  dplyr::summarize(concordance = mean(concordance)) %>%
  ungroup() %>%
  group_by(variable) %>%
  dplyr::summarize(concordance = mean(concordance))

tab = quartile_conc %>%
  arrange(desc(concordance)) %>%
  gt::gt() %>%
  gt::fmt_number(columns = concordance, decimals = 3) %>%
  cols_align(columns = everything(), align = "left"); tab

tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)

