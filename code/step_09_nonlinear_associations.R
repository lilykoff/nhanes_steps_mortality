library(tidyverse)
library(tidymodels)
library(future)
library(censored)
library(furrr)
library(paletteer)
library(survival)
library(splines)
library(gt)
library(rms)
source(here::here("code", "utils.R"))


df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

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
                ~!is.na(.x))) %>% # no missing data
  mutate(event_time = permth_exm / 12) # event time in years = person months since exam / 12

df_mortality_win =
  df_mortality %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~winsorize(.x, quantile(.x, probs = c(0, 0.99)))))

# make categorical quartile variables for each step estimate
df = df_mortality_win %>%
  mutate(across(contains("total"),
                ~ factor(ntile(.x, 4)),
                .names = "{.col}_quartile")) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2, weight_norm = weight / mean(weight))

vars = df %>% select(contains("quartile")) %>% colnames()

# get multivariable regression estimates for quartile regression
res = map_dfr(.x = vars,
              .f = function(var){
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
              tidy(conf.int = TRUE, exponentiate = TRUE) %>%
              filter(grepl(var, term)) %>%
              select(term, estimate, p.value, conf.low, conf.high)
              })

vars2 = df %>% select(contains("total") & !contains("quartile")) %>% colnames()

# get median steps in each quartile and # events
get_median_and_events = function(var){
  df %>%
    select(variable = all_of(var), quartile = all_of(paste0(var, "_quartile")), mortstat) %>%
    group_by(quartile) %>%
    summarize(median = median(variable),
              n_events = sum(mortstat),
              n = n()) %>%
    ungroup() %>%
    mutate(name = sub(".*total\\_", "", var),
           quartile = as.character(quartile))
}

median_df = map_dfr(.x = vars2, .f = get_median_and_events)

tab = res %>%
  mutate(quartile = sub(".*_quartile", "", term),
         measure = sub(".*total\\_(.+)\\_quartile.*", "\\1", term)) %>%
  mutate(est = paste0(sprintf("%0.2f",round(estimate, 2)),
                               " (", sprintf("%0.2f",round(conf.low, 2)), ", ", sprintf("%0.2f",round(conf.high, 2)), ")"),
         p = if_else(p.value < 0.001, "<0.001", format.pval(p.value, digits = 3))) %>%
  select(quartile, measure, est, p) %>%
  full_join(median_df, by = c("quartile", "measure" = "name")) %>%
  arrange(measure, quartile) %>%
  mutate(across(c(est, p), ~if_else(is.na(.x), "[Reference]", .x))) %>%
  mutate(median = round(median, 0)) %>%
  filter(!grepl("AC", measure), !grepl("PAX", measure)) %>%
  select(measure, quartile, median, n_events, n, est, p) %>%
  group_by(measure) %>%
  gt::gt() %>%
  cols_align(columns = everything(), align = "left"); tab

tab %>%
  as.data.frame() %>%
  kableExtra::kbl("latex", booktabs = TRUE)

# paletteer_d("colorBlindness::SteppedSequential5Steps")

extra =
  median_df %>%
  select(quartile, name) %>%
  filter(quartile == "1") %>%
  mutate(estimate = 1,
         conf.low = 1,
         conf.high = 1) %>%
  rename(measure = name)

res %>%
  mutate(quartile = sub(".*_quartile", "", term),
         measure = sub(".*total\\_(.+)\\_quartile.*", "\\1", term)) %>%
  bind_rows(extra) %>%
  filter(!grepl("log10AC", term), !grepl("log10PAX", term)) %>%
  mutate(quartile = factor(quartile, levels = c("4", "3", "2", "1")),
         quartile2 = factor(quartile, levels = rev(levels(quartile))),
         measure = factor(measure, levels = c("actisteps", "adeptsteps",
                                              "oaksteps", "vssteps", "vsrevsteps", "scrfsteps", "scsslsteps",
                                              "AC", "PAXMTSM"),
                          labels = c("Actilife", "ADEPT", "Oak", "Verisense", "Verisense rev.", "Stepcount RF",
                                     "Stepcount SSL", "AC", "MIMS")),
         measure2 = factor(measure, rev(levels(measure)))) %>%
  filter(!is.na(measure2)) %>%
  ggplot(aes(y = estimate, x = measure2, color = quartile)) +
  geom_point(shape = 15, position = position_dodge(width = .5), size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,
                 position = position_dodge(width = .5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  coord_flip() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legent.title = element_text(size = 14)) +
  labs(y = "Adjusted Hazard Ratio (95% Confidence Interval)", x = "") +
  theme(legend.position = c(0.9, 0.8)) +
  scale_y_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1))+
  scale_color_manual(name = "Quartile",
                     values = c("4" = "#2C85B2",
                                "3" = "#85B22CFF",
                                "2" = "#B26F2CFF",
                                "1" = "#B22C2CFF"))
ggsave(
  here::here("manuscript", "figures", "step_quartiles.png"),
  dpi = 400,
  width = 10,
  height = 8
)

# steps per day in 1000 step increments, for scrf
df =
  df %>%
  mutate(cut_scrf = cut(total_scrfsteps, breaks = c(0, 2000, 4000, 6000, 8000, 10000, 300000),
                        include.lowest = TRUE))

summary_scrf =
  df %>%
  group_by(cut_scrf) %>%
  summarize(n = n(),
            n_events = sum(mortstat))

model =
  formula = as.formula(paste0("Surv(event_time, mortstat) ~", "cut_scrf", "+
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

result_scrf = coxph(formula, data = df, weights = weight_norm) %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("cut", term)) %>%
  select(term, estimate, p.value, conf.low, conf.high) %>%
  mutate(cut_scrf = factor(sub(".*cut\\_scrf", "", term))) %>%
  full_join(summary_scrf) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~if_else(is.na(.x), 1, .x))) %>%
  mutate(cut_scrf = factor(cut_scrf, levels = c("[0,2e+03]","(2e+03,4e+03]",
                                                 "(4e+03,6e+03]","(6e+03,8e+03]",
                                                 "(8e+03,1e+04]","(1e+04,3e+05]"),
                                                labels = c("[0-2]", "(2-4]", "(4-6]", "(6-8]", "(8-10]", ">10")))

# paletteer_d("ggthemr::flat")



result_scrf %>%
  mutate(cut_scrf2 = factor(cut_scrf, levels = rev(levels(cut_scrf)))) %>%
  ggplot(aes(x = estimate, y = cut_scrf2, color = cut_scrf)) +
  geom_point(shape = 15, size = 1.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = .7) +
  geom_vline(aes(xintercept = 1), linetype = "dashed") +
  theme(legend.position = "none") +
  labs(x = "Adjusted Hazard Ratio (95% Confidence Interval)", y = "Steps Category (x1000)") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  geom_text(aes(label= paste0("n = ", n, "\n", "Events = ", n_events), x = 1.2),
             size = 3, color = "black") +
  scale_color_manual(values = c("black", "#E74C3CFF", "#F39C12FF", "#9B59B6FF", "#3498DBFF", "#2ECC71FF"))

# repeat but for verisense rev
df =
  df %>%
  mutate(cut_vsrev = cut(total_vsrevsteps, breaks = c(0, 2000, 4000, 6000, 8000, 10000, 300000),
                        include.lowest = TRUE))

summary_vsrev =
  df %>%
  group_by(cut_vsrev) %>%
  summarize(n = n(),
            n_events = sum(mortstat))

model =
  formula = as.formula(paste0("Surv(event_time, mortstat) ~", "cut_vsrev", "+
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

result_vsrev = coxph(formula, data = df, weights = weight_norm) %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("cut", term)) %>%
  select(term, estimate, p.value, conf.low, conf.high) %>%
  mutate(cut_vsrev = factor(sub(".*cut\\_vsrev", "", term))) %>%
  full_join(summary_vsrev) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~if_else(is.na(.x), 1, .x))) %>%
  mutate(cut_vsrev = factor(cut_vsrev, levels = c("[0,2e+03]","(2e+03,4e+03]",
                                                "(4e+03,6e+03]","(6e+03,8e+03]",
                                                "(8e+03,1e+04]","(1e+04,3e+05]"),
                           labels = c("[0-2]", "(2-4]", "(4-6]", "(6-8]", "(8-10]", ">10")))

result_vsrev %>%
  mutate(cut_vsrev2 = factor(cut_vsrev, levels = rev(levels(cut_vsrev)))) %>%
  ggplot(aes(x = estimate, y = cut_vsrev2, color = cut_vsrev)) +
  geom_point(shape = 15, size = 1.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = .7) +
  geom_vline(aes(xintercept = 1), linetype = "dashed") +
  theme(legend.position = "none") +
  labs(x = "Adjusted Hazard Ratio (95% Confidence Interval)", y = "Steps (x1000) Estimated by Verisense rev") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  geom_text(aes(label= paste0("n = ", n, "\n", "Events = ", n_events), x = 1.2),
            size = 3, color = "black") +
  scale_color_manual(values = c("black", "#E74C3CFF", "#F39C12FF", "#9B59B6FF", "#3498DBFF", "#2ECC71FF"))

result_scrf %>%
  mutate(type = "Stepcount RF") %>%
  rename(yval = cut_scrf) %>%
  bind_rows(result_vsrev %>% mutate(type = "Verisense rev") %>% rename(yval = cut_vsrev)) %>%
  mutate(yval = fct_rev(factor(yval, levels = c("[0-2]", "(2-4]", "(4-6]", "(6-8]", "(8-10]", ">10"))),
         colorval = factor(yval, levels = c("[0-2]", "(2-4]", "(4-6]", "(6-8]", "(8-10]", ">10"))) %>%
  ggplot(aes(x = estimate, y = yval, color =colorval)) +
  geom_point(shape = 15, size = 1.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = .7) +
  geom_vline(aes(xintercept = 1), linetype = "dashed") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legent.title = element_text(size = 14),
        strip.text = element_text(size = 12)) +
  facet_wrap(.~type) +
  labs(x = "Adjusted Hazard Ratio (95% Confidence Interval)", y = "Steps (x1000)") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  geom_text(aes(label= paste0("n = ", n, "\n", "Events = ", n_events), x = 1.2),
            size = 3, color = "black") +
  scale_color_manual(values = c("black", "#E74C3CFF", "#F39C12FF", "#9B59B6FF", "#3498DBFF", "#2ECC71FF"))


# restricted cubic spline knots
knot_vars = df %>%
  select(contains("total") & contains("steps") & !contains("quartile") & !contains("adept")) %>%
  # select(contains("total") & contains("steps") & !contains("quartile")) %>%
  colnames()

means = df %>%
  slice(1) %>%
  select(age_in_years_at_screening,
         cat_education,
         bin_diabetes,
         cat_alcohol,
         cat_bmi,
         chf,
         chd,
         stroke,
         cancer,
         bin_diabetes,
         cat_smoke,
         bin_mobilityproblem,
         general_health_condition,
         heartattack,
         race_hispanic_origin,
         gender)

all_results =
  map_dfr(.x = knot_vars,
        .f = function(x){
          tempdf = df %>%
            rename(variable = all_of(x))
          knots = quantile(tempdf$variable, probs = c(0.05, 0.5, 0.95))
          formula = as.formula(paste0("Surv(event_time, mortstat) ~ rcs(variable, parms = knots)+
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

          model = coxph(formula, data = tempdf, weights = weight_norm)
          new_data = data.frame(variable = seq(min(tempdf$variable), max(tempdf$variable), length.out = 500)) %>%
            bind_cols(means)

          ref_data =  data.frame(variable = c(2000)) %>%
            bind_cols(means)
          # Predict linear predictor using the fitted model
          ref = predict(model, newdata = ref_data, se.fit=TRUE, type = "lp", level = 0.95)$fit # set age=45 as reference value similar to "dd$limits["Adjust to","age"] <- 45"

          predictions = predict(model, newdata = new_data, se.fit=TRUE, type = "lp", level = 0.95)

          data.frame(steps = new_data$variable,
                          lp = predictions$fit,
                          hr = exp(predictions$fit - ref),
                          # odds_ratio = plogis(predictions$fit-ref),
                          lwr = exp((predictions$fit - 1.96 * predictions$se.fit)-ref),
                          upr = exp((predictions$fit + 1.96 * predictions$se.fit)-ref)) %>%
            mutate(stepvar = x)
        })

all_results %>%
  ggplot(aes(x = steps, y = hr, color = stepvar))+
  geom_line() +
  # geom_ribbon(aes(ymin = lwr, ymax = upr), col = "grey", alpha = 0.5,
              # linewidth = 0)+
  scale_y_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks=seq(2000, 20000, 2000), limits = c(2000, 20000)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  theme_light() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12)) +
  labs(x = "Daily steps", y = "Hazard Ratio (compared to 2000 steps/day)")

rug_dat =
  df %>%
  select(mortstat,total_scrfsteps, total_actisteps, total_vsrevsteps,
         total_vssteps, total_scsslsteps, total_oaksteps)  %>%
  filter(mortstat == 1) %>%
  mutate(hr = NA_real_) %>%
  pivot_longer(cols = starts_with("total"), names_to = "stepvar") %>%
  mutate(stepvar = factor(stepvar,
                          levels= c("total_actisteps",
                                    "total_oaksteps",
                                    "total_scrfsteps",
                                    "total_scsslsteps",
                                    "total_vssteps",
                                    "total_vsrevsteps"),
                          labels = c("Actilife", "Oak", "Stepcount RF", "Stepcount SSL",
                                     "Verisense", "Verisense rev.")))

col_vec = c("#000000FF", "#CC79A7FF", "#E69F00FF", "#D55E00FF", "#56B4E9FF", "#0072B2FF")
names(col_vec) = c("Actilife", "Oak", "Stepcount RF","Stepcount SSL", "Verisense", "Verisense rev.")


all_results %>%
  mutate(stepvar = factor(stepvar,
                         levels= c("total_actisteps",
                            "total_oaksteps",
                            "total_scrfsteps",
                            "total_scsslsteps",
                            "total_vssteps",
                            "total_vsrevsteps"),
                          labels = c("Actilife", "Oak", "Stepcount RF", "Stepcount SSL",
                                     "Verisense", "Verisense rev."))) %>%
  ggplot(aes(x = steps, y = hr, color = stepvar, fill = stepvar))+
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5,
  linewidth = 0)+
  facet_wrap(.~stepvar) +
  scale_y_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks=seq(2000, 20000, 2000), limits = c(2000, 20000),
                     labels = seq(2, 20, 2)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  theme_light() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12)) +
  labs(x = "Daily Steps (x1000)", y = "Hazard Ratio (compared to 2000 steps/day)") +
  theme(legend.position = "none") +
  scale_color_manual(values = col_vec) +
  scale_fill_manual(values = col_vec) +
  geom_rug(data = rug_dat, aes(x = value), sides = "b", color = "red", size = 0.5, alpha = 0.5)   # Add rug plot

ggsave(
  here::here("manuscript", "figures", "dose_response.png"),
  dpi = 400,
  width = 10,
  height = 8
)



knots = quantile(df$total_scrfsteps, probs = c(0.05, 0.5, 0.95))
# model
model = coxph(Surv(event_time, mortstat) ~ rcs(total_scrfsteps, parms = knots) +
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
                  general_health_condition,
              data = df,
              weights = weight_norm)

# Summary of the model
summary(model)
# new data for predictions - covariates don't matter so just take first row
means = df %>%
  slice(1) %>%
  select(age_in_years_at_screening,
         cat_education,
         bin_diabetes,
         cat_alcohol,
         cat_bmi,
         chf,
         chd,
         stroke,
         cancer,
         bin_diabetes,
         cat_smoke,
         bin_mobilityproblem,
         general_health_condition,
        heartattack,
        race_hispanic_origin,
        gender)

# Create a new data frame for predictions
new_data = data.frame(total_scrfsteps = seq(min(df$total_scrfsteps), max(df$total_scrfsteps), length.out = 500)) %>%
  bind_cols(means)

ref_data =  data.frame(total_scrfsteps = c(2000)) %>%
  bind_cols(means)
# Predict linear predictor using the fitted model
ref = predict(model, newdata = ref_data, se.fit=TRUE, type = "lp", level = 0.95)$fit # set age=45 as reference value similar to "dd$limits["Adjust to","age"] <- 45"

predictions = predict(model, newdata = new_data, se.fit=TRUE, type = "lp", level = 0.95)

x2 = data.frame(steps = new_data$total_scrfsteps,
                lp = predictions$fit,
                hr = exp(predictions$fit - ref),
                 # odds_ratio = plogis(predictions$fit-ref),
                 lwr = exp((predictions$fit - 1.96 * predictions$se.fit)-ref),
                 upr = exp((predictions$fit + 1.96 * predictions$se.fit)-ref))

# add rug for events
rug_dat =
  df %>%
  select(mortstat,total_scrfsteps)  %>%
  filter(mortstat == 1) %>%
  mutate(hr = NA_real_)

x2 %>%
  ggplot(aes(x = steps, y = hr))+
  geom_line(col = "black") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), col = "grey", alpha = 0.5,
              linewidth = 0)+
  scale_y_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks=seq(2000, 20000, 2000), limits = c(2000, 20000)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  theme_light() +
  theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14),
         strip.text = element_text(size = 12)) +
  labs(x = "Daily steps estimated by Stepcount RF", y = "Hazard Ratio (compared to 2000 steps/day)") +
  geom_rug(data = rug_dat, aes(x = total_scrfsteps), sides = "b", color = "red", size = 0.5, alpha = 0.5)   # Add rug plot

# repeat for vs rev
knots = quantile(df$total_vsrevsteps, probs = c(0.05, 0.5, 0.95))
# model
model = coxph(Surv(event_time, mortstat) ~ rcs(total_vsrevsteps, parms = knots) +
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
                general_health_condition,
              data = df,
              weights = weight_norm)

# Summary of the model
summary(model)
# new data for predictions - covariates don't matter so just take first row
means = df %>%
  slice(1) %>%
  select(age_in_years_at_screening,
         cat_education,
         bin_diabetes,
         cat_alcohol,
         cat_bmi,
         chf,
         chd,
         stroke,
         cancer,
         bin_diabetes,
         cat_smoke,
         bin_mobilityproblem,
         general_health_condition,
         heartattack,
         race_hispanic_origin,
         gender)

# Create a new data frame for predictions
new_data = data.frame(total_vsrevsteps = seq(min(df$total_vsrevsteps), max(df$total_vsrevsteps), length.out = 500)) %>%
  bind_cols(means)

ref_data =  data.frame(total_vsrevsteps = c(2000)) %>%
  bind_cols(means)
# Predict linear predictor using the fitted model
ref = predict(model, newdata = ref_data, se.fit=TRUE, type = "lp", level = 0.95)$fit # set age=45 as reference value similar to "dd$limits["Adjust to","age"] <- 45"

predictions = predict(model, newdata = new_data, se.fit=TRUE, type = "lp", level = 0.95)

x2 = data.frame(steps = new_data$total_vsrevsteps,
                lp = predictions$fit,
                hr = exp(predictions$fit - ref),
                lwr = exp((predictions$fit - 1.96 * predictions$se.fit)-ref),
                upr = exp((predictions$fit + 1.96 * predictions$se.fit)-ref))

# add rug for events
rug_dat =
  df %>%
  select(mortstat,total_vsrevsteps)  %>%
  filter(mortstat == 1) %>%
  mutate(hr = NA_real_)

x2 %>%
  ggplot(aes(x = steps, y = hr))+
  geom_line(col = "black") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), col = "grey", alpha = 0.5,
              linewidth = 0)+
  scale_y_continuous(trans = "log",
                     labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks=seq(2000, 20000, 2000), limits = c(2000, 20000)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  theme_light() +
  labs(x = "Daily steps estimated by Verisense rev.", y = "Hazard Ratio (compared to 2000 steps/day)") +
  geom_rug(data = rug_dat, aes(x = total_vsrevsteps), sides = "b", color = "red", size = 0.5)
