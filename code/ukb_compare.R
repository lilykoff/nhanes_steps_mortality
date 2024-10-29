# ukb compare
rm(list = ls())
library(tidyverse)

day_level =
  readRDS(here::here("data", "accelerometry", "summarized", "pa_df_day_level.rds"))


medians =
  day_level %>%
  group_by(SEQN) %>%
  summarize(num_valid_days = n(),
            across(c(contains("peak"), contains("total")), ~median(.x, na.rm = TRUE)))
medians
demo = readRDS(here::here("data", "demographics", "processed", "subset_G_H_tidy.rds"))

medians_age =
  medians %>%
  left_join(demo %>% select(SEQN, age = age_in_years_at_screening), by = "SEQN")

summary =
  medians_age %>%
  mutate(age_cat = cut(age, breaks = c(40, 49, 59, 69, 79), include.lowest = TRUE)) %>%
  group_by(age_cat) %>%
  summarize(across(contains("steps"), list(median = ~median(.x, na.rm = TRUE),
                                           iqr = ~IQR(.x, na.rm = TRUE))))

summary_small =
  summary %>%
  select(age_cat, contains("scssl")) %>%
  drop_na() %>%
  select(age_cat, contains("total"), contains("scssl"))

summary_small %>%
  select(age_cat, total = total_scsslsteps_median, iqr = total_scsslsteps_iqr,
         peak1 = peak1_scsslsteps_median, peak1iqr = peak1_scsslsteps_iqr) %>%
  mutate(iqr_lb = total - 0.5*iqr,
         iqr_ub = total + 0.5*iqr,
         total_iqr_fmt = paste0("[", round(iqr_lb, 0), ",", round(iqr_ub, 0), "]"),
         peak1iqr_lb = peak1 - 0.5*peak1iqr,
         peak1iqr_ub = peak1 + 0.5*peak1iqr,
         total_iqr = paste0("[", round(iqr_lb, 0), ",", round(iqr_ub, 0), "]"),
         peak1_iqr = paste0("[", round(peak1iqr_lb, 0), ",", round(peak1iqr_ub, 0), "]")) %>%
  select(age_cat, total,total_iqr, peak1, peak1_iqr)
