##### CREATE INCLUSION SUMMARY DF


# what: make DF of SEQN and PAXDAYM included in the analysis based on criteria
library(tidyverse)
raw_files = list.files(here::here("data", "accelerometry", "minute_level"), pattern = ".*nhanes\\_1440.*csv.xz", recursive = TRUE,
                               full.names = TRUE)
mims_file = raw_files[grepl("_PAXMTSM.csv.xz", raw_files)]
wt_file = raw_files[grepl("_PAXPREDM.csv.xz", raw_files)]
flag_file = raw_files[grepl("FLGSM", raw_files)]


mims = read_csv(mims_file,
                col_types = list(SEQN = col_character()))

zero_mims =
  mims %>%
  group_by(SEQN, PAXDAYM, PAXDAYWM) %>%
  pivot_longer(cols = starts_with("min")) %>%
  summarize(MIMS_0 = sum(value <= 0, na.rm = TRUE), .groups = "drop")

rm(mims)

wt = read_csv(wt_file,
              col_types = list(SEQN = col_character()))



flags = read_csv(flag_file,
                 col_types = list(SEQN = col_character()))

# find flag indices
flag_ind = flags %>%
  select(starts_with("min")) %>%
  mutate(across(starts_with("min"), ~if_else(.x, NA_integer_, 1))) %>%
  as.matrix()

flag =
  flags %>%
  group_by(SEQN, PAXDAYM, PAXDAYWM) %>%
  pivot_longer(cols = starts_with("min")) %>%
  summarize(flag = sum(value, na.rm = TRUE), .groups = "drop")

wt_mat =
  wt %>%
  select(starts_with("min")) %>%
  as.matrix()

wt_mat[is.na(flag_ind)] <- NA

wt_na =
  wt %>%
  select(-starts_with("min")) %>%
  bind_cols(as.data.frame(wt_mat))

wt_summ =
  wt_na %>%
  group_by(SEQN, PAXDAYM, PAXDAYWM) %>%
  pivot_longer(cols = starts_with("min")) %>%
  summarize(W = sum(value == 1, na.rm = TRUE),
            S = sum(value == 2, na.rm = TRUE),
            N = sum(value == 3, na.rm = TRUE),
            U = sum(value == 4, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(non_flag_wear = W + S + U) %>%
  select(SEQN, PAXDAYM, PAXDAYWM, non_flag_wear)

wt_summ_all =
  wt %>%
  group_by(SEQN, PAXDAYM, PAXDAYWM) %>%
  pivot_longer(cols = starts_with("min")) %>%
  summarize(W = sum(value == 1, na.rm = TRUE),
            S = sum(value == 2, na.rm = TRUE),
            N = sum(value == 3, na.rm = TRUE),
            U = sum(value == 4, na.rm = TRUE),
            .groups = "drop")
# join everything together
inclusion_summary =
  zero_mims %>%
  left_join(wt_summ, by = c("SEQN", "PAXDAYM", "PAXDAYWM")) %>%
  left_join(wt_summ_all, by = c("SEQN", "PAXDAYM", "PAXDAYWM")) %>%
  left_join(flag, by = c("SEQN", "PAXDAYM", "PAXDAYWM"))

inclusion_summary =
  inclusion_summary %>%
  mutate(criteria_1368 = non_flag_wear >= 1368,
         criteria_wake7 = W >= 60 * 7,
         criteria_MIMS = (W + S + U + N - MIMS_0) >= 60 * 7, # are nonzero MIMS minutes > 7 hours?
         include = criteria_1368 & criteria_wake7 & criteria_MIMS)



readr::write_csv(inclusion_summary, here::here("data", "accelerometry", "inclusion_summary.csv.gz"))

