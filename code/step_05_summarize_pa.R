# aggregate pa
library(tidyverse)
library(data.table)
library(gtsummary)
library(furrr)

n_cores = parallel::detectCores() - 1
wt_file = readr::read_csv(here::here("data", "accelerometry", "inclusion_summary.csv.gz"),
                          col_types = cols(SEQN = col_character(),
                                           PAXDAYM = col_integer()))
valid_days =
  wt_file %>%
  filter(include) %>%
  select(SEQN, PAXDAYM, W, S, U, N, flag, non_flag_wear, MIMS_0)

# pa_df = readr::read_csv(here::here("data", "accelerometry", "nhanes_minute_level_pa.csv.gz"))

pa_files = list.files(here::here("data", "accelerometry", "minute_level"), pattern = ".*nhanes\\_1440.*rds")
pa_files = pa_files[!grepl("all", pa_files) & !grepl("PAXFLGSM", pa_files) & !grepl("PAXPREDM", pa_files)]

pa_vars = sub(".*nhanes\\_1440\\_(.+).rds.*", "\\1", pa_files)

get_day_summaries = function(variable){
  fpath = file.path(here::here("data", "accelerometry", "minute_level"), paste0("nhanes_1440_", variable, ".rds"))

  x = readRDS(fpath) %>% as.data.table()
  x_filt = x %>%
    right_join(valid_days %>% select(SEQN, PAXDAYM), by = c("SEQN", "PAXDAYM"))
  rm(x)
  x_day = x_filt[
    , .(SEQN, PAXDAYM, PAXDAYWM,
        total = rowSums(.SD, na.rm = TRUE), # get daily sum
        peak1 = apply(.SD, 1, max, na.rm = TRUE), # get daily peak
        peak30 = apply(.SD, 1, function(row) {
          top_30_values = sort(row, decreasing = TRUE)[1:30]
          mean(top_30_values, na.rm = TRUE) # get mean of top 30 values
        })
    ),
    .SDcols = patterns("^min_") # select all columns that start with min_
  ]
  rm(x_filt)

  x_day =
    x_day %>%
    rename_with(.cols = c(contains("total"), contains("peak")),
                .fn = ~paste0(.x, "_", variable))
  x_day

}
plan(multisession, workers = n_cores)
day_summaries = furrr::future_map(.x  = pa_vars, .f = get_day_summaries, .progress = TRUE)
# if don't want to parallelize, can run
# day_summaries = map(.x  = pa_vars, .f = get_day_summaries, .progress = TRUE)

all_measures = day_summaries %>%
  reduce(left_join, by = c("SEQN", "PAXDAYM", "PAXDAYWM")) %>%
  left_join(wt_file %>% select(SEQN, PAXDAYM, W, S, U, N, flag, non_flag_wear, MIMS_0), by = c("SEQN", "PAXDAYM")) %>%
  rename(wear_min = W,
         sleep_min = S,
         unknown_min = U,
         nonwear_min = N,
         flagged_min = flag,
         non_flagged_wear_min = non_flag_wear,
         zero_MIMS_min = MIMS_0)

subject_summaries =
  all_measures %>%
  group_by(SEQN) %>%
  summarize(num_valid_days = n(),
            across(c(contains("peak"), contains("total")), ~mean(.x, na.rm = TRUE)))

saveRDS(subject_summaries, here::here("data", "accelerometry", "summarized", "pa_df_subject_level.rds"))
saveRDS(all_measures, here::here("data", "accelerometry", "summarized", "pa_df_day_level.rds"))
