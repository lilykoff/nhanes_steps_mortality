# aggregate pa
library(tidyverse)
library(data.table)
library(gtsummary)
library(furrr)
force = TRUE
target_files = c(here::here("data", "accelerometry", "summarized", "cadence_df_subject_level.rds"),
                 here::here("data", "accelerometry", "summarized", "cadence_df_day_level.rds"))

if(!all(file.exists(target_files)) ||
   force) {
  if (!dir.exists(here::here("data", "accelerometry", "summarized"))) {
    dir.create(here::here("data", "accelerometry", "summarized"),
               recursive = TRUE)
  }
  n_cores = parallel::detectCores() - 1
  wt_file = readr::read_csv(
    here::here("data", "accelerometry", "inclusion_summary.csv.gz"),
    col_types = cols(SEQN = col_character(), PAXDAYM = col_integer())
  )
  valid_days =
    wt_file %>%
    filter(include) %>%
    select(SEQN, PAXDAYM, W, S, U, N, flag, non_flag_wear, MIMS_0)

  # pa_df = readr::read_csv(here::here("data", "accelerometry", "nhanes_minute_level_pa.csv.gz"))


  # pa_df = readr::read_csv(here::here("data", "accelerometry", "nhanes_minute_level_pa.csv.gz"))

  pa_files = list.files(here::here("data", "accelerometry", "minute_level"),
                        pattern = ".*nhanes\\_1440.*csv.xz", recursive = TRUE, full.names = TRUE)
  step_files = pa_files[!grepl("all", pa_files) &
                        !grepl("PAXFLGSM", pa_files) & !grepl("PAXPREDM", pa_files) &
                      !grepl("PAXMTSM", pa_files) & !grepl("AC", pa_files)]


  fpath = step_files[1]
  get_cadence_summaries = function(fpath) {
    variable = sub(".*nhanes\\_1440\\_(.+).csv.xz.*", "\\1", fpath)
    x = read_csv(fpath, col_types = list(SEQN = col_character())) %>% as.data.table()
    x_filt = x %>%
      right_join(valid_days %>% select(SEQN, PAXDAYM),
                 by = c("SEQN", "PAXDAYM"))
    rm(x)

    # x_filt = x_filt %>% slice(1:1000)

    ## get cadence
    dt_long = melt(
      x_filt,
      id.vars = c("SEQN", "PAXDAYM", "PAXDAYWM"),
      variable.name = "minute",
      value.name = "steps",
      variable.factor =  FALSE,
      na.rm = FALSE
    )
    dt_long[, min_num := as.integer(sub(".*min\\_", "", minute))]

    # Order data by min_num within groups efficiently
    setorder(dt_long, SEQN, PAXDAYM, PAXDAYWM, min_num)


    # Identify minutes with steps >= 60
    dt_long[, bout_flag := steps >= 60]

    # Group consecutive rows where bout_flag == TRUE
    dt_long[, group_id := rleid(bout_flag), by = .(SEQN, PAXDAYM, PAXDAYWM)]

    # Filter for bouts where the condition is met
    valid_bouts = dt_long[bout_flag == TRUE, .N, by = .(SEQN, PAXDAYM, PAXDAYWM, group_id)][N >= 2]

    # Merge back to get steps for valid bouts
    result = dt_long[valid_bouts, on = .(SEQN, PAXDAYM, PAXDAYWM, group_id)][, .(mean_bout_cadence = mean(steps)), by = .(SEQN, PAXDAYM, PAXDAYWM)]
    result = result %>%
      as_tibble() %>%
      rename_with(.cols = contains("cadence"),
                  .fn = ~ paste0(.x, "_", variable))
    result

  }
  plan(multisession, workers = n_cores)
  day_summaries = furrr::future_map(.x  = step_files,
                                    .f = get_cadence_summaries,
                                    .progress = TRUE)
  plan(sequential)
  # if don't want to parallelize, can run
  # day_summaries = map(.x  = pa_vars, .f = get_day_summaries, .progress = TRUE)

  all_measures = day_summaries %>%
    reduce(left_join, by = c("SEQN", "PAXDAYM", "PAXDAYWM"))

  subject_summaries =
    all_measures %>%
    group_by(SEQN) %>%
    summarize(num_valid_days = n(), across(contains("cadence"), ~ mean(.x, na.rm = TRUE)))

  saveRDS(
    subject_summaries,
    here::here(
      "data",
      "accelerometry",
      "summarized",
      "cadence_df_subject_level.rds"
    )
  )
  saveRDS(
    all_measures,
    here::here(
      "data",
      "accelerometry",
      "summarized",
      "cadence_df_day_level.rds"
    )
  )
}
