library(tidyverse)
library(curl)
library(httr)
force = FALSE
folder_name = "minute-level-step-counts-and-physical-activity-data-from-the-national-health-and-nutrition-examination-survey-nhanes-2011-2014-1.0.0"

download_csv_xz = function(fname, force = FALSE) {
  zip_base_url = "https://physionet.org/files/minute-level-step-count-nhanes/1.0.0/"
  output_dir = here::here("data", "accelerometry", "minute_level")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  outfile = file.path(output_dir, fname)
  if(!file.exists(outfile) || force) {
    url = paste0(
      zip_base_url,
      fname
    )
    curl::curl_download(url, outfile, quiet = FALSE)
  }
}

fnames = c("nhanes_1440_AC.csv.xz",
           "nhanes_1440_PAXFLGSM.csv.xz",
           "nhanes_1440_PAXMTSM.csv.xz",
           "nhanes_1440_PAXPREDM.csv.xz",
           "nhanes_1440_scsslsteps.csv.xz")
sapply(fnames, download_csv_xz, force = force)


