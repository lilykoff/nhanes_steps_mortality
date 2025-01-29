library(tidyverse)
library(curl)
library(httr)
force = FALSE
folder_name = "minute-level-step-counts-and-physical-activity-data-from-the-national-health-and-nutrition-examination-survey-nhanes-2011-2014-1.0.0"

if(!file.exists(
  here::here(
    "data",
    "accelerometry",
    "minute_level",
    folder_name,
    "nhanes_1440_AC.csv.xz"
  )
) ||
force) {
  zip_url =
    "https://physionet.org/static/published-projects/minute-level-step-count-nhanes/minute-level-step-counts-and-physical-activity-data-from-the-national-health-and-nutrition-examination-survey-nhanes-2011-2014-1.0.0.zip"

  zip_file = "minute_level_data.zip"
  output_dir = here::here("data", "accelerometry", "minute_level")

  # Download the file using httr
  response = GET(zip_url, write_disk(zip_file, overwrite = TRUE), progress())

  # Check the response status
  if (response$status_code == 200) {
    message("File downloaded successfully: ", zip_file)
  } else {
    stop("Failed to download file. HTTP status: ",
         response$status_code)
  }

  # Check if the file was downloaded successfully
  if (file.exists(zip_file)) {
    message("ZIP file downloaded successfully: ", zip_file)

    # Create a directory for extraction if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Extract the ZIP file
    unzip(zip_file, exdir = output_dir)
    message("ZIP file extracted to: ", output_dir)
  } else {
    message("File download failed.")
  }

  # List extracted files
  list.files(output_dir, recursive = TRUE)

  # remove the zip file
  file.remove(zip_file)
}

