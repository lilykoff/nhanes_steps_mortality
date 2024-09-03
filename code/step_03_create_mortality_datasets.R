# mortality
library(readr)
library(dplyr)

# download data HERE
# https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_2011_2012_MORT_2019_PUBLIC.dat

download_mort_dats = function(url){
  dest = here::here("data", "demographics", "raw", sub(".*\\/", "", url))
  curl::curl_download(url, destfile = dest, quiet = FALSE)
}
urls = c("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_2011_2012_MORT_2019_PUBLIC.dat",
         "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_2013_2014_MORT_2019_PUBLIC.dat")

lapply(urls, download_mort_dats)


fpath_g = here::here("data", "demographics", "raw", "NHANES_2011_2012_MORT_2019_PUBLIC.dat")  # full .DAT name here
fpath_h = here::here("data", "demographics", "raw", "NHANES_2013_2014_MORT_2019_PUBLIC.dat")  # full .DAT name here


# read in the fixed-width format ASCII file
mort_g <- read_fwf(file=fpath_g,
                col_types = "iiiiiiii",
                fwf_cols(seqn = c(1,6),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = c("", ".")
)

mort_h <- read_fwf(file=fpath_h,
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)

mort_raw = bind_rows(mort_g, mort_h)
write_csv(mort_raw, here::here("data", "demographics", "processed", "mortality_G_H_raw.csv.gz"))

# process data
mort_processed =
  bind_rows(mort_g, mort_h) %>%
  rename(SEQN = seqn) %>%
  mutate(
    cat_eligibility = case_when(
      eligstat == 1 ~ "Eligible",
      eligstat == 2 ~ "Under age 18, not available for public release",
      eligstat == 3 ~ "Ineligible"
    ),
    # bin_mortality =
    #   case_when(
    #     mortstat == 0 ~ 0,
    #     mortstat == 1 ~ 1,
    #     TRUE ~ NA
    #   ),
    cat_cod = case_when(
      ucod_leading == 1 ~ "diseases of heart",
      ucod_leading == 2 ~ "malignant neoplasms",
      ucod_leading == 3 ~ "chronic lower respiratory diseases",
      ucod_leading == 4 ~ "accident",
      ucod_leading == 5 ~ "cerebrovascular diseases",
      ucod_leading == 6 ~ "alzheimers",
      ucod_leading == 7 ~ "diabetes",
      ucod_leading == 8 ~ "influenza and pneumonia",
      ucod_leading == 9 ~ "nephritis, nephrotic syndrome and nephrosis",
      ucod_leading == 10 ~ "other",
      TRUE ~ NA
    ),
    mortstat = replace(mortstat, ucod_leading == 4, 0))  # replace accidents with censored
    # select(SEQN, starts_with("cat"), starts_with("bin"), starts_with("val"))
saveRDS(mort_processed, here::here("data", "demographics", "processed", "mortality_G_H_tidy.rds"))
