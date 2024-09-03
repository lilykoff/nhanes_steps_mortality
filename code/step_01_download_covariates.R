# install.packages('sjlabelled')
# formerly create_covariate_datasets_from_raw.R
library(tidyverse)
library(sjlabelled)
source(here::here("code", "utils.R"))

# download the xpts
get_xpt = function(nh_table) {
  nh_table = nh_table_name(nh_table)
  table = normalize_table_name(nh_table)
  # outdir = table_to_outdir(nh_table)
  outdir = "demographics/raw"
  stopifnot(!is.na(outdir))
  data_dir = here::here("data", outdir)
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  url = nhanes_xpt_url(nh_table)
  outfile = normalize_table_name(nh_table)
  outfile = paste0(outfile, ".XPT")
  file = file.path(data_dir, outfile)
  if (!file.exists(file)) {
    curl::curl_download(url, destfile = file, quiet = FALSE)
  }
  file
}

names = c("ALQ", "DEMO", "DIQ","BMX", "HSQ", "HUQ", "HSQ", "MCQ", "PAQ", "PFQ", "SMQ")
names_g = paste(names, "G", sep = "_")
names_h = paste(names, "H", sep = "_")
names_y = paste(names[!(names%in%c("ALQ", "HSQ"))], "Y", sep = "_")

purrr::map(.x = c(names_g, names_h, names_y), .f = get_xpt)

# read in the XPTs - do a bunch of manipulations
demo_g = haven::read_xpt(here::here("data", "demographics", "raw", "DEMO_G.XPT"))
demo_h = haven::read_xpt(here::here("data", "demographics", "raw", "DEMO_H.XPT"))
demo_y = haven::read_xpt(here::here("data", "demographics", "raw", "DEMO_Y.XPT"))

# translate columns; need to manually fix levels for household size waves in waves G and Y to match wave H
demo_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'DEMO_G',
                           colnames = colnames(demo_g),
                           data = demo_g) %>%
  mutate(DMDHHSIZ = factor(ifelse(
    DMDHHSIZ == 7, "7 or more people in the Househol", DMDHHSIZ
  )),
  DMDFMSIZ = factor(ifelse(
    DMDFMSIZ == 7, "7 or more people in the Househol", DMDFMSIZ
  )))
demo_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'DEMO_H',
                           colnames = colnames(demo_h),
                           data = demo_h)
demo_y_trans =
  nhanesA::nhanesTranslate(nh_table = "Y_DEMO",
                           colnames = colnames(demo_y),
                           data = demo_y) %>%
  mutate(DMDHHSIZ = factor(ifelse(
    DMDHHSIZ == 7, "7 or more people in the Househol", DMDHHSIZ
  )),
  DMDFMSIZ = factor(ifelse(
    DMDFMSIZ == 7, "7 or more people in the Househol", DMDFMSIZ
  )))


demo = bind_rows(demo_g, demo_h, demo_y)

# get subset of variables for analysis
demo_analytic =
  demo_g_trans %>%
  select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDRETH3, RIDEXMON,
          RIDEXAGM, DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
         INDHHIN2, INDFMIN2, INDFMPIR) %>%
  bind_rows(
    demo_h_trans %>%
      select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDRETH3, RIDEXMON,
              RIDEXAGM, DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
             INDHHIN2, INDFMIN2, INDFMPIR)
  )

# get same subset but make labels the column names and add column names as labels
demo_analytic_labeled =
  demo_g_trans %>%
  select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDRETH3, RIDEXMON,
         RIDEXAGM, DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
         INDHHIN2, INDFMIN2, INDFMPIR) %>%
label_to_colnames() %>%
  bind_rows(
    demo_h_trans %>%
      select(SEQN,SDDSRVYR,RIDSTATR, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDRETH1, RIDRETH3, RIDEXMON,
              RIDEXAGM, DMDEDUC3, DMDEDUC2, DMDMARTL, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA,
             INDHHIN2, INDFMIN2, INDFMPIR) %>%
      label_to_colnames()
  ) %>%
  set_label(., label = colnames(demo_analytic)) # this adds nhanes col names as labels

# same thing for Y
demo_y_analytic =
  demo_y_trans %>%
  select(SEQN, RIDSTATR,RIAGENDR,RIDAGEYR,RIDRETH1,RIDEXMON,RIDEXAGY,
         DMDEDUC3, WTINT,WTMEC,SDMVPSU,SDMVSTRA,INDHHIN2, INDFMIN2,INDFMPIR)

demo_y_analytic_labeled =
  demo_y_analytic %>%
  label_to_colnames() %>%
  set_label(., label = colnames(demo_y_analytic))

# repeat process for smoking questionnaire
smq_g = haven::read_xpt(here::here("data", "demographics", "raw", "SMQ_G.XPT"))
smq_h = haven::read_xpt(here::here("data", "demographics", "raw", "SMQ_H.XPT"))
smq_y = haven::read_xpt(here::here("data", "demographics", "raw", "SMQ_Y.XPT"))

smq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'SMQ_G',
                           colnames = colnames(smq_g),
                           data = smq_g)
smq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'SMQ_H',
                           colnames = colnames(smq_h),
                           data = smq_h)
smq_y_trans =
  nhanesA::nhanesTranslate(nh_table = "Y_SMQ",
                           colnames = colnames(smq_y),
                           data = smq_y)
smq = bind_rows(smq_g, smq_h, smq_y)

smq_analytic =
  smq_g_trans %>%
  select(SEQN, SMQ020, SMD030, SMQ040, SMD055) %>%
  bind_rows(
    smq_h_trans %>%
      select(SEQN, SMQ020, SMD030, SMQ040, SMD055))

smq_y_analytic = smq_y_trans

smq_analytic_labeled =
  smq_g_trans %>%
  select(SEQN, SMQ020, SMD030, SMQ040, SMD055) %>%
  label_to_colnames() %>%
  bind_rows(smq_h_trans %>%
              select(SEQN, SMQ020, SMD030, SMQ040, SMD055) %>%
              label_to_colnames()) %>%
  set_label(., label = colnames(smq_analytic))

smq_y_analytic_labeled =
  smq_y_trans %>%
  label_to_colnames() %>%
  set_label(., label = colnames(smq_y_analytic))

# alcohol
alq_g = haven::read_xpt(here::here("data", "demographics", "raw", "ALQ_G.XPT"))
alq_h = haven::read_xpt(here::here("data", "demographics", "raw", "ALQ_H.XPT"))

alq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'ALQ_G',
                           colnames = colnames(alq_g),
                           data = alq_g)
alq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'ALQ_H',
                           colnames = colnames(alq_h),
                           data = alq_h)

alq = bind_rows(alq_g, alq_h)

alq_analytic = bind_rows(alq_g_trans, alq_h_trans)

alq_analytic_labeled =
  bind_rows(alq_g_trans %>% label_to_colnames(),
            alq_h_trans %>% label_to_colnames()) %>%
  set_label(., label = colnames(alq_analytic))

# height weight bmi
bmx_g = haven::read_xpt(here::here("data", "demographics", "raw", "BMX_G.XPT"))
bmx_h = haven::read_xpt(here::here("data", "demographics", "raw", "BMX_H.XPT"))
bmx_y = haven::read_xpt(here::here("data", "demographics", "raw", "BMX_Y.XPT"))

bmx_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'BMX_G',
                           colnames = colnames(bmx_g),
                           data = bmx_g)
bmx_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'BMX_H',
                           colnames = colnames(bmx_h),
                           data = bmx_h)
bmx_y_trans =
  nhanesA::nhanesTranslate(nh_table = 'Y_BMX',
                           colnames = colnames(bmx_y),
                           data = bmx_y)
bmx = bind_rows(bmx_g, bmx_h, bmx_y)

bmx_analytic =
  bmx_g_trans %>% select(SEQN, BMXWT, BMXHT, BMXBMI) %>%
  bind_rows(bmx_h_trans  %>% select(SEQN, BMXWT, BMXHT, BMXBMI))

bmx_y_analytic =
  bmx_y_trans %>%
  select(SEQN, BMXWT, BMXHT, BMXBMI)

bmx_analytic_labeled =
  bmx_g_trans %>%
  select(SEQN, BMXWT, BMXHT, BMXBMI) %>% label_to_colnames() %>%
  bind_rows(bmx_h_trans  %>%
              select(SEQN, BMXWT, BMXHT, BMXBMI) %>%
              label_to_colnames()) %>%
  set_label(., label = colnames(bmx_analytic))

bmx_y_analytic_labeled =
  bmx_y_trans %>%
  select(SEQN, BMXWT, BMXHT, BMXBMI) %>%
  label_to_colnames() %>%
  set_label(., label = colnames(bmx_y_analytic))

# diabetes
diq_g = haven::read_xpt(here::here("data", "demographics", "raw", "DIQ_G.XPT"))
diq_h = haven::read_xpt(here::here("data", "demographics", "raw", "DIQ_H.XPT"))
diq_y = haven::read_xpt(here::here("data", "demographics", "raw", "DIQ_Y.XPT"))

diq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'DIQ_G',
                           colnames = colnames(diq_g),
                           data = diq_g)
diq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'DIQ_H',
                           colnames = colnames(diq_h),
                           data = diq_h)
diq_y_trans =
  nhanesA::nhanesTranslate(nh_table = 'Y_DIQ',
                           colnames = colnames(diq_y),
                           data = diq_y)
diq = bind_rows(diq_g, diq_h, diq_y)

diq_analytic =
  diq_g_trans %>%
  select(SEQN, DIQ010, DID040) %>%
  bind_rows(diq_h_trans %>%
              select(SEQN, DIQ010, DID040))

diq_y_analytic = diq_y_trans %>% select(SEQN, DIQ010, DID040)


diq_analytic_labeled =
  diq_g_trans %>%
  select(SEQN, DIQ010, DID040) %>%
  label_to_colnames() %>%
  bind_rows(diq_h_trans %>%
              select(SEQN, DIQ010, DID040) %>%
              label_to_colnames()) %>%
  set_label(., label = colnames(diq_analytic))

diq_y_analytic_labeled =
  diq_y_trans %>%
  select(SEQN, DIQ010, DID040) %>%
  label_to_colnames() %>%
  set_label(., label = colnames(diq_y_analytic))

# MCQ
mcq_g = haven::read_xpt(here::here("data", "demographics", "raw", "MCQ_G.XPT"))
mcq_h = haven::read_xpt(here::here("data", "demographics", "raw", "MCQ_H.XPT"))
mcq_y = haven::read_xpt(here::here("data", "demographics", "raw", "MCQ_Y.XPT"))
cols = c("SEQN","MCQ080","MCQ160A","MCQ180A","MCQ160B","MCQ180B","MCQ160C",
              "MCQ180C","MCQ160D","MCQ180D","MCQ160E","MCQ180E","MCQ160F","MCQ180F",
              "MCQ160G","MCQ180G","MCQ220")


mcq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'MCQ_H',
                           colnames = cols,
                           data = mcq_h)

mcq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'MCQ_G',
                           colnames = cols,
                           data = mcq_g)
mcq_y_trans =
  nhanesA::nhanesTranslate(nh_table = 'Y_MCQ',
                           colnames = cols,
                           data = mcq_y)
mcq = bind_rows(mcq_g, mcq_h, mcq_y)

mcq_analytic =
  mcq_g_trans %>%
  select(
    SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
    MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
    MCQ160G,MCQ180G,MCQ220
  ) %>%
  bind_rows(
    mcq_h_trans %>% select(
      SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
      MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
      MCQ160G,MCQ180G,MCQ220
    )
  )

mcq_y_analytic = mcq_y_trans

mcq_analytic_labeled =
  mcq_g_trans %>%
  select(SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
         MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
         MCQ160G,MCQ180G,MCQ220) %>%
  label_to_colnames() %>%
  bind_rows(
    mcq_h_trans %>%
      select(
        SEQN,MCQ080,MCQ160A, MCQ180A,MCQ160B,MCQ180B, MCQ160C,
        MCQ180C,MCQ160D, MCQ180D,MCQ160E,MCQ180E,MCQ160F,MCQ180F,
        MCQ160G,MCQ180G,MCQ220) %>%
      label_to_colnames()
  ) %>%
  set_label(., label = colnames(mcq_analytic))

mcq_y_analytic_labeled =
  mcq_y_trans %>%
  label_to_colnames() %>%
  set_label(., label = colnames(mcq_y_analytic))

# PAQ
paq_g = haven::read_xpt(here::here("data", "demographics", "raw", "PAQ_G.XPT"))
paq_h = haven::read_xpt(here::here("data", "demographics", "raw", "PAQ_H.XPT"))
paq_y = haven::read_xpt(here::here("data", "demographics", "raw", "PAQ_Y.XPT"))

paq_h_trans = nhanesA::nhanesTranslate(
  nh_table = 'PAQ_H',
  colnames = c("SEQN", "PAD615", "PAD630", "PAD680", "PAD645", "PAD660",
               "PAQ722", "PAQ724AA", "PAQ724S"),
  data = paq_h
)

paq_g_trans = nhanesA::nhanesTranslate(nh_table = 'PAQ_G',
                                       colnames = c("SEQN", "PAD615", "PAD630", "PAD680", "PAD645", "PAD660"),
                                       data = paq_g)
paq_y_trans = nhanesA::nhanesTranslate(
  nh_table = 'Y_PAQ',
  colnames = c("SEQN","PAD615","PAD630","PAD680","PAD645","PAD660","PAQ722","PAQ724AA","PAQ724S","PAQ755","PAQ744","PAQ762"),
  data = paq_y
)
paq = bind_rows(paq_g, paq_h, paq_y)

paq_analytic =
  paq_g_trans %>%
  select(SEQN, PAD615, PAD630, PAQ706, PAD680, PAD645, PAD660) %>%
  bind_rows(
    paq_h_trans %>%
      select(SEQN, PAD615, PAD630, PAQ706, PAD680, PAD645, PAD660, PAQ722, PAQ724AA, PAQ724S)
  )

paq_y_analytic =
  paq_y_trans %>%
  select(SEQN,PAD615,PAD630,PAQ706,PAD680,PAD645,PAD660, PAQ722, PAQ724AA, PAQ724S, PAQ755,  PAQ744, PAQ762)

paq_analytic_labeled =
  paq_g_trans %>%
  select(SEQN, PAD615, PAD630, PAQ706, PAD680, PAD645, PAD660) %>%
  label_to_colnames() %>%
  bind_rows(
    paq_h_trans %>%
      select(SEQN, PAD615, PAD630, PAQ706, PAD680, PAD645, PAD660, PAQ722, PAQ724AA, PAQ724S) %>%
      label_to_colnames() %>%
      rename("Days physically active at least 60 min" = "Days physically active at least 60 min.")
  ) %>%
  set_label(., label = colnames(paq_analytic))

paq_y_analytic_labeled =
  paq_y_trans %>%
  select(SEQN,PAD615,PAD630,PAQ706,PAD680,PAD645,PAD660, PAQ722, PAQ724AA, PAQ724S, PAQ755,  PAQ744, PAQ762) %>%
  label_to_colnames() %>%
  set_label(., label = colnames(paq_y_analytic))

# PFQ
pfq_g = haven::read_xpt(here::here("data", "demographics", "raw", "PFQ_G.XPT"))
pfq_h = haven::read_xpt(here::here("data", "demographics", "raw", "PFQ_H.XPT"))
pfq_y = haven::read_xpt(here::here("data", "demographics", "raw", "PFQ_Y.XPT"))

pfq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'PFQ_H',
                           colnames = colnames(pfq_h),
                           data = pfq_h)

pfq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'PFQ_G',
                           colnames = colnames(pfq_g),
                           data = pfq_g)
pfq_y_trans =
  nhanesA::nhanesTranslate(nh_table = 'Y_PFQ',
                           colnames = colnames(pfq_y),
                           data = pfq_y)

pfq = bind_rows(pfq_g, pfq_h, pfq_y)

pfq_analytic =
  pfq_g_trans %>%
  select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ059, PFQ057) %>%
  bind_rows(pfq_h_trans %>%
              select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ057,PFQ059))

pfq_y_analytic =
  pfq_y_trans %>%
  select(SEQN, PFQ020, PFQ030, PFQ033)

pfq_analytic_labeled =
  pfq_g_trans %>%
  select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ059, PFQ057) %>%
  label_to_colnames() %>%
  bind_rows(
    pfq_h_trans %>%
      select(SEQN, PFQ020, PFQ030, PFQ049, PFQ054, PFQ061B, PFQ061C, PFQ061H, PFQ059, PFQ057) %>%
      label_to_colnames()) %>%
  set_label(., label = colnames(pfq_analytic))

pfq_y_analytic_labeled =
  pfq_y_trans %>%
  select(SEQN, PFQ020, PFQ030, PFQ033) %>%
  label_to_colnames() %>%
  set_label(., label = colnames(pfq_y_analytic))

# HSQ
hsq_g = haven::read_xpt(here::here("data", "demographics", "raw", "HSQ_G.XPT"))
hsq_h = haven::read_xpt(here::here("data", "demographics", "raw", "HSQ_H.XPT"))

hsq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'HSQ_H',
                           colnames = colnames(hsq_h),
                           data = hsq_h)

hsq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'HSQ_G',
                           colnames = colnames(hsq_g),
                           data = hsq_g)


hsq = bind_rows(hsq_g, hsq_h)

hsq_analytic =
  hsq_g_trans %>%
  select(SEQN, HSD010) %>%
  bind_rows(hsq_h_trans %>%
              select(SEQN, HSD010))


hsq_analytic_labeled =
  hsq_g_trans %>%
  select(SEQN, HSD010) %>%
  label_to_colnames() %>%
  bind_rows(
    hsq_h_trans %>%
      select(SEQN, HSD010) %>%
      label_to_colnames()) %>%
  set_label(., label = colnames(hsq_analytic))

# HUQ
huq_g = haven::read_xpt(here::here("data", "demographics", "raw", "HUQ_G.XPT"))
huq_h = haven::read_xpt(here::here("data", "demographics", "raw", "HUQ_H.XPT"))

huq_h_trans =
  nhanesA::nhanesTranslate(nh_table = 'HUQ_H',
                           colnames = colnames(huq_h),
                           data = huq_h)

huq_g_trans =
  nhanesA::nhanesTranslate(nh_table = 'HUQ_G',
                           colnames = colnames(huq_g),
                           data = huq_g)


huq = bind_rows(huq_g, huq_h)

huq_analytic =
  huq_g_trans %>%
  select(SEQN, HUQ010) %>%
  bind_rows(huq_h_trans %>%
              select(SEQN, HUQ010))


huq_analytic_labeled =
  huq_g_trans %>%
  select(SEQN, HUQ010) %>%
  label_to_colnames() %>%
  bind_rows(
    huq_h_trans %>%
      select(SEQN, HUQ010) %>%
      label_to_colnames()) %>%
  set_label(., label = colnames(huq_analytic))



# bind all together
all =
  demo %>%
  left_join(smq, by = "SEQN") %>%
  left_join(alq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(diq, by = "SEQN") %>%
  left_join(mcq, by = "SEQN") %>%
  left_join(paq, by = "SEQN") %>%
  left_join(pfq, by = "SEQN") %>%
  left_join(hsq, by = "SEQN") %>%
  left_join(huq, by = "SEQN")

if(!dir.exists(here::here("data", "demographics", "processed"))){
  dir.create(here::here("data", "demographics", "processed"), showWarnings = FALSE, recursive = TRUE)
}
readr::write_csv(all, here::here("data", "demographics", "processed", "all_G_H_Y_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_HSQ_HUQ_raw.csv.gz"))

all_analytic =
  demo_analytic %>%
  left_join(smq_analytic, by = "SEQN") %>%
  left_join(alq_analytic, by = "SEQN") %>%
  left_join(bmx_analytic, by = "SEQN") %>%
  left_join(diq_analytic, by = "SEQN") %>%
  left_join(mcq_analytic, by = "SEQN") %>%
  left_join(paq_analytic, by = "SEQN") %>%
  left_join(pfq_analytic, by = "SEQN") %>%
  left_join(hsq_analytic, by = "SEQN") %>%
  left_join(huq_analytic, by = "SEQN")

readr::write_csv(all_analytic, here::here("data", "demographics", "processed", "subset_G_H_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_HSQ_HUQ_raw.csv.gz"))

all_y_analytic =
  demo_y_analytic %>%
  left_join(smq_y_analytic, by = "SEQN") %>%
  left_join(bmx_y_analytic, by = "SEQN") %>%
  left_join(diq_y_analytic, by = "SEQN") %>%
  left_join(mcq_y_analytic, by = "SEQN") %>%
  left_join(paq_y_analytic, by = "SEQN") %>%
  left_join(pfq_y_analytic, by = "SEQN")

readr::write_csv(all_y_analytic, here::here("data", "demographics", "processed", "subset_Y_DEMO_BMX_DIQ_MCQ_PAQ_PFQ_raw.csv.gz"))

all_analytic_labeled =
  demo_analytic_labeled %>%
  left_join(smq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(alq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(bmx_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(diq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(mcq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(paq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(pfq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(hsq_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(huq_analytic_labeled, by = "Respondent sequence number")


readr::write_csv(all_analytic_labeled, here::here("data", "demographics", "processed", "subset_G_H_DEMO_ALQ_BMX_DIQ_MCQ_PAQ_PFQ_HSQ_HUQ_translated.csv.gz"))

all_y_analytic_labeled =
  demo_y_analytic_labeled %>%
  left_join(smq_y_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(bmx_y_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(diq_y_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(mcq_y_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(paq_y_analytic_labeled, by = "Respondent sequence number") %>%
  left_join(pfq_y_analytic_labeled, by = "Respondent sequence number")

readr::write_csv(all_y_analytic_labeled, here::here("data", "demographics", "processed", "subset_Y_DEMO_BMX_DIQ_MCQ_PAQ_PFQ_translated.csv.gz"))

