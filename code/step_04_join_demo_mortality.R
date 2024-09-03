library(tidyverse)

demo = readRDS(here::here("data", "demographics", "processed", "subset_G_H_tidy.rds"))
mortality = readRDS(here::here("data", "demographics", "processed", "mortality_G_H_tidy.rds"))

mortality = mortality %>%
  rename(cod_diabetes = diabetes,
         cod_hyperten = hyperten)
# checks
# mort_ids = mortality$SEQN
# demo_ids = demo$SEQN %>% unname()
# length(demo_ids)
# length(mort_ids)
# sum(demo_ids %in% mort_ids)
# sum(mort_ids %in% demo_ids)
demo_mortality = left_join(demo, mortality, by = "SEQN")
demo_mort = full_join(demo,mortality,by="SEQN")
all.equal(demo_mort,demo_mortality)


saveRDS(demo_mort, here::here("data", "demographics", "processed", "covariates_mortality_G_H_tidy.rds"))
