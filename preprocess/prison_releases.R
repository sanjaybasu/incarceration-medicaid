library(here)
library(tidyverse)
#Download data from https://www.icpsr.umich.edu/web/ICPSR/studies/38555
#Extract data in ../data/raw
#Code below reflects downloading the data using the "R" version

data_dir = file.path(here::here(), "data","raw","ICPSR_38555")

load(file.path(data_dir,"DS0001","38555-0001-Data.rda")) # State and Fed prisons


df <- da38555.0001 %>% filter(YEAR < 2020) %>% 
  group_by(STATEID, STATE) %>% 
  arrange(desc(YEAR)) %>% slice(1) %>% 
  mutate(releases_M = RLTOTM - RLAWOLM - RLDEATHM - RLTRANM - RLESCAPM,
         releases_F = RLTOTF - RLAWOLF - RLDEATHF - RLTRANF - RLESCAPF
  ) %>% ungroup() %>%
  select(STATE, releases_M, releases_F) %>%
  pivot_longer(cols = starts_with("releases"), names_to = "sex_gender", values_to = "N_prison") %>%
  mutate(sex_gender = recode(sex_gender, releases_M = "Male", releases_F = "Female"))

write_csv(df, file = file.path(here::here(), "data","prisonReleasesNPS_bySexState.csv"))
