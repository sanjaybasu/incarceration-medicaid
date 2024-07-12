library(here)
library(tidyverse)
#Download data from https://www.icpsr.umich.edu/web/ICPSR/studies/38323
#Extract data in ../data/raw
#Code below reflects downloading the data using the "R" version

data_dir = file.path(here::here(), "data","raw","ICPSR_38323")

library(srvyr)
library(naniar)

load(file.path(data_dir, "DS0002","38323-0002-Data.rda"))

#ADMISSION AND RELEASE ARE YEARLY
release_vars <- c("RELEASEMALE","RELEASEFEMALE")

jail.svy <- da38323.0002 %>%
  as_survey(weights = FINALWT, strata = STATE, id = ID)

#DC only has one jail so set this flag:
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

state.sex <- jail.svy %>%
  group_by(STATE) %>%
  summarize_at(c("RELEASEMALE", "RELEASEFEMALE"), .fun = list(Tot = ~survey_total(., vartype = "se"))) %>% ungroup() 

state_long.sex <- state.sex %>%
  select(-ends_with("_se")) %>%
  pivot_longer(cols = ends_with("_Tot"), names_to = "sex", names_transform = ~gsub("_Tot","",.), values_to = "Total") %>%
  left_join(
    state.sex %>%
      select(-ends_with("_Tot")) %>%
      pivot_longer(cols = ends_with("_se"), names_to = "sex", names_transform = ~gsub("_Tot_se","",.), values_to = "se")
  ) %>%
  mutate(se = ifelse(se == 0, NA, se),
         sex = str_to_title(gsub("^RELEASE","",sex)))

write_csv(state_long.sex, file.path(here::here(), "data","jailCensus_byStateSex.csv"))


