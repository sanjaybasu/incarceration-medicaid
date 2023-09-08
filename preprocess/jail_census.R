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
race_vars <- c("WHITE","BLACK","HISP", "RACE_OU2") #,"AIAN","ASIAN","NHOPI","TWORACE","OTHERRACE","RACEDK")

jail.svy <- da38323.0002 %>%
  mutate(RACE_OU2 = AIAN + ASIAN + NHOPI + TWORACE + OTHERRACE + RACEDK) %>%
  as_survey(weights = FINALWT, strata = STATE, id = ID)

#DC only has one jail so set this flag:
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

state.m <- jail.svy %>%
  group_by(STATE) %>%
  summarize_at(all_of(race_vars), .fun = list(Tot = ~survey_total(coalesce(./RACETOTAL*RELEASEMALE,0), vartype = "se"))) %>%
  mutate(sex = "Male") %>% ungroup() 
state.f <- jail.svy %>%
  group_by(STATE) %>%
  summarize_at(all_of(race_vars), .fun = list(Tot = ~survey_total(coalesce(./RACETOTAL*RELEASEFEMALE,0), vartype = "se"))) %>%
  mutate(sex = "Female") %>% ungroup()

state_est <- bind_rows(state.m, state.f)

state_long.t <- state_est %>%
  select(-ends_with("_se")) %>%
  pivot_longer(cols = ends_with("_Tot"), names_to = "race", names_transform = ~gsub("_Tot","",.), values_to = "Total")
state_long.se <- state_est %>%
  select(-ends_with("_Tot")) %>%
  pivot_longer(cols = ends_with("_se"), names_to = "race", names_transform = ~gsub("_Tot_se","",.), values_to = "se")
state_long <- state_long.t %>%
  left_join(state_long.se) %>%
  mutate(se = ifelse(se == 0, NA, se),
         race = case_when(race == "RACE_OU2" ~ "Multiracial, other, or missing",
                          race == "HISP" ~ "Hispanic",
                          T ~ paste0(str_to_title(race), " (NH)")
                          ))

write_csv(state_long, file.path(here::here(), "data","jailReleases_byStateSexRace.csv"))


