# The jail census file contains the number of releases per year by state, race, and sex. There’s isn’t disaggregated data by age in the jail census.
# The proportions in the stays dataset represents the proportion of all individuals in the strata (state, race, and sex) that are in a respective age group and length of stay threshold.
# You can calculate the number of jail releases by state x race x sex x age group x length of stay by joining the two datasets by state x race x sex, then multiplying “N” (jail census) by p (jail stays).
setwd("~/Downloads")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
census <- read_csv("jailCensus_byStateSexRace.csv")
stays <- read_csv("jailStays_byStateSexRaceAge.csv")
statedata <- read_csv("statedata.csv")

jail = census %>%
  full_join(stays, by = c("STATE" = "state", "race" = "raceEthnicity", "sex" = "sex_gender_standardized")) %>%
  mutate(releases = Total * p.m) %>%
  filter(!is.na(ageCat)) %>%
  filter(los_cutoff==60)
summed_data <- jail %>%
  filter(ageCat %in% c('65 or Older', '55-64')) %>%
  group_by(STATE, sex, race, ageCat) %>%
  summarise(releases = sum(releases, na.rm = TRUE)) %>%
  ungroup()
summed_data$ageCat <- '55+'
jail <- jail %>%
  filter(!(ageCat %in% c('65 or Older', '55-64'))) %>%
  rename("state" = "STATE")
jail <- bind_rows(jail, summed_data) %>%
  full_join(statedata, by = c("state" = "code")) %>%
  select(state, ageCat, sex, race, releases)


write.csv(jail, file = "jail.csv")



prison <- read_csv("prisonrel.lastyr.perstate.subgroups.csv")

prison = prison %>% 
  pivot_longer(
    cols = 2:61,
    names_to = c("sex", "ageCat", "race"),
    names_pattern = "(.*); (.*); (.*)",
    values_to = "releases"
  )  %>%
  full_join(statedata, by = c("state" = "state")) %>%
  select(code, ageCat, sex, race, releases)

prison = prison %>%
  mutate(race = str_replace(race, "White, non-Hispanic", "White"),
         race = str_replace(race, "Black, non-Hispanic", "Black"),
         race = str_replace(race, "Hispanic, any race", "Hispanic"),
         race = str_replace(race, "Missing race", "Other or Unknown")) %>%
  filter(if_any(race, ~ !(.x %in% c("Other race(s), non-Hispanic")))) %>%
  mutate(ageCat = str_replace(ageCat, "NA age", "Under 18")) %>%
  rename("state" = "code")


write.csv(prison, file = "prison.csv")


merged_data <- prison %>%
  full_join(jail, by=c("state", "ageCat", "sex", "race")) %>%
  group_by(state, ageCat, sex, race) %>%
  summarise(releases = sum(releases.x,releases.y, na.rm=T))

write.csv(merged_data, file = "merged_data.csv")




