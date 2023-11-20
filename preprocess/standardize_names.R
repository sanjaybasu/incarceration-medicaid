library(readr)
library(here)
library(dplyr)
library(stringr)
library(tidyr)

path_to_data <- file.path(here::here(), "data")

## load in all files and standardize names
### Jail releases in 2019 by state, race, and sex. Should be combined with the jail_stays df
df_jailReleases <- read_csv(file.path(here::here(), "data","jailCensus_byStateSexRace.csv")) %>%
  rename(state = STATE, sex_gender = sex, race_ethnicity = race, N = Total) %>%
  mutate(race_ethnicity = case_when(
    grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)","",race_ethnicity),
    TRUE ~ race_ethnicity
  ))
comment(df_jailReleases) <- "Counts of total jail releases in 2019 by state x race x sex. Excludes jails that only keep people for <72hr."

### Jail stays. Jail releases does not contain age data, so the proportions in jail stays should be multiplies by the total number of releases
###   by state, race, and sex to get the number of people in an age group with length of stay (los) at or above the cutoff
df_jailStays <- read_csv(file.path(here::here(), "data","jailStays_byStateSexRaceAge.csv")) %>%
  rename(sex_gender = sex_gender_standardized, race_ethnicity = raceEthnicity, p_stay_m = p.m, p_stay_lwr = p.lwr, p_stay_upr = p.upr) %>%
  separate(ageCat, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(age_min = case_when(ageCat == "65 or Older" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(ageCat == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max))
         ) %>%
  select(-c(p, se, ageCat))
comment(df_jailStays) <- "Proportion of state x race x sex that are in age group and meet the length of stay cutoff.\nTo get the number of people estimated to have los >= los_cutoff within each age group, multiply the poportions by total jail releases by state x race x sex."

### Prison releases
state_cross <- tibble(state_name = state.name, state_abb = state.abb)
df_prisonReleases <- read_csv(file.path(here::here(), "data","prisonrel.lastyr.perstate.subgroups.csv")) %>%
  left_join(state_cross, by=c("state" = "state_name")) %>%
  select(-state) %>%
  rename(state = state_abb) %>%
  pivot_longer(-state, names_to = "subgroup", values_to = "N")

df_prisonReleases <- df_prisonReleases %>%
  filter(subgroup != "RELYR") %>%
  separate(subgroup, c("sex_gender","age_cat","race_ethnicity"), sep = "; ") %>%
  separate(age_cat, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(race_ethnicity = case_when(grepl("^Other", race_ethnicity) ~ "Other",
                                   grepl("^Missing", race_ethnicity) ~ "Unknown",
                                   grepl(", non-Hispanic", race_ethnicity) ~ gsub(", non-Hispanic", "", race_ethnicity),
                                   grepl("^Hispanic", race_ethnicity) ~ "Hispanic"
                                   ),
         age_min = as.numeric(ifelse(age_cat == "55+",55,age_min)), age_max = as.numeric(age_max)) %>%
  select(-age_cat)
comment(df_prisonReleases) <- "Total prison releases. Note there are quite a few missing in race_ethnicity (coded as 'Unknown')."

### Health outcomes incidence by sex, rage, and age
df_healthOutcomes <- read_csv(file.path(here::here(), "data","healthOutcomes_bySexRaceAge.csv")) %>%
  rename(sex_gender = sex, race_ethnicity = raceEthnicity, p_outcome_m = p, p_outcome_lwr = p.lwr, p_outcome_upr = p.upr, p_outcome_se = se) %>%
  separate(ageCat, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(race_ethnicity = case_when(race_ethnicity == "Multiracial, other, or missing" ~ "Other or Unknown",
                                    grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)", "", race_ethnicity),
                                    T ~ race_ethnicity),
         age_min = case_when(ageCat == "65 or Older" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(ageCat == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max))
  ) %>%
  select(-ageCat)
comment(df_healthOutcomes) <- "Proportion of incarcerated population with a given health outcome by age x sex x race.\nTo get the number of people estimated to a health outcome within each state, multiply the proportions by total releases by state x race x sex x age."

### NHANES Health outcomes incidence by sex, rage, and age
df_NHANES <- read_csv(file.path(here::here(), "data","nhanes_byGenderAgeGroupRace.csv"))  %>%
  rename(sex_gender = Gender, race_ethnicity = Race, p_outcome_m = pred, p_outcome_lwr = pred.lwr, p_outcome_upr = pred.upr, p_outcome_se = pred.se) %>%
  separate(Age.Group, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(race_ethnicity = case_when(race_ethnicity == "Other" ~ "Other or Unknown",
                                    grepl("^(NH )", race_ethnicity) ~ gsub("^(NH )", "", race_ethnicity),
                                    T ~ race_ethnicity),
         age_min = case_when(Age.Group == "65 and over" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(Age.Group == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max))
  ) %>%
  select(-c(Age.Group, p, p.lwr, p.upr, se))
comment(df_NHANES) <- "Proportion of Medicaid population with a given health outcome by age x sex x race."

### Recidivism
df_recidivism <- read_csv(file.path(here::here(), "data","recidisivm_byTypeSexRaceAge.csv")) %>%
  rename(sex_gender = Gender, race_ethnicity = Race) %>%
  separate(Age, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(age_min = case_when(Age == "65 or older" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(Age == "24 or younger" ~ 24,
                             !is.na(age_max) ~ as.numeric(age_max))
  ) %>%
  select(-Age)
comment(df_recidivism) <- "Monthly recidivism rate as a proportion. Time-to-recidivism is defined relative to their first rearrest. Note this is data from prisons only."  

save(df_jailReleases, df_jailStays, df_prisonReleases, df_recidivism, df_healthOutcomes, df_NHANES, file = file.path(here::here(), "data", "input_standardized.rda"))
  
