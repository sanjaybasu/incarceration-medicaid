library(readr)
library(here)
library(dplyr)
library(stringr)
library(tidyr)

path_to_data <- file.path(here::here(), "data")

## load in all files and standardize names
### Jail releases in 2019 by state, race, and sex. Should be combined with the jail_stays df
df_jailReleasesTotal <- read_csv(file.path(here::here(), "data","jailCensus_byStateSexRace.csv")) %>%
  rename(state = STATE, sex_gender = sex, race_ethnicity = race, N = Total) %>%
  mutate(race_ethnicity = case_when(
    grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)","",race_ethnicity),
    race_ethnicity == "Multiracial, other, or missing" ~ "Other or Unknown",
    TRUE ~ race_ethnicity
  ))

### Jail stays. Jail releases does not contain age data, so the proportions in jail stays should be multiplies by the total number of releases
###   by state, race, and sex to get the number of people in an age group with length of stay (los) at or above the cutoff
df_jailStays <- read_csv(file.path(here::here(), "data","jailStays_byStateSexRaceAge.csv")) %>%
  rename(sex_gender = sex_gender_standardized, race_ethnicity = raceEthnicity, p_stay_m = p.m, p_stay_lwr = p.lwr, p_stay_upr = p.upr, p_stay_se = se) %>%
  separate(ageCat, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(age_min = case_when(ageCat == "65+" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(ageCat == "<18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max))
         ) %>%
  select(-c(p, ageCat))

df_jailReleases <- df_jailStays %>% 
  left_join(df_jailReleasesTotal %>% rename(N_se = se)) %>%
  transmute(state, race_ethnicity, sex_gender, age_min, age_max, los_cutoff, 
            N_total = N, N_se, p = p_stay_m, p_se = p_stay_se,
            N_jail = N*p_stay_m,
            N_jail_se = N*p_stay_m * sqrt((N_se/N)^2 + p_stay_se^2))
comment(df_jailReleases) <- "Total jail releases by length of stay."

### Prison releases
age_split <-  read_csv(file.path(here::here(), "data", "prison_age_split.csv")) %>%
  rename(sex_gender = RV0005, race_ethnicity = raceEthnicity, age_cat = ageCat1) %>%
  mutate(race_ethnicity = case_when(grepl("^Multiracial", race_ethnicity) ~ "Other or Unknown",
                                    grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)", "", race_ethnicity),
                                    grepl("^Hispanic", race_ethnicity) ~ "Hispanic"
  ))

state_cross <- tibble(state_name = state.name, state_abb = state.abb)
df_prisonReleases <- read_csv(file.path(here::here(), "data","prisonrel.lastyr.perstate.subgroups.csv")) %>%
  left_join(state_cross, by=c("state" = "state_name")) %>%
  select(-state) %>%
  rename(state = state_abb) %>%
  pivot_longer(-state, names_to = "subgroup", values_to = "N") %>%
  filter(subgroup != "RELYR") %>%
  separate(subgroup, c("sex_gender","age_cat","race_ethnicity2"), sep = "; ")  %>%
  mutate(race_ethnicity = case_when(grepl("^Other", race_ethnicity2) ~ "Other or Unknown",
                                    grepl("^Missing", race_ethnicity2) ~ "Other or Unknown",
                                    grepl(", non-Hispanic", race_ethnicity2) ~ gsub(", non-Hispanic", "", race_ethnicity2),
                                    grepl("^Hispanic", race_ethnicity2) ~ "Hispanic"
  )) %>%
  group_by(state, sex_gender, age_cat, race_ethnicity) %>%
  summarize(N = sum(N, na.rm = T)) %>% ungroup()

df_45plus <- df_prisonReleases %>%
  filter(age_cat %in% c("45-54","55+")) %>%
  left_join(age_split) %>%
  mutate(N.1 = N*pred, N.0 = N-N.1) %>%
  pivot_longer(cols = c("N.0","N.1")) %>%
  transmute(state, sex_gender, race_ethnicity, N = value, 
            age_cat = case_when(
              age_cat == "45-54" & name == "N.0" ~ "35-49",
              age_cat == "45-54" ~ "50-64",
              age_cat == "55+" & name == "N.0" ~ "50-64",
              age_cat == "55+" ~ "65 or older"))

df_prisonReleases <- df_prisonReleases %>%
  filter(!(age_cat %in% c("45-54","55+"))) %>%
  mutate(age_cat = case_when(
    age_cat %in% c("18-24","25-34") ~ "18-34",
    age_cat == "35-44" ~ "35-49"
  )) %>% filter(!is.na(age_cat)) %>%
  bind_rows(df_45plus) %>%
  group_by(state, sex_gender, age_cat, race_ethnicity) %>% summarize(N_prison = sum(N, na.rm = T)) %>% ungroup()

df_prisonReleases <- df_prisonReleases %>% ungroup() %>%
  separate(age_cat, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(age_min = as.numeric(ifelse(age_cat == "65 or older",65,age_min)), age_max = as.numeric(age_max)) %>%
  select(-age_cat)
comment(df_prisonReleases) <- "Total prison releases. Note there are quite a few missing in race_ethnicity (coded as 'Unknown')."

### Health outcomes incidence by sex, rage, and age
df_healthOutcomes <- read_csv(file.path(here::here(), "data","healthOutcomes_bySexRaceAge.csv")) %>%
  rename(sex_gender = sex, race_ethnicity = raceEthnicity, p_outcome_m = p, p_outcome_lwr = p.lwr, p_outcome_upr = p.upr, p_outcome_se = se) %>%
  separate(ageCat2, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(race_ethnicity = case_when(race_ethnicity == "Multiracial, other, or missing" ~ "Other or Unknown",
                                    grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)", "", race_ethnicity),
                                    T ~ race_ethnicity),
         age_min = case_when(ageCat2 == "65+" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(ageCat2 == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max))
  ) %>%
  select(-c(ageCat2, ageCat, n, neff))
comment(df_healthOutcomes) <- "Proportion of incarcerated population with a given health outcome by age x sex x race.\nTo get the number of people estimated to a health outcome within each state, multiply the proportions by total releases by state x race x sex x age."

### NHANES Health outcomes incidence by sex, rage, and age
df_nhanes <- read_csv(file.path(here::here(), "data","nhanes_byGenderAgeGroupRace.csv"))  %>%
  rename(race_ethnicity = Race, p_outcome_m = pred, p_outcome_lwr = pred.lwr, p_outcome_upr = pred.upr, p_outcome_se = pred.se) %>%
  separate(Age.Group, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(race_ethnicity = case_when(race_ethnicity == "Other" ~ "Other or Unknown",
                                    grepl("^(NH )", race_ethnicity) ~ gsub("^(NH )", "", race_ethnicity),
                                    T ~ race_ethnicity),
         sex_gender = case_when(Gender == "Men" ~ "Male",
                                Gender == "Women" ~ "Female"), 
         age_min = case_when(Age.Group == "65 and over" ~ 65,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(Age.Group == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max))
  ) %>%
  select(-c(Age.Group, Gender, p, p.lwr, p.upr, se))
comment(df_nhanes) <- "Proportion of Medicaid population with a given health outcome by age x sex x race."

df_ami <- read_csv(file.path(here::here(), "data","NSDUH_AMI_Results.csv")) %>%
  rename(sex_gender = Gender, race_ethnicity = Race, p_outcome_m = amipy, p_outcome_se = se) %>%
  separate(Age.Group, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(health_outcome = "AMI",
         sex_gender = case_when(sex_gender == "Men" ~ "Male",
                                sex_gender == "Women" ~ "Female"),
         age_min = case_when(Age.Group == "65 and over" ~ 65,
                             Age.Group == "19-34" ~ 18,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(Age.Group == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max)),
         race_ethnicity = case_when(race_ethnicity == "Other" ~ "Other or Unknown",
                                    grepl("^(NH )", race_ethnicity) ~ gsub("^(NH )", "", race_ethnicity),
                                    T ~ race_ethnicity)
         ) %>% select(-Age.Group)

df_smi <- read_csv(file.path(here::here(), "data","NSDUH_SMI_Results.csv")) %>%
  rename(sex_gender = Gender, race_ethnicity = Race, p_outcome_m = smipy, p_outcome_se = se) %>%
  separate(Age.Group, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(health_outcome = "SMI",
         sex_gender = case_when(sex_gender == "Men" ~ "Male",
                                sex_gender == "Women" ~ "Female"),
         age_min = case_when(Age.Group == "65 and over" ~ 65,
                             Age.Group == "19-34" ~ 18,
                             !is.na(age_min) ~ as.numeric(age_min)),
         age_max = case_when(Age.Group == "Under 18" ~ 17,
                             !is.na(age_max) ~ as.numeric(age_max)),
         race_ethnicity = case_when(race_ethnicity == "Other" ~ "Other or Unknown",
                                    grepl("^(NH )", race_ethnicity) ~ gsub("^(NH )", "", race_ethnicity),
                                    T ~ race_ethnicity)
  ) %>% select(-Age.Group)

df_nsduh <- bind_rows(df_ami, df_smi)
comment(df_nsduh) <- "Proportion of Medicaid population with a given health outcome by age x sex x race."

# ### Recidivism
# df_recidivism <- read_csv(file.path(here::here(), "data","recidisivm_byTypeSexRaceAge.csv")) %>%
#   rename(sex_gender = Gender, race_ethnicity = Race) %>%
#   separate(Age, c("age_min","age_max"), sep = "-", remove = F) %>%
#   mutate(age_min = case_when(Age == "65 or older" ~ 65,
#                              !is.na(age_min) ~ as.numeric(age_min)),
#          age_max = case_when(Age == "24 or younger" ~ 24,
#                              !is.na(age_max) ~ as.numeric(age_max))
#   ) %>%
#   select(-Age)
# comment(df_recidivism) <- "Monthly recidivism rate as a proportion. Time-to-recidivism is defined relative to their first rearrest. Note this is data from prisons only."  

save(df_jailReleases, df_prisonReleases, df_healthOutcomes, df_nhanes, df_nsduh, file = file.path(here::here(), "data", "input_standardized.rda"))
  
