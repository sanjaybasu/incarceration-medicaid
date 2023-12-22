library(readr)
library(here)
library(dplyr)
library(stringr)
library(tidyr)

path_to_data <- file.path(here::here(), "data")

## load in all files and standardize names
### Jail releases in 2019 by state and sex. Should be combined with the jail_stays df
df_jailReleasesTotal <- read_csv(file.path(here::here(), "data","jailCensus_byStateSex.csv")) %>%
  rename(state = STATE, sex_gender = sex, N = Total) # %>% race_ethnicity = race, 
  # mutate(race_ethnicity = case_when(
  #   grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)","",race_ethnicity),
  #   race_ethnicity == "Multiracial, other, or missing" ~ "Other or Unknown",
  #   TRUE ~ race_ethnicity
  # ))

### Jail stays. The proportions in jail stays should be multiplies by the total number of releases
###   by state and sex to get the number of people with length of stay (los) at or above the cutoff
df_jailStays <- read_csv(file.path(here::here(), "data","jailStays_byStateSex.csv")) %>%
  rename(sex_gender = sex_gender_standardized, p_stay_m = p.m, p_stay_lwr = p.lwr, p_stay_upr = p.upr, p_stay_se = se) %>% # race_ethnicity = raceEthnicity, 
  # separate(ageCat, c("age_min","age_max"), sep = "-", remove = F) %>%
  # mutate(age_min = case_when(ageCat == "65+" ~ 65,
  #                            !is.na(age_min) ~ as.numeric(age_min)),
  #        age_max = case_when(ageCat == "<18" ~ 17,
  #                            !is.na(age_max) ~ as.numeric(age_max))
  #        ) %>%
  select(-c(p)) # ageCat

smallStates <- df_jailStays %>% 
  filter(los_cutoff == 30) %>% 
  group_by(state) %>% 
  summarize(nStrata = sum(nStrata,na.rm = T)) %>% 
  filter(nStrata < 5000, !is.na(state)) %>%
  pull(state)

df_jailReleases <- df_jailStays %>% 
  filter(!(state %in% smallStates)) %>%
  right_join(df_jailReleasesTotal %>% rename(N_se = se))

# Used model output with random effects for state to estimate approximate releases in states without enough or any data
df_jailReleasesSub <- df_jailReleases %>%
  filter(is.na(p_stay_m)) %>%
  select(state, sex_gender, N, N_se) %>%
  left_join(df_jailStays %>% filter(is.na(state)) %>% select(-state), relationship = "many-to-many") %>%
  mutate(p_stay_se = NA)

df_jailReleases <- df_jailReleases %>%
  filter(!is.na(p_stay_m)) %>%
  bind_rows(df_jailReleasesSub) %>%
  transmute(state, sex_gender, los_cutoff, # race_ethnicity, age_min, age_max, 
            N_total = N, N_se, p = p_stay_m, p_se = p_stay_se,
            N_jail = N*p_stay_m,
            N_jail_se = N*p_stay_m * sqrt((N_se/N)^2 + p_stay_se^2))
comment(df_jailReleases) <- "Total jail releases by length of stay."

### Prison releases
# age_split <-  read_csv(file.path(here::here(), "data", "prison_age_split.csv")) %>%
#   rename(sex_gender = RV0005, race_ethnicity = raceEthnicity, age_cat = ageCat1) %>%
#   mutate(race_ethnicity = case_when(grepl("^Multiracial", race_ethnicity) ~ "Other or Unknown",
#                                     grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)", "", race_ethnicity),
#                                     grepl("^Hispanic", race_ethnicity) ~ "Hispanic"
#   ))

state_cross <- tibble(state_name = state.name, state_abb = state.abb) %>%
  bind_rows(tibble(state_name = "District of Columbia", state_abb = "DC"))

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

df_prisonReleases <- df_prisonReleases %>%
  group_by(state, sex_gender) %>%
  summarise(N_prison = sum(N, na.rm = T)) %>% ungroup()

# df_45plus <- df_prisonReleases %>%
#   filter(age_cat %in% c("45-54","55+")) %>%
#   left_join(age_split) %>%
#   mutate(N.1 = N*pred, N.0 = N-N.1) %>%
#   pivot_longer(cols = c("N.0","N.1")) %>%
#   transmute(state, sex_gender, race_ethnicity, N = value, 
#             age_cat = case_when(
#               age_cat == "45-54" & name == "N.0" ~ "35-49",
#               age_cat == "45-54" ~ "50-64",
#               age_cat == "55+" & name == "N.0" ~ "50-64",
#               age_cat == "55+" ~ "65 or older"))
# 
# df_prisonReleases <- df_prisonReleases %>%
#   filter(!(age_cat %in% c("45-54","55+"))) %>%
#   mutate(age_cat = case_when(
#     age_cat %in% c("18-24","25-34") ~ "18-34",
#     age_cat == "35-44" ~ "35-49"
#   )) %>% filter(!is.na(age_cat)) %>%
#   bind_rows(df_45plus) %>%
#   group_by(state, sex_gender, age_cat, race_ethnicity) %>% summarize(N_prison = sum(N, na.rm = T)) %>% ungroup()
# 
# df_prisonReleases <- df_prisonReleases %>% ungroup() %>%
#   separate(age_cat, c("age_min","age_max"), sep = "-", remove = F) %>%
#   mutate(age_min = as.numeric(ifelse(age_cat == "65 or older",65,age_min)), age_max = as.numeric(age_max)) %>%
#   select(-age_cat)
comment(df_prisonReleases) <- "Total prison releases. Note VT uses end-of-year data. AK, CT, DE, HI, RI, VT have combined jail/prison systems"

## AK is the only combined jail/prison system in the jail dataset.
### We will use the data from the prison releases file, since it's more complete.
df_jailReleases <- df_jailReleases %>%
  filter(state != "AK")

## Jail and prison release dataset
df_releases <- df_prisonReleases %>%
  rename(n_releases = N_prison) %>%
  mutate(system = "State prison") %>%
  bind_rows(
    df_jailReleases %>% transmute(state, sex_gender, los_cutoff, 
                                  n_releases = N_jail, se_releases = N_jail_se, system = "Jail")
  ) %>%
  mutate(combined_system = state %in% c("AK","CT","DE","HI","RI","VT"))

comment(df_releases) <- "Total releases. Note that AK, CT, DE, HI, RI, VT have combined jail/prison systems. VT prison data missing."

### Health outcomes in adults by system and sex
df_healthOutcomes <- read_csv(file.path(here::here(), "data","healthOutcomes_bySystemSex.csv")) %>%
  select(-System) %>%
  rename(system = System2, sex_gender = sex, p_outcome_m = p,
         p_outcome_lwr = p.lwr, p_outcome_upr = p.upr, p_outcome_se = se) %>% #race_ethnicity = raceEthnicity, 
  # separate(ageCat2, c("age_min","age_max"), sep = "-", remove = F) %>%
  # mutate(race_ethnicity = case_when(race_ethnicity == "Multiracial, other, or missing" ~ "Other or Unknown",
  #                                   grepl(" \\(NH\\)", race_ethnicity) ~ gsub(" \\(NH\\)", "", race_ethnicity),
  #                                   T ~ race_ethnicity),
  #        age_min = case_when(ageCat2 == "65+" ~ 65,
  #                            !is.na(age_min) ~ as.numeric(age_min)),
  #        age_max = case_when(ageCat2 == "Under 18" ~ 17,
  #                            !is.na(age_max) ~ as.numeric(age_max))
  # ) %>%
  select(-c(n, neff)) # ageCat2, ageCat, 
comment(df_healthOutcomes) <- "Proportion of incarcerated population with a given health outcome by age x sex x race.\nTo get the number of people estimated to a health outcome within each state, multiply the proportions by total releases by state x race x sex x age."

### NHANES Health outcomes incidence by sex, rage, and age
df_nhanes <- read_csv(file.path(here::here(), "data","nhanes_byGenderAdults.csv"))  %>%
  rename(p_outcome_m = pred, p_outcome_lwr = pred.lwr, p_outcome_upr = pred.upr, p_outcome_se = pred.se) %>% # race_ethnicity = Race, 
  # separate(Age.Group, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(sex_gender = case_when(Gender == "Men" ~ "Male",
                                Gender == "Women" ~ "Female"), 
         # race_ethnicity = case_when(race_ethnicity == "Other" ~ "Other or Unknown",
         #                            grepl("^(NH )", race_ethnicity) ~ gsub("^(NH )", "", race_ethnicity),
         #                            T ~ race_ethnicity),
         # age_min = case_when(Age.Group == "65 and over" ~ 65,
         #                     !is.na(age_min) ~ as.numeric(age_min)),
         # age_max = case_when(Age.Group == "Under 18" ~ 17,
         #                     !is.na(age_max) ~ as.numeric(age_max)),
         health_outcome = recode(health_outcome,
                                 ast = "asthma", ckd = "kidney", dm = "diabetes", 
                                 hbv = "hepB", hcv = "hepC",
                                 htn = "hypertension", str = "stroke", dep = "depression"
                                 )
  ) %>%
  select(-c(Gender, p, p.lwr, p.upr, se)) # Age.Group
comment(df_nhanes) <- "Proportion of Medicaid population with a given health outcome by age x sex x race."

df_nsduh <- read_csv(file.path(here::here(), "data","NSDUH_AMI_Adults_Results.csv")) %>%
  mutate(health_outcome = "AMI", p_outcome_m = amipy) %>%
  bind_rows(read_csv(file.path(here::here(), "data","NSDUH_SMI_Adults_Results.csv")) %>%
              mutate(health_outcome = "SMI", p_outcome_m = smipy)) %>%
  rename(sex_gender = Gender, p_outcome_se = se) %>% # race_ethnicity = Race, 
  # separate(Age.Group, c("age_min","age_max"), sep = "-", remove = F) %>%
  mutate(sex_gender = case_when(sex_gender == "Men" ~ "Male",
                                sex_gender == "Women" ~ "Female") #,
         # age_min = case_when(Age.Group == "65 and over" ~ 65,
         #                     Age.Group == "19-34" ~ 18,
         #                     !is.na(age_min) ~ as.numeric(age_min)),
         # age_max = case_when(Age.Group == "Under 18" ~ 17,
         #                     !is.na(age_max) ~ as.numeric(age_max)),
         # race_ethnicity = case_when(race_ethnicity == "Other" ~ "Other or Unknown",
         #                            grepl("^(NH )", race_ethnicity) ~ gsub("^(NH )", "", race_ethnicity),
         #                            T ~ race_ethnicity)
         ) %>% select(-c(smipy, amipy)) #Age.Group

comment(df_nsduh) <- "Proportion of Medicaid population with a given health outcome by sex."

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

save(df_releases, df_healthOutcomes, df_nhanes, df_nsduh, file = file.path(here::here(), "data", "input_standardized.rda"))
  
