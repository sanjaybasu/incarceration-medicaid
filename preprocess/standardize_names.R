library(readr)
library(here)
library(dplyr)
library(stringr)
library(tidyr)

path_to_data <- file.path(here::here(), "data")

## load in all files and standardize names
### Jail releases in 2019 by state and sex. Should be combined with the jail_stays df
df_jailReleasesTotal <- read_csv(file.path(here::here(), "data","jailCensus_byStateSex.csv")) %>%
  rename(state = STATE, sex_gender = sex, N = Total)

### Jail stays. The proportions in jail stays should be multiplies by the total number of releases
###   by state and sex to get the number of people with length of stay (los) at or above the cutoff
df_jailStays <- read_csv(file.path(here::here(), "data","jailStays_byStateSex.csv")) %>%
  rename(sex_gender = sex_gender_standardized, p_stay_m = p.m, p_stay_lwr = p.lwr, p_stay_upr = p.upr, p_stay_se = se) %>% 
  dplyr::select(-p)

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


state_cross <- tibble(state_name = state.name, state_abb = state.abb) %>%
  bind_rows(tibble(state_name = "District of Columbia", state_abb = "DC"))


df_prisonReleases <- read_csv(file.path(here::here(), "data","prisonReleasesNPS_bySexState.csv")) %>%
  rename(state = STATE) %>%
  filter(!(state %in% c("ST","US")))

df_fedPrisons <- read_csv(file.path(here::here(), "data","2021-bop-origin.csv")) %>%
  mutate(state_name = tolower(`Legal Residence`), p_fed = Inmates / sum(Inmates)) %>%
  left_join(state_cross %>% mutate(state_name = tolower(state_name))) %>%
  transmute(state_abb, p_fed, state = "FE") %>%
  right_join(df_prisonReleases %>% filter(state == "FE"), relationship = "many-to-many") %>%
  transmute(state = state_abb, sex_gender, N_prison = N_prison * p_fed)

df_prisonReleases <- bind_rows(df_prisonReleases %>% mutate(system = "State prison"), 
                               df_fedPrisons %>% mutate(system = "Federal prison"))

comment(df_prisonReleases) <- "Total prison releases. Note VT uses end-of-year data. AK, CT, DE, HI, RI, VT have combined jail/prison systems"

## AK is the only combined jail/prison system in the jail dataset.
df_jailReleases <- df_jailReleases

## Jail and prison release dataset
df_releases <- df_prisonReleases %>%
  rename(n_releases = N_prison) %>%
  filter(state != "FE") %>%
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
         p_outcome_lwr = p.lwr, p_outcome_upr = p.upr, p_outcome_se = se) %>%
  select(-c(n, neff))
comment(df_healthOutcomes) <- "Proportion of incarcerated population with a given health outcome by age x sex x race.\nTo get the number of people estimated to a health outcome within each state, multiply the proportions by total releases by state x race x sex x age."

### NHANES Health outcomes incidence by sex, rage, and age
df_nhanes <- read_csv(file.path(here::here(), "data","nhanes_byGenderAdults.csv"))  %>%
  rename(p_outcome_m = pred, p_outcome_lwr = pred.lwr, p_outcome_upr = pred.upr, p_outcome_se = pred.se) %>%
  mutate(sex_gender = Gender, 
         health_outcome = recode(health_outcome,
                                 ast = "asthma", ckd = "kidney", dm = "diabetes", 
                                 hbv = "hepB", hcv = "hepC",
                                 htn = "hypertension", str = "stroke", dep = "depression"
                                 )
  ) %>%
  select(-c(Gender, p, p.lwr, p.upr, se)) # Age.Group
comment(df_nhanes) <- "Proportion of Medicaid population with a given health outcome by age x sex x race."

df_nsduh <- read_csv(file.path(here::here(), "data","NSDUH_Adults_Results.csv")) %>%
  mutate(p_outcome_m = p) %>%
  rename(sex_gender = Gender, p_outcome_se = se) %>%
  mutate(sex_gender = case_when(sex_gender == "Men" ~ "Male",
                                sex_gender == "Women" ~ "Female"))

comment(df_nsduh) <- "Proportion of Medicaid population with a given health outcome by sex."

save(df_releases, df_healthOutcomes, df_nhanes, df_nsduh, file = file.path(here::here(), "data", "input_standardized.rda"))
  
