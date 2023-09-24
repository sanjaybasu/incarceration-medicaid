library(readr)
library(here)
library(dplyr)
library(predictrace)
library(stringr)
library(tidyr)
library(rlang)

path_to_jail_data <- file.path(here::here(), "data","raw","JailRosterData")
jdi_data_rosters <- read_csv(file.path(path_to_jail_data, "jdi_1693451429.csv"))

# Flag == 1 means there are data issues
# Also excluded were state-operated facilities in Alaska, Connecticut, Delaware, Hawaii, Rhode Island, and Vermont, which have combined jail-prison systems
process_name <- function(x){
  x <- gsub("[0-9]|\\.|x","",x)
  x <- gsub("( JR)|( SR)$","",x)
  surname <- gsub("\\-+$","",word(x,-1))
  surname <- gsub("\\-+","\\-",surname)
  surnames <- strsplit(surname,"-")
  return(surnames)
}

hisp_df = jdi_data_rosters %>%
  mutate(race = tolower(trimws(race)), ethnicity = tolower(trimws(ethnicity)),
    hispFlag = case_when(
    !is.na(race) & race %in% c("h","l","hisanic","hispanic","hispanic / black", "hispanic / white", "hispanic or latino","latino","puerto rican","w / h", "w/h", "white/hispanic",
                               "hispanic/latin/mexican","hispanic/latino","hispanic latino","hisp","hsp","mexican/hispanic", "mexican-latin american","white/hispanic latin",
                               "latino/hispanic","hs","white-hisp","black-hisp","black w/ hispanic origin","white w/ hispanic origin","white w/ hispanic origin",
                               "indian or alaskan native w/ hispanic origin", "asian or pacific islander w/ hispanic origin","hispan","mexican", "white hispanic",
                               "hispanic black", "hispanic white","cuban","mexican/national","h - hispanic or latino","black hispanic","hispanic/latino-oregon") ~ 1,
    !is.na(ethnicity) & ethnicity %in% c("h","hispanic or latino","hispanic","mexican","puerto rican","cuban","uruguayan","honduran","colombian","brazilian","bolivian",
                     "guyanese","latin american/hispanic (not further defined)","nicaraguan","ecuadorian","venezuelan","latin american/hispanic (not elsewhere classified)",
                     "costa rican","paraguayan","peruvian","h-white/latin, hispanic","h-indian/alask_nativ, hispanic","h-black, hispanic",
                     "latino") ~ 1, 
    TRUE ~ 0
  )) %>%
  select(id, person_id, race_ethnicity_standardized, race, ethnicity, hispFlag) %>%
  filter(hispFlag == 1 | race_ethnicity_standardized %in% c("Other POC","Unknown Race"))
  
hisp_df <- hisp_df %>%
  mutate(surname = process_name(person_id)) %>%
  unnest(surname) %>%
  mutate(givenname = word(person_id, 1),
         hispSurname = predict_race(surname, probability = F, surname = T)$likely_race,
         hispGivenname = predict_race(givenname, probability = F, surname = F)$likely_race,
         predHispS = grepl("hisp",hispSurname), 
         predHispG = grepl("hisp",hispGivenname))

hisp_df2 <- hisp_df %>%
  group_by(id, person_id, race_ethnicity_standardized, race, ethnicity, hispFlag,predHispG) %>%
  summarise(predHispS = any(predHispS)) %>% ungroup() %>%
  mutate(predHisp = pmax(predHispS, predHispG),
         hispanic = pmax(hispFlag, predHisp, na.rm = T))

#only merge persons from Other or Unknown Race
jdi_data_rosters  <- jdi_data_rosters %>%
  left_join(hisp_df2 %>% filter(race_ethnicity_standardized %in% c("Other POC","Unknown Race")) %>% select(id, hispFlag, predHisp, hispanic)) %>%
  replace_na(list(hispanic = 0))

jdi_data_rosters <- jdi_data_rosters %>%
  mutate(ageCat = cut(age, breaks = c(0,17,24,34,44,54,64,110), 
                      labels = c("Under 18","18-24","25-34","35-44","45-54","55-64","65 or Older"))) %>%
  group_by(state, county) %>%
  mutate(flagF = first_seen == min(first_seen, na.rm = T),
         flagL = last_seen ==  max(last_seen, na.rm = T)) %>% #flag observations that start or end on first or last day of data collection for state + county
  ungroup()

rm(hisp_df)
rm(hisp_df2)
gc()
save(jdi_data_rosters, file = file.path(path_to_jail_data, "jdi_hispanic.RData"))

library(MASS)
# library(fitdistrplus)
# library(logspline)
# # https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
# descdist(x.days, discrete = TRUE)
# 
# fit.nb <- fitdist(x.days, "nbinom")
# fit.poisson <- fitdist(x.days, "pois")
# plot(fit.nb)
# plot(fit.poisson)

date_covid_policy_start <- as.Date("2020-03-01")
date_covid_policy_end <- as.Date("2021-12-31") #note this refers to when jail population rebounded to pre-Covid levels.
# Citation: https://www.pewtrusts.org/en/research-and-analysis/articles/2022/11/17/many-jails-are-as-full-as-they-were-before-covid-19-pandemic

#exclude anyone that was admitted or released within this date range

jdi_data_rosters <- jdi_data_rosters %>%
  mutate(flagCovidStart = ifelse(between(first_seen, date_covid_policy_start, date_covid_policy_end), 1, 0),
         flagCovidEnd = ifelse(between(last_seen, date_covid_policy_start, date_covid_policy_end), 1, 0))

print(jdi_data_rosters %>% group_by(flag, flagF, flagL) %>% summarise(p = n() / nrow(jdi_data_rosters)))
n_tot <- jdi_data_rosters %>% filter(flag == 0, !flagF, !flagL) %>% nrow()
print(jdi_data_rosters %>% filter(flag == 0, !flagF, !flagL) %>% group_by(flagCovidStart, flagCovidEnd) %>% summarise(p = n() / n_tot))

#very few (0.5%) that were released during covid (and weren't admitted during covid). We choose not censor as it could bias data

jdi_data_rosters <- jdi_data_rosters %>% 
  filter(flag == 0, !flagF, !flagL, flagCovidStart == 0) %>%
  mutate(raceEthnicity = case_when(
              hispanic == 1 ~ "Hispanic",
              race_ethnicity_standardized %in% c("White","Black") ~ race_ethnicity_standardized,
              TRUE ~ "Other or Unknown"),
         year_admit = format(first_seen,"%Y"),
         year_release = format(last_seen,"%Y")) %>%
  dplyr::select(id, last_seen, state, county, raceEthnicity, age, ageCat, sex_gender_standardized, los)

state_county <- jdi_data_rosters %>%
  dplyr::select(state, county) %>%
  distinct() %>%
  mutate(countyid = factor(1:n()))

jdi_data_rosters <- jdi_data_rosters %>%
  left_join(state_county)

gc()

save(jdi_data_rosters, file = file.path(path_to_jail_data, "jdi_flag.RData"))

# Many jails do not report age (age missing in ~18.6% of bookings).
# No jail census information. Filter to state-county-admit years with < 5% missing, then find % ageCat by state, race, sex
# county_year_NA <- jdi_data_rosters %>%
#   group_by(state, county, countyid, year_admit) %>%
#   summarize(pNA = sum(is.na(ageCat))/n()) 
# 
# jdi_complete <- jdi_data_rosters %>%
#   inner_join(county_year_NA %>%
#                filter(pNA < .05) %>% 
#                select(countyid, year_admit))
# 
# #WV data has around 20% missing (only one county's data listed). TBD what to do
# ageEsts <- jdi_complete %>%
#   filter(!is.na(ageCat)) %>%
#   group_by(state, year_admit, raceEthnicity, sex_gender_standardized, ageCat) %>%
#   summarize(n = n()) %>% ungroup() %>%
#   group_by(state, year_admit, raceEthnicity, sex_gender_standardized) %>%
#   mutate(p = n/sum(n))

#multiple imputation
library(Amelia)
library(lubridate)
library(parallel)
library(snow)
numCores <- detectCores()-1

jdi_data_agg <- jdi_data_rosters %>%
  group_by(state, raceEthnicity, sex_gender_standardized) %>%
  count() %>%
  ungroup()

write_csv(jdi_data_agg, file = file.path(path_to_jail_data, "jdi_agg.csv"))

jdi_data_rosters <- jdi_data_rosters %>% 
  mutate(release_month = floor_date(last_seen),
         age = factor(age, ordered = T),
         countyid = factor(countyid)
  ) %>%
  dplyr::select(id, los, state, last_seen, countyid, release_month, raceEthnicity, sex_gender_standardized, age) %>%
  filter(los > 29)

gc()
imp <- amelia(jdi_data_rosters, m=5, p2s = 1, idvars = c("id","last_seen","los"),
              cs="state",
              noms=c("countyid","raceEthnicity","sex_gender_standardized"),
              ords=c("age"),ts = "release_month",
              parallel="snow",ncpus=numCores)
save(imp, file = file.path(path_to_jail_data, "jdi_impO.RData"))

imputations <- lapply(imp$imputations, 
                          function(x) {
                            x$ageN <- as.numeric(as.character(x$age))
                            return(x)
                            })

imp_age <- bind_rows(imputations) %>% select(ageN) %>% rename(age = ageN) %>% mutate(group = "Imputed")
imp_age2 <- imputations$imp1[!imp$missMatrix[,"age"],"ageN"] %>% rename(age = ageN) %>% mutate(group = "Observed")
imp_age <- bind_rows(imp_age, imp_age2)

pMiss <- sum(imp$missMatrix[,"age"])/nrow(imp$missMatrix)

library(ggplot2)
g <- ggplot(imp_age, aes(x = age, color = group)) + 
  geom_density(adjust = 2, alpha = 0.5) +  # smooth since age is discrete
  theme_classic(base_size = 14) + 
  xlab(paste0("Age -- Missing frac: ", round(pMiss, digits = 3)))
  
path_to_figs <- file.path(here::here(), "preprocess", "figs")
ggsave(file.path(path_to_figs,"imputed_density.pdf"), width = 6, height = 6)


#negative binomial is a much better fit
library(pscl)
mod <- zeroinfl(los-1 ~ ageCat + race + state + countyid | 1, 
                dist = "negbin",
              data = jdi_data_rosters, subset = state %in% c("CA","WA"))

#https://library.virginia.edu/data/articles/simulating-data-for-count-models
z <- rbinom(n = n, size = 1, 
            prob = 1/(1 + exp(-(-0.8 + 1.8 * (male == 1))))) 
y_sim <- ifelse(z == 0, 0, 
                rnbinom(n = n, 
                        mu = exp(1.3 + 1.5 * (male == 1)), 
                        size = 2))

y_base <- rnbinom(n = 1000, #number of simulated samples
                 mu = exp(0.912486 + 2.386545 + 0.259958 + -0.114982),  #this is where the intercept and slopes go
                 size = 0.408672) #theta
y_sim <- rnbinom(n = 1000, #number of simulated samples
                 mu = exp(0.912486 + 2.386545 + 0.259958 + -0.114982),  #this is where the intercept and slopes go
                 size = 0.408672) #theta

#use zero inflated negative binomials
baseline = as.data.frame(list(x=y_base))
sim = as.data.frame(list(x = y_sim))
real = as.data.frame(list(x = jdi_data_rosters %>% filter(state == "WA", ageCat == "18-24", race == "Black") %>% pull(los)))
ggplot() + 
  geom_histogram(data = sim, aes(x=x, y=..density..), color = "red", fill = NA) +
  geom_histogram(data = real, aes(x=x, y=..density..), color = "black", fill = NA)

#AL: aggregate data from Montana and Idaho
#Connecticut & RI: Aggregate data from 
