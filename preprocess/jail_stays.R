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
  mutate(ageCat = cut(age, breaks = c(0, 17.5, 34.5, 49.5, 64.5, 100), labels = c("<18","18-34","35-49","50-64","65+"))) %>%
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
  dplyr::select(id, year_admit, year_release, first_seen, last_seen, state, county, raceEthnicity, age, ageCat, sex_gender_standardized, los)

state_county <- jdi_data_rosters %>%
  dplyr::select(state, county) %>%
  distinct() %>%
  mutate(countyid = factor(1:n()))

jdi_data_rosters <- jdi_data_rosters %>%
  left_join(state_county)

gc()

save(jdi_data_rosters, file = file.path(path_to_jail_data, "jdi_flag.RData"))

# Many jails do not report age (age missing in ~18.6% of bookings).
# No jail census information. Filter to state-county-admit years with < 20% missing among obs with los >= 30, then find % ageCat by state, race, sex
county_year_NA <- jdi_data_rosters %>%
  group_by(state, county, countyid, year_admit) %>%
  summarize(pNA = sum(is.na(ageCat) & los >= 30)/sum(los >= 30), p72 = sum(los > 3)/n()) %>% ungroup()
hist(county_year_NA$pNA)

print("Removing county-years that are only 72hr holding. Filtering out all jails where entire population stays 3 days or less")
n_jails72 <- sum(county_year_NA$p72 == 0)
jdi_complete <- jdi_data_rosters %>%
  left_join(county_year_NA) %>%
  filter(p72 > 0)
n_removed72 <- nrow(jdi_data_rosters) - nrow(jdi_complete)
print(paste0("    Number of county-years removed ", n_jails72, " (", round(100*n_jails72/nrow(county_year_NA), 1), "%)"))
print(paste0("    Number of observations removed ", n_removed72, " (", round(100*n_removed72/nrow(jdi_data_rosters), 1), "%)"))


print("Removing jails-year with <20% missing age & los >= 30")
n_jailsM <- nrow(county_year_NA %>% filter(p72 > 0, pNA >= 0.2))
n_removedM <- nrow(jdi_complete %>% filter(pNA >= 0.2))
print(paste0("    Number of counties-years removed ", n_jailsM, " (", 
             round(100*n_jailsM/nrow(county_year_NA %>% filter(p72 > 0)), 1), "%)"))
print(paste0("    Number of observations removed ", n_removedM, " (", round(100*n_removedM/nrow(jdi_complete), 1), "%)"))

jdi_complete <- jdi_complete %>%
  filter(pNA < 0.2)

print("Additional number of NAs")
n_NA <- sum(is.na(jdi_complete$age))
print(paste0("    Number of individuals with missing age and los >= 30", n_NA, " (", round(100*n_NA/nrow(jdi_complete), 1), "%)"))

losEsts <- jdi_complete %>% 
  mutate(los30 = as.numeric(los > 29), 
         los60 = as.numeric(los > 59),
         los90 = as.numeric(los > 89)) %>%
  group_by(state, countyid) %>%
  summarize(n30 = sum(los30), n60 = sum(los60), n90 = sum(los90), nStrata = n()) %>% ungroup()

ageEsts <- jdi_complete %>% 
  filter(!is.na(ageCat), sex_gender_standardized %in% c("Male","Female")) %>%
  mutate(los30 = as.numeric(los > 29), 
         los60 = as.numeric(los > 59),
         los90 = as.numeric(los > 89)) %>%
  group_by(state, countyid, raceEthnicity, sex_gender_standardized, ageCat) %>%
  summarize(n30 = sum(los30), n60 = sum(los60), n90 = sum(los90), n = n()) %>% ungroup() %>%
  group_by(state, countyid, raceEthnicity, sex_gender_standardized) %>%
  mutate(nStrata = sum(n), p30 = n30/nStrata, p60 = n60/nStrata, p90 = n90/nStrata) %>% ungroup()

# We aggregate data by state, county, agecat, sex, and race
##  We fit a mixed effects model with county as random effect, with outcomes as the proportion of the strata population being from an age group
##  Stratas are defined by state, sex, and race
hist(ageEsts$p30)

library(glmmTMB)
library(bbmle)
mod.poisson <- glmmTMB(n30~state + (1|countyid) + raceEthnicity + ageCat + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = poisson,
                    verbose = F,
                    data=ageEsts)

mod.znb1 <- update(mod.poisson,family=nbinom1)
mod.znb2 <- update(mod.poisson,family=nbinom2)
mod.hnb1 <- update(mod.poisson, family=truncated_nbinom1)

mod.iznb2 <- glmmTMB(n30~state + raceEthnicity * ageCat + sex_gender_standardized + (1|countyid) + offset(log(nStrata)),
                       ziformula=~1,
                       family = nbinom2,
                       verbose = F,
                       data=ageEsts)
mod.i2znb2 <- glmmTMB(n30~state + raceEthnicity + ageCat * sex_gender_standardized + (1|countyid) + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     data=ageEsts)

AICtab(mod.poisson,mod.znb1,mod.znb2,mod.hnb1,mod.iznb2,mod.i2znb2)

## we will use the mod.iznb2 formula for los >= 30, los >= 60, los >= 90
mod.n30 <- mod.iznb2
rm(mod.poisson,mod.znb1,mod.znb2,mod.hnb1,mod.iznb2,mod.i2znb2)

mod.n60 <- glmmTMB(n60~state + raceEthnicity * ageCat + sex_gender_standardized + (1|countyid) + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)),
                     data=ageEsts)

mod.n90 <- glmmTMB(n90~state + raceEthnicity * ageCat + sex_gender_standardized + (1|countyid) + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)),
                     verbose = F,
                     data=ageEsts)

mod.n30s <- glmmTMB(n30~state + (1|countyid) + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     data=losEsts)

mod.n60s <- glmmTMB(n60~state + (1|countyid) + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n90s <- glmmTMB(n90~state + (1|countyid) + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n30s2 <- glmmTMB(n30~(1|state) + (1|countyid) + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n60s2 <- glmmTMB(n60~(1|state) + (1|countyid) + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n90s2 <- glmmTMB(n90~(1|state) + (1|countyid) + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)


if(F){
  # some visualization to look at fit
  pStays <- ageEsts %>% 
    group_by(state, raceEthnicity, sex_gender_standardized, ageCat) %>%
    summarize(nStrata = mean(nStrata), countyid = NA) %>% ungroup()
  
  p30 <- predict(mod.znb2, type = "response") / ageEsts$nStrata
  p30C <- predict(mod.iznb2, newdata = pStays, re.form = NA ,type = "response") / pStays$nStrata
  
  
  datC <- ageEsts %>% group_by(state, raceEthnicity, sex_gender_standardized, ageCat) %>%
    summarize(p30 = weighted.mean(p30, nStrata), p30.min = min(p30), p30.max = max(p30), nStrata2 = mean(nStrata)) %>%
    ungroup() %>% mutate(group = "obs") %>% rename(nStrata = nStrata2)
  
  datC <- bind_rows(datC, cbind(pStays, p30 = p30C) %>% mutate(group = "pred"))
  
  dat <- bind_rows(ageEsts %>% mutate(group = "obs"),
                   cbind(ageEsts %>% dplyr::select(-p30), p30 = p30) %>% dplyr::mutate(group = "pred"))
  
  dat_wide <- dat %>%
    pivot_wider(id_cols = c(state, raceEthnicity, sex_gender_standardized, ageCat, countyid, nStrata), 
                names_from = "group", values_from = "p30")
  
  ggplot(dat_wide, aes(x = obs, y = pred)) + 
    geom_point(size = 0.1, alpha = 0.2) + 
    geom_smooth(method = "lm", mapping = aes(weight = nStrata), 
                color = "red", show.legend = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggtitle("Q-Q plot (weighted line)") +
    theme_classic()
  
  states <- unique(pStays$state)
  ggplot(datC %>% filter(state %in% states[26:50], sex_gender_standardized == "Female"), 
         aes(x = ageCat, color = raceEthnicity, group = interaction(raceEthnicity, group))) +
    geom_line(aes(y = p30, linetype = group)) + 
    # geom_errorbar(aes(ymin = p30.min, ymax = p30.max)) + 
    facet_wrap(~state, ncol = 5) +
    theme_classic()
}

df_comb <- ageEsts %>% 
  expand(state, raceEthnicity, sex_gender_standardized, ageCat) %>%
  dplyr::mutate(countyid = NA, nStrata = 1)

df_combs <- losEsts %>%
  tidyr::expand(state) %>%
  dplyr::mutate(countyid = NA, nStrata = 1)
df_combs2 <- df_combs[1,] %>% mutate(state = NA)
  

npred30 <- predict(mod.n30, newdata = df_comb, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p30.m = fit, p30.se = se.fit)
npred60 <- predict(mod.n60, newdata = df_comb, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p60.m = fit, p60.se = se.fit)
npred90 <- predict(mod.n90, newdata = df_comb, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p90.m = fit, p90.se = se.fit)

npred30s <- predict(mod.n30s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p30.m = fit, p30.se = se.fit)
npred60s <- predict(mod.n60s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p60.m = fit, p60.se = se.fit)
npred90s <- predict(mod.n90s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p90.m = fit, p90.se = se.fit)
npred30s2 <- predict(mod.n30s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p30.m = fit, p30.se = se.fit)
npred60s2 <- predict(mod.n60s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p60.m = fit, p60.se = se.fit)
npred90s2 <- predict(mod.n90s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p90.m = fit, p90.se = se.fit)


df_comb <- cbind(df_comb %>% 
                   dplyr::select(-nStrata), 
                 npred30, npred60, npred90) %>% 
  dplyr::select(-countyid)

df_combs <- cbind(bind_rows(df_combs, df_combs2) %>%
                    dplyr::select(-nStrata), 
                 bind_rows(npred30s, npred30s2), 
                 bind_rows(npred60s, npred60s2), 
                 bind_rows(npred90s, npred90s2)) %>% 
  dplyr::select(-countyid)

df_comb_long <- df_comb %>%
  dplyr::select(-ends_with(".se")) %>%
  pivot_longer(matches("^p[0-9]"), names_to = "los_cutoff", values_to = "p") %>%
  dplyr::mutate(los_cutoff = as.numeric(gsub("^p","",gsub("\\.m$","",los_cutoff)))) %>%
  left_join(
    df_comb %>%
      dplyr::select(-ends_with(".m")) %>%
      pivot_longer(matches("^p[0-9]"), names_to = "los_cutoff", values_to = "se") %>%
      dplyr::mutate(los_cutoff = as.numeric(gsub("^p","",gsub("\\.se$","",los_cutoff))))
  ) %>%
  dplyr::mutate(p.m = exp(p), 
         p.lwr = exp(p - (qnorm(0.975) * se)),
         p.upr = exp(p + (qnorm(0.975) * se)))

df_combs_long <- df_combs %>%
  dplyr::select(-ends_with(".se")) %>%
  pivot_longer(matches("^p[0-9]"), names_to = "los_cutoff", values_to = "p") %>%
  dplyr::mutate(los_cutoff = as.numeric(gsub("^p","",gsub("\\.m$","",los_cutoff)))) %>%
  left_join(
    df_combs %>%
      dplyr::select(-ends_with(".m")) %>%
      pivot_longer(matches("^p[0-9]"), names_to = "los_cutoff", values_to = "se") %>%
      dplyr::mutate(los_cutoff = as.numeric(gsub("^p","",gsub("\\.se$","",los_cutoff))))
  ) %>%
  dplyr::mutate(p.m = exp(p), 
                p.lwr = exp(p - (qnorm(0.975) * se)),
                p.upr = exp(p + (qnorm(0.975) * se)))

states <- unique(df_comb_long$state)

ageEsts_long <- ageEsts %>%
  group_by(state, raceEthnicity, sex_gender_standardized, ageCat) %>%
  summarise(n30 = sum(n30, na.rm = T), n60 = sum(n60, na.rm = T), n90 = sum(n90, na.rm = T), nStrata = sum(nStrata)) %>%
  dplyr::mutate(p30 = n30/nStrata, p60 = n60/nStrata, p90 = n90/nStrata) %>%
  dplyr::select(-matches("^n.0$")) %>%
  pivot_longer(starts_with("p"), names_to = "los_cutoff", values_to = "p.raw", names_transform = ~as.numeric(gsub("p","",.)))

losEsts_long <- losEsts %>%
  group_by(state) %>%
  summarise(n30 = sum(n30, na.rm = T), n60 = sum(n60, na.rm = T), n90 = sum(n90, na.rm = T), nStrata = sum(nStrata)) %>%
  dplyr::mutate(p30 = n30/nStrata, p60 = n60/nStrata, p90 = n90/nStrata) %>%
  dplyr::select(-matches("^n.0$")) %>%
  pivot_longer(starts_with("p"), names_to = "los_cutoff", values_to = "p.raw", names_transform = ~as.numeric(gsub("p","",.)))

df_comb_long <- df_comb_long %>%
  left_join(ageEsts_long)

df_combs_long <- df_combs_long %>%
  left_join(losEsts_long)

library(ggplot2)

ggplot(df_comb_long, aes(x = p.raw, y = p.m)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  coord_equal(xlim=c(0,.25),ylim=c(0,.25)) + #bulk of points fall within this range
  theme_classic()

ggplot(df_combs_long, aes(x = p.raw, y = p.m)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  coord_equal(xlim=c(0,.25),ylim=c(0,.25)) + #bulk of points fall within this range
  theme_classic()

summary(df_comb_long$p.raw - df_comb_long$p.m)
sd(df_comb_long$p.raw - df_comb_long$p.m, na.rm = T) #within the confidence intervals

summary(df_combs_long$p.raw - df_combs_long$p.m)
sd(df_combs_long$p.raw - df_combs_long$p.m, na.rm = T) #within the confidence intervals

for(los in c(30, 60, 90)){
  for(s in 1:2){
    state.range = 1:25
    if(s == 2) state.range = 25:length(states)
    
    for(gender in c("Male","Female")){
      g <- ggplot(df_comb_long %>% filter(state %in% states[state.range], los_cutoff == los, sex_gender_standardized == gender), 
                  aes(x = ageCat, fill = raceEthnicity, group = raceEthnicity)) +
        geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2, color = NA) +
        geom_line(aes(y = p.m, color = raceEthnicity)) + 
        ylab("Proportion of strata (state, sex, race) with los≥n") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_wrap(~state, ncol = 5) +
        theme_classic()
      
      ggsave(g, filename = file.path(here::here(), "preprocess","figs",paste0("jailStays_los",los,"_s",s,"sex",gender, ".pdf")),
             width = 12, height = 8)
    }
  }
}

losEsts_long2 <- losEsts %>%
  pivot_longer(c(n30, n60, n90)) %>%
  mutate(p = value/nStrata, los_cutoff = as.numeric(gsub("^n","",name)))

g <- ggplot(df_combs_long, 
            aes()) +
  geom_point(aes(x = los_cutoff, y = p.m)) +
  geom_errorbar(aes(x = los_cutoff, ymin = p.lwr, ymax = p.upr)) + 
  geom_jitter(data = losEsts_long2, aes(x = los_cutoff, y = p), alpha = 0.5, size = 0.5) +
  ylab("Proportion of strata (state, sex, race) with los≥n") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~state, ncol = 5, scale = "free") +
  theme_classic()

ggsave(g, filename = file.path(here::here(), "preprocess","figs",paste0("jailStays_los.pdf")),
       width = 12, height = 16)

write_csv(df_comb_long, file = file.path(here::here(), "data", "jailStays_byStateSexRaceAge.csv"))
write_csv(df_combs_long, file = file.path(here::here(), "data", "jailStays_byLOS.csv"))

# #multiple imputation
# 
# jdi_data_rosters <- jdi_data_rosters %>% 
#   mutate(release_month = floor_date(last_seen),
#          age = factor(age, ordered = T),
#          countyid = factor(countyid),
#          los30up = los > 29
#   ) %>%
#   dplyr::select(id, los, state, last_seen, countyid, release_month, raceEthnicity, sex_gender_standardized, age)
# 
# jdi_agg <- jdi_data_rosters %>%
#   group_by()
#   
# 
# library(Amelia)
# library(lubridate)
# library(parallel)
# library(snow)
# numCores <- detectCores()-3
# 
# 
# jdi_data_rosters <- jdi_data_rosters %>% 
#   mutate(release_month = floor_date(last_seen),
#          age = factor(age, ordered = T),
#          countyid = factor(countyid)
#   ) %>%
#   dplyr::select(id, los, state, last_seen, countyid, release_month, raceEthnicity, sex_gender_standardized, age) %>%
#   filter(los > 29)
# 
# gc()
# imp <- amelia(jdi_data_rosters, m=5, p2s = 1, idvars = c("id","last_seen","los"),
#               cs="state",
#               noms=c("countyid","raceEthnicity","sex_gender_standardized"),
#               ords=c("age"),ts = "release_month",
#               parallel="snow",ncpus=numCores)
# save(imp, file = file.path(path_to_jail_data, "jdi_impO.RData"))
# 
# imputations <- lapply(imp$imputations, 
#                           function(x) {
#                             x$ageN <- as.numeric(as.character(x$age))
#                             return(x)
#                             })
# 
# imp_age <- bind_rows(imputations) %>% select(ageN) %>% rename(age = ageN) %>% mutate(group = "Imputed")
# imp_age2 <- imputations$imp1[!imp$missMatrix[,"age"],"ageN"] %>% rename(age = ageN) %>% mutate(group = "Observed")
# imp_age <- bind_rows(imp_age, imp_age2)
# 
# pMiss <- sum(imp$missMatrix[,"age"])/nrow(imp$missMatrix)
# 
# library(ggplot2)
# g <- ggplot(imp_age, aes(x = age, color = group)) + 
#   geom_density(adjust = 2, alpha = 0.5) +  # smooth since age is discrete
#   theme_classic(base_size = 14) + 
#   xlab(paste0("Age -- Missing frac: ", round(pMiss, digits = 3)))
#   
# path_to_figs <- file.path(here::here(), "preprocess", "figs")
# ggsave(file.path(path_to_figs,"imputed_density.pdf"), width = 6, height = 6)
# 
# 
# #negative binomial is a much better fit
# library(pscl)
# mod <- zeroinfl(los-1 ~ ageCat + race + state + countyid | 1, 
#                 dist = "negbin",
#               data = jdi_data_rosters, subset = state %in% c("CA","WA"))
# 
# #https://library.virginia.edu/data/articles/simulating-data-for-count-models
# z <- rbinom(n = n, size = 1, 
#             prob = 1/(1 + exp(-(-0.8 + 1.8 * (male == 1))))) 
# y_sim <- ifelse(z == 0, 0, 
#                 rnbinom(n = n, 
#                         mu = exp(1.3 + 1.5 * (male == 1)), 
#                         size = 2))
# 
# y_base <- rnbinom(n = 1000, #number of simulated samples
#                  mu = exp(0.912486 + 2.386545 + 0.259958 + -0.114982),  #this is where the intercept and slopes go
#                  size = 0.408672) #theta
# y_sim <- rnbinom(n = 1000, #number of simulated samples
#                  mu = exp(0.912486 + 2.386545 + 0.259958 + -0.114982),  #this is where the intercept and slopes go
#                  size = 0.408672) #theta
# 
# #use zero inflated negative binomials
# baseline = as.data.frame(list(x=y_base))
# sim = as.data.frame(list(x = y_sim))
# real = as.data.frame(list(x = jdi_data_rosters %>% filter(state == "WA", ageCat == "18-24", race == "Black") %>% pull(los)))
# ggplot() + 
#   geom_histogram(data = sim, aes(x=x, y=..density..), color = "red", fill = NA) +
#   geom_histogram(data = real, aes(x=x, y=..density..), color = "black", fill = NA)
# 
# #AL: aggregate data from Montana and Idaho
# #Connecticut & RI: Aggregate data from 
