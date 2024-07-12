library(readr)
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(rlang)

path_to_jail_data <- file.path(here::here(), "data","raw","JailRosterData")
jdi_data_rosters <- read_csv(file.path(path_to_jail_data, "jdi_1693451429.csv"))

# Flag == 1 means there are data issues
date_covid_policy_start <- as.Date("2020-03-01")
date_covid_policy_end <- as.Date("2021-12-31") #note this refers to when jail population rebounded to pre-Covid levels.
# Citation: https://www.pewtrusts.org/en/research-and-analysis/articles/2022/11/17/many-jails-are-as-full-as-they-were-before-covid-19-pandemic

#Filter for those that were admitted after Covid jail population levels rebounded, were not admitted or released on the first or last days we have for counties, and other data errors

jdi_data_rosters <- jdi_data_rosters %>%
  group_by(state, county) %>%
  mutate(flagF = as.numeric(first_seen == max(first_seen, na.rm = T)),
         flagL = as.numeric(last_seen == max(last_seen, na.rm = T)),
         flagCovidStart = as.numeric(first_seen > date_covid_policy_end)) %>% ungroup() 

print(jdi_data_rosters %>% group_by(flag, flagF, flagL, flagCovidStart) %>% summarise(p = n() / nrow(jdi_data_rosters)))
n_tot <- jdi_data_rosters %>% filter(flag == 0, flagF == 0, flagL == 0, flagCovidStart == 1) %>% nrow()
print(n_tot)

jdi_data_rosters <- jdi_data_rosters %>% 
  filter(flag == 0, flagF == 0, flagL == 0, flagCovidStart == 1) %>%
  mutate(year_admit = format(first_seen,"%Y"),
         year_release = format(last_seen,"%Y")) %>%
  dplyr::select(id, year_admit, year_release, first_seen, last_seen, state, county, los, sex_gender_standardized)

state_county <- jdi_data_rosters %>%
  dplyr::select(state, county) %>%
  distinct() %>%
  mutate(countyid = factor(1:n()))

jdi_data_rosters <- jdi_data_rosters %>%
  left_join(state_county)

gc()

save(jdi_data_rosters, file = file.path(path_to_jail_data, "jdi_flag.RData"))

county72 <- jdi_data_rosters %>%
  group_by(state, county, countyid) %>%
  summarize(p72 = sum(los > 3)/n()) %>% ungroup()
hist(county72$p72)

print("Removing county-years that are only 72hr holding. Filtering out all jails where 90+% of the population stays 3 days or fewer")
n_jails72 <- sum(county72$p72 <= 0.1)
jdi_complete <- jdi_data_rosters %>%
  left_join(county72) %>%
  filter(p72 > 0.1)
n_removed72 <- nrow(jdi_data_rosters) - nrow(jdi_complete)
print(paste0("    Number of counties removed ", n_jails72, " (", round(100*n_jails72/nrow(county72), 1), "%)"))
print(paste0("    Number of observations removed ", n_removed72, " (", round(100*n_removed72/nrow(jdi_data_rosters), 1), "%)"))

losEsts <- jdi_complete %>% 
  mutate(los3 = as.numeric(los > 2),
         los7 = as.numeric(los > 6),
         los14 = as.numeric(los > 13),
         los30 = as.numeric(los > 29), 
         los60 = as.numeric(los > 59),
         los90 = as.numeric(los > 89),
         sex_gender_standardized = factor(sex_gender_standardized, levels = c("Male","Female","Unknown Gender","Trans"))
         ) %>%
  group_by(state, countyid, sex_gender_standardized) %>%
  summarize(n3 = sum(los3), n7 = sum(los7), n14 = sum(los14), 
            n30 = sum(los30), n60 = sum(los60), n90 = sum(los90), nStrata = n()) %>% ungroup() %>%
  filter(!is.na(sex_gender_standardized))

# We aggregate data by state, county, and sex
##  We fit a mixed effects model with county as random effect, with outcomes as the proportion of the strata population being from a sex/gender group
##  Stratas are defined by state and sex/gender

library(glmmTMB)
library(bbmle)

# Just adjusting for sex

mod.n3s <- glmmTMB(n3~state + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n7s <- glmmTMB(n7~state + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n14s <- glmmTMB(n14~state + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n30s <- glmmTMB(n30~state + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     data=losEsts)

mod.n60s <- glmmTMB(n60~state + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n90s <- glmmTMB(n90~state + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n3s2 <- glmmTMB(n3~(1|state) + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     data=losEsts)

mod.n7s2 <- glmmTMB(n7~(1|state) + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     data=losEsts)

mod.n14s2 <- glmmTMB(n14~(1|state) + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                     ziformula=~1,
                     family = nbinom2,
                     verbose = F,
                     data=losEsts)

mod.n30s2 <- glmmTMB(n30~(1|state) + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n60s2 <- glmmTMB(n60~(1|state) + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom2,
                    verbose = F,
                    data=losEsts)

mod.n90s2 <- glmmTMB(n90~(1|state) + (1|countyid) + sex_gender_standardized + offset(log(nStrata)),
                    ziformula=~1,
                    family = nbinom1, #nbinom2 doesn't converge
                    verbose = F,
                    data=losEsts)

df_combs <- losEsts %>%
  tidyr::expand(state, sex_gender_standardized) %>%
  dplyr::mutate(countyid = NA, nStrata = 1)
df_combs2 <- df_combs[1:length(unique(df_combs$sex_gender_standardized)),] %>% mutate(state = NA)
  
npred3s <- predict(mod.n3s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p3.m = fit, p3.se = se.fit)
npred7s <- predict(mod.n7s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p7.m = fit, p7.se = se.fit)
npred14s <- predict(mod.n14s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p14.m = fit, p14.se = se.fit)
npred30s <- predict(mod.n30s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p30.m = fit, p30.se = se.fit)
npred60s <- predict(mod.n60s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p60.m = fit, p60.se = se.fit)
npred90s <- predict(mod.n90s, newdata = df_combs, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p90.m = fit, p90.se = se.fit)

npred3s2 <- predict(mod.n3s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p3.m = fit, p3.se = se.fit)
npred7s2 <- predict(mod.n7s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p7.m = fit, p7.se = se.fit)
npred14s2 <- predict(mod.n14s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p14.m = fit, p14.se = se.fit)
npred30s2 <- predict(mod.n30s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p30.m = fit, p30.se = se.fit)
npred60s2 <- predict(mod.n60s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p60.m = fit, p60.se = se.fit)
npred90s2 <- predict(mod.n90s2, newdata = df_combs2, re.form = NA ,type = "link", se.fit = T) %>% as.data.frame() %>% rename(p90.m = fit, p90.se = se.fit)

df_combs <- cbind(bind_rows(df_combs, df_combs2) %>%
                    dplyr::select(-nStrata), 
                  bind_rows(npred3s, npred3s2), 
                  bind_rows(npred7s, npred7s2), 
                  bind_rows(npred14s, npred14s2), 
                  bind_rows(npred30s, npred30s2), 
                  bind_rows(npred60s, npred60s2), 
                  bind_rows(npred90s, npred90s2)) %>% 
  dplyr::select(-countyid)

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

states <- unique(df_combs_long$state)

losEsts_long <- losEsts %>%
  group_by(state, sex_gender_standardized) %>%
  summarise(n3 =  sum(n3, na.rm = T), n7 =  sum(n7, na.rm = T), n14 =  sum(n14, na.rm = T),
    n30 = sum(n30, na.rm = T), n60 = sum(n60, na.rm = T), n90 = sum(n90, na.rm = T), nStrata = sum(nStrata)) %>%
  dplyr::mutate(p3 = n3/nStrata, p7 = n7/nStrata, p14 = n14/nStrata, 
                p30 = n30/nStrata, p60 = n60/nStrata, p90 = n90/nStrata) %>%
  dplyr::select(-matches("^n[0-9]")) %>%
  pivot_longer(starts_with("p"), names_to = "los_cutoff", values_to = "p.raw", names_transform = ~as.numeric(gsub("p","",.)))

df_combs_long <- df_combs_long %>%
  left_join(losEsts_long)

library(ggplot2)

ggplot(df_combs_long, aes(x = p.raw, y = p.m, fill = sex_gender_standardized, color = sex_gender_standardized)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  coord_equal(xlim=c(0,.25),ylim=c(0,.25)) + #bulk of points fall within this range
  theme_classic()

summary(df_combs_long$p.raw - df_combs_long$p.m)
sd(df_combs_long$p.raw - df_combs_long$p.m, na.rm = T) #within the confidence intervals

losEsts_long2 <- losEsts %>%
  pivot_longer(c(n3, n7, n14, n30, n60, n90)) %>%
  mutate(p = value/nStrata, los_cutoff = as.numeric(gsub("^n","",name)))

i = 1
for(st in split(states, rep_len(1:3, length(states)))){
  g <- ggplot(df_combs_long %>% 
                dplyr::mutate(los_cutoff = factor(los_cutoff, levels = c(3, 7, 14, 30, 60, 90))) %>% 
                dplyr::filter(state %in% st, sex_gender_standardized %in% c("Male","Female")), 
              aes(color = sex_gender_standardized, fill = sex_gender_standardized, 
                  group = interaction(los_cutoff, sex_gender_standardized))) +
    geom_jitter(data = losEsts_long2 %>% 
                  dplyr::mutate(los_cutoff = factor(los_cutoff, levels = c(3, 7, 14, 30, 60, 90)), alp = 0.5) %>% 
                  dplyr::filter(state %in% st) %>%
                  bind_rows(losEsts_long2 %>% 
                              dplyr::filter(state %in% st) %>%
                              tidyr::expand(state, sex_gender_standardized, los_cutoff) %>% 
                              dplyr::mutate(p = 0, alp = 0.0, los_cutoff = factor(los_cutoff, levels = c(3, 7, 14, 30, 60, 90)))) %>%
                  dplyr::filter(sex_gender_standardized %in% c("Male","Female")), 
                aes(x = los_cutoff, y = p), shape = 1,
                size = 0.5, alpha = 0.3, position = position_jitterdodge(dodge.width=0.9)) +
    geom_point(aes(x = los_cutoff, y = p.m), position = position_dodge(width=0.9)) +
    geom_point(aes(x = los_cutoff, y = p.raw), position = position_dodge(width=0.9), shape = 4) +
    geom_errorbar(aes(x = los_cutoff, ymin = p.lwr, ymax = p.upr), 
                  position = position_dodge(width=0.9), width = 0.25) + 
    scale_color_manual("Sex",values = c("#0b6d64","#6f4fa1")) +
    scale_fill_manual("Sex",values = c("#0b6d64","#6f4fa1")) +
    scale_y_continuous("Proportion of strata (state, sex) with los≥n") +
    coord_cartesian(ylim=c(0, 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
    facet_wrap(~state, ncol = 5) +
    theme_classic()
  
  ggsave(g, filename = file.path(here::here(), "preprocess","figs",paste0("jailStays_los", i, ".pdf")),
         width = 12, height = 6)
  i = i + 1
}


write_csv(df_combs_long, file = file.path(here::here(), "data", "jailStays_byStateSex.csv"))

