library(dplyr)
library(readr)
library(ggplot2)
library(colorspace)
library(aods3)
library(ggpubr)
require(here)
library(grid)
library(tidycensus)
library(sf)

# As of January 2020
# States that have since expanded Medicare: NE, OK, MO, SD, ND
states_nonexp <- tibble(state_name = c("Alabama", "Florida", "Georgia", "Kansas", "Mississippi", 
                   "Missouri", "Nebraska", "North Carolina", "Oklahoma", 
                   "South Carolina", "South Dakota", "Tennessee", "Texas", 
                   "Wisconsin", "Wyoming"))

state_cross <- tibble(state_name = state.name, state_abb = state.abb) %>%
  bind_rows(tibble(state_name = "District of Columbia", state_abb = "DC"))

states_nonexp <- states_nonexp %>% left_join(state_cross)

load(file.path(here::here(), "data", "input_standardized.rda"))

# Monthly average between April 2019 to March 2020
df_medicaid <- bind_rows(
  read_csv(file.path(here::here(), "data", "MedicaidEnrollment_2019AprJun.csv")),
  read_csv(file.path(here::here(), "data", "MedicaidEnrollment_2019JulSep.csv")),
  read_csv(file.path(here::here(), "data", "MedicaidEnrollment_2019OctDec.csv")),
  read_csv(file.path(here::here(), "data", "MedicaidEnrollment_2020JanMar.csv"))
)

ggplot(df_medicaid %>% 
         mutate(x = ISOdate(enrollment_year, enrollment_month, 1)),
       aes(x = x, y = total_medicaid_enrollees)) + 
  geom_line() +
  geom_point() + 
  facet_wrap(~state, scale = "free_y")

n_medicaid <- df_medicaid %>%
  mutate(state = state.abb[match(state,state.name)]) %>%
  group_by(state) %>%
  summarize(n_medicaid = mean(total_medicaid_enrollees, na.rm = T)) %>% ungroup()

# State-level counts (all prison and long-term jail stays)
n_releases <- df_releases %>%
  # filter(!combined_system) %>% # filter out combined jail/prison systems
  group_by(state, system, los_cutoff, combined_system) %>%
  summarise(n_releases = sum(n_releases, na.rm = T)) %>% ungroup() %>% # Yearly
  arrange(state, system, desc(los_cutoff)) %>%
  group_by(state, system, combined_system) %>%
  mutate(n = ifelse(los_cutoff == 90 | is.na(los_cutoff), n_releases, n_releases - lag(n_releases)),
         cohort = case_when(
           # los_cutoff == 3 ~ "Jail (3-6 days)",
           # los_cutoff == 7 ~ "Jail (7-13 days)",
           # los_cutoff == 14 ~ "Jail (14-29 days)",
           los_cutoff == 30 ~ "Jail (30-59 days)",
           los_cutoff == 60 ~ "Jail (60-89 days)",
           los_cutoff == 90 ~ "Jail (≥90 days)",
           T ~ system
         )) %>% ungroup() %>% select(-c(los_cutoff, n_releases)) %>%
  filter(!is.na(cohort))

n_releases <- n_releases %>%
  mutate(cohort = factor(cohort, levels = rev(c("Federal prison","State prison", "Jail (≥90 days)","Jail (60-89 days)","Jail (30-59 days)",
                                                "Jail (14-29 days)","Jail (7-13 days)","Jail (3-6 days)"
                                                )))) %>%
  inner_join(n_medicaid) %>%
  filter(!is.na(cohort)) %>%
  mutate(p = n/n_medicaid) %>%
  group_by(state) %>%
  mutate(p_incarc = sum(p, na.rm = T)) %>% ungroup() %>%
  mutate(exp = ifelse(
    state %in% states_nonexp$state_abb, "Non-expansion",
    "Expansion"),
    state = ifelse(combined_system,
                   paste0(state, "*"),
                   state)
    ) %>%
  arrange(desc(exp), desc(p_incarc))

n_releases$state <- factor(n_releases$state, levels = unique(n_releases$state))
n_releases$exp <- factor(n_releases$exp, levels = c("Non-expansion","Expansion"))
levels(n_releases$exp) <- c("Non-Expansion States", "Expansion States")

n.pal <- c(tail(rev(sequential_hcl(5, palette = "BluYl")), -1), "black")

x.pos = nrow(states_nonexp)
x.max = length(levels(n_releases$state))
x.offset = 0.3
y.pos = -0.01
y.height = 0.0025

g <- ggplot(n_releases, 
            aes(x = state, fill = cohort, y = p)) +
  geom_bar(position="stack", stat="identity", linewidth = 0.25) +
  scale_y_continuous("Yearly releases compared to Medicaid enrollment (%)", labels = scales::percent_format(), 
                     expand = c(0,0), limits = c(0,0.15), breaks = seq(0, .15, .05)) +
  scale_x_discrete(NULL, expand = c(0,0)) +
  scale_fill_manual(values = n.pal)  + 
  labs(fill = NULL, color = NULL) + 
  # facet_grid(~exp, scales = "free_x", space = "free", switch = "x") +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.93,0.6), 
        legend.text = element_text(size=13),
        strip.text.x = element_text(hjust = 0, margin=margin(l=0,b=10), size = 16),
        plot.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(t = 1, r = 1, b = 4, l = 1, unit = "lines")
        ) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 1-x.offset, ymin = y.pos, xmax = x.pos + x.offset, ymax = y.pos) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 1-x.offset, ymin = y.pos, xmax = 1-x.offset, ymax = y.pos + y.height) +
  annotation_custom(grob = linesGrob(), 
                    xmin = x.pos + x.offset, ymin = y.pos, xmax = x.pos + x.offset, ymax = y.pos + y.height) +
  annotation_custom(grob = linesGrob(), 
                    xmin = x.pos + 1 - x.offset, ymin = y.pos, xmax = x.max + x.offset, ymax = y.pos) +
  annotation_custom(grob = linesGrob(), 
                    xmin = x.pos + 1 - x.offset, ymin = y.pos, xmax = x.pos + 1 - x.offset, ymax = y.pos + y.height) +
  annotation_custom(grob = linesGrob(), 
                    xmin = x.max + x.offset, ymin = y.pos, xmax = x.max + x.offset, ymax = y.pos + y.height) +
  annotation_custom(grob = textGrob("Non-Expansion States", 
                                    gp=gpar(col="black", fontsize=13, fontface="bold")), 
                    xmin = (1 + x.pos)/2, ymin = y.pos - 2*y.height, xmax = (1 + x.pos)/2, ymax = y.pos - 2*y.height) +
  annotation_custom(grob = textGrob("Expansion States", 
                                    gp=gpar(col="black", fontsize=13, fontface="bold")), 
                    xmin = (x.pos + 1 + x.max)/2, ymin = y.pos - 2*y.height, xmax =  (x.pos + 1 + x.max)/2, ymax = y.pos - 2*y.height) +
  coord_cartesian(clip = "off") 

g

ggsave(file.path(here::here(), "figs", "compare_release_medicaid.pdf"), g, device = cairo_pdf, width = 16, height = 9)
ggsave(file.path(here::here(), "figs", "compare_release_medicaid.png"), g, width = 16, height = 9)

print(n_releases %>% filter(state %in% c("MA","NY","SD","WY")) %>% select(state, p_incarc) %>% distinct())

print(n_releases %>% 
        filter(!combined_system) %>%
        group_by(state, exp, n_medicaid) %>%
        summarize(n = sum(n, na.rm = T)) %>%
        group_by(exp) %>% 
        summarize(n = sum(n, na.rm = T), n_medicaid = sum(n_medicaid, na.rm = T)) %>% ungroup() %>% 
        mutate(p = n/n_medicaid) %>% 
        group_by(exp) %>% 
        summarize(p_incarc = sum(p)))

### MAP version
library(fiftystater)
data("fifty_states")
n_releases_all <- n_releases %>%
  mutate(state = gsub("\\*","",state)) %>%
  select(state, p_incarc, exp) %>%
  distinct() %>%
  left_join(state_cross, by = c("state"="state_abb")) %>%
  mutate(state_name = tolower(state_name))

map.state <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
map.state <- map.state %>%
  left_join(n_releases_all, by = c("ID"="state_name"))

map.nes <- map.state %>%
  filter(exp == "Non-Expansion States")

library(wesanderson)

g <- ggplot(n_releases_all, aes(map_id = state_name)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = p_incarc), color = "black", map = fifty_states, linewidth = 0.15) + 
  geom_map(data = n_releases_all %>% filter(exp == "Non-Expansion States"),
           aes(fill = p_incarc), color = "grey35", map = fifty_states, linewidth = 1.25) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(NULL, breaks = NULL) + 
  scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_gradientn("Yearly releases\ncompared to\nMedicaid enrollment (%)\n", 
                       colors = wes_palette("Zissou1", type = "continuous"),
                       labels = scales::percent_format(), limits = c(0, NA)) +
  theme(legend.position = c(0.91, 0.28), 
        legend.title = element_text(size=15),
        legend.text = element_text(size=14),
        legend.key.height = unit(1.25, "cm"),
        legend.key.width = unit(1, "cm"),
        panel.background = element_blank()) +
  fifty_states_inset_boxes()

g

ggsave(file.path(here::here(), "figs", "release_medicaid_maps.pdf"), g, device = cairo_pdf, width = 16, height = 9)
ggsave(file.path(here::here(), "figs", "release_medicaid_maps.png"), g, width = 16, height = 9)

# get state level estimates of prison + jail group. and split by sex?
#don't need to worry about SE for prisons
df_prison <- df_releases %>%
  filter(system != "Jail") %>%
  group_by(state) %>%
  summarize(n_prison_male = sum(n_releases[sex_gender == "Male"], na.rm = T),
            n_prison = sum(n_releases, na.rm = T))

df_jail <- df_releases %>%
  filter(system == "Jail") %>%
  group_by(state, combined_system, los_cutoff) %>%
  summarize(n_jail_male = sum(n_releases[sex_gender == "Male"]),
            n_jail = sum(n_releases),
            se_releases = sqrt(sum(se_releases)^2)
            )

df_release_est <- df_jail %>%
  left_join(df_prison, by = "state") %>% ungroup() %>%
  filter(!combined_system) %>%
  transmute(state, los_cutoff,
            n_releases = n_jail + n_prison,
            p_male = (n_jail_male + n_prison_male)/n_releases,
            se_releases) %>%
  left_join(n_medicaid)

df_release_est <- bind_rows(
  df_release_est,
  df_release_est %>% ungroup() %>%
  group_by(los_cutoff) %>%
  dplyr::reframe(
            p_male = sum(p_male * n_releases, na.rm = T)/sum(n_releases, na.rm = T),
            n_releases = sum(n_releases, na.rm = T),
            n_medicaid = sum(n_medicaid, na.rm = T),
            se_releases = sqrt(sum(se_releases^2, na.rm = T))) %>%
  ungroup() %>% mutate(state = "US")) %>%
  mutate(cohort = "prison + jails", 
         p_releases = n_releases / n_medicaid, 
         p_se_releases = se_releases / n_medicaid)

df_release_est <- df_release_est %>%
  mutate(
    logit_p = log(p_releases / (1-p_releases)),
    logit_se = p_se_releases / (p_releases * (1 - p_releases)),
    logit_lower = logit_p - qnorm(0.975) * logit_se,
    logit_upper = logit_p + qnorm(0.975) * logit_se,
    p_releases_lwr = exp(logit_lower) / (1 + exp(logit_lower)),
    p_releases_upr = exp(logit_upper) / (1 + exp(logit_upper))
  ) %>% select(-starts_with("logit"))

df_release_est$state = factor(df_release_est$state, 
                              levels = df_release_est %>% 
                                filter(los_cutoff == 30) %>% 
                                arrange(los_cutoff, desc(p_releases)) %>%
                                pull(state) %>% unique())

df_release_est <- df_release_est %>%
  mutate(ci = paste0(round(p_releases * 100, digits = 1), "% (",
                     round(p_releases_lwr * 100, digits = 1), "—",
                     round(p_releases_upr * 100, digits = 1), ")"
                     ))

g <- ggplot(df_release_est %>% filter(los_cutoff== 30, !is.na(p_releases)),
            aes(x = state, y = p_releases, 
                ymin = p_releases_lwr, ymax = p_releases_upr)) +
  geom_point() +
  geom_errorbar(width = .35) +
  scale_y_continuous("Yearly releases compared to average monthly Medicaid enrollment (%)", labels = scales::percent_format(), 
                     expand = c(0,0)) +
  scale_x_discrete("State", expand = c(0,0)) +
  facet_wrap("~los_cutoff", strip.position = "bottom") +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.87,0.9),
        strip.placement = "outside",  # Place strip labels outside the plot
        strip.background = element_blank()) 
g

## Compare levels of health outcomes
p_HO_incarc <- df_releases %>%
  filter(!combined_system, is.na(los_cutoff) | los_cutoff == 30) %>%
  left_join(df_healthOutcomes, relationship = "many-to-many") %>%
  group_by(state, sex_gender, health_outcome) %>%
  mutate(
    w_releases = n_releases/sum(n_releases, na.rm = T), 
    w_releases_se = se_releases/sum(n_releases, na.rm = T),
    w_p = w_releases * p_outcome_m,
    w_se = ifelse(
      is.na(w_releases_se), w_releases * p_outcome_se,
      w_p * sqrt((w_releases_se/w_releases)^2 + (p_outcome_se/p_outcome_m)^2))
    ) %>%
  summarize(
    p_outcome = weighted.mean(p_outcome_m, w_releases, na.rm = T),
    p_outcome_se = sqrt(sum(w_se^2))
  )

p_HO_incarc_total <- df_releases %>%
  filter(!combined_system, is.na(los_cutoff) | los_cutoff == 30) %>%
  group_by(sex_gender, system) %>%
  summarize(se_releases = sqrt(sum(se_releases^2, na.rm = T)),
            n_releases = sum(n_releases, na.rm = T)) %>% ungroup() %>% 
  left_join(df_healthOutcomes) %>%
  group_by(sex_gender, health_outcome) %>%
  mutate(
    w_releases = n_releases/sum(n_releases, na.rm = T), 
    w_releases_se = se_releases/sum(n_releases, na.rm = T),
    w_p = w_releases * p_outcome_m,
    w_se = ifelse(
      is.na(w_releases_se), w_releases * p_outcome_se,
      w_p * sqrt((w_releases_se/w_releases)^2 + (p_outcome_se/p_outcome_m)^2))
  ) %>%
  summarize(
    p_outcome = weighted.mean(p_outcome_m, w_releases, na.rm = T),
    p_outcome_se = sqrt(sum(w_se^2))
  )

## Should get proportions & CI nationally.
## Note, not very variable for prisons (similar age, sex, race distribution), ask Sanjay if they'd expect big age distirubitons between adults
p_HO_medicaid <- df_nhanes %>%
  bind_rows(df_nsduh) %>%
  transmute(health_outcome, sex_gender, n, p_outcome = p_outcome_m, p_outcome_se, p_outcome_lwr, p_outcome_upr)

p_HO <- bind_rows(p_HO_incarc_total %>% mutate(cohort = "Reentry"), 
                  p_HO_medicaid %>% mutate(cohort = "Medicaid")) %>%
  mutate(logit_p = log(p_outcome / (1-p_outcome)),
         logit_se = p_outcome_se / (p_outcome * (1 - p_outcome)),
         logit_lower = logit_p - qnorm(0.975) * logit_se,
         logit_upper = logit_p + qnorm(0.975) * logit_se,
         p_outcome_lwr = ifelse(is.na(p_outcome_lwr), exp(logit_lower) / (1 + exp(logit_lower)), p_outcome_lwr),
         p_outcome_upr = ifelse(is.na(p_outcome_upr), exp(logit_upper) / (1 + exp(logit_upper)), p_outcome_upr),
         health_outcome = recode(health_outcome,
                                 MPD_month = "Moderate\n(K6: 5-12)",
                                 SPD_month = "Serious\n(K6: ≥13)",
                                 mpd = "Moderate\n(K6: 5-12)",
                                 spd = "Serious\n(K6: ≥13)",
                                 hepC = "Hepatitis C",
                                 depression = "Depression",
                                 asthma = "Asthma", 
                                 cvd = "Cardiovascular\nDisease", 
                                 hypertension = "Hypertension", 
                                 diabetes = "Diabetes", 
                                 stroke = "Stroke", 
                                 kidney = "Kidney\nProblems",
                                 mhDiagnosis_any = "Mental health diagnosis",
                                 ami_any = "Mental health diagnosis, treatment, or prescription",
                                 smi_any = "Serious mental illness",
                                 sud_any = "SUD",
                                 sud_smi = "SUD or SMI",
                                 sud_smi_idd = "SUD, SMI,\nor I/DD",
                                 sud_ami_idd = "SUD, AMI,\nor I/DD",
                                 sud_ami_idd_chronic = "SUD, AMI, I/DD,\nHIV, Hep C, or\nChronic condition"
                                 )
         ) %>%
  select(-starts_with("logit"))

vars_ho <- c("Moderate\n(K6: 5-12)","Serious\n(K6: ≥13)",
             "Hypertension","Asthma","Cardiovascular\nDisease","Diabetes","Hepatitis C","Kidney\nProblems","Stroke")

y.pos = -0.06
y.height = 0.015

g <- ggplot(p_HO  %>% 
              mutate(health_outcome = factor(health_outcome, levels = vars_ho),
                     sex_gender = factor(sex_gender, levels = c("Male","Female"))
                     ) %>%
              filter(!is.na(health_outcome)),
            aes(x = health_outcome, y = p_outcome)) +
  geom_point(aes(color = cohort, group = cohort), position = position_dodge(width=0.45)) +
  geom_errorbar(aes(color = cohort, group = cohort, ymin = p_outcome_lwr, ymax = p_outcome_upr), position = position_dodge(width=0.45), width = .35) +
  scale_y_continuous("Prevalence (%)", expand = c(0,0), labels = scales::percent_format(accuracy = 1), limits = c(NA,0.6), breaks = seq(0,0.6,0.1)) +
  scale_x_discrete(NULL) +
  scale_color_manual(NULL,values = c("#2596be","#be4d25")) + 
  theme_classic(base_size = 14) +  
  theme(strip.text.x = element_text(hjust = 0, margin=margin(l=0,b=10), size = 16),
        plot.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(t = 1, r = 0, b = 4, l = 0, unit = "lines"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.94,0.95),
        legend.text=element_text(size=14),
        legend.key.width = unit(1.5,"cm")
        ) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 0.6, ymin = y.pos, xmax = 2.4, ymax = y.pos) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 0.6, ymin = y.pos, xmax = 0.6, ymax = y.pos + y.height) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 2.4, ymin = y.pos, xmax = 2.4, ymax = y.pos + y.height) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 2.6, ymin = y.pos, xmax = 9.4, ymax = y.pos) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 2.6, ymin = y.pos, xmax = 2.6, ymax = y.pos + y.height) +
  annotation_custom(grob = linesGrob(), 
                    xmin = 9.4, ymin = y.pos, xmax = 9.4, ymax = y.pos + y.height) +
  annotation_custom(grob = textGrob("Psychological\ndistress", 
                                    gp=gpar(col="black", fontsize=13, fontface="bold")), 
                    xmin = 1.5, ymin = y.pos - 2*y.height, xmax = 1.5, ymax = y.pos - 2*y.height) +
  annotation_custom(grob = textGrob("Chronic\nconditions", 
                                    gp=gpar(col="black", fontsize=13, fontface="bold")), 
                    xmin = 6, ymin = y.pos - 2*y.height, xmax = 6, ymax = y.pos - 2*y.height) +
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(override.aes = list(size=3, linewidth = 1))) +
  facet_wrap(~sex_gender, nrow = 1)

g

ggsave(file.path(here::here(), "figs", "compare_disease_prevalence.pdf"), g, device = cairo_pdf, width = 20, height = 7)
ggsave(file.path(here::here(), "figs", "compare_disease_prevalence.png"), g, width = 20, height = 7)

print(p_HO %>% filter(grepl("Serious\n", health_outcome)) %>% 
        mutate(ci = paste0(round(p_outcome * 100, digits = 1), "%, 95% CI ",
                           round(p_outcome_lwr * 100, digits = 1), "—",
                           round(p_outcome_upr * 100, digits = 1), "%")) %>%
        select(sex_gender, cohort, health_outcome, ci)
        )

# Restrictions by groups (%?)
vars_ho <- c("SUD","SUD or SMI","SUD, SMI,\nor I/DD","SUD, AMI,\nor I/DD","SUD, AMI, I/DD,\nHIV, Hep C, or\nChronic condition")
g <- ggplot(p_HO %>% 
              mutate(health_outcome = factor(health_outcome, levels = vars_ho),
                     sex_gender = factor(sex_gender, levels = c("Male","Female")),
                     p_outcome_upr = pmin(1, p_outcome_upr)
              ) %>%
              filter(!is.na(health_outcome), cohort == "Reentry"),
            aes(x = health_outcome, y = p_outcome)) +
  geom_point(aes(color = sex_gender, group = sex_gender), position = position_dodge(width=0.45)) +
  geom_errorbar(aes(color = sex_gender, group = sex_gender, ymin = p_outcome_lwr, ymax = p_outcome_upr), 
                position = position_dodge(width=0.45), width = .35) +
  scale_y_continuous("Prevalence (%)", expand = c(0,0), labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_discrete(NULL) +
  scale_color_manual(NULL,values = c("#0b6d64","#6f4fa1")) + 
  theme_classic(base_size = 14) +  
  theme(strip.text.x = element_text(hjust = 0, margin=margin(l=0,b=10), size = 16),
        plot.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        legend.title=element_blank(),
        legend.background = element_rect(linewidth=0.25, linetype="solid", 
                                         colour ="black"),
        legend.margin=margin(c(1,5,5,5)),
        plot.margin = margin(t = 1, r = 0, b = 1, l = 0, unit = "lines"),
        legend.position = c(0.92,0.25),
  ) +
  coord_cartesian(clip = "off")
g

print(p_HO %>% filter(health_outcome %in% c("Hepatitis C", "Kidney\nProblems")) %>% 
        mutate(ci = paste0(round(p_outcome * 100, digits = 1), "%, 95% CI ",
                           round(p_outcome_lwr * 100, digits = 1), "—",
                           round(p_outcome_upr * 100, digits = 1), "%")) %>%
        select(sex_gender, cohort, health_outcome, ci)
)

ggsave(file.path(here::here(), "figs", "eligibility.pdf"), g, device = cairo_pdf, width = 10, height = 6)
ggsave(file.path(here::here(), "figs", "eligibility.png"), g, width = 10, height = 6)

# Health profiles by race and age group for jails and prisons?



