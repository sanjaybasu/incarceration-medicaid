library(dplyr)
library(readr)
library(ggplot2)
library(colorspace)
library(aods3)
require(here)

load(file.path(here::here(), "data", "input_standardized.rda"))
df_medicaid <- read_csv(file.path(here::here(), "data", "medicaid_pop.csv"), skip = 2) # Monthly

# State-level counts (all prison and long-term jail stays)
n_releases <- df_releases %>%
  filter(!combined_system) %>% # filter out combined jail/prison systems
  group_by(state, system, los_cutoff) %>%
  summarise(n_releases = sum(n_releases, na.rm = T)/12) %>% ungroup() %>% # Monthly
  arrange(state, system, desc(los_cutoff)) %>%
  group_by(state, system) %>%
  mutate(n = ifelse(los_cutoff == 90 | is.na(los_cutoff), n_releases, n_releases - lag(n_releases)),
         cohort = case_when(
           los_cutoff == 3 ~ "Jail (3-6 days)",
           los_cutoff == 7 ~ "Jail (7-13 days)",
           los_cutoff == 14 ~ "Jail (14-29 days)",
           los_cutoff == 30 ~ "Jail (30-59 days)",
           los_cutoff == 60 ~ "Jail (60-89 days)",
           los_cutoff == 90 ~ "Jail (≥90 days)",
           T ~ system
         )) %>% ungroup() %>% select(-c(los_cutoff, n_releases))
n_medicaid <- df_medicaid %>%
  filter(Location != "United States", !is.na(`Total Monthly Medicaid/CHIP Enrollment`)) %>%
  transmute(state = state.abb[match(Location,state.name)], 
            n_medicaid = `Total Monthly Medicaid/CHIP Enrollment`)

n_releases <- n_releases %>%
  mutate(cohort = factor(cohort, levels = rev(c("State prison","Jail (≥90 days)","Jail (60-89 days)","Jail (30-59 days)",
                                                "Jail (14-29 days)","Jail (7-13 days)","Jail (3-6 days)"
                                                )))) %>%
  inner_join(n_medicaid) %>%
  mutate(p = n/n_medicaid) %>%
  group_by(state) %>%
  mutate(p_incarc = sum(p)) %>% ungroup() %>%
  arrange(desc(p_incarc))

n_releases$state <- factor(n_releases$state, levels = unique(n_releases$state))

n.pal <- c(tail(rev(sequential_hcl(7, palette = "BluYl")), -1), "black")

g <- ggplot(n_releases %>% filter(!is.na(state)), 
            aes(x = state, fill = cohort, y = p)) +
  geom_bar(position="stack", stat="identity", linewidth = 0.25) +
  scale_y_continuous("Releases compared to Medicaid/CHIP Enrollment (%)", labels = scales::percent_format(), 
                     expand = c(0,0), limits = c(0,0.026)) +
  scale_x_discrete("State", expand = c(0,0)) +
  scale_fill_manual(values = n.pal)  + 
  labs(fill = NULL, color = NULL) + 
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.87,0.9))
g

ggsave(file.path(here::here(), "figs", "compare_release_medicaid.pdf"), g, device = cairo_pdf, width = 14, height = 9)
ggsave(file.path(here::here(), "figs", "compare_release_medicaid.png"), g, width = 14, height = 9)

## Compare levels of health outcomes
p_HO_incarc <- df_releases %>%
  filter(!combined_system, is.na(los_cutoff) | los_cutoff == 30) %>%
  left_join(df_healthOutcomes, relationship = "many-to-many") %>%
  mutate(n_HO = n_releases * p_outcome_m, 
         n_HO_se = ifelse(is.na(se_releases), n_releases * p_outcome_se,
                          n_releases*p_outcome_m * sqrt((se_releases/n_releases)^2 + (p_outcome_se/p_outcome_m)^2))
         ) %>%
  group_by(state, sex_gender, health_outcome) %>%
  summarize(
    n_outcome = sum(n_HO),
    n_outcome_se = sqrt(sum(n_HO_se^2, na.rm = T)),
    n_releases = sum(n_releases, na.rm = T)
  ) %>% ungroup() %>%
  mutate(p_outcome = n_outcome/n_releases,
         p_outcome_se = n_outcome_se/n_releases)

p_HO_incarc_total <- df_releases %>%
  filter(!combined_system, is.na(los_cutoff) | los_cutoff == 30) %>%
  group_by(sex_gender, system) %>%
  summarize(se_releases = sqrt(sum(se_releases^2, na.rm = T)),
            n_releases = sum(n_releases, na.rm = T)) %>% ungroup() %>% 
  left_join(df_healthOutcomes) %>%
  mutate(
    n_outcome = n_releases * p_outcome_m,
    n_outcome_se = ifelse(is.na(se_releases), n_releases * p_outcome_se,
                          n_releases*p_outcome_m * sqrt((se_releases/n_releases)^2 + (p_outcome_se/p_outcome_m)^2))
  ) %>%
  group_by(health_outcome, sex_gender) %>%
  summarize(
    n_outcome_se = sqrt(sum(n_outcome_se^2, na.rm = T)),
    n_outcome = sum(n_outcome, na.rm = T),
    n_releases = sum(n_releases, na.rm = T)
  ) %>% ungroup() %>%
  mutate(p_outcome = n_outcome/n_releases,
         p_outcome_se = n_outcome_se/n_releases)

## Should get proportions & CI nationally.
## Note, not very variable for prisons (similar age, sex, race distribution), ask Sanjay if they'd expect big age distirubitons between adults
p_HO_medicaid <- df_nhanes %>%
  bind_rows(df_nsduh) %>%
  transmute(health_outcome, sex_gender, n, p_outcome = p_outcome_m, p_outcome_se, p_outcome_lwr, p_outcome_upr)

p_HO <- bind_rows(p_HO_incarc_total %>% mutate(cohort = "Releasees"), 
                  p_HO_medicaid %>% mutate(cohort = "Medicaid/CHIP")) %>%
  mutate(p_outcome_lwr = ifelse(is.na(p_outcome_lwr), 
                                invlink(log(p_outcome) - (qnorm(0.975)*p_outcome_se), type = "log"), 
                                p_outcome_lwr),
         p_outcome_upr = ifelse(is.na(p_outcome_upr), 
                                invlink(log(p_outcome) + (qnorm(0.975)*p_outcome_se), type = "log"), 
                                p_outcome_upr),
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
         )

vars_ho <- c("Moderate\n(K6: 5-12)","Serious\n(K6: ≥13)",
             "Hypertension","Asthma","Hepatitis C","Cardiovascular\nDisease","Diabetes","Chronic\nKidney\nDisease","Stroke")

library(ggpubr)
library(grid)

y.pos = -0.05
y.height = 0.015

g <- ggplot(p_HO %>% 
              mutate(health_outcome = factor(health_outcome, levels = vars_ho),
                     sex_gender = factor(sex_gender, levels = c("Male","Female"))
                     ) %>%
              filter(!is.na(health_outcome)),
            aes(x = health_outcome, y = p_outcome)) +
  geom_point(aes(color = cohort, group = cohort), position = position_dodge(width=0.45)) +
  geom_errorbar(aes(color = cohort, group = cohort, ymin = p_outcome_lwr, ymax = p_outcome_upr), position = position_dodge(width=0.45), width = .35) +
  scale_y_continuous("Population (%)", expand = c(0,0), labels = scales::percent_format(accuracy = 1), limits = c(NA,0.5), breaks = seq(0,0.5,0.1)) +
  scale_x_discrete(NULL) +
  scale_color_manual(NULL,values = c("#0a92a2","#f37c54")) + 
  theme_classic(base_size = 14) +  
  theme(strip.text.x = element_text(hjust = 0, margin=margin(l=0,b=10), size = 16),
        plot.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(t = 1, r = 0, b = 4, l = 0, unit = "lines"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.93,0.95),
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

ggsave(file.path(here::here(), "figs", "compare_disease_prevalence.pdf"), g, device = cairo_pdf, width = 20, height = 7)
ggsave(file.path(here::here(), "figs", "compare_disease_prevalence.png"), g, width = 20, height = 7)

# Restrictions by groups (%?)
vars_ho <- c("SUD","SUD or SMI","SUD, SMI,\nor I/DD","SUD, AMI,\nor I/DD","SUD, AMI, I/DD,\nHIV, Hep C, or\nChronic condition")
g <- ggplot(p_HO %>% 
              mutate(health_outcome = factor(health_outcome, levels = vars_ho),
                     sex_gender = factor(sex_gender, levels = c("Male","Female")),
                     p_outcome_upr = pmin(1, p_outcome_upr)
              ) %>%
              filter(!is.na(health_outcome), cohort == "Releasees"),
            aes(x = health_outcome, y = p_outcome)) +
  geom_point(aes(color = sex_gender, group = sex_gender), position = position_dodge(width=0.45)) +
  geom_errorbar(aes(color = sex_gender, group = sex_gender, ymin = p_outcome_lwr, ymax = p_outcome_upr), 
                position = position_dodge(width=0.45), width = .35) +
  scale_y_continuous("Population (%)", expand = c(0,0), labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
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

ggsave(file.path(here::here(), "figs", "eligibility.pdf"), g, device = cairo_pdf, width = 10, height = 6)
ggsave(file.path(here::here(), "figs", "eligibility.pdf.png"), g, width = 10, height = 6)

# Health profiles by race and age group for jails and prisons?



