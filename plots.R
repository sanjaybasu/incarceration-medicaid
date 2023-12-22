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
df_healthOutcomes <- df_healthOutcomes %>%
  mutate(health_outcome = recode(health_outcome, ami_any = "AMI"))

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


# Health profiles by race and age group for jails and prisons?

# Restrictions by health types (%?)

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
                                p_outcome_upr)
         )

ggplot(p_HO %>% filter(health_outcome %in% intersect(unique(p_HO_incarc_total$health_outcome), unique(p_HO_medicaid$health_outcome))),
       aes(x = health_outcome, y = p_outcome, ymin = p_outcome_lwr, ymax = p_outcome_upr, color = cohort, group = cohort)) +
  geom_point(position = position_dodge(width=0.45)) +
  geom_errorbar(position = position_dodge(width=0.45), width = .3) +
  scale_y_continuous("Population (%)", expand = c(0,0.1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete("Condition", expand = c(0,0)) +
  theme_classic(base_size = 14) +
  facet_wrap(~sex_gender, nrow = 2)



