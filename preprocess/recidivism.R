# Download data from recidivism cohort (2012)
# Download data from recidivism cohort (2005)

# from 2005 data, get recidivism by month (rprts05p0510f01). Scale by 1 year recidivism rate in 2012 data. split by rearrest, reconviction, and reincarceration
# note for recidivism data, impossible to get state-level estimates since there's variance in definitions for recidivism

# assume independence between recidivism by sex, age, and race
#There are also some cross tables with the federal system: https://themilsource.com/content/files/sites/default/files/pdf/research-and-publications/research-publications/2017/20171207_Recidivism-Age.pdf (page 24)
# Figure out how to combine. we have aggregate overall rates, but may be more difficult to combine the estimates
# The fed one is recidivism after 5 years, which is not the most helpful. 5 year ~ 3 year. 

library(readr)
library(here)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(rlang)
library(ggplot2)

path_to_recidivism05 <- file.path(here::here(), "data","raw","rprts05p0510")
path_to_recidivism08 <- file.path(here::here(), "data","raw","rpr24s0810yfup0818")
path_to_recidivism12 <- file.path(here::here(), "data","raw","rpr34s125yfup1217")
path_to_releases <- file.path(here::here(), "data","raw","ICPSR_37021")

time_to_recidiv <- read_csv(file.path(path_to_recidivism05, "rprts05p0510f01.csv"), skip = 13) %>%
  filter(!is.na(`All releases`)) %>%
  mutate(`Time to rearrest (in months)` = as.numeric(`Time to rearrest (in months)`))

time_to_recidiv08 <- read_csv(file.path(path_to_recidivism08, "rpr24s0810yfup0818f02.csv"), skip = 10) %>%
  filter(!is.na(`Arrest/a`)) %>%
  mutate(`Time to rearrest (in months)` = as.numeric(gsub('.{2}$', '', `Year of release`))*12) %>%
  rename(`All releases` = `Arrest/a`, `Conviction` = `Conviction/b`, `Return to prison` = `Return to prison/c`) %>%
  select(-c(`Year of release`, ...3, ...5, ...7)) %>%
  filter(`Time to rearrest (in months)` <= max(time_to_recidiv$`Time to rearrest (in months)`))

time_to_recidiv_long <- time_to_recidiv %>% 
  pivot_longer(cols = 2:4, names_to = "recidivism_type", values_to = "cum.recidivism") %>%
  rename(Month = `Time to rearrest (in months)`) %>%
  mutate(recidivism_type = recode(recidivism_type, 
                                  `All releases` = "rearrested", 
                                  Conviction = "reconvicted",
                                  `Return to prison` = "return to prison"),
         cum.recidivism = cum.recidivism / 100
         )

time_to_recidiv_long08 <- time_to_recidiv08 %>%
  pivot_longer(cols = 1:3, names_to = "recidivism_type", values_to = "cum.recidivism") %>%
  rename(Month = `Time to rearrest (in months)`) %>%
  mutate(recidivism_type = recode(recidivism_type, 
                                  `All releases` = "rearrested", 
                                  Conviction = "reconvicted",
                                  `Return to prison` = "return to prison"),
         cum.recidivism = cum.recidivism / 100
  )
  
#plot recidivism (cumulative % of first recidivism event) by type
#note only the 2005 one has it by month
# ggplot(time_to_recidiv_long, 
#        aes(x = Month, y = cum.recidivism, color = recidivism_type)) + 
#   geom_line() +
#   theme_classic(base_size = 14) +
#   scale_y_continuous("Persons released (%)", labels=scales::percent_format(accuracy = 1)) +
#   labs(color = NULL)

#load in states in study. 34 states had data on arrests
states_sampled <- read_csv(file.path(path_to_recidivism12, "rpr34s125yfup1217at01.csv"), skip = 11) %>%
  filter(!is.na(`Sample size`))
#31 states had data to assess reconvictions (excludes KY, LA, ME)
states_reconvict <- states_sampled %>%
  filter(!is.na(State), !(State %in% c("Kentucky","Louisiana","Maine"))) %>%
  pull(State)
#only 21 states had data to assess reincarcerations
states_reincarc <- c("Arizona", "California", "Colorado", "Florida", "Georgia",
                       "Iowa", "Kansas", "Massachusetts", "Michigan", "Missouri",
                       "Nevada", "New Jersey", "New York", "North Carolina", "Ohio",
                       "Oklahoma", "Tennessee", "Texas", "Washington", "Wisconsin","Wyoming")

# Get aggregate statistics for the proportion of people released between 2011-2013 by state, race, age, and gender
## We average between 3 years to improve numeric stability
## Data is from the National Corrections Reporting Program, 1991-2016: Selected Variables (ICPSR 37021)
load(file.path(path_to_releases,"DS0003","37021-0003-Data.rda"))
df_releases <- da37021.0003 %>%
  filter(RELYR %in% c(2011, 2012, 2013)) %>%
  group_by(STATE,SEX,RACE,AGERLSE) %>% 
  count()
rm(da37021.0003)

df_releases <- df_releases %>% ungroup() %>%
  mutate_at(c("STATE", "SEX", "RACE", "AGERLSE"), .funs = ~word(as.character(.), start = 2, end = -1))

df_releases <- df_releases %>%
  mutate(RACE = case_when(
    grepl("^Hispanic", RACE) ~ "Hispanic",
    grepl("White", RACE) ~ "White",
    grepl("Black", RACE) ~ "Black",
    TRUE ~ "Other or Unknown"
  ))

n_missing <- df_releases %>% filter(AGERLSE == "0") %>% pull(n) %>% sum()
print(paste("Removing ", n_missing, " (", round(100*n_missing/sum(df_releases$n), 3), "%), releases that have missing ages."))
df_releases <- df_releases %>%
  filter(AGERLSE != "0")

# We need to convert the age groups in the release file to the one in the recidivism file.
## We split the 35-44 age group evenly between the 25-39 and 40-54 age groups
## Since there's fewer older individuals, we do a 3:1 split between the 55-64 and 65+ age groups
print(sum(df_releases$n))
df_releases <- rbind(df_releases,
                     df_releases %>%
                       filter(AGERLSE == "35-44 years") %>%
                       mutate(n = n/2, AGERLSE = "25-34 years"),
                     df_releases %>%
                       filter(AGERLSE == "35-44 years") %>%
                       mutate(n = n/2, AGERLSE = "45-54 years"),
                     df_releases %>%
                       filter(AGERLSE == "55+ years") %>%
                       mutate(n = n*0.75, AGERLSE = "55-64 years"),
                     df_releases %>%
                       filter(AGERLSE == "55+ years") %>%
                       mutate(n = n*0.25, AGERLSE = "65 or older")) %>%
  filter(!(AGERLSE %in% c("35-44 years","55+ years")))
print(sum(df_releases$n))

df_releases <- df_releases %>%
  group_by(STATE,SEX,RACE,AGERLSE) %>% summarize(n=sum(n)) %>% ungroup()

#Note alabama has some weird numbers. only 1 Black NH male per year. Since our analysis doesn't include it, will ignore, but flagging for QC purposes
df_releases <- df_releases %>%
  group_by(STATE) %>%
  mutate(p = n/sum(n))

df_releases <- df_releases %>%
  mutate(AGERLSE = case_when(
    AGERLSE == "18-24 years" ~ "24 or younger",
    AGERLSE == "25-34 years" ~ "25-39",
    AGERLSE == "45-54 years" ~ "40-54",
    TRUE ~ gsub(" years$", "", AGERLSE)
  ))

write_csv(df_releases, file.path(here::here(), "data","raw","prison_releases_2011to2013.csv"))

#load in recidivism at 1 year mark by race, age, and sex
##We calculate these stats, with the reference being Male, White, and 25-39
process_recidivism <- function(rtype = c("rearrested","reconvicted","return to prison")){
  states_recidiv = na.omit(states_sampled$State)
  nskip = 10
  
  print(rtype)
  
  if(rtype == "rearrested"){
    finame = "rpr34s125yfup1217t04.csv"
    cname = "All releases"
    }
  if(rtype == "reconvicted"){
    finame = "rpr34s125yfup1217t07.csv"
    cname = "Conviction"
    states_recidiv = states_reconvict
    }
  if(rtype == "return to prison"){
    finame = "rpr34s125yfup1217t08.csv"
    cname = "Return to prison"
    states_recidiv = states_reincarc
  }
  
  recidiv_stats <- read_csv(file.path(path_to_recidivism12, finame), skip = nskip) %>%
    filter(!is.na(`Year 1`))
  perc_cols <- colSums(recidiv_stats == "%", na.rm = T)
  
  recidiv_stats <- recidiv_stats[,names(perc_cols)[perc_cols == 0]] %>%
    select(-Characteristic) %>%
    rename(Characteristic = ...2) %>%
    mutate(Characteristic = gsub("(\\*)|(/a)|(,b)","",iconv(ifelse(is.na(Characteristic), ...3, Characteristic), "UTF-8","ASCII",sub="-")),
           Category = case_when(
             Characteristic == "All released prisoners" ~ "Combined",
             Characteristic %in% c("Male","Female") ~ "Gender",
             Characteristic %in% c("White","Black","Hispanic","American Indian/Alaska Native","Asian/Native Hawaiian/Other Pacific Islander","Other") ~ "Race",
             TRUE ~ "Age"
           ),
           Ref = ifelse(Characteristic %in% c("All released prisoners","Male","White","25-39"), T, F)
    ) %>%
    select(-...3)
  
  recidiv_stats <- recidiv_stats %>%
    group_by(Category) %>%
    mutate_at(vars(starts_with("Year")),
              .funs = list(RR = ~ . / .[Ref]))
  
  recidiv_stats_long <- recidiv_stats %>%
    select(-matches("[0-9]$")) %>%
    pivot_longer(cols = ends_with("RR")) %>%
    mutate(Year = as.numeric(gsub("_RR","",gsub("Year ","",name))))
  
  recidiv_comb <- recidiv_stats %>% 
    ungroup() %>%
    filter(Category == "Combined") %>%
    pivot_longer(matches("^Year [0-9]$"), names_to = "Year", 
                 values_to = "Recidiv_2012", 
                 names_transform = ~as.numeric(gsub("Year ","",.))) %>%
    select(Year, Recidiv_2012)
  recidiv_stats_long <- recidiv_stats_long %>% filter(Category != "Combined")
  
  if(F){
    ## QA plots
    #Fairly stable relative rates except for Race == "Other"
    library(cowplot)
    reincarc_out <- by(data = reincarc_stats_long,
                       INDICES = reincarc_stats_long$Category,
                       FUN = function(m){
                         lab_title <- m$Category[1]
                         m <- ggplot(m %>%
                                       mutate(Characteristic = ifelse(Ref, paste0(Characteristic, " (Ref)"), Characteristic)), 
                                     aes(x = Year, y = value, color = Characteristic)) + 
                           geom_line() + 
                           labs(y="Relative rate") +
                           ggtitle(lab_title) + 
                           theme_classic(base_size = 14)
                       }
    )
    do.call(plot_grid, c(reincarc_out, align = "v", ncol = 1))
  }
  
  #Get combinations of all variables
  expanded_cats <- expand.grid(list(
    Gender = recidiv_stats_long %>% filter(Category == "Gender") %>% pull(Characteristic) %>% unique(),
    Race = recidiv_stats_long %>% filter(Category == "Race") %>% pull(Characteristic) %>% unique(),
    Age = recidiv_stats_long %>% filter(Category == "Age", Characteristic != "40 or older") %>% pull(Characteristic) %>% unique(),
    Year = 1:5
  ))
  
  rr_expanded <- sapply(c("Gender","Race","Age"), function(x){
    expanded_cats %>% mutate(rid = 1:n()) %>%
      merge(recidiv_stats_long, 
            by.x = c(x,"Year"), by.y = c("Characteristic","Year")) %>%
      arrange(rid) %>% #note merge reorders the matrix so you must have this
      pull(value)}) %>% as.data.frame()
  
  colnames(rr_expanded) <- paste0("RR_",colnames(rr_expanded))
  
  # We assume that there are no interactions between Race, Age, and Gender
  ## RR is the overall rate
  rr_expanded$RR <- apply(rr_expanded, 1, prod)
  
  rr_expanded <- cbind(expanded_cats, rr_expanded)
  
  # Filter to just the states involved in sampling and weight based on the recidivism sampling
  ## We'll use this sampling to weight the states in the release dataset
  recidiv_sampling <- states_sampled %>% filter(State %in% states_recidiv) %>% mutate(pWt = `Weighted total/a`/sum(`Weighted total/a`))
  df_releases_c <- recidiv_sampling %>% select(State, pWt) %>%
    left_join(df_releases, by = c("State" = "STATE"))
  
  df_releases_c <- df_releases_c %>%
    mutate(pT = p*pWt) %>%
    group_by(SEX, RACE, AGERLSE) %>%
    summarize(p = sum(pT))
  
  # Now combine back with the recidivism rates.
  ## Note that df_releases_c represents the weighted average proportions by group
  
  # The releases dataset aggregates AAPI and AI/AN into the other category. We will use stats from the BJS report in 2012 to split recidivism rates
  ## https://bjs.ojp.gov/sites/g/files/xyckuh236/files/media/document/p21st.pdf
  n.tot <- 1512430
  n.wbh <- 470900 + 536600 + 336100 # n for White, Black and Hispanic
  n.aian <- 21500
  n.aapi <- 17000
  n.other <- n.tot - n.wbh - n.aian - n.aapi
  n.allOther <- n.aian + n.aapi + n.other
  p.other <- n.other/n.allOther
  p.aian <- n.aian/n.allOther
  p.aapi <- n.aapi/n.allOther # ~ 8 : 1 : 1 split between other : aian : aapi
  
  rr_expanded <- rr_expanded %>%
    mutate(p.tmp = case_when(
      Race == "American Indian/Alaska Native" ~ p.aian,
      Race == "Asian/Native Hawaiian/Other Pacific Islander" ~ p.aapi,
      Race == "Other" ~ p.other,
      TRUE ~ 1),
      RR = RR*p.tmp,
      Race = as.character(Race),
      Race = ifelse(Race %in% c("American Indian/Alaska Native","Asian/Native Hawaiian/Other Pacific Islander","Other"), "Other or Unknown", Race)
    )  %>%
    group_by(Gender, Race, Age, Year) %>%
    summarize(RR = sum(RR))
  
  rr_expanded <- rr_expanded %>%
    left_join(df_releases_c, by = c("Gender" = "SEX", "Race" = "RACE", "Age" = "AGERLSE"))
  
  rr_expanded <- rr_expanded %>%
    mutate(RR_wt = p*RR)
  
  rr_by_year = rr_expanded %>%
    group_by(Year) %>%
    summarize(RR_wt = sum(RR_wt))
  
  base_rates_by_year <- time_to_recidiv %>%
    mutate(Year = `Time to rearrest (in months)`/12) %>%
    merge(rr_by_year) %>%
    merge(recidiv_comb)
  base_rates_by_year$Recidiv_new05 <- tail(c(0,base_rates_by_year[[cname]]) - lag(c(0,base_rates_by_year[[cname]]), 1), -1)
  base_rates_by_year$Recidiv_new12 <- tail(c(0,base_rates_by_year$Recidiv_2012) - lag(c(0,base_rates_by_year$Recidiv_2012), 1), -1)
  print(range(base_rates_by_year$RR_wt))
  
  df_base = time_to_recidiv %>% select(`Time to rearrest (in months)`) %>% 
    mutate(cohort = "2005") %>%
    bind_rows(time_to_recidiv08 %>% 
                select(`Time to rearrest (in months)`) %>% 
                mutate(cohort = "2008"))
  df_base$cum.recidivism = c(time_to_recidiv[[cname]], time_to_recidiv08[[cname]])
  df_base = bind_rows(df_base, 
                      base_rates_by_year %>% 
                        rename(cum.recidivism = Recidiv_2012) %>%
                        select(`Time to rearrest (in months)`, cum.recidivism) %>%
                        mutate(cohort = "2012")) %>%
    rename(Month = `Time to rearrest (in months)`) %>%
    mutate(cum.recidivism = cum.recidivism / 100) %>%
    filter(Month > 0)
  
  formula.init <- cum.recidivism ~ SSmicmen(Month, Vm, K)
  m.init <- nls(formula.init, df_base, subset = cohort == "2005")
  
  formula.m <- cum.recidivism ~ (Vm1*as.numeric(cohort == "2005") + Vm2*as.numeric(cohort == "2008") + Vm3*as.numeric(cohort == "2012")) * Month/(K+Month)
  m <- nls(formula.m, df_base, 
           start = list(Vm1 = coef(m.init)[["Vm"]], 
                        Vm2 = coef(m.init)[["Vm"]], 
                        Vm3 = coef(m.init)[["Vm"]], 
                        K = coef(m.init)[["K"]]))
  
  mpred <- tibble(Month = rep(1:60, 3), cohort = sort(rep(c("2005","2008","2012"), 60)))
  mpred$cum.recidivism <- predict(m, mpred)
  ggplot(df_base,
         aes(x = Month, y = cum.recidivism, color = cohort)) +
    geom_line() +
    geom_line(data = mpred,
              linetype = "dashed") +
    theme_classic(base_size = 14) +
    labs(y = "New reincarcerations (%)", color = NULL)
  
  recidiv.fitted <- mpred %>%
    filter(cohort == "2012") %>%
    arrange(Month) %>%
    mutate(p.recidivism = 100*coalesce(cum.recidivism - lag(cum.recidivism, 1), first(cum.recidivism)))

  # time_to_recidiv <- time_to_recidiv %>% 
  #   arrange(`Time to rearrest (in months)`)
  # time_to_recidiv$Recidiv_new <- time_to_recidiv[[cname]] - lag(time_to_recidiv[[cname]], 1)
  # 
  # # exponential function
  # time_to_recidiv <- time_to_recidiv %>% filter(!is.na(Recidiv_new)) %>% 
  #   mutate(Month = `Time to rearrest (in months)`)
  # formula.m <- Recidiv_new ~ SSasymp(Month, Asym, R0, lrc)
  # m=nls(formula.m, time_to_recidiv)
  # 
  # ypred <- predict(m, tibble(Month = 1:60))
  # mpred <- tibble(Month = 1:60, Recidiv_new = ypred)
  # 
  # ggplot(time_to_recidiv, 
  #        aes(x = Month, y = Recidiv_new)) + 
  #   geom_line() +
  #   geom_line(data = mpred,
  #             colour = "red") +
  #   theme_classic(base_size = 14) +
  #   labs(y = "New reincarcerations (%)", color = NULL)
  # 
  # plot(1:60,as.numeric(residuals(m)))
  # abline(0,0)
  
  time_to_recidiv <- time_to_recidiv %>%
    left_join(recidiv.fitted %>% select(-c(cohort, cum.recidivism)) %>%
                rename(Recidiv_newS = p.recidivism), 
              by = c("Time to rearrest (in months)"="Month")) %>%
    mutate(Ref_Recidiv_newS = Recidiv_newS / mean(base_rates_by_year$RR_wt))
  
  if(F){
    # QA
    g <- ggplot(rr_expanded, aes(x = Year, y = RR, color = Race, linetype = Gender)) + 
      geom_line() + 
      facet_wrap(~Age) +
      theme_classic(base_size = 14)
  }
  
  #there are some inconsistencies, but the numbers are relatively stable over the years.
  ## We assume that the relative rate by Race, Age, Sex group is constant across time (summarize over the years with the mean)
  rr_expanded_avg <- rr_expanded %>%
    group_by(Gender, Race, Age, p) %>%
    summarize(RR = mean(RR)) %>% ungroup() %>%
    rename(pPopulation = p)
  
  recid_by_month <- time_to_recidiv %>%
    select(`Time to rearrest (in months)`, Ref_Recidiv_newS) %>%
    rename(Month = `Time to rearrest (in months)`, Recidiv_ref = Ref_Recidiv_newS) %>%
    filter(!is.na(Recidiv_ref))
  
  #To combine these two files, we multiply the monthly recidivism rate by the relative rate to get the group-wise rate
  x <- expand.grid(1:nrow(rr_expanded_avg),1:nrow(recid_by_month))
  rr_combined <- cbind(rr_expanded_avg[x[,1],], recid_by_month[x[,2],]) %>%
    mutate(p.recidivism = Recidiv_ref * RR / 100,
           recidivism_type = rtype) # %>%
    # select(-c(RR, Recidiv_ref,pPopulation))
  
  recidiv_comb <- recidiv_comb %>%
    mutate(group = "raw") %>%
    bind_rows(recidiv.fitted %>% 
                mutate(Recidiv_2012 = cum.recidivism*100, group = "predicted") %>% 
                select(Month, Recidiv_2012, group)) %>%
    mutate(recidivism_type = rtype)
  
  return(list(rr = rr_combined, 
              raw = recidiv_comb))
}

df_rearrested <- process_recidivism("rearrested")
df_reconvicted <- process_recidivism("reconvicted")
df_return <- process_recidivism("return to prison")

df_recidivism <- bind_rows(df_rearrested$rr, df_reconvicted$rr, df_return$rr)
df_raw <- bind_rows(df_rearrested$raw, df_reconvicted$raw, df_return$raw)

# Check totals to see if it aligns with original dataset
df_agg <- df_recidivism %>% 
  group_by(recidivism_type,Month) %>% 
  summarize(p.recidivism = sum(p.recidivism*pPopulation)) %>% ungroup() %>%
  arrange(recidivism_type, Month) %>%
  group_by(recidivism_type) %>%
  mutate(cum.recidivism = cumsum(p.recidivism))

df_plot <- bind_rows(time_to_recidiv_long %>% mutate(cohort = "2005"),
                     time_to_recidiv_long08 %>% mutate(cohort = "2008"),
                     df_raw %>% 
                       filter(group == "raw") %>%
                       mutate(Month = Year * 12, cum.recidivism = Recidiv_2012/100, cohort = "2012"),
                     df_agg %>% mutate(cohort = "2012 (fitted)")
                     ) %>% select(Month, recidivism_type, cum.recidivism, cohort)
df_plot <- df_plot %>% expand(recidivism_type, cohort) %>% mutate(Month = 0, cum.recidivism = 0) %>% # add zeros
  bind_rows(df_plot) %>%
  distinct() 

g <- ggplot(df_plot %>% 
              mutate(cohort = relevel(factor(cohort), ref = "2012 (fitted)")),
            aes(x = Month, y = cum.recidivism, color = recidivism_type)) + 
  geom_line(aes(linewidth = cohort)) +
  scale_linewidth_manual(values = c(1, rep(0.25, 3))) +
  geom_point(data = df_plot %>% filter(!grepl("fitted", cohort)), aes(shape = cohort), size = 1.5) + 
  theme_classic()

ggsave(g, file = file.path(here::here(), "preprocess","figs","recidisivm_agg.pdf"), width = 10, height = 6)

write_csv(df_recidivism %>% select(-c(pPopulation, RR, Recidiv_ref)), 
          file.path(here::here(), "data","recidisivm_byTypeSexRaceAge.csv"))

g <- ggplot(df_recidivism, 
            aes(x = Month, y = p.recidivism, color = Race, linetype = Gender)) + 
  geom_line() + 
  scale_y_continuous("Monthly recidivism rate (%)", label = scales::percent) +
  facet_grid(recidivism_type~Age) +
  theme_classic(base_size = 14) +
  guides(linewidth = NULL)

ggsave(g, file = file.path(here::here(), "preprocess","figs","recidisivm.pdf"), width = 10, height = 8)

