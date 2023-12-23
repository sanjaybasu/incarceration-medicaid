library(here)
library(tidyverse)
#Download data from https://www.icpsr.umich.edu/web/NACJD/studies/37692
#Extract data in ../data/raw
#Code below reflects downloading the data using the "R" version

data_dir = file.path(here::here(), "data","raw","ICPSR_37692")

#########################
# Functions
#########################
getAny <- function(indVars, df){
  df2 <- df %>% 
    select(!!indVars) %>%
    mutate_all(.funs = ~coalesce(ifelse(. == "(1) 1 = Yes", 1, 0), 0)) 
  varAny <- as.numeric(rowSums(df2) > 0.5)
}

#########################
# Variables to aggregate over
#########################
aggVars <- list(
  age = "RV0001",
  ageCat = "RV0002",
  ageCorrect = "V0012",
  sentenced = "V0073",
  sex = "RV0005",
  raceEthnicity6 = "RV0003",
  raceEthnicity5 = "RV0003B",
  state = "V0772"
)

#health outcomes
#   any refers to diagnoses by a medical provider at any point in time
#   current refers to a provider telling a patient they currently have the disease
analysisVars <- list(
  cvd = "V1229",
  
  hypertension = "V1226" ,
  
  diabetes = "V1228",
  
  kidney = "V1230",
  
  asthma = "V1247",
  
  stroke = "V1227",
  
  stroke_problems = "V1242",

  hepB = "V1235",

  hepC = "V1236",
  
  hiv = "V1237",
  
  autoimmune = "V1231",
  
  liver = "V1233",
  
  tb = "V1234",
  
  cancer = "V1225",
  
  depression = "V1186",
  
  # health insurance related measures
  #   these reflect insurance status during the 30 days prior to arrest
  #   using their "upcoded" versions
  insurance_employer = "V1148",
  insurance_direct = "V1149", # purchased directly from insurance company
  insurance_other = "V1155",
  
  #Govt assistance measures
  publicAssistance_30d = "V1151", #on public assistance during 30 days prior to arrest
  va_eligible = "V1164",
  
  #other
  homeless_12m = "V0961"
)

#any 'yes' to: AUD1, any of AUD10-21
alcoholUseVars <- c("V1268",paste0("V", 1277:1288))

#any 'yes' to: DU1d or DU6d or DU8d or DU10d [prior history], with indications of dependence: any of DUD1, DUD2, DUD5, DUD6, or any of DUD11-22
opioidUseVars <- c("V1294","V1318","V1342", paste0("V135", c(0:1, 4:5)), paste0("V", 1360:1371))

#any 'yes' to: MH7. MH diagnosis (lifetime). Also includes history of hospitalization, prescriptions, and treatment for MH.
## Prescription (at time of arrest or during incarceration)

mhdiagVars <- paste0("V", 1185:1191)

mhhistVars <- c("V1199","V1201","V1202","V1204")

mhcurrVars <- c("V1203","V1205")

#k6
k6Vars <- paste0("V",1179:1184)

load(file.path(data_dir,"DS0001","37692-0001-Data.rda")) # State and Fed prisons

# define SMI as difficulty finding a job b/c of mental health OR hospitalization
smiVars <- c("V1091", "V1200")

# I/DD: attention deficit disorder, learning disability, or enrolled in special education classes + cognitive difficulties
iddVars <- c(paste0("V0", 942:944))

allVars <- unique(unname(unlist(c(tail(aggVars,-1), analysisVars, alcoholUseVars, opioidUseVars, mhdiagVars, mhhistVars, mhcurrVars, smiVars, k6Vars, iddVars))))

#replace several responses with NA
spiData <- da37692.0001 %>% 
  mutate_at(all_of(allVars),
            .funs = ~recode_factor(., 
                                   `(-9) -9 = Response missing due to CAPI issue` = NA_character_,
                                   `(-8) -8 = Skipped` = NA_character_,
                                   `(-2) -2 = Refusal` = NA_character_,
                                   `(-1) -1 = Don't Know` = NA_character_,
                                   `(98) 98 = DK/REF` = NA_character_,
                                   `(99) 99 = Blank` = NA_character_
                                   )
            )

#replace age with NA if respondee stated the age listed was wrong
#combine other, 2+ races, and missing
#replace missing states
ageVars = unname(unlist(aggVars[c("age","ageCat")]))
spiData <- spiData %>%
  mutate_at(all_of(ageVars),
            .funs = ~if_else(V0012 == "(1) 1 = Yes", ., NA)
  ) %>%
  mutate(raceEthnicity = recode_factor(RV0003B,
                                       `(4) 4 = Other (NH), single race` = "Multiracial, other, or missing",
                                       `(5) 5 = 2+ Races (NH)` = "Multiracial, other, or missing",
                                       `(9) 9 = Uncategorized - Missing` = "Multiracial, other, or missing"
                                       ),
         ageCat2 = cut(RV0001, c(0, 17.5, 34.5, 49.5, 64.5, 100), labels = c("<18","18-34","35-49","50-64","65+"))
  ) %>%
  mutate_at(k6Vars, .funs = ~recode(., 
                                    `(1) 1 = All of the Time` = 4,
                                    `(2) 2 = Most of the Time` = 3,
                                    `(3) 3 = Some of the Time` = 2,
                                    `(4) 4 = A Little of the Time` = 1,
                                    `(5) 5 = None of the Time` = 0
  ))

levels(spiData[[aggVars$state]]) <- c(NA, NA, NA, tail(levels(spiData[[aggVars$state]]), -3))

#code mental health any
spiData$mhDiagnosis_any <- getAny(mhdiagVars, spiData)

# code chronic conditions
spiData$chronic_any <- getAny(unname(unlist(analysisVars[c("cvd","hypertension","diabetes","kidney","asthma","stroke","hepC","hiv","autoimmune", "liver", "tb","cancer")])), spiData)

#code opioid use
spiData$opioidUse_any = getAny(opioidUseVars, spiData)

#code alcohol use
spiData$alcoholUse_any = getAny(alcoholUseVars, spiData)

# I/DD
spiData$idd_any <- getAny(iddVars, spiData)

# K6
spiData$K6 <- as.numeric(rowSums(spiData[k6Vars], na.rm = T))

spiData$SPD = as.numeric(spiData$K6 > 12)

spiData$MPD = as.numeric(between(spiData$K6, 4.5, 12.5))

# Mental health
## SMI
spiData$smi_any <- getAny(c(smiVars,"SPD"), spiData)

## AMI
spiData$ami_any <- getAny(c(mhdiagVars, mhhistVars, mhcurrVars,"MPD",smiVars,"SPD"), spiData)

# SUD
spiData$sud_any <- as.numeric(rowSums(spiData[c("alcoholUse_any","opioidUse_any")], na.rm = T) > 0)

# SUD + SMI
spiData$sud_smi <- as.numeric(rowSums(spiData[c("sud_any","smi_any")], na.rm = T) > 0)

# SUD + SMI + IDD
spiData$sud_smi_idd <- as.numeric(rowSums(spiData[c("sud_any","smi_any","idd_any")], na.rm = T) > 0)

# SUD + AMI + IDD
spiData$sud_ami_idd <- as.numeric(rowSums(spiData[c("sud_any","ami_any","idd_any")], na.rm = T) > 0)

# SUD + MI + chronic
spiData$sud_ami_idd_chronic <- as.numeric(rowSums(spiData[c("sud_any","ami_any","idd_any","chronic_any")], na.rm = T) > 0)

colVars <- unlist(c(analysisVars, mhDiagnosis_any = "mhDiagnosis_any", opioidUse_any = "opioidUse_any", alcoholUse_any = "alcoholUse_any",
                    idd_any = "idd_any", smi_any = "smi_any", ami_any = "ami_any", chronic_any = "chronic_any", 
                    sud_any = "sud_any", sud_smi = "sud_smi",  sud_smi_idd = "sud_smi_idd", sud_ami_idd = "sud_ami_idd", 
                    sud_ami_idd_chronic = "sud_ami_idd_chronic",
                    spd = "SPD", mpd = "MPD"
                    ))

#all outcomes are binary. replace NA with 0
spiData2 <- spiData %>%
  mutate_at(all_of(unname(unlist(analysisVars))), .funs = ~coalesce(2-as.integer(.),0))

#include survey replicates
library(survey)

repwts <- paste0("V", 1586:1949)
spi_svy <- svrepdesign(
  data = spiData2, 
  weights = ~V1585, 
  repweights = spiData2[, repwts], 
  type = "JK1",
  scale = 1,
  rscales = rep(c(.944, .978, .917, .667, .986, .994, .966, .75, .95, .5),
                times = c(18,   46,   12,    3,   70,  160,   29,   4,  20,  2)
  ) )

aggVars = unlist(aggVars)
aggVarsShort = c(ageCat2 = "ageCat2", aggVars[names(aggVars) %in% c( "sex")], raceEthnicity = "raceEthnicity")
aggForm = reformulate(aggVarsShort)
colForm = reformulate(colVars)

dat_n <- spiData2 %>%
  group_by(across(all_of(unname(aggVarsShort)))) %>%
  summarize(n = n(), neff = sum(V1585)^2/sum(V1585^2)) # Kish's Effective Sample Size

dat_n2 <- spiData2 %>%
  group_by(V0071, RV0005) %>%
  summarize(n = n(), neff = sum(V1585)^2/sum(V1585^2)) # Kish's Effective Sample Size

getSurveyStats <- function(cname, agg_form, df_n){
  require(dplyr)
  require(survey)
  df <- svyby(reformulate(cname), 
                    by=agg_form, 
                    design = spi_svy, 
                    FUN = svyciprop, 
                    vartype="ci",
                    method="beta",
                    keep.var = T,
                    keep.names = F
  ) %>% as.data.frame() %>%
    mutate(health_outcome = cname) %>%
    rename(p.lwr = ci_l, p.upr = ci_u)
  df$p = df[[cname]]
  df[[cname]] = NULL
  
  df2 <- svyby(reformulate(cname), 
              by=agg_form, 
              design = spi_svy, 
              FUN = svymean, 
              vartype="se",
              method="beta",
              keep.var = T,
              keep.names = F
  ) %>% as.data.frame()
  df2[[cname]] <- NULL
  
  df <- df %>%
    left_join(df2) %>%
    left_join(df_n) %>%
    mutate(
      p.lwr = case_when(
        p == 0 ~ 0, 
        p == 1 ~ pmax(0, 1-3/neff), # Rule of 3
        T ~ p.lwr),
      p.upr = case_when(
        p == 0 ~ pmin(1, 3/neff),
        p == 1 ~ 1,
        T ~ p.upr
      )
    )
  return(df)
}

require(parallel)
library(future.apply)
plan(multisession, workers = parallel::detectCores()-1) ## Parallelize using five cores
rm(spiData)
rm(da37692.0001)

aggStats <- bind_rows(future_lapply(unname(colVars), function(x) getSurveyStats(x, aggForm, dat_n)))
aggStats2 <- bind_rows(future_lapply(unname(colVars), function(x) getSurveyStats(x, ~V0071 + RV0005, dat_n2)))

rename_outcomes <- function(x) names(colVars)[colVars == x]
aggStats$health_outcome <- unname(sapply(aggStats$health_outcome, rename_outcomes))
aggStats2$health_outcome <- unname(sapply(aggStats2$health_outcome, rename_outcomes))

colnames(aggStats)[1:length(aggVarsShort)] <- names(aggVarsShort)
colnames(aggStats2)[1:2] <- c("System", "sex")

library(stringr)
levels(aggStats$ageCat) <- unname(sapply(levels(aggStats$ageCat), 
                                             function(x) strsplit(x, "= ")[[1]][2]))
levels(aggStats$sex) <- unname(sapply(levels(aggStats$sex), 
                                             function(x) strsplit(x, "= ")[[1]][2]))
levels(aggStats$raceEthnicity) <- unname(sapply(levels(aggStats$raceEthnicity), 
                                          function(x) {
                                            if(grepl("=",x)) strsplit(x, "= ")[[1]][2]
                                            else x
                                          }))
aggStats$raceEthnicity <- factor(aggStats$raceEthnicity, 
                                     levels = c("Hispanic","Black (NH)","White (NH)","Multiracial, other, or missing"))

levels(aggStats2$System) <- unname(sapply(levels(aggStats2$System), 
                                  function(x) strsplit(x, "= ")[[1]][2]))
levels(aggStats2$sex) <- unname(sapply(levels(aggStats2$sex), 
                                      function(x) strsplit(x, "= ")[[1]][2]))

#would not recommend using aggregate stats for public assistance and homelessness data for jails 
saveCrossPlots <- function(plotVars, filename){
  g <- ggplot(aggStats %>% 
                filter(health_outcome %in% plotVars) %>%
                mutate(health_outcome = factor(health_outcome, levels = plotVars)),
              aes(x = ageCat, 
                  y = p, ymin = p.lwr, ymax = p.upr,
                  color = sex, fill = sex, group = sex)) +
    geom_line() +
    scale_y_continuous("Population (%)", labels = scales::percent, expand = c(0,0),
                       limits = c(0, NA)) +
    scale_x_discrete("Age", guide = guide_axis(angle = 45)) +
    geom_ribbon(alpha = 0.5) +
    theme_bw() + 
    facet_grid(health_outcome ~ raceEthnicity, scales = "free")
  
  ggsave(filename, g, width = 10, height = 10)
}

plotVars <- names(colVars)

savedir = file.path(here::here(), "preprocess","figs")
saveCrossPlots(plotVars[1:6], file.path(savedir,"med1.pdf"))
saveCrossPlots(plotVars[7:12], file.path(savedir,"med2.pdf"))
saveCrossPlots(plotVars[13:18], file.path(savedir,"med3.pdf"))

aggStats_systems <- aggStats2 %>% 
  mutate(System2 = recode(System,
                         `State correctional authorities such as the state department of corrections` = "State prison",
                         `Local correctional authorities such as local jails or detention centers` = "Jail",
                         `Federal Bureau of Prisons` = "Federal prison")) %>%
  filter(System2 %in% c("State prison","Jail","Federal prison"))

g <- ggplot(aggStats_systems %>% mutate(health_outcome = factor(health_outcome, levels = plotVars)),
            aes(x = System2, color = sex, group = interaction(System2, sex),
                y = p, ymin = p.lwr, ymax = p.upr)) +
  geom_point(position = position_dodge(width=0.5)) +
  geom_errorbar(width = 0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous("Population (%)", labels = scales::percent, expand = c(0,0.1),
                     limits = c(0, NA)) +
  scale_x_discrete("Correctional system", guide = guide_axis(angle = 45)) +
  theme_bw() + 
  facet_wrap( ~ health_outcome, scales = "free")

ggsave(file.path(savedir,"med4.pdf"), g, width = 10, height = 10)

aggStats <- aggStats %>%
  relocate(health_outcome, .after = last_col())

write_csv(aggStats, file = file.path(here::here(), "data","healthOutcomes_bySexRaceAge.csv"))
write_csv(aggStats_systems, file = file.path(here::here(), "data","healthOutcomes_bySystemSex.csv"))


