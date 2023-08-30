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
  cvd_any = "V1229",
  cvd_current = "V1244",
  
  hypertension_any = "V1226" ,
  hypertension_current = "V1241",
  
  diabetes_any = "V1228",
  diabetes_current = "V1243",
  
  kidney_any = "V1230",
  kidney_current = "V1245",
  
  #note no copd
  asthma_any = "V1232",
  asthma_current = "V1247",
  
  cancer_any = "V1225",
  cancer_current = "V1240",
  
  stroke_any = "V1227",
  stroke_current = "V1242", #currently experiencing problems from
  
  liver_any = "V1233",
  liver_current = "V1248",
  
  autoimmune_any = "V1231",
  autoimmune_current = "V1246", #arthritis/gout/lupus/fibromyalgia
  
  hepB_any = "V1235",
  hepB_current = "V1249",
  
  hepC_any = "V1236",
  hepC_current = "V1250",
  
  hiv_any = "V1237",
  stdOther_any = "V1238",
  tb_any = "V1234",
  
  # health insurance related measures
  #   these reflect insurance status during the 30 days prior to arrest
  #   using their "upcoded" versions
  insurance_employer = "V1148",
  insurance_direct = "V1149", # purchased directly from insurance company
  insurance_other = "V1155",
  
  #Govt assistance measures
  publicAssistance_30d = "V1151", #on public assistance during 30 days prior to arrest
  publicAssistance_child = "V1170", # was on public assistance as child
  va_eligible = "V1164",
  
  #other
  homeless_12m = "V0961",
  homeless_30d = "V0958",
  homeless_child = "V1165"
)

#any 'yes' to: AUD1, any of AUD10-21
alcoholUseVars <- c("V1268",paste0("V", 1277:1288))

#any 'yes' to: DU1d or DU6d or DU8d or DU10d [prior history], with indications of dependence: any of DUD1, DUD2, DUD5, DUD6, or any of DUD11-22
opioidUseVars <- c("V1294","V1318","V1342", paste0("V135", c(0:1, 4:5)), paste0("V", 1360:1371))

#any 'yes' to: MH7
mentalHealthVars <- paste0("V", 1185:1191)

load(file.path(data_dir,"DS0001","37692-0001-Data.rda")) # State and Fed prisons

allVars <- unname(unlist(c(tail(aggVars,-1), analysisVars, alcoholUseVars, opioidUseVars, mentalHealthVars)))

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
                                       )
  )
levels(spiData[[aggVars$state]]) <- c(NA, NA, NA, tail(levels(spiData[[aggVars$state]]), -3))

#code mental health any
spiData$mhDiagnosis_any <- getAny(mentalHealthVars, spiData)

#code opioid use
spiData$opioidUse_any = getAny(opioidUseVars, spiData)

#code alcohol use
spiData$alcoholUse_any = getAny(alcoholUseVars, spiData)

colVars <- unlist(c(analysisVars, mhDiagnosis_any = "mhDiagnosis_any", opioidUse_any = "opioidUse_any", alcoholUse_any = "alcoholUse_any"))

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
aggVarsShort = c(aggVars[names(aggVars) %in%c("ageCat","sex")], raceEthnicity = "raceEthnicity")
aggForm = reformulate(aggVarsShort)
colForm = reformulate(colVars)

getSurveyStats <- function(cname){
  require(dplyr)
  require(survey)
  df <- svyby(reformulate(cname), 
                    by=aggForm, 
                    design = spi_svy, 
                    FUN = svyciprop, 
                    vartype="ci",
                    method="beta",
                    keep.names = F
  ) %>% as.data.frame() %>%
    mutate(health_outcome = cname) %>%
    rename(lwr = ci_l, upr = ci_u)
  df$mean = df[[cname]]
  df[[cname]] = NULL
  return(df)
}

require(parallel)
library(future.apply)
plan(multisession, workers = parallel::detectCores()-1) ## Parallelize using five cores
rm(spiData)
rm(da37692.0001)
rm(spiData2)

aggStats <- bind_rows(future_lapply(unname(colVars), getSurveyStats))

rename_outcomes <- function(x) names(colVars)[colVars == x]
aggStats$health_outcome <- unname(sapply(aggStats$health_outcome, rename_outcomes))

colnames(aggStats)[1:length(aggVarsShort)] <- names(aggVarsShort)

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

#would not recommend using aggregate stats for public assistance and homelessness data for jails 
saveCrossPlots <- function(plotVars, filename){
  g <- ggplot(aggStats %>% 
                filter(health_outcome %in% plotVars) %>%
                mutate(health_outcome = factor(health_outcome, levels = plotVars)),
              aes(x = ageCat, 
                  y = mean, ymin = lwr, ymax = upr,
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

plotVars <- names(colVars)[!grepl("_current", names(colVars))]
plotVars <- c(plotVars[1:5], tail(plotVars, 3), head(tail(plotVars, -5), -3))

savedir = file.path(here::here(), "preprocess","figs")
saveCrossPlots(plotVars[1:6], file.path(savedir,"med1.pdf"))
saveCrossPlots(plotVars[7:12], file.path(savedir,"med2.pdf"))
saveCrossPlots(plotVars[13:18], file.path(savedir,"med3.pdf"))
saveCrossPlots(plotVars[19:26], file.path(savedir,"med4.pdf"))

aggStats <- aggStats %>%
  relocate(health_outcome, .after = last_col())

write_csv(aggStats, file.path(here::here(), "data","healthOutcomes_bySexRaceAge.csv"))


