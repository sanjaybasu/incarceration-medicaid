library(here)
library(tidyverse)
#Download data from https://www.icpsr.umich.edu/web/NACJD/studies/37692
#Extract data in ../data/raw
#Code below reflects downloading the data using the "R" version

# Prison releases data only provides information on age groups (18-24, 25-34, 35-44, 45-54, 55+)
## to align age groups with NHANES survey data, we use the age x sex x race/ethnicity distribution from the Survey of Prison Inmates, which collects age in years.

data_dir = file.path(here::here(), "data","raw","ICPSR_37692")

aggVars <- c(
  age = "RV0001",
  sex = "RV0005",
  raceEthnicity6 = "RV0003"
  # state = "V0772" #Not using State because of small sample sizes
)

load(file.path(data_dir,"DS0001","37692-0001-Data.rda")) # State and Fed prisons

spiData <- da37692.0001 %>% 
  mutate_at(all_of(aggVars),
            .funs = ~recode_factor(., 
                                   `(-9) -9 = Response missing due to CAPI issue` = NA_character_,
                                   `(-8) -8 = Skipped` = NA_character_,
                                   `(-2) -2 = Refusal` = NA_character_,
                                   `(-1) -1 = Don't Know` = NA_character_,
                                   `(98) 98 = DK/REF` = NA_character_,
                                   `(99) 99 = Blank` = NA_character_
            )
  )

ageVars = unname(unlist(aggVars["age"]))
spiData <- spiData %>%
  mutate_at(all_of(ageVars),
            .funs = ~if_else(V0012 == "(1) 1 = Yes", ., NA)
  ) %>%
  mutate(raceEthnicity = recode_factor(RV0003B,
                                       `(4) 4 = Other (NH), single race` = "Multiracial, other, or missing",
                                       `(5) 5 = 2+ Races (NH)` = "Multiracial, other, or missing",
                                       `(9) 9 = Uncategorized - Missing` = "Multiracial, other, or missing"
  ),
  ageCat1 = cut(RV0001, c(0, 17.5, 24.5, 34.5, 44.5, 54.5, 100), labels = c("<18","18-24","25-34","35-44","45-54","55+")),
  ageCat2 = cut(RV0001, c(0, 17.5, 34.5, 49.5, 64.5, 100), labels = c("<18","18-34","35-49","50-64","65+")),
  ageA = case_when(
    ageCat1 == "45-54" & ageCat2 == "50-64" ~ 1,
    ageCat1 == "55+" & ageCat2 == "65+" ~ 1,
    T ~ 0
  )
  )

#include survey replicates
library(survey)

repwts <- paste0("V", 1586:1949)
spi_svy <- svrepdesign(
  data = spiData, 
  weights = ~V1585, 
  repweights = spiData[, repwts], 
  type = "JK1",
  scale = 1,
  rscales = rep(c(.944, .978, .917, .667, .986, .994, .966, .75, .95, .5),
                times = c(18,   46,   12,    3,   70,  160,   29,   4,  20,  2)
  ) )

byvars <- c(aggVars[["sex"]], "raceEthnicity","ageCat1")
aggForm = reformulate(byvars)

df.raw <- svyby(~ageA, 
            by=aggForm, 
            design = subset(spi_svy, RV0001 > 44), 
            FUN = svyciprop, 
            vartype="ci",
            method="beta",
            keep.var = T,
            keep.names = F
)  %>%
  as.data.frame() %>%
  rename(p.lwr = ci_l, p.upr = ci_u, p = ageA)

mod.ni <- svyglm(reformulate(c(0, "RV0005 + raceEthnicity * ageCat1"), response = "ageA"), 
            design = subset(spi_svy, RV0001 > 44), 
            family=quasibinomial())

xterms <- labels(terms(mod.ni$terms))
x2terms <- setdiff(xterms, names(mod.ni$xlevels))
x2terms <- x2terms[!grepl(":", x2terms)]
x2levels <- lapply(x2terms, function(x) unique(design$variables[[x]]))
names(x2levels) <- x2terms

df.combs <- expand.grid(c(x2levels, mod.ni$xlevels))
y.pred <- predict(mod.ni, newdata = df.combs, se.fit = T, type = "link") %>% as.data.frame()
mod.invlink = mod.ni$family$linkinv
df.pred <- list(pred = mod.invlink(y.pred$link),
                pred.upr = mod.invlink(y.pred$link + (qnorm(0.975) * y.pred$SE)),
                pred.lwr = mod.invlink(y.pred$link - (qnorm(0.975) * y.pred$SE)),
                pred.se = y.pred$SE
) %>% as.data.frame() %>% bind_cols(df.combs)

df.long <- df.raw %>%
  mutate(cohort = "raw") %>%
  bind_rows(df.pred %>% rename(p = pred, p.lwr = pred.lwr, p.upr = pred.upr) %>% mutate(cohort = "fit"))

ggplot(df.long, 
       aes(x = raceEthnicity, y = p, ymin = p.lwr, ymax = p.upr, color = cohort, group = cohort)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.2) + facet_wrap(ageCat1~RV0005)

df <- df.raw %>% left_join(df.pred)

library(stringr)
levels(df$RV0005) <- unname(sapply(levels(df$RV0005), 
                                      function(x) strsplit(x, "= ")[[1]][2]))
levels(df$raceEthnicity) <- unname(sapply(levels(df$raceEthnicity), 
                                                function(x) {
                                                  if(grepl("=",x)) strsplit(x, "= ")[[1]][2]
                                                  else x
                                                }))
df$raceEthnicity <- factor(df$raceEthnicity, 
                                 levels = c("Hispanic","Black (NH)","White (NH)","Multiracial, other, or missing"))


write_csv(df, file = file.path(here::here(), "data", "prison_age_split.csv"))

