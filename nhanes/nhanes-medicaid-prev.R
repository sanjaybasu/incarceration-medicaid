# Load necessary libraries
library(nhanesA)
library(survey)
library(tidyverse)

# Load the demographic data to obtain the survey weights
demo_data <- nhanes("P_DEMO")

# Load health insurance data to identify those with Medicaid
hiq_data <- nhanes("P_HIQ")

# Hypertension
bpq_data <- nhanes("P_BPQ")

# Diabetes
diq_data <- nhanes("P_DIQ")

# Asthma
mcq_data <- nhanes("P_MCQ")

# Chronic Kidney Disease
kiq_data <- nhanes("P_KIQ_U")

# Hepatitis 
heq_data <- nhanes("P_HEQ")

# Depression 
dpq_data <- nhanes("P_DPQ")

# Alcohol Binge Drinking
alq_data <- nhanes("P_ALQ")


One <- left_join(demo_data, hiq_data, by="SEQN") %>%
  left_join(bpq_data, by="SEQN") %>%
  left_join(diq_data, by="SEQN") %>%
  left_join(mcq_data, by="SEQN") %>%
  left_join(kiq_data, by="SEQN") %>%
  left_join(heq_data, by="SEQN") %>%
  left_join(dpq_data, by="SEQN") %>%
  left_join(alq_data, by="SEQN") %>%
# Define indicator for analysis population of interest: adults aged 20 and over with a valid depression score
  mutate(# create indicator for overall summary
        one = 1,
        inAnalysis= (HIQ032D == 4 & !is.na(WTINTPRP)),
         Gender = factor(RIAGENDR, labels=c("Men", "Women")),
         Age.Group = cut(RIDAGEYR, breaks=c(-Inf,17,25,35,45,55,65,Inf),labels=c("Under 18", "18-24","25-34","35-44","45-54","55-64","65 and over")),
         Race = factor(c(2, 2, 3, 1, 4, 4, 4)[RIDRETH3], labels = c('NH Black','Hispanic', 'NH White', 'Other')),
        cvd = (MCQ160B==1) | (MCQ160C==1) | (MCQ160D==1) | (MCQ160E==1),
        str = (MCQ160F==1),
        htn = (BPQ020==1),
        dm = (DIQ010==1),
        ast = (MCQ035==1),
        ckd = (KIQ022==1),
        hbv = (HEQ010==1),
        hcv = (HEQ030==1),
        phq9 = rowSums(select(.,paste0("DPQ0", 1:9, "0")), na.rm=TRUE),
        dep = (phq9>=5)
        )


#' ## Define survey design 
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTINTPRP, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score 
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

#' ## Analysis
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count,na.rm.all=T , na.rm.by=T, na.rm=TRUE) 
  p <- svyby(varformula, byformula, design, svymean, na.rm.all=T , na.rm.by=T, na.rm=TRUE) 
  outSum <- left_join(select(c,-se), p) 
  outSum
}


#' ### Calculate prevalence overall, by gender, by age group, and by age and gender
# Overall
cvd_overall=write_csv(getSummary(~cvd, ~one, NHANES),"cvd_overall.csv")
#' By sex
cvd_sex=write_csv(getSummary(~cvd, ~Gender, NHANES),"cvd_sex.csv")
#' By age
cvd_age=write_csv(getSummary(~cvd, ~Age.Group, NHANES),"cvd_age.csv")
#' By race
cvd_race=write_csv(getSummary(~cvd, ~Race, NHANES),"cvd_race.csv")
#' By sex and age and race
cvd_all=write_csv(getSummary(~cvd, ~Gender + Age.Group + Race, NHANES),"cvd_all.csv")



#' ### Calculate prevalence overall, by gender, by age group, and by age and gender
# Overall
str_overall=write_csv(getSummary(~str, ~one, NHANES),"str_overall.csv")
#' By sex
str_sex=write_csv(getSummary(~str, ~Gender, NHANES),"str_sex.csv")
#' By age
str_age=write_csv(getSummary(~str, ~Age.Group, NHANES),"str_age.csv")
#' By race
str_race=write_csv(getSummary(~str, ~Race, NHANES),"str_race.csv")
#' By sex and age and race
str_all=write_csv(getSummary(~str, ~Gender + Age.Group + Race, NHANES),"str_all.csv")



#' ### Calculate prevalence overall, by gender, by age group, and by age and gender
# Overall
htn_overall=write_csv(getSummary(~htn, ~one, NHANES),"htn_overall.csv")
#' By sex
htn_sex=write_csv(getSummary(~htn, ~Gender, NHANES),"htn_sex.csv")
#' By age
htn_age=write_csv(getSummary(~htn, ~Age.Group, NHANES),"htn_age.csv")
#' By race
htn_race=write_csv(getSummary(~htn, ~Race, NHANES),"htn_race.csv")
#' By sex and age and race
htn_all=write_csv(getSummary(~htn, ~Gender + Age.Group + Race, NHANES),"htn_all.csv")


# Overall
dm_overall=write_csv(getSummary(~dm, ~one, NHANES),"dm_overall.csv")
#' By sex
dm_sex=write_csv(getSummary(~dm, ~Gender, NHANES),"dm_sex.csv")
#' By age
dm_age=write_csv(getSummary(~dm, ~Age.Group, NHANES),"dm_age.csv")
#' By race
dm_race=write_csv(getSummary(~dm, ~Race, NHANES),"dm_race.csv")
#' By sex and age and race
dm_all=write_csv(getSummary(~dm, ~Gender + Age.Group + Race, NHANES),"dm_all.csv")


# Overall
ast_overall=write_csv(getSummary(~ast, ~one, NHANES),"ast_overall.csv")
#' By sex
ast_sex=write_csv(getSummary(~ast, ~Gender, NHANES),"ast_sex.csv")
#' By age
ast_age=write_csv(getSummary(~ast, ~Age.Group, NHANES),"ast_age.csv")
#' By race
ast_race=write_csv(getSummary(~ast, ~Race, NHANES),"ast_race.csv")
#' By sex and age and race
ast_all=write_csv(getSummary(~ast, ~Gender + Age.Group + Race, NHANES),"ast_all.csv")



# Overall
ckd_overall=write_csv(getSummary(~ckd, ~one, NHANES),"ckd_overall.csv")
#' By sex
ast_sex=write_csv(getSummary(~ckd, ~Gender, NHANES),"ckd_sex.csv")
#' By age
ast_age=write_csv(getSummary(~ckd, ~Age.Group, NHANES),"ckd_age.csv")
#' By race
ast_race=write_csv(getSummary(~ckd, ~Race, NHANES),"ckd_race.csv")
#' By sex and age and race
ast_all=write_csv(getSummary(~ckd, ~Gender + Age.Group + Race, NHANES),"ckd_overall.csv")



# Overall
hbv_overall=write_csv(getSummary(~hbv, ~one, NHANES),"hbv_overall.csv")
#' By sex
hbv_sex=write_csv(getSummary(~hbv, ~Gender, NHANES),"hbv_sex.csv")
#' By age
hbv_age=write_csv(getSummary(~hbv, ~Age.Group, NHANES),"hbv_age.csv")
#' By race
hbv_race=write_csv(getSummary(~hbv, ~Race, NHANES),"hbv_race.csv")
#' By sex and age and race
hbv_all=write_csv(getSummary(~hbv, ~Gender + Age.Group + Race, NHANES),"hbv_all.csv")



# Overall
hcv_overall=write_csv(getSummary(~hcv, ~one, NHANES),"hcv_overall.csv")
#' By sex
hcv_sex=write_csv(getSummary(~hcv, ~Gender, NHANES),"hcv_sex.csv")
#' By age
hcv_age=write_csv(getSummary(~hcv, ~Age.Group, NHANES),"hcv_age.csv")
#' By race
hcv_race=write_csv(getSummary(~hcv, ~Race, NHANES),"hcv_race.csv")
#' By sex and age and race
hcv_all=write_csv(getSummary(~hcv, ~Gender + Age.Group + Race, NHANES),"hcv_all.csv")



# Overall
dep_overall=write_csv(getSummary(~dep, ~one, NHANES),"dep_overall.csv")
#' By sex
dep_sex=write_csv(getSummary(~dep, ~Gender, NHANES),"dep_sex.csv")
#' By age
dep_age=write_csv(getSummary(~dep, ~Age.Group, NHANES),"dep_age.csv")
#' By race
dep_race=write_csv(getSummary(~dep, ~Race, NHANES),"dep_race.csv")
#' By sex and age and race
dep_all=write_csv(getSummary(~dep, ~Gender + Age.Group + Race, NHANES),"dep_all.csv")




