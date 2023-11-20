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
  df.c <- svyby(varformula, byformula, design, unwtd.count,na.rm.all=T , na.rm.by=T, keep.names = F, na.rm=TRUE) %>% as.data.frame() %>% 
    select(-se) %>% rename(n = counts)
  df.p <- svyby(varformula, byformula, design, 
             FUN = svyciprop, 
             vartype="ci",
             method="beta",
             keep.var = T,
             keep.names = F, 
             na.rm.all=T , na.rm.by=T, na.rm=TRUE)  %>% as.data.frame() %>%
    rename(p.lwr = ci_l, p.upr = ci_u)
  df.p$p <- df.p[[as.character(varformula)[2]]]
  df.p[[as.character(varformula)[2]]] <- NULL
  df.p$health_outcome <- as.character(varformula)[2]
  
  vars.g <- labels(terms(byformula))
  df.v <- svyby(varformula, byformula, design, 
             FUN = svymean, 
             vartype="se",
             method="beta",
             keep.var = T,
             keep.names = F, 
             na.rm.all=T , na.rm.by=T, na.rm=TRUE)  %>% as.data.frame() %>%
    select(!!vars.g, tail(names(.), 1))
  var.se <- tail(colnames(df.v), 1)
  df.v$se <- df.v[[var.se]]
  df.v[[var.se]] <- NULL
  
  outSum <- left_join(df.c, df.p) %>% left_join(df.v)
  return(outSum)
}

health_outcomes <- c("cvd","str","htn","dm","ast","ckd","hbv","hcv","dep")
groups <- c("one","Gender","Age.Group","Race","Gender + Age.Group + Race")

for(g in groups){
  df <- bind_rows(lapply(health_outcomes, 
                         function(x) getSummary(reformulate(x), reformulate(g), NHANES)))
  write_csv(df, file.path(here::here(), "data",paste0("nhanes_by", gsub("\\.|\\+| ","",g), ".csv")))
}



