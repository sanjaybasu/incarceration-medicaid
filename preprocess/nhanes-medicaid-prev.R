# Load necessary libraries
library(nhanesA)
library(survey)
library(tidyverse)
library(tidyr)
library(jtools)

# Load the demographic data to obtain the survey weights
demo_data <- nhanes("P_DEMO", cleanse_numeric = T)

# Load health insurance data to identify those with Medicaid
hiq_data <- nhanes("P_HIQ", cleanse_numeric = T)

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
dpq_data <- dpq_data %>%
  mutate_at(vars(matches("^DPQ0[1-9]0$", perl = T)), ~(ifelse(as.integer(.) > 4.5, NA, as.integer(.))-1))

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
        inAnalysis= (HIQ032D == 4 & !is.na(WTINTPRP)),
        Gender = RIAGENDR,
        cvd = (MCQ160B=="Yes") | (MCQ160C=="Yes") | (MCQ160D=="Yes") | (MCQ160E=="Yes"),
        str = (MCQ160F=="Yes"),
        htn = (BPQ020=="Yes"),
        dm = (DIQ010=="Yes"),
        ast = case_when(!is.na(MCQ010) & !is.na(MCQ035) & MCQ010 == "Yes" & MCQ035 == "Yes" ~ 1,
                        !is.na(MCQ010) ~ 0),
        ckd = (KIQ022=="Yes"),
        hbv = (HEQ010=="Yes"),
        hcv = (HEQ030=="Yes"),
        phq9 = rowSums(select(.,paste0("DPQ0", 1:9, "0")), na.rm=TRUE),
        dep = (phq9>=5)
        )


#' ## Define survey design 
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTINTPRP, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score 
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

NHANES_adults <- subset(NHANES, RIDAGEYR > 17)

# Age stats by sex
age.mean <- svyby(~RIDAGEYR, ~Gender, NHANES_adults, na.rm = TRUE, svymean)
print(age.mean)

### Analysis
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Fit data
  byvars <- labels(terms(byformula))
  varvars <- labels(terms(varformula))
  
  print(varvars)
  
  if(length(byvars) == 3){
    mod.ni <- svyglm(reformulate(c(0, "Gender * Age.Group + Race"), response = varvars), 
                     design=design, family=quasibinomial())
  } else {
    mod.ni <- svyglm(reformulate(c(0, paste0(byvars, collapse = "*")), response = varvars), 
                     design=design, family=quasibinomial())
  }

  xterms <- labels(terms(mod.ni$terms))
  x2terms <- setdiff(xterms, names(mod.ni$xlevels))
  x2terms <- x2terms[!grepl(":", x2terms)]
  x2levels <- lapply(x2terms, function(x) unique(design$variables[[x]]))
  names(x2levels) <- x2terms
  
  df.combs <- expand.grid(c(x2levels, mod.ni$xlevels))
  # df.combs <- design$variables %>% tidyr::expand(!!!syms(byvars))
  
  SE.svystat<-function(object,...){
    v<-vcov(object)
    if (!is.matrix(v) || NCOL(v)==1) sqrt(v) else sqrt(diag(v))
  }
  
  fit.pred <- predict(mod.ni, newdata = df.combs, se.fit = T, type = "link")
  y.pred <- as.data.frame(list(link = as.vector(fit.pred), SE = SE.svystat(fit.pred)))
  mod.invlink = mod.ni$family$linkinv
  df.pred <- list(pred = mod.invlink(y.pred$link),
                pred.upr = mod.invlink(y.pred$link + (qnorm(0.975) * y.pred$SE)),
                pred.lwr = mod.invlink(y.pred$link - (qnorm(0.975) * y.pred$SE)),
                pred.se = y.pred$SE
                ) %>% as.data.frame() %>% bind_cols(df.combs)
  
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
  if(var.se != "se") df.v[[var.se]] <- NULL
  
  outSum <- left_join(df.c, df.p) %>% left_join(df.v) %>% left_join(df.pred)
  return(outSum)
}

health_outcomes <- c("cvd","str","htn","dm","ast","ckd","hbv","hcv","dep")
groups <- c("Gender")

#18+
for(g in groups){
  df <- bind_rows(lapply(health_outcomes, 
                         function(x) getSummary(reformulate(x), reformulate(g), NHANES_adults)))
  write_csv(df, file.path(here::here(), "data",paste0("nhanes_by", gsub("\\.|\\+| ","",g), "Adults.csv")))
}

df <- read_csv(file = file.path(here::here(), "data",paste0("nhanes_byGenderAdults.csv")))
df.pred <- df %>%
  select(Gender, n, health_outcome, starts_with("pred")) %>%
  mutate(group = "predicted")
colnames(df.pred) <- gsub("^pred","p",colnames(df.pred))
df <- df %>%
  select(Gender, n, health_outcome, p, p.upr, p.lwr, se) %>%
  rename(p.se = se) %>%
  mutate(group = "raw") %>% 
  bind_rows(df.pred)

ggplot(df, 
       aes(x = health_outcome, y = p, ymax = p.upr, ymin = p.lwr, 
           group = group, fill = group, color = group)) +
  geom_point(position = position_dodge(width = 0.5)) + geom_errorbar(position = position_dodge(width = 0.5), width = 0.5) +
  facet_wrap(~Gender)

