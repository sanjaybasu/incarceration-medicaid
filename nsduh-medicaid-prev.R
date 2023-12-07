library(tidyverse)
library(haven)
library(survey)

load("~/Downloads/NSDUH_2021.RData")                                                                

medicaid_data <- PUF2021_100622 %>% 
  mutate(
    inAnalysis = (caidchip == 1),
    Gender = factor(irsex, labels = c("Men", "Women")),
    Age.Group = factor(c(1, 2, 2, 3, 4, 5)[CATAG6], labels = c("Under 18", "19-34", "35-49", "50-64", "65 and over")),
    Race = factor(c(3, 1, 4, 4, 4, 4, 2)[NEWRACE2], labels = c('NH Black', 'Hispanic', 'NH White', 'Other'))
  )

nsduh_all <- svydesign(data = medicaid_data, id = ~QUESTID2, strata = ~VESTR_C, weights = ~ANALWT_C, nest = TRUE)
nsduh <- subset(nsduh_all, inAnalysis)

# Calculate AMI and SMI by Gender, Age Group, and Race using svyby
ami_results <- svyby(~amipy, ~Gender + Age.Group + Race, nsduh, svymean, na.rm = TRUE)
smi_results <- svyby(~smipy, ~Gender + Age.Group + Race, nsduh, svymean, na.rm = TRUE)

print(ami_results)
print(smi_results)

# Export results to CSV
write.csv(ami_results, "NSDUH_AMI_Results.csv", row.names = FALSE)
write.csv(smi_results, "NSDUH_SMI_Results.csv", row.names = FALSE)
