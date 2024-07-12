library(tidyverse)
library(haven)
library(survey)

load(file.path(here::here(), "data","raw","NSDUH_2022.Rdata"))

medicaid_data <- NSDUH_2022 %>% 
  mutate(
    inAnalysis = (caidchip == 1 & AGE3 > 3.5),
    Gender = factor(irsex, labels = c("Men", "Women")),
    MPD = as.numeric(between(KSSLR6MON, 4.5, 12.5))
  )

print(table(medicaid_data$inAnalysis))
print(medicaid_data %>% filter(inAnalysis) %>% count(Gender))

nsduh_all <- svydesign(data = medicaid_data, id = ~QUESTID2, strata = ~VESTR_C, weights = ~ANALWT2_C, nest = TRUE)

nsduh <- subset(nsduh_all, inAnalysis)
nsduh_adults <- subset(nsduh, CATAG6 != 1)


spd_adults_results <- svyby(~spdpstmon, ~Gender, nsduh_adults, svymean, na.rm = TRUE)

mpd_adults_results <- svyby(~MPD, ~Gender, nsduh_adults, svymean, na.rm = TRUE)

df_adults_results <- bind_rows(
  spd_adults_results %>% rename(p = spdpstmon) %>% mutate(health_outcome = "SPD_month"),
  mpd_adults_results %>% rename(p = MPD) %>% mutate(health_outcome = "MPD_month")
)

# # Export results to CSV
write.csv(df_adults_results, file.path(here::here(), "data","NSDUH_Adults_Results.csv"), row.names = FALSE)
