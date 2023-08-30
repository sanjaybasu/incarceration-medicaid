{r}
library(dplyr)
library(ggplot2)

simulate_intervention_and_icers <- function(population_with_costs, intervention_impact) {
  
  # Apply interventions and calculate post-intervention costs and QALYs
  population_post_intervention <- population_with_costs %>%
    # Modify the event-to-visit probabilities and average lengths of stay based on the intervention
    mutate(
      ER_visit_probability = ER_visit_probability * (1 - intervention_impact$ER_visit_reduction),
      hospital_visit_probability = hospital_visit_probability * (1 - intervention_impact$hospital_visit_reduction),
      recidivism_probability = recidivism_probability * (1 - intervention_impact$recidivism_reduction),
      avg_hospital_stay = avg_hospital_stay * (1 + intervention_impact$hospital_stay_adjustment),
      avg_incarceration_stay = avg_incarceration_stay * (1 + intervention_impact$incarceration_stay_adjustment)
    ) %>%
    # Recalculate costs using the adjusted probabilities and lengths of stay
    mutate(
      ER_visit_cost = ER_visit_probability * ER_event_cost,
      hospitalization_cost = hospital_visit_probability * avg_hospital_stay * hospital_day_cost,
      incarceration_cost = recidivism_probability * avg_incarceration_stay * incarceration_day_cost,
      total_cost = ER_visit_cost + hospitalization_cost + incarceration_cost + cardiovascular_event * cardiovascular_event_cost + diabetes_event * diabetes_event_cost + heart_failure_event * heart_failure_event_cost + kidney_event * kidney_event_cost + asthma_COPD_event * asthma_COPD_event_cost + mental_health_event * mental_health_event_cost + overdoses * overdose_event_cost + deaths * death_event_cost
    )
  
  # Aggregating results by subgroup
  aggregated_results <- population_post_intervention %>%
    group_by(state, age_group, sex, race_ethnicity) %>%
    summarise(
      mean_cost = mean(total_cost),
      mean_qaly = mean(qaly),
      mean_outpatient = mean(predicted_outpatient_visits),
      mean_ER = mean(predicted_ER_visits),
      mean_hospitalizations = mean(predicted_hospitalizations),
      mean_deaths = mean(deaths),
      n = n(),
      sd_cost = sd(total_cost),
      sd_qaly = sd(qaly)
    ) %>%
    mutate(
      lower_CI_cost = mean_cost - 1.96 * (sd_cost / sqrt(n)),
      upper_CI_cost = mean_cost + 1.96 * (sd_cost / sqrt(n)),
      lower_CI_qaly = mean_qaly - 1.96 * (sd_qaly / sqrt(n)),
      upper_CI_qaly = mean_qaly + 1.96 * (sd_qaly / sqrt(n))
    )
  
  return(aggregated_results)
}
