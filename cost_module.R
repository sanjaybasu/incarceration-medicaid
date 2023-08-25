{r}
# Import the cost details
cost_parameters <- read.csv("cost_parameters.csv")

# Function to estimate costs
estimate_costs <- function(population_with_outcomes, cost_parameters) {
  
  # Using dplyr to join and calculate costs
  population_with_costs <- population_with_outcomes %>%
    left_join(cost_parameters) %>%
    mutate(
      ER_visit_cost = ER_visit_probability * ER_event_cost,
      hospitalization_cost = hospital_visit_probability * avg_hospital_stay * hospital_day_cost,
      outpatient_visit_cost = outpatient_visit_probability * outpatient_event_cost, # Added this line
      incarceration_cost = recidivism_probability * avg_incarceration_stay * incarceration_day_cost,
      total_cost = ER_visit_cost + hospitalization_cost + outpatient_visit_cost + incarceration_cost, 
      # Apply 3% annual discount rate
      discounted_cost = total_cost / (1 + 0.03)^year
    )

  return(population_with_costs)
}

# Estimate costs over 10 years
population_with_costs <- estimate_costs(population_with_outcomes, cost_parameters)
