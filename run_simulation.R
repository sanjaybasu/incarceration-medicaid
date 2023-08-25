{r}


# Run the function
results <- simulate_intervention_and_icers(population_with_costs, intervention_impact)

# Visualizing the impact by age group, sex, and race/ethnicity
ggplot(results, aes(x = age_group, y = mean_cost, fill = race_ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(sex ~ .) +
  labs(title = "Impact of Interventions by Age Group, Sex, and Race/Ethnicity", y = "Mean Cost") +
  theme_minimal()


# Sample structure for `simulation_results`
# simulation_results <- data.frame(
#   race_ethnicity = c(...),
#   population = c(...),
#   outcome1 = c(...),
#   outcome2 = c(...),
#   ...
# )

calculate_sii_rii <- function(data, outcome_name) {
  # Calculate cumulative relative rank
  data <- data %>%
    arrange(desc(!!sym(outcome_name))) %>%
    mutate(
      cum_prop = cumsum(population) / sum(population),
      midrank = cum_prop - (population / 2 / sum(population))
    )

  # Run weighted regression to calculate SII
  model_sii <- lm(as.formula(paste(outcome_name, "~ midrank")), data = data, weights = data$population)
  sii <- coef(model_sii)[2]

  # Run weighted regression to calculate RII
  model_rii <- lm(as.formula(paste("log(", outcome_name, ") ~ midrank")), data = data, weights = data$population)
  rii <- exp(coef(model_rii)[2])

  return(list(sii = sii, rii = rii))
}

# Apply the function to each outcome in the dataset
outcomes <- names(simulation_results)[!(names(simulation_results) %in% c("race_ethnicity", "population"))]

results <- lapply(outcomes, function(outcome) {
  calculate_sii_rii(simulation_results, outcome)
})

# Print the results
names(results) <- outcomes
results

