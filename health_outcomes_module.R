{r}
event_probabilities <- read.csv("event_probabilities.csv")

# Function to simulate health outcomes over a 10-year duration
simulate_health_outcomes <- function(simulated_population, event_probabilities) {
  
  # Define QALY weights for different health states
  qaly_weights <- list(
    cardiovascular = 0.9,
    diabetes = 0.85,
    heart_failure = 0.8,
    kidney = 0.85,
    asthma_COPD = 0.8,
    mental_health = 0.75,
    overdose = 0.7,
    death = 0
  )
  
  # Loop through each year
  for(year in 1:10) {
    # Loop through each individual in the population
    for(i in 1:nrow(simulated_population)) {
      individual <- simulated_population[i, ]
      
      # Age the individual by one year
      individual$age <- individual$age + 1
      
      # Extract attributes
      age <- individual$age
      sex <- individual$sex
      race_ethnicity <- individual$race_ethnicity
      
      # Get corresponding probabilities from the CSV
      probabilities <- event_probabilities %>%
        filter(age == !!age, sex == !!sex, race_ethnicity == !!race_ethnicity)
      
      # Simulate if individual dies within that year based on the mortality risk
      if(rbinom(1, 1, probabilities$death_event_subsequent_months) == 1) {
        simulated_population$age_at_death[i] <- individual$age
        simulated_population$qaly[i] <- qaly_weights$death * (1 / (1 + 0.03)^year)
        next # Skip further processing for this individual if they died
      }

      # Initialize QALY for the individual for this year
      qaly_for_year <- 1 

      # Define a helper function to simulate event and update QALY
      simulate_event_and_update_qaly <- function(event_name, condition) {
        if (condition) {
          event_occurred <- rbinom(1, 1, probabilities[[paste(event_name, event_suffix, sep="_")]])
          if (event_occurred == 1) {
            qaly_for_year <- qaly_for_year * qaly_weights[[event_name]]
          }
          return(event_occurred)
        }
        return(0)
      }

      # Cardiovascular events
      simulated_population$cardiovascular_event[i] <- simulate_event_and_update_qaly("cardiovascular", individual$cardiovascular_disease)
      # Diabetes events
      simulated_population$diabetes_event[i] <- simulate_event_and_update_qaly("diabetes", individual$diabetes)
      # Heart failure events
      simulated_population$heart_failure_event[i] <- simulate_event_and_update_qaly("heart_failure", individual$heart_failure)
      # Kidney events
      simulated_population$kidney_event[i] <- simulate_event_and_update_qaly("kidney", individual$kidney_disease)
      # Asthma/COPD events
      simulated_population$asthma_COPD_event[i] <- simulate_event_and_update_qaly("asthma_COPD", individual$asthma_COPD)
      # Mental health events
      simulated_population$mental_health_event[i] <- simulate_event_and_update_qaly("mental_health", individual$mental_illness)
      # Overdoses
      simulated_population$overdoses[i] <- simulate_event_and_update_qaly("overdose", individual$substance_use)
      # Simulate recidivism event (assuming it's independent of health states)
      simulated_population$recidivism_event[i] <- rbinom(1, 1, probabilities[[paste("recidivism_event", event_suffix, sep="_")]])

      # Update QALY for the individual for this year after all events, adjusted for discounting
      simulated_population$qaly[i] <- simulated_population$qaly[i] + qaly_for_year * (1 / (1 + 0.03)^year)
    }
  }
  
  return(simulated_population)
}

# Simulate health outcomes over 10 years
population_with_outcomes <- simulate_health_outcomes(simulated_population, event_probabilities)
