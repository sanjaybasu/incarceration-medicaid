{r}
# Import the population details and disease probabilities
population_details <- read.csv("population_details_csv.csv")
disease_probabilities <- read.csv("disease_probabilities_csv.csv")

# Function to get disease probability based on person's characteristics
get_disease_probability <- function(age, sex, race_ethnicity, disease) {
  row <- subset(disease_probabilities, age == age & sex == sex & race_ethnicity == race_ethnicity)
  return(row[[disease]])
}

# Function to simulate population based on the provided details
simulate_population <- function(population_details) {
  simulated_population <- list()
  
  for(i in 1:nrow(population_details)) {
    state <- population_details$state[i]
    age_range <- strsplit(as.character(population_details$age_range[i]), "-")[[1]]
    min_age <- as.numeric(age_range[1])
    max_age <- as.numeric(age_range[2])
    sampled_age <- sample(min_age:max_age, 1) # Randomly sample an age from the age range
    sex <- population_details$sex[i]
    race_ethnicity <- population_details$race_ethnicity[i]
    count <- population_details$count[i]
    
    df <- data.frame(
      id = seq(1, count),
      state = rep(state, count),
      age = rep(sampled_age, count),  # Use the sampled age
      sex = rep(sex, count),
      race_ethnicity = rep(race_ethnicity, count),
      
      cardiovascular_disease = sample(c(TRUE, FALSE), count, replace = TRUE, 
                                      prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "cardiovascular_disease"), 
                                               1 - get_disease_probability(sampled_age, sex, race_ethnicity, "cardiovascular_disease"))),
      
      diabetes = sample(c(TRUE, FALSE), count, replace = TRUE, 
                        prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "diabetes"), 
                                 1 - get_disease_probability(sampled_age, sex, race_ethnicity, "diabetes"))),
      
      heart_failure = sample(c(TRUE, FALSE), count, replace = TRUE, 
                             prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "heart_failure"), 
                                      1 - get_disease_probability(sampled_age, sex, race_ethnicity, "heart_failure"))),
      
      kidney_disease = sample(c(TRUE, FALSE), count, replace = TRUE, 
                              prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "kidney_disease"), 
                                       1 - get_disease_probability(sampled_age, sex, race_ethnicity, "kidney_disease"))),
      
      asthma_COPD = sample(c(TRUE, FALSE), count, replace = TRUE, 
                           prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "asthma_COPD"), 
                                    1 - get_disease_probability(sampled_age, sex, race_ethnicity, "asthma_COPD"))),
      
      mental_illness = sample(c(TRUE, FALSE), count, replace = TRUE, 
                              prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "mental_illness"), 
                                       1 - get_disease_probability(sampled_age, sex, race_ethnicity, "mental_illness"))),
      
      substance_use = sample(c(TRUE, FALSE), count, replace = TRUE, 
                             prob = c(get_disease_probability(sampled_age, sex, race_ethnicity, "substance_use"), 
                                      1 - get_disease_probability(sampled_age, sex, race_ethnicity, "substance_use")))
    )
    
    simulated_population[[i]] <- df
  }
  
  return(bind_rows(simulated_population))
}

# Simulate the population
simulated_population <- simulate_population(population_details)

