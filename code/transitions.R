# transitions.R - Markov transition matrix generation and application

library(dplyr)
library(tidyr)

# Generate transition matrix for specific age group and gender
# Returns: matrix with transition probabilities
generate_transition_matrix <- function(params, age_group, gender) {
  states <- params$states
  n_states <- length(states)
  
  # Initialize transition matrix
  trans_mat <- matrix(0, nrow = n_states, ncol = n_states,
                      dimnames = list(from = states, to = states))
  
  # Death is absorbing state
  trans_mat["Death", "Death"] <- 1
  
  # Get age-specific mortality rate
  base_mortality <- params$mortality_rates[[age_group]]
  mortality_multipliers <- params$mortality_multipliers
  
  # MCI transitions (from base transition probabilities)
  trans_mat["MCI", "Undx_Mild"] <- params$transition_probs$mci_to_undx_mild
  trans_mat["MCI", "Other_Healthy"] <- params$transition_probs$mci_to_healthy
  trans_mat["MCI", "Death"] <- base_mortality * mortality_multipliers[["MCI"]]
  trans_mat["MCI", "MCI"] <- 1 - sum(trans_mat["MCI", ])
  
  # Healthy transitions (age-specific from parameters)
  trans_mat["Other_Healthy", "Undx_Mild"] <- params$transition_probs$healthy_to_undx_mild_by_age[age_group]
  trans_mat["Other_Healthy", "MCI"] <- params$transition_probs$healthy_to_mci_by_age[age_group]
  trans_mat["Other_Healthy", "Death"] <- base_mortality * mortality_multipliers[["Other_Healthy"]]
  trans_mat["Other_Healthy", "Other_Healthy"] <- 1 - sum(trans_mat["Other_Healthy", ])
  
  # Undiagnosed dementia transitions (from parameters which already include AI effect)
  trans_mat["Undx_Mild", "Dx_Mild"] <- params$transition_probs$undx_mild_to_dx_mild
  trans_mat["Undx_Moderate", "Dx_Moderate"] <- params$transition_probs$undx_moderate_to_dx_moderate
  trans_mat["Undx_Severe", "Dx_Severe"] <- params$transition_probs$undx_severe_to_dx_severe
  
  # Progression rates (from parameters which already include AI effect)
  trans_mat["Undx_Mild", "Undx_Moderate"] <- params$transition_probs$undx_mild_to_moderate
  trans_mat["Undx_Moderate", "Undx_Severe"] <- params$transition_probs$undx_moderate_to_severe
  
  # Add mortality for undiagnosed states
  for (state in c("Undx_Mild", "Undx_Moderate", "Undx_Severe")) {
    trans_mat[state, "Death"] <- base_mortality * mortality_multipliers[[state]]
  }
  
  # Diagnosed dementia transitions (from parameters which already include AI effect)
  trans_mat["Dx_Mild", "Dx_Moderate"] <- params$transition_probs$dx_mild_to_moderate
  trans_mat["Dx_Moderate", "Dx_Severe"] <- params$transition_probs$dx_moderate_to_severe
  
  # Add mortality for diagnosed states
  for (state in c("Dx_Mild", "Dx_Moderate", "Dx_Severe")) {
    trans_mat[state, "Death"] <- base_mortality * mortality_multipliers[[state]]
  }
  
  # Apply gender-specific adjustments
  if (gender == "Female" && !is.null(params$transition_probs$gender_adjustments$Female)) {
    # Get female adjustment factors from parameters
    female_adjustments <- params$transition_probs$gender_adjustments$Female
    
    # Apply adjustments from parameters
    trans_mat["MCI", "Undx_Mild"] <- trans_mat["MCI", "Undx_Mild"] * 
      female_adjustments$mci_to_undx_mild_multiplier
    trans_mat["Undx_Mild", "Undx_Moderate"] <- trans_mat["Undx_Mild", "Undx_Moderate"] * 
      female_adjustments$undx_mild_to_moderate_multiplier
    trans_mat["Dx_Mild", "Dx_Moderate"] <- trans_mat["Dx_Mild", "Dx_Moderate"] * 
      female_adjustments$dx_mild_to_moderate_multiplier
  }
  
  # Calculate stay probabilities (diagonal)
  for (state in states[states != "Death"]) {
    trans_mat[state, state] <- 1 - sum(trans_mat[state, -which(states == state)])
  }
  
  # Validate and normalize
  trans_mat <- validate_and_normalize_matrix(trans_mat)
  
  return(trans_mat)
}

# Validate transition matrix properties
# Returns: normalized matrix with rows summing to 1
validate_and_normalize_matrix <- function(trans_mat) {
  tolerance <- 1e-10
  
  # Check each row
  for (i in 1:nrow(trans_mat)) {
    row_sum <- sum(trans_mat[i, ])
    
    # If row doesn't sum to 1, normalize it
    if (abs(row_sum - 1) > tolerance) {
      if (row_sum > 0) {
        trans_mat[i, ] <- trans_mat[i, ] / row_sum
      } else {
        # If all zeros, make it a stay transition
        trans_mat[i, i] <- 1
      }
    }
    
    # Ensure no negative values
    trans_mat[i, ] <- pmax(trans_mat[i, ], 0)
  }
  
  # Ensure Death is absorbing
  trans_mat["Death", ] <- 0
  trans_mat["Death", "Death"] <- 1
  
  return(trans_mat)
}

# Apply transitions to cohort data frame
# Returns: updated cohort after transitions
apply_transitions <- function(cohort_df, params) {
  # Initialize result with same structure as input
  new_cohort <- cohort_df %>%
    mutate(
      new_population = 0,
      new_original_population = 0
    )
  
  # Get unique subgroups
  subgroups <- cohort_df %>%
    select(age_group, gender, area) %>%
    distinct()
  
  # Apply transitions for each subgroup
  for (i in 1:nrow(subgroups)) {
    subgroup <- subgroups[i, ]
    
    # Get transition matrix for this subgroup
    trans_mat <- generate_transition_matrix(
      params,
      age_group = subgroup$age_group,
      gender = subgroup$gender
    )
    
    # Extract current population vectors for this subgroup
    current_pop <- cohort_df %>%
      filter(
        age_group == subgroup$age_group,
        gender == subgroup$gender,
        area == subgroup$area
      ) %>%
      arrange(match(state, params$states)) %>%
      pull(population)
    
    current_original_pop <- cohort_df %>%
      filter(
        age_group == subgroup$age_group,
        gender == subgroup$gender,
        area == subgroup$area
      ) %>%
      arrange(match(state, params$states)) %>%
      pull(original_population)
    
    # Apply transition matrix to both population types
    new_pop <- as.vector(current_pop %*% trans_mat)
    new_original_pop <- as.vector(current_original_pop %*% trans_mat)
    
    # Create mapping of new populations
    population_mapping <- data.frame(
      state = params$states,
      new_pop = new_pop,
      new_original_pop = new_original_pop,
      stringsAsFactors = FALSE
    )
    
    # Update new cohort for this specific subgroup
    for (j in 1:nrow(population_mapping)) {
      new_cohort <- new_cohort %>%
        mutate(
          new_population = if_else(
            age_group == subgroup$age_group & 
              gender == subgroup$gender & 
              area == subgroup$area &
              state == population_mapping$state[j],
            population_mapping$new_pop[j],
            new_population
          ),
          new_original_population = if_else(
            age_group == subgroup$age_group & 
              gender == subgroup$gender & 
              area == subgroup$area &
              state == population_mapping$state[j],
            population_mapping$new_original_pop[j],
            new_original_population
          )
        )
    }
  }
  
  # Replace old populations with new
  new_cohort <- new_cohort %>%
    mutate(
      population = new_population,
      original_population = new_original_population,
      new_population = NULL,
      new_original_population = NULL
    )
  
  return(new_cohort)
}

# Get specific transition probability
# Returns: single probability value
get_transition_probability <- function(from_state, to_state, params, age_group, gender) {
  trans_mat <- generate_transition_matrix(params, age_group, gender)
  trans_mat[from_state, to_state]
}

# Generate report of transition probabilities
# Returns: data frame with all transitions for inspection
generate_transition_report <- function(params) {
  results <- list()
  
  for (age_group in params$age_groups) {
    for (gender in params$gender_groups) {
      trans_mat <- generate_transition_matrix(params, age_group, gender)
      
      # Convert to long format
      trans_df <- as.data.frame(trans_mat) %>%
        mutate(from_state = rownames(trans_mat)) %>%
        pivot_longer(
          cols = -from_state,
          names_to = "to_state",
          values_to = "probability"
        ) %>%
        filter(probability > 0) %>%
        mutate(
          age_group = age_group,
          gender = gender,
          ai_enabled = params$ai_enabled
        )
      
      results[[length(results) + 1]] <- trans_df
    }
  }
  
  bind_rows(results) %>%
    arrange(age_group, gender, from_state, to_state)
}

# Helper function to extract key progression metrics
# Returns: summary of important transition rates
get_progression_summary <- function(params) {
  summary_list <- list()
  
  for (age in params$age_groups) {
    for (gender in params$gender_groups) {
      trans_mat <- generate_transition_matrix(params, age, gender)
      
      key <- paste(age, gender, sep = "_")
      summary_list[[key]] <- list(
        healthy_to_mci = trans_mat["Other_Healthy", "MCI"],
        healthy_to_dementia = trans_mat["Other_Healthy", "Undx_Mild"],
        mci_to_dementia = trans_mat["MCI", "Undx_Mild"],
        mild_to_moderate_undx = trans_mat["Undx_Mild", "Undx_Moderate"],
        mild_to_moderate_dx = trans_mat["Dx_Mild", "Dx_Moderate"],
        diagnosis_rate_mild = trans_mat["Undx_Mild", "Dx_Mild"],
        diagnosis_rate_severe = trans_mat["Undx_Severe", "Dx_Severe"]
      )
    }
  }
  
  # Convert to data frame
  do.call(rbind, lapply(names(summary_list), function(key) {
    parts <- strsplit(key, "_")[[1]]
    data.frame(
      age_group = paste(parts[1:length(parts)-1], collapse = " "),
      gender = parts[length(parts)],
      metric = names(summary_list[[key]]),
      value = unlist(summary_list[[key]]),
      stringsAsFactors = FALSE
    )
  }))
}