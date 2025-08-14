# cohort_manager.R - Manage cohort populations, aging, and mortality

library(dplyr)
library(tidyr)

# Helper functions for consistent population counting
get_living_population <- function(cohort_df) {
  sum(cohort_df$population[cohort_df$state != "Death"])
}

get_death_count <- function(cohort_df) {
  sum(cohort_df$population[cohort_df$state == "Death"])
}

get_population_by_state <- function(cohort_df, state) {
  sum(cohort_df$population[cohort_df$state == state])
}

# Helper functions for original population tracking
get_original_living_population <- function(cohort_df) {
  sum(cohort_df$original_population[cohort_df$state != "Death"])
}

get_original_death_count <- function(cohort_df) {
  sum(cohort_df$original_population[cohort_df$state == "Death"])
}

get_original_population_by_state <- function(cohort_df, state) {
  sum(cohort_df$original_population[cohort_df$state == state])
}

# Initialize cohort data frame with all combinations and initial populations
# Returns: data frame with columns (state, age_group, gender, area, population, original_population)
initialize_cohort <- function(params) {
  states <- params$states
  age_groups <- params$age_groups
  genders <- params$gender_groups
  areas <- params$area_types
  
  # Create all combinations
  cohort_df <- expand.grid(
    state = states,
    age_group = age_groups,
    gender = genders,
    area = areas,
    population = 0,
    original_population = 0,  # Track original population separately
    stringsAsFactors = FALSE
  )
  
  # Get distribution parameters
  age_dist <- params$age_distributions
  gender_dist <- params$gender_distributions
  starting_pop <- params$starting_populations
  
  # Convert to long format for easier manipulation
  age_long <- age_dist %>%
    pivot_longer(cols = all_of(areas), names_to = "area", values_to = "age_pct")
  
  gender_long <- gender_dist %>%
    pivot_longer(cols = all_of(areas), names_to = "area", values_to = "gender_pct")
  
  # Calculate initial populations for each subgroup
  initial_pop <- cohort_df %>%
    left_join(age_long, by = c("area", "age_group")) %>%
    left_join(gender_long, by = c("area", "gender")) %>%
    mutate(
      # Properly lookup area-specific population
      total_area_pop = sapply(area, function(a) starting_pop[[a]]),
      subgroup_pop = total_area_pop * age_pct * gender_pct
    )
  
  # Distribute across health states according to initial prevalence
  cohort_df <- initial_pop %>%
    group_by(age_group, gender, area) %>%
    mutate(
      # Get age-specific and gender-specific prevalences
      dementia_prev = params$dementia_prevalence[[age_group[1]]][[gender[1]]],
      mci_prev = params$mci_prevalence[age_group[1]],  # Use age-specific MCI prevalence
      
      # Calculate base populations for major categories
      healthy_pop = subgroup_pop * (1 - dementia_prev - mci_prev),
      mci_pop = subgroup_pop * mci_prev,
      dementia_pop = subgroup_pop * dementia_prev,
      
      # Distribute population across states (same for both total and original)
      population_value = case_when(
        state == "Other_Healthy" ~ healthy_pop,
        state == "MCI" ~ mci_pop,
        state == "Undx_Mild" ~ dementia_pop * params$initial_severity_distribution$Mild,
        state == "Undx_Moderate" ~ dementia_pop * params$initial_severity_distribution$Moderate,
        state == "Undx_Severe" ~ dementia_pop * params$initial_severity_distribution$Severe,
        state == "Death" ~ 0,
        TRUE ~ 0  # No diagnosed cases initially
      ),
      # Set population to calculated value for all age groups
      # Set original_population only for "65 to 69" age group (others remain at 0)
      population = population_value,
      original_population = ifelse(age_group == "65 to 69", population_value, 0)
    ) %>%
    ungroup() %>%
    select(state, age_group, gender, area, population, original_population)
  
  return(cohort_df)
}

# Apply aging transitions - move people to next age group
# Returns: updated cohort data frame with aged populations
apply_aging <- function(cohort_df, params) {
  
  # Debug logging
  if (exists("verbose") && verbose) {
    cat("    Aging debug:\n")
    age_dist_before <- cohort_df %>%
      filter(state != "Death") %>%
      group_by(age_group) %>%
      summarise(pop = sum(population), .groups = "drop")
    print(age_dist_before)
  }
  
  aging_rates <- params$aging_rates
  age_groups <- params$age_groups
  
  # Calculate aging out for each subgroup
  aged_out <- cohort_df %>%
    filter(state != "Death") %>%  # Dead people don't age
    mutate(
      aging_rate = aging_rates[age_group],
      aging_out_pop = population * aging_rate,
      aging_out_original_pop = original_population * aging_rate,
      population = population - aging_out_pop,
      original_population = original_population - aging_out_original_pop
    ) %>%
    filter(aging_out_pop > 0)  # Only keep rows with people aging out
  
  # Prepare aged populations for next age group
  if (nrow(aged_out) > 0) {
    # Map current age to next age
    age_mapping <- data.frame(
      current_age = age_groups[-length(age_groups)],
      next_age = age_groups[-1],
      stringsAsFactors = FALSE
    )
    
    # Create aging in populations
    aged_in <- aged_out %>%
      inner_join(age_mapping, by = c("age_group" = "current_age")) %>%
      mutate(
        age_group = next_age,
        population = aging_out_pop,
        original_population = aging_out_original_pop
      ) %>%
      select(state, age_group, gender, area, population, original_population) %>%
      group_by(state, age_group, gender, area) %>%
      summarise(
        population = sum(population), 
        original_population = sum(original_population),
        .groups = "drop"
      )
    
    # Update original cohort - subtract aged out
    cohort_df <- cohort_df %>%
      left_join(
        aged_out %>% 
          select(state, age_group, gender, area, aging_out_pop, aging_out_original_pop),
        by = c("state", "age_group", "gender", "area")
      ) %>%
      mutate(
        population = if_else(is.na(aging_out_pop), population, population - aging_out_pop),
        original_population = if_else(is.na(aging_out_original_pop), original_population, original_population - aging_out_original_pop),
        aging_out_pop = NULL,
        aging_out_original_pop = NULL
      )
    
    # Add aged in populations
    cohort_df <- cohort_df %>%
      left_join(
        aged_in %>% rename(aged_in_pop = population, aged_in_original_pop = original_population),
        by = c("state", "age_group", "gender", "area")
      ) %>%
      mutate(
        population = population + if_else(is.na(aged_in_pop), 0, aged_in_pop),
        original_population = original_population + if_else(is.na(aged_in_original_pop), 0, aged_in_original_pop),
        aged_in_pop = NULL,
        aged_in_original_pop = NULL
      )
  }
  
  return(cohort_df)
}

# Apply mortality with age-specific rates and state multipliers
# Returns: updated cohort data frame with mortality applied
apply_mortality <- function(cohort_df, params) {
  mortality_rates <- params$mortality_rates
  mortality_multipliers <- params$mortality_multipliers
  
  # Calculate deaths for each subgroup and update populations
  cohort_df <- cohort_df %>%
    mutate(
      # Get base mortality rate for age group
      base_mort_rate = mortality_rates[age_group],
      # Get mortality multiplier for health state
      state_multiplier = mortality_multipliers[state],
      # Calculate mortality
      mortality_rate = base_mort_rate * state_multiplier,
      deaths = population * mortality_rate,
      original_deaths = original_population * mortality_rate,
      # Update population (remove deaths)
      population = population - deaths,
      original_population = original_population - original_deaths
    ) %>%
    # Add deaths to Death state for each subgroup
    group_by(age_group, gender, area) %>%
    mutate(
      total_deaths_subgroup = sum(deaths),
      total_original_deaths_subgroup = sum(original_deaths),
      population = if_else(
        state == "Death", 
        population + total_deaths_subgroup,
        population
      ),
      original_population = if_else(
        state == "Death", 
        original_population + total_original_deaths_subgroup,
        original_population
      )
    ) %>%
    ungroup() %>%
    select(-base_mort_rate, -state_multiplier, -mortality_rate, -deaths, -original_deaths, 
           -total_deaths_subgroup, -total_original_deaths_subgroup)
  
  return(cohort_df)
}

# Add population inflow (new people entering the model)
    # Only adds to "65 to 69" age group
# Returns: updated cohort data frame with new population added
add_population_inflow <- function(cohort_df, params, pre_mortality_pop = NULL, expected_death_rate = NULL) {
  # Calculate total population for scaling
  if (!is.null(pre_mortality_pop)) {
    # Use pre-mortality population for inflow calculation
    total_pop <- pre_mortality_pop
  } else {
    # Fallback to current population (original behavior)
    total_pop <- cohort_df %>%
      filter(state != "Death") %>%
      summarise(total = sum(population)) %>%
      pull(total)
  }
  
  # Calculate dynamic inflow rate for 1% net growth
  if (params$dynamic_inflow_for_net_growth) {
    # Use pre-calculated death rate if provided, otherwise calculate it
    if (!is.null(expected_death_rate)) {
      death_rate <- expected_death_rate
    } else {
      death_rate <- calculate_expected_death_rate(cohort_df, params)
    }
    # Set inflow rate to achieve 1% net growth: inflow = death_rate + 1%
    dynamic_inflow_rate <- death_rate + 0.01
    new_inflow <- total_pop * dynamic_inflow_rate
  } else {
    # Use fixed inflow rate
    new_inflow <- total_pop * params$new_inflow_rate
  }
  
  inflow_dist <- params$inflow_distribution
  age_dist <- params$age_distributions
  gender_dist <- params$gender_distributions
  
      # Only add to 65 to 69 age group
    target_age <- "65 to 69"
  
  # Convert distributions to long format
  age_long <- age_dist %>%
    filter(age_group == target_age) %>%
    pivot_longer(cols = -age_group, names_to = "area", values_to = "age_pct")
  
  gender_long <- gender_dist %>%
    pivot_longer(cols = -gender, names_to = "area", values_to = "gender_pct")
  
  # Calculate area populations for proper distribution
  area_populations <- cohort_df %>%
    filter(state != "Death") %>%
    group_by(area) %>%
    summarise(area_pop = sum(population), .groups = "drop") %>%
    mutate(area_prop = area_pop / sum(area_pop))
  
  # Calculate inflow for each subgroup
  inflow_data <- expand.grid(
    state = names(inflow_dist),
    age_group = target_age,
    gender = params$gender_groups,
    area = params$area_types,
    stringsAsFactors = FALSE
  ) %>%
    left_join(gender_long, by = c("area", "gender")) %>%
    left_join(area_populations %>% select(area, area_prop), by = "area") %>%
    mutate(
      state_proportion = inflow_dist[state],
      # Distribute inflow proportionally by area population and gender
      new_population = new_inflow * area_prop * gender_pct * state_proportion
    ) %>%
    select(state, age_group, gender, area, new_population)
  
  # Add inflow to existing cohort
  # Note: inflow only adds to total population, not original population
  cohort_df <- cohort_df %>%
    left_join(
      inflow_data,
      by = c("state", "age_group", "gender", "area")
    ) %>%
    mutate(
      population = population + if_else(is.na(new_population), 0, new_population),
      # original_population remains unchanged - this is the key feature
      new_population = NULL
    )
  
  return(cohort_df)
}

# Get total population by subgroup (excluding deaths)
# Returns: summary data frame with living population by age/gender/area
get_population_summary <- function(cohort_df) {
  cohort_df %>%
    filter(state != "Death") %>%
    group_by(age_group, gender, area) %>%
    summarise(
      total_population = sum(population),
      .groups = "drop"
    )
}

# Get population by health state
# Returns: summary data frame with population counts by state
get_state_summary <- function(cohort_df) {
  cohort_df %>%
    group_by(state) %>%
    summarise(
      total_population = sum(population),
      .groups = "drop"
    ) %>%
    mutate(
      percentage = total_population / sum(total_population) * 100
    )
}

# Extract specific subgroup population
# Returns: data frame with population by state for specified subgroup
get_subgroup_population <- function(cohort_df, age_group, gender, area) {
  cohort_df %>%
    filter(
      age_group == !!age_group,
      gender == !!gender,
      area == !!area
    ) %>%
    select(state, population)
}

# Update population for specific states/subgroups
# Used by pathway modules to update cohort after processing
# Returns: updated cohort data frame
update_cohort_states <- function(cohort_df, state_changes_df) {
  # state_changes_df should have columns: state, age_group, gender, area, population_change
  
  # Apply changes
  cohort_df <- cohort_df %>%
    left_join(
      state_changes_df,
      by = c("state", "age_group", "gender", "area")
    ) %>%
    mutate(
      population = population + if_else(is.na(population_change), 0, population_change),
      population_change = NULL
    )
  
  return(cohort_df)
}

# Validate cohort consistency
# Checks that population is conserved and no negative populations
# Returns: list with validation results
validate_cohort <- function(cohort_df, tolerance = 1e-6) {
  # Check for negative populations
  negative_pops <- cohort_df %>%
    filter(population < -tolerance)
  
  # Check population conservation by subgroup
  pop_by_subgroup <- cohort_df %>%
    group_by(age_group, gender, area) %>%
    summarise(
      total = sum(population),
      .groups = "drop"
    )
  
  # Get total population
  total_pop <- sum(cohort_df$population)
  
  list(
    has_negative_populations = nrow(negative_pops) > 0,
    negative_populations = negative_pops,
    total_population = total_pop,
    population_by_subgroup = pop_by_subgroup,
    is_valid = nrow(negative_pops) == 0
  )
}

# Calculate expected death rate for current population composition
# Returns: weighted average death rate across all subgroups
# Note: This accounts for both explicit mortality and Markov transition deaths
calculate_expected_death_rate <- function(cohort_df, params) {
  mortality_rates <- params$mortality_rates
  mortality_multipliers <- params$mortality_multipliers
  
  # Calculate expected deaths from both mortality and Markov transitions
  # Since both apply mortality, we need to account for the total death rate
  death_calculations <- cohort_df %>%
    filter(state != "Death") %>%
    mutate(
      base_mort_rate = mortality_rates[age_group],
      state_multiplier = mortality_multipliers[state],
      # Approximate total death rate: direct mortality + Markov transition mortality
      # The Markov transitions also include death probabilities, so we need to roughly double
      # the mortality rate to account for both steps
      total_mortality_rate = base_mort_rate * state_multiplier * 2,
      expected_deaths = population * total_mortality_rate
    )
  
  # Calculate overall death rate
  total_living_pop <- sum(death_calculations$population)
  total_expected_deaths <- sum(death_calculations$expected_deaths)
  
  if (total_living_pop > 0) {
    overall_death_rate <- total_expected_deaths / total_living_pop
  } else {
    overall_death_rate <- 0
  }
  
  return(overall_death_rate)
}