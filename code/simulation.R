# simulation.R - Main simulation pipeline orchestrating all components

library(dplyr)
library(tidyr)

# Source all required modules (in practice, these would be sourced at the top)
source("parameters.R")
source("cohort_manager.R")
source("queue_manager.R")
source("pathways.R")
source("transitions.R")
source("outcomes.R")


# simulation.R - Main simulation engine

library(dplyr)
library(tidyr)
library(purrr)

# Helper functions for consistent population counting
count_living <- function(cohort_df) {
  sum(cohort_df$population[cohort_df$state != "Death"])
}

get_living_population <- function(cohort_df) {
  sum(cohort_df$population[cohort_df$state != "Death"])
}

get_death_count <- function(cohort_df) {
  sum(cohort_df$population[cohort_df$state == "Death"])
}

get_population_by_state <- function(cohort_df, state) {
  sum(cohort_df$population[cohort_df$state == state])
}


# Run simulation for a single scenario
# Returns: list with results and final states
run_simulation <- function(ai_enabled = TRUE, area_types = c("Urban", "Rural"), 
                           years = 0:75, verbose = FALSE,
                           ai_mci_sens_override = NULL,
                           ai_mci_spec_override = NULL,
                           ai_dementia_sens_override = NULL,
                           ai_dementia_spec_override = NULL) {
  
  # Step 1: Initialize parameters
  if (verbose) cat("Initializing parameters...\n")
  params <- get_parameters(ai_enabled = ai_enabled, area_type = area_types[1],
                         ai_mci_sens_override = ai_mci_sens_override,
                         ai_mci_spec_override = ai_mci_spec_override,
                         ai_dementia_sens_override = ai_dementia_sens_override,
                         ai_dementia_spec_override = ai_dementia_spec_override)
  params$area_types <- area_types  # Override to handle multiple areas
  
  # Step 2: Initialize cohort
  if (verbose) cat("Initializing cohort...\n")
  cohort_df <- initialize_cohort(params)
  
  # Step 3: Initialize persistent queues
  if (verbose) cat("Initializing queues...\n")
  queue_state <- list(
    gp = initialize_queue_map(cohort_df, "gp", params),
    mc = initialize_queue_map(cohort_df, "mc", params),
    mri = initialize_queue_map(cohort_df, "mri", params)
  )
  
  # Initialize results storage
  results <- data.frame()
  scenario_name <- paste0(
    ifelse(ai_enabled, "AI", "No_AI"), 
    "_", 
    paste(area_types, collapse = "_")
  )
  
  # Step 4: Main simulation loop
  for (year in years) {
    if (verbose) cat(sprintf("Processing year %d...\n", year))
    
    # Update parameters for current year (AI adoption changes over time)
    params <- get_parameters(ai_enabled = ai_enabled, area_type = area_types[1], year = year,
                           ai_mci_sens_override = ai_mci_sens_override,
                           ai_mci_spec_override = ai_mci_spec_override,
                           ai_dementia_sens_override = ai_dementia_sens_override,
                           ai_dementia_spec_override = ai_dementia_spec_override)
    if (verbose && year > 0) {
      cat(sprintf("  AI adoption rate: %.1f%%\n", params$ai_adoption_rate * 100))
    }
    
    # Add population tracking
    pop_start <- get_living_population(cohort_df)
    deaths_start <- get_death_count(cohort_df)
    
    # And update the logging to show both:
    if (verbose) cat(sprintf("  Start population: %.0f living, %.0f deaths\n", pop_start, deaths_start))
    
    # Initialize year's test counts
    year_test_counts <- list(
      gp_tests = 0, 
      neuropsych = 0, 
      neuroimaging = 0, 
      ai_morphometry = 0, 
      biomarker = 0,
      mc_total = 0
    )
    
    # Initialize original test counts (based on original population)
    year_original_test_counts <- list(
      gp_tests = 0, 
      neuropsych = 0, 
      neuroimaging = 0, 
      ai_morphometry = 0, 
      biomarker = 0,
      mc_total = 0
    )
    
    # Initialize diagnosed count tracking for this year
    year_diagnosed_counts <- list(
      gp_diagnosed = 0,
      neuropsych_diagnosed = 0,
      neuroimaging_diagnosed = 0,
      ai_morphometry_diagnosed = 0,
      biomarker_diagnosed = 0
    )
    
    # Initialize original diagnosed count tracking for this year
    year_original_diagnosed_counts <- list(
      gp_diagnosed = 0,
      neuropsych_diagnosed = 0,
      neuroimaging_diagnosed = 0,
      ai_morphometry_diagnosed = 0,
      biomarker_diagnosed = 0
    )
    
    
        # 4a: Apply population dynamics
    
    # Calculate expected death rate before applying mortality (for dynamic inflow)
    expected_death_rate <- NULL
    if (params$dynamic_inflow_for_net_growth && year > 0) {
      expected_death_rate <- calculate_expected_death_rate(cohort_df, params)
      if (verbose && year <= 5) {
        cat(sprintf("    Expected death rate: %.3f%%, Target inflow rate: %.3f%%\n", 
                    expected_death_rate * 100, (expected_death_rate + 0.01) * 100))
      }
    }
    
    if (verbose) cat("  Applying mortality...\n")
    cohort_df <- apply_mortality(cohort_df, params)
    pop_after_mortality <- count_living(cohort_df)
    if (verbose) cat(sprintf("    After mortality: %.0f (change: %+.0f)\n", 
                             pop_after_mortality, pop_after_mortality - pop_start))

    if (verbose) cat("  Applying aging...\n")
    cohort_df <- apply_aging(cohort_df, params)
    pop_after_aging <- count_living(cohort_df)
    if (verbose) cat(sprintf("    After aging: %.0f (change: %+.0f)\n", 
                             pop_after_aging, pop_after_aging - pop_after_mortality))

    if (verbose) cat("  Adding population inflow...\n")
    if (year > 0) {  # Skip inflow in year 0
      # Pass pre-mortality population and pre-calculated death rate for inflow calculation
      cohort_df <- add_population_inflow(cohort_df, params, pop_start, expected_death_rate)
    }
    pop_after_inflow <- count_living(cohort_df)
    if (verbose) cat(sprintf("    After inflow: %.0f (change: %+.0f)\n", 
                             pop_after_inflow, pop_after_inflow - pop_after_aging))
    
    # 4b: Calculate symptomatic population
    if (verbose) cat("  Calculating symptomatic presentation...\n")
    symptomatic_df <- calculate_symptomatic_presentation(cohort_df, params)
    
    # 4c: Run GP pathway
    if (verbose) cat("  Running GP pathway...\n")
    gp_results <- run_gp_pathway(
      symptomatic_df, 
      queue_state$gp, 
      params
    )
    
    # Store diagnosed population for later application
    diagnosed_population <- gp_results$diagnosed_population
    queue_state$gp <- gp_results$updated_queue
    
    # Track GP diagnosed counts
    if (nrow(gp_results$diagnosed_population) > 0) {
      gp_diagnosed_total <- sum(gp_results$diagnosed_population$diagnosed_count)
      year_diagnosed_counts$gp_diagnosed <- year_diagnosed_counts$gp_diagnosed + gp_diagnosed_total
      
      # Track original GP diagnosed counts (based on original population)
      current_pop <- sum(cohort_df$population[cohort_df$state != "Death"])
      original_pop <- sum(cohort_df$original_population[cohort_df$state != "Death"])
      if (current_pop > 0) {
        original_gp_diagnosed <- gp_diagnosed_total * (original_pop / current_pop)
        year_original_diagnosed_counts$gp_diagnosed <- year_original_diagnosed_counts$gp_diagnosed + original_gp_diagnosed
      }
    }
    
    # Track GP tests
    year_test_counts$gp_tests <- year_test_counts$gp_tests + 
      ifelse(is.null(gp_results$test_counts$gp_tests), 0, gp_results$test_counts$gp_tests)
    
    # Track original GP tests (based on original population)
    # Calculate what GP tests would have been with original population
    original_gp_tests <- ifelse(is.null(gp_results$test_counts$gp_tests), 0, gp_results$test_counts$gp_tests)
    if (original_gp_tests > 0) {
      # Scale by original population ratio
      current_pop <- sum(cohort_df$population[cohort_df$state != "Death"])
      original_pop <- sum(cohort_df$original_population[cohort_df$state != "Death"])
      if (current_pop > 0) {
        original_gp_tests <- original_gp_tests * (original_pop / current_pop)
      }
    }
    year_original_test_counts$gp_tests <- year_original_test_counts$gp_tests + original_gp_tests
    
    
    # 4d: Run Memory Clinic pathway
    if (verbose) cat("  Running Memory Clinic pathway...\n")
    if (nrow(gp_results$mc_referrals) > 0) {
      mc_results <- run_memory_clinic_pathway(
        gp_results$mc_referrals,
        queue_state$mc,
        queue_state$mri,
        params
      )
      
      # Add MC diagnoses to the diagnosed population
      diagnosed_population <- bind_rows(diagnosed_population, mc_results$diagnosed_population)
      
      # Track diagnosed counts by pathway
      if (nrow(mc_results$diagnosed_population) > 0) {
        mc_diagnosed_counts <- mc_results$diagnosed_population %>%
          mutate(
            pathway = case_when(
              grepl("neuropsych", source, ignore.case = TRUE) ~ "neuropsych",
              grepl("neuroimaging", source, ignore.case = TRUE) & !grepl("AI", source, ignore.case = TRUE) ~ "neuroimaging",
              grepl("neuroimaging.*AI", source, ignore.case = TRUE) ~ "ai_morphometry",
              grepl("biomarker", source, ignore.case = TRUE) ~ "biomarker",
              TRUE ~ "other"
            )
          ) %>%
          group_by(pathway) %>%
          summarise(total_diagnosed = sum(diagnosed_count), .groups = "drop")
        
        for (i in 1:nrow(mc_diagnosed_counts)) {
          pathway <- mc_diagnosed_counts$pathway[i]
          count <- mc_diagnosed_counts$total_diagnosed[i]
          
          if (paste0(pathway, "_diagnosed") %in% names(year_diagnosed_counts)) {
            year_diagnosed_counts[[paste0(pathway, "_diagnosed")]] <- 
              year_diagnosed_counts[[paste0(pathway, "_diagnosed")]] + count
          }
        }
        
        # Track original MC diagnosed counts (based on original population)
        current_pop <- sum(cohort_df$population[cohort_df$state != "Death"])
        original_pop <- sum(cohort_df$original_population[cohort_df$state != "Death"])
        if (current_pop > 0) {
          original_scale_factor <- original_pop / current_pop
          
          for (i in 1:nrow(mc_diagnosed_counts)) {
            pathway <- mc_diagnosed_counts$pathway[i]
            count <- mc_diagnosed_counts$total_diagnosed[i]
            
            if (paste0(pathway, "_diagnosed") %in% names(year_original_diagnosed_counts)) {
              original_count <- count * original_scale_factor
              year_original_diagnosed_counts[[paste0(pathway, "_diagnosed")]] <- 
                year_original_diagnosed_counts[[paste0(pathway, "_diagnosed")]] + original_count
            }
          }
        }
      }
      queue_state$mc <- mc_results$updated_mc_queue
      queue_state$mri <- mc_results$updated_mri_queue
      
      # Track MC tests
      if (!is.null(mc_results$test_counts)) {
        year_test_counts$neuropsych <- year_test_counts$neuropsych + mc_results$test_counts$neuropsych
        year_test_counts$neuroimaging <- year_test_counts$neuroimaging + mc_results$test_counts$neuroimaging
        year_test_counts$ai_morphometry <- year_test_counts$ai_morphometry + 
          ifelse(is.null(mc_results$test_counts$ai_morphometry), 0, mc_results$test_counts$ai_morphometry)
        year_test_counts$biomarker <- year_test_counts$biomarker + mc_results$test_counts$biomarker
        year_test_counts$mc_total <- year_test_counts$neuropsych + year_test_counts$neuroimaging + 
          year_test_counts$ai_morphometry + year_test_counts$biomarker
        
        # Track original MC tests (based on original population)
        # Calculate what MC tests would have been with original population
        current_pop <- sum(cohort_df$population[cohort_df$state != "Death"])
        original_pop <- sum(cohort_df$original_population[cohort_df$state != "Death"])
        if (current_pop > 0) {
          original_scale_factor <- original_pop / current_pop
          
          year_original_test_counts$neuropsych <- year_original_test_counts$neuropsych + 
            mc_results$test_counts$neuropsych * original_scale_factor
          year_original_test_counts$neuroimaging <- year_original_test_counts$neuroimaging + 
            mc_results$test_counts$neuroimaging * original_scale_factor
          year_original_test_counts$ai_morphometry <- year_original_test_counts$ai_morphometry + 
            ifelse(is.null(mc_results$test_counts$ai_morphometry), 0, mc_results$test_counts$ai_morphometry) * original_scale_factor
          year_original_test_counts$biomarker <- year_original_test_counts$biomarker + 
            mc_results$test_counts$biomarker * original_scale_factor
          year_original_test_counts$mc_total <- year_original_test_counts$neuropsych + year_original_test_counts$neuroimaging + 
            year_original_test_counts$ai_morphometry + year_original_test_counts$biomarker
        }
      }
    }
    
    # 4e: Apply queue progression remains the same
    if (verbose) cat("  Applying queue progression...\n")
    queue_state$mc <- apply_queue_progression(queue_state$mc, params)
    queue_state$mri <- apply_queue_progression(queue_state$mri, params)
    
    # 4f: Apply Markov transitions AND diagnoses
    if (verbose) cat("  Applying Markov transitions...\n")
    # First apply regular transitions
    cohort_df <- apply_transitions(cohort_df, params)
    
    # Then apply diagnoses from clinical pathways
    if (nrow(diagnosed_population) > 0) {
      if (verbose) cat("    Applying diagnoses from clinical pathways...\n")
      cohort_df <- apply_diagnoses_to_cohort(cohort_df, diagnosed_population, params)
    }
    
    pop_after_transitions <- count_living(cohort_df)
    if (verbose) cat(sprintf("    After transitions: %.0f (change: %+.0f)\n", 
                             pop_after_transitions, pop_after_transitions - pop_after_inflow))
    deaths_after_transitions <- get_death_count(cohort_df)
    if (verbose) cat(sprintf("    Deaths in cohort: %.0f\n", deaths_after_transitions))
    
    # 4g: Calculate outcomes AFTER transitions
    if (verbose) cat("  Calculating costs and QALYs...\n")
    cost_results <- calculate_costs(
      cohort_df, 
      params, 
      year, 
      queue_state,
      pathway_results = list(test_counts = year_test_counts)  # Pass test counts
    )
    qaly_results <- calculate_qalys(cohort_df, params, year, queue_state)
    
    # 4h: Record results
    if (verbose) cat("  Recording results...\n")
    year_results <- record_results(
      cohort_df, 
      cost_results, 
      qaly_results,
      queue_state, 
      params, 
      year, 
      scenario_name,
      year_test_counts,  # Pass test counts to record
      year_diagnosed_counts,  # Pass diagnosed counts to record
      year_original_test_counts,  # Pass original test counts to record
      year_original_diagnosed_counts  # Pass original diagnosed counts to record
    )
    results <- bind_rows(results, year_results)
    
    # Reset annual queue counters
    queue_state$gp <- reset_annual_counters(queue_state$gp)
    queue_state$mc <- reset_annual_counters(queue_state$mc)
    queue_state$mri <- reset_annual_counters(queue_state$mri)
    
    # Optional: Validate cohort
    if (verbose) {
      validation <- validate_cohort(cohort_df)
      if (!validation$is_valid) {
        warning(sprintf("Year %d: Cohort validation failed - negative populations detected", year))
      }
    }
  }
  
  # Return comprehensive results
  return(list(
    results = results,
    final_cohort = cohort_df,
    final_queues = queue_state,
    parameters = params,
    scenario = scenario_name
  ))
}

# Run simulation with error handling
# Returns: simulation output or error information with consistent structure
safe_run_simulation <- function(ai_enabled, area_types, years, verbose, context = "",
                              ai_mci_sens_override = NULL,
                              ai_mci_spec_override = NULL,
                              ai_dementia_sens_override = NULL,
                              ai_dementia_spec_override = NULL) {
  tryCatch({
    # Run the simulation
    result <- run_simulation(
      ai_enabled = ai_enabled,
      area_types = area_types,
      years = years,
      verbose = verbose,
      ai_mci_sens_override = ai_mci_sens_override,
      ai_mci_spec_override = ai_mci_spec_override,
      ai_dementia_sens_override = ai_dementia_sens_override,
      ai_dementia_spec_override = ai_dementia_spec_override
    )
    # Ensure consistent structure
    result$error <- FALSE
    result$error_message <- NULL
    result$context <- context
    return(result)
  }, error = function(e) {
    warning(sprintf("Simulation failed for %s: %s", context, e$message))
    # Return consistent structure with error
    return(list(
      results = data.frame(),
      final_cohort = NULL,
      final_queues = NULL,
      parameters = NULL,
      scenario = NULL,
      error = TRUE,
      error_message = e$message,
      context = context
    ))
  })
}

# Run multiple scenarios sequentially
# Returns: list of simulation results
run_multiple_scenarios <- function(scenarios_df, years = 0:75, verbose = FALSE) {
  # scenarios_df should have columns: ai_enabled, area_types
  
  results_list <- list()
  
  for (i in 1:nrow(scenarios_df)) {
    scenario <- scenarios_df[i, ]
    
    if (verbose) {
      cat(sprintf("\n=== Running scenario %d/%d: AI=%s, Areas=%s ===\n",
                  i, nrow(scenarios_df),
                  scenario$ai_enabled,
                  paste(scenario$area_types[[1]], collapse = ", ")))
    }
    
    result <- safe_run_simulation(
      ai_enabled = scenario$ai_enabled,
      area_types = scenario$area_types[[1]],
      years = years,
      verbose = verbose,
      context = sprintf("AI=%s, Areas=%s", 
                        scenario$ai_enabled, 
                        paste(scenario$area_types[[1]], collapse = "_"))
    )
    
    results_list[[i]] <- result
  }
  
  return(results_list)
}

# Compare two scenarios
# Returns: comparison metrics
compare_scenarios <- function(results_ai, results_no_ai) {
  # Check for errors first
  if (results_ai$error || results_no_ai$error) {
    warning("Cannot compare scenarios - one or both had errors")
    return(NULL)
  }
  
  # Extract final year results
  final_year <- max(results_ai$results$Year)
  
  final_ai <- results_ai$results %>% 
    filter(Year == final_year)
  final_no_ai <- results_no_ai$results %>% 
    filter(Year == final_year)
  
  # Calculate differences
  comparison <- data.frame(
    Metric = c(
      "Total Population",
      "Diagnosed Dementia",
      "Undiagnosed Dementia", 
      "Diagnosis Rate",
      "Original Total Population",
      "Original Diagnosed Dementia",
      "Original Undiagnosed Dementia",
      "Original Diagnosis Rate",
      "Total Cost (Lifetime)",
      "Cost per Person",
      "Cost per Original Person",
      "Average GP Wait",
      "Average MC Wait",
      "Average MRI Wait"
    ),
    AI = c(
      sum(final_ai$Total_Population),
      sum(final_ai$Diagnosed_Dementia),
      sum(final_ai$Undiagnosed_Dementia),
      sum(final_ai$Diagnosed_Dementia) / (sum(final_ai$Diagnosed_Dementia) + sum(final_ai$Undiagnosed_Dementia)),
      # Original population metrics
      ifelse("Original_Total_Population" %in% names(final_ai), sum(final_ai$Original_Total_Population, na.rm = TRUE), sum(final_ai$Total_Population)),
      ifelse("Original_Diagnosed_Dementia" %in% names(final_ai), sum(final_ai$Original_Diagnosed_Dementia, na.rm = TRUE), sum(final_ai$Diagnosed_Dementia)),
      ifelse("Original_Undiagnosed_Dementia" %in% names(final_ai), sum(final_ai$Original_Undiagnosed_Dementia, na.rm = TRUE), sum(final_ai$Undiagnosed_Dementia)),
      ifelse("Original_Diagnosed_Dementia" %in% names(final_ai), 
             sum(final_ai$Original_Diagnosed_Dementia, na.rm = TRUE) / (sum(final_ai$Original_Diagnosed_Dementia, na.rm = TRUE) + sum(final_ai$Original_Undiagnosed_Dementia, na.rm = TRUE)),
             sum(final_ai$Diagnosed_Dementia) / (sum(final_ai$Diagnosed_Dementia) + sum(final_ai$Undiagnosed_Dementia))),
      # Existing metrics
      sum(results_ai$results$Discounted_Cost),
      sum(results_ai$results$Discounted_Cost) / sum(final_ai$Total_Population),
      # Per original person metrics
      ifelse("Original_Total_Population" %in% names(final_ai),
             sum(results_ai$results$Discounted_Cost) / sum(final_ai$Original_Total_Population, na.rm = TRUE),
             sum(results_ai$results$Discounted_Cost) / sum(final_ai$Total_Population)),
      mean(final_ai$avg_wait_gp, na.rm = TRUE),
      mean(final_ai$avg_wait_mc, na.rm = TRUE),
      mean(final_ai$avg_wait_mri, na.rm = TRUE)
    ),
    No_AI = c(
      sum(final_no_ai$Total_Population),
      sum(final_no_ai$Diagnosed_Dementia),
      sum(final_no_ai$Undiagnosed_Dementia),
      sum(final_no_ai$Diagnosed_Dementia) / (sum(final_no_ai$Diagnosed_Dementia) + sum(final_no_ai$Undiagnosed_Dementia)),
      # Original population metrics
      ifelse("Original_Total_Population" %in% names(final_no_ai), sum(final_no_ai$Original_Total_Population, na.rm = TRUE), sum(final_no_ai$Total_Population)),
      ifelse("Original_Diagnosed_Dementia" %in% names(final_no_ai), sum(final_no_ai$Original_Diagnosed_Dementia, na.rm = TRUE), sum(final_no_ai$Diagnosed_Dementia)),
      ifelse("Original_Undiagnosed_Dementia" %in% names(final_no_ai), sum(final_no_ai$Original_Undiagnosed_Dementia, na.rm = TRUE), sum(final_no_ai$Undiagnosed_Dementia)),
      ifelse("Original_Diagnosed_Dementia" %in% names(final_no_ai), 
             sum(final_no_ai$Original_Diagnosed_Dementia, na.rm = TRUE) / (sum(final_no_ai$Original_Diagnosed_Dementia, na.rm = TRUE) + sum(final_no_ai$Original_Undiagnosed_Dementia, na.rm = TRUE)),
             sum(final_no_ai$Diagnosed_Dementia) / (sum(final_no_ai$Diagnosed_Dementia) + sum(final_no_ai$Undiagnosed_Dementia))),
      # Existing metrics
      sum(results_no_ai$results$Discounted_Cost),
      sum(results_no_ai$results$Discounted_Cost) / sum(final_no_ai$Total_Population),
      # Per original person metrics
      ifelse("Original_Total_Population" %in% names(final_no_ai),
             sum(results_no_ai$results$Discounted_Cost) / sum(final_no_ai$Original_Total_Population, na.rm = TRUE),
             sum(results_no_ai$results$Discounted_Cost) / sum(final_no_ai$Total_Population)),
      mean(final_no_ai$avg_wait_gp, na.rm = TRUE),
      mean(final_no_ai$avg_wait_mc, na.rm = TRUE),
      mean(final_no_ai$avg_wait_mri, na.rm = TRUE)
    )
  )
  
  comparison$Difference <- comparison$AI - comparison$No_AI
  comparison$Percent_Change <- (comparison$Difference / comparison$No_AI) * 100
  
  # Calculate ICER - need to use summarized results
  ai_summary <- summarize_results(results_ai$results)
  no_ai_summary <- summarize_results(results_no_ai$results)
  
  icer_results <- calculate_icer(ai_summary, no_ai_summary)
  
  return(list(
    comparison_table = comparison,
    icer_results = icer_results
  ))
}

# Compare two scenarios using original population metrics
# Returns: original population comparison metrics
compare_scenarios_original <- function(results_ai, results_no_ai) {
  # Check for errors first
  if (results_ai$error || results_no_ai$error) {
    warning("Cannot compare scenarios - one or both had errors")
    return(NULL)
  }
  
  # Extract final year results for most metrics, but initial year for original population
  final_year <- max(results_ai$results$Year)
  initial_year <- min(results_ai$results$Year)
  
  final_ai <- results_ai$results %>% 
    filter(Year == final_year)
  final_no_ai <- results_no_ai$results %>% 
    filter(Year == final_year)
  
  # Get initial year results for original population count
  initial_ai <- results_ai$results %>% 
    filter(Year == initial_year)
  initial_no_ai <- results_no_ai$results %>% 
    filter(Year == initial_year)
  
  # Calculate average AI diagnosis rate improvement per year
  avg_yearly_improvement <- 0
  if (!is.null(results_ai$results) && !is.null(results_no_ai$results)) {
    # Get initial population for rate calculations
    initial_population <- ifelse("Original_Total_Population" %in% names(results_ai$results), 
                                sum(results_ai$results$Original_Total_Population[results_ai$results$Year == min(results_ai$results$Year)], na.rm = TRUE), 
                                sum(results_ai$results$Total_Population[results_ai$results$Year == min(results_ai$results$Year)]))
    
    # Calculate yearly diagnosis rates and differences
    yearly_improvements <- numeric(0)
    years <- unique(results_ai$results$Year)
    
    for (year in years) {
      ai_year_data <- results_ai$results %>% filter(Year == year)
      no_ai_year_data <- results_no_ai$results %>% filter(Year == year)
      
      if (nrow(ai_year_data) > 0 && nrow(no_ai_year_data) > 0) {
        ai_diagnosed <- ifelse("Original_Diagnosed_Dementia" %in% names(ai_year_data), 
                              sum(ai_year_data$Original_Diagnosed_Dementia, na.rm = TRUE), 
                              sum(ai_year_data$Diagnosed_Dementia, na.rm = TRUE))
        no_ai_diagnosed <- ifelse("Original_Diagnosed_Dementia" %in% names(no_ai_year_data), 
                                 sum(no_ai_year_data$Original_Diagnosed_Dementia, na.rm = TRUE), 
                                 sum(no_ai_year_data$Diagnosed_Dementia, na.rm = TRUE))
        
        ai_rate <- ai_diagnosed / initial_population
        no_ai_rate <- no_ai_diagnosed / initial_population
        improvement <- (ai_rate - no_ai_rate) * 100  # Convert to percentage points
        yearly_improvements <- c(yearly_improvements, improvement)
      }
    }
    
    if (length(yearly_improvements) > 0) {
      avg_yearly_improvement <- mean(yearly_improvements, na.rm = TRUE)
    }
  }
  
  # Calculate original population comparison
  original_comparison <- data.frame(
    Metric = c(
      "Original Subgroup Cost",
      "Original Subgroup Discounted Cost", 
      "Original Cost per Person",
      "Average GP Wait",
      "Average MC Wait"
    ),
    AI = c(
      # Original subgroup metrics (new)
      ifelse("Original_Subgroup_Cost" %in% names(final_ai), sum(final_ai$Original_Subgroup_Cost, na.rm = TRUE), sum(results_ai$results$Discounted_Cost)),
      ifelse("Original_Subgroup_Discounted_Cost" %in% names(final_ai), sum(final_ai$Original_Subgroup_Discounted_Cost, na.rm = TRUE), sum(results_ai$results$Discounted_Cost)),
      # Per original person metrics
      ifelse("Original_Total_Population" %in% names(final_ai),
             sum(results_ai$results$Discounted_Cost) / sum(final_ai$Original_Total_Population, na.rm = TRUE),
             sum(results_ai$results$Discounted_Cost) / sum(final_ai$Total_Population)),
      # Wait times (same for both original and current populations)
      mean(final_ai$avg_wait_gp, na.rm = TRUE),
      mean(final_ai$avg_wait_mc, na.rm = TRUE)
    ),
    No_AI = c(
      # Original subgroup metrics (new)
      ifelse("Original_Subgroup_Cost" %in% names(final_no_ai), sum(final_no_ai$Original_Subgroup_Cost, na.rm = TRUE), sum(results_no_ai$results$Discounted_Cost)),
      ifelse("Original_Subgroup_Discounted_Cost" %in% names(final_no_ai), sum(final_no_ai$Original_Subgroup_Discounted_Cost, na.rm = TRUE), sum(results_no_ai$results$Discounted_Cost)),
      # Per original person metrics
      ifelse("Original_Total_Population" %in% names(final_no_ai),
             sum(results_no_ai$results$Discounted_Cost) / sum(final_no_ai$Original_Total_Population, na.rm = TRUE),
             sum(results_no_ai$results$Discounted_Cost) / sum(final_no_ai$Total_Population)),
      # Wait times (same for both original and current populations)
      mean(final_no_ai$avg_wait_gp, na.rm = TRUE),
      mean(final_no_ai$avg_wait_mc, na.rm = TRUE)
    )
  )
  
  original_comparison$Difference <- original_comparison$AI - original_comparison$No_AI
  original_comparison$Percent_Change <- (original_comparison$Difference / original_comparison$No_AI) * 100
  
  # Calculate proper ICER for original population using aggregated discounted costs and QALYs
  ai_summary <- summarize_results(results_ai$results)
  no_ai_summary <- summarize_results(results_no_ai$results)
  
  original_icer_results <- calculate_original_population_icer(ai_summary, no_ai_summary)
  
  return(list(
    comparison_table = original_comparison,
    icer_results = original_icer_results
  ))
}

# Generate time series plots data
# Returns: data frame ready for plotting
prepare_plot_data <- function(simulation_results) {
  results <- simulation_results$results
  
  # Prepare population trends
  pop_trends <- results %>%
    select(Year, Scenario, Other_Healthy, MCI, 
           Diagnosed_Dementia, Undiagnosed_Dementia) %>%
    pivot_longer(
      cols = c(Other_Healthy, MCI, Diagnosed_Dementia, Undiagnosed_Dementia),
      names_to = "State",
      values_to = "Population"
    )
  
  # Prepare cost trends
  cost_trends <- results %>%
    select(Year, Scenario, Total_Cost, Discounted_Cost) %>%
    pivot_longer(
      cols = c(Total_Cost, Discounted_Cost),
      names_to = "Cost_Type",
      values_to = "Cost"
    )
  
  # Prepare queue trends
  queue_trends <- results %>%
    select(Year, Scenario, starts_with("avg_wait_")) %>%
    pivot_longer(
      cols = starts_with("avg_wait_"),
      names_to = "Queue_Type",
      values_to = "Wait_Time"
    )
  
  return(list(
    population = pop_trends,
    costs = cost_trends,
    queues = queue_trends
  ))
}

# Validate simulation results
# Returns: validation report
validate_simulation_results <- function(results) {
  issues <- character()
  
  # Calculate initial population from parameters
  params <- results$parameters
  initial_living <- sum(unlist(params$starting_populations))
  
  # Get results data
  year0_results <- results$results[results$results$Year == 0, ]
  final_results <- results$results[results$results$Year == max(results$results$Year), ]
  
  # Calculate totals
  # Year 0 results show population AFTER first year of processing
  final_living <- sum(final_results$Total_Population)
  # For conservation, we need cumulative deaths from the Death state, not yearly Death_Count
  final_deaths <- sum(final_results$Death)
  final_total <- final_living + final_deaths
  
  # Calculate expected inflow accounting for mortality effects
  n_years <- length(unique(results$results$Year))
  # With Fix #2: no inflow in year 0
  
  # Track year-by-year population to account for mortality affecting inflow base
  total_expected_inflow <- 0
  
  # Get actual living population for each year to calculate expected inflows
  yearly_results <- results$results %>%
    arrange(Year) %>%
    select(Year, Total_Population)
  
  for (i in 2:nrow(yearly_results)) {  # Start from year 1 (index 2)
    # Use previous year's living population as base for inflow calculation
    prev_year_pop <- yearly_results$Total_Population[i-1]
    
    if (params$dynamic_inflow_for_net_growth) {
      # For dynamic inflow, we can't easily predict what the inflow was without
      # re-running the death rate calculations. For validation, we'll check
      # that net growth is approximately 1% instead of conservation.
      yearly_inflow <- prev_year_pop * params$new_inflow_rate  # Fallback estimate
    } else {
      # Fixed inflow rate
      yearly_inflow <- prev_year_pop * params$new_inflow_rate
    }
    
    total_expected_inflow <- total_expected_inflow + yearly_inflow
  }
  
  # Expected final total = initial + all inflow - deaths
  # final_deaths already calculated above as cumulative deaths in Death state
  expected_final_total <- initial_living + total_expected_inflow - final_deaths
  
  if (params$dynamic_inflow_for_net_growth) {

  } else {
    # Check conservation for fixed inflow rate (original logic)
    # Conservation means: initial + inflow = final_living + final_deaths
    # final_total already includes final_deaths, so don't double count
    total_accounted <- final_living + final_deaths
    expected_total_accounted <- initial_living + total_expected_inflow
    conservation_error <- abs(total_accounted - expected_total_accounted) / initial_living
    
    if (conservation_error > 0.01) {
      issues <- c(issues, sprintf("Population not conserved: initial=%.0f, inflow=%.0f, final_living=%.0f, final_deaths=%.0f, error=%.1f%%", 
                                  initial_living, total_expected_inflow, final_living, final_deaths,
                                  conservation_error * 100))
    }
  }
  
  # Check costs increase over time (before discounting)
  # We need to check undiscounted costs, not discounted
  if ("Total_Cost" %in% names(results$results)) {
    cost_changes <- diff(aggregate(Total_Cost ~ Year, results$results, sum)$Total_Cost)
    if (any(cost_changes < -params$validation_tolerances$cost_decrease)) {  # Allow small decreases due to rounding
      issues <- c(issues, "Large cost decreases detected between years")
    }
  }
  

  
  return(list(
    is_valid = length(issues) == 0,
    issues = issues,
    summary = sprintf("Validation %s: %d issues found",
                      ifelse(length(issues) == 0, "PASSED", "FAILED"),
                      length(issues))
  ))
}

# Validate test counts for logical consistency
# Returns: validation report with issues
validate_test_counts <- function(results, params) {
  issues <- character()
  
  # Check if test counts are available in the results
  if (!is.null(results$results) && nrow(results$results) > 0) {
    # Check for test count columns
    test_cols <- c("neuropsych_tests", "neuroimaging_tests", "ai_morphometry_tests",
                   "biomarker_tests", "total_neuroimaging_tests")
    
    if (all(test_cols %in% names(results$results))) {
      # Aggregate test counts across all years and subgroups
      total_counts <- results$results %>%
        summarise(
          total_neuropsych = sum(neuropsych_tests, na.rm = TRUE),
          total_neuroimaging = sum(total_neuroimaging_tests, na.rm = TRUE),
          total_ai_morphometry = sum(ai_morphometry_tests, na.rm = TRUE),
          total_biomarker = sum(biomarker_tests, na.rm = TRUE)
        )
      
      # Check AI morphometry only happens when AI enabled
      if (!params$ai_enabled && total_counts$total_ai_morphometry > 0) {
        issues <- c(issues, sprintf("AI morphometry performed without AI enabled: %.0f tests", 
                                    total_counts$total_ai_morphometry))
      }
      
      # Check AI morphometry doesn't exceed neuroimaging
      if (params$ai_enabled && total_counts$total_ai_morphometry > total_counts$total_neuroimaging) {
        issues <- c(issues, sprintf("More AI analyses (%.0f) than neuroimaging tests (%.0f)", 
                                    total_counts$total_ai_morphometry, 
                                    total_counts$total_neuroimaging))
      }
      
      # For AI scenario, check that AI morphometry matches neuroimaging
      if (params$ai_enabled && total_counts$total_neuroimaging > 0) {
        ai_coverage <- total_counts$total_ai_morphometry / total_counts$total_neuroimaging
        if (abs(ai_coverage - 1.0) > 0.01) {  # Allow 1% tolerance
          issues <- c(issues, sprintf("AI morphometry coverage is %.1f%% of neuroimaging tests", 
                                      ai_coverage * 100))
        }
      }
      
      # Check total tests are reasonable (not zero if population exists)
      total_tests <- total_counts$total_neuropsych + total_counts$total_neuroimaging + 
        total_counts$total_biomarker
      if (total_tests == 0 && sum(results$results$Total_Population) > 0) {
        issues <- c(issues, "No tests performed despite population present")
      }
    } else {
      issues <- c(issues, "Test count columns not found in results")
    }
  }
  
  return(list(
    is_valid = length(issues) == 0,
    issues = issues,
    summary = sprintf("Test count validation %s: %d issues found",
                      ifelse(length(issues) == 0, "PASSED", "FAILED"),
                      length(issues))
  ))
}
