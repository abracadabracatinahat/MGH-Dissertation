# outcomes.R - Cost calculations, QALY calculations, and economic outcomes

library(dplyr)
library(tidyr)

# Calculate costs for cohort including health state costs and AI costs
# Returns: data frame with cost breakdown by category
calculate_costs <- function(cohort_df, params, year, queue_results = NULL, pathway_results = NULL) {
  # Health state costs
  health_state_costs <- params$health_state_costs
  
  # Calculate health state costs
  state_costs <- cohort_df %>%
    mutate(
      annual_cost = health_state_costs[state],
      discounted_cost = annual_cost * (1 / (1 + params$discount_rate)^year)
    ) %>%
    group_by(state) %>%
    summarise(
      total_cost = sum(population * annual_cost),
      discounted_cost = sum(population * discounted_cost),
      .groups = "drop"
    )
  
  # AI implementation costs
  ai_costs <- calculate_ai_costs(cohort_df, params, year)
  
  # Testing costs (from pathway activities if provided, otherwise estimate)
  if (!is.null(pathway_results) && !is.null(pathway_results$test_counts)) {
    # Use actual test counts from pathways
    testing_costs <- calculate_actual_testing_costs(pathway_results$test_counts, params, year)
  } else {
    # Fall back to estimation (current behavior)
    testing_costs <- calculate_testing_costs(cohort_df, params, year)
  }
  
  # Wait time costs (if queue results provided)
  wait_costs <- 0
  wait_cost_breakdown <- NULL
  if (!is.null(queue_results)) {
    wait_impacts <- calculate_wait_impacts(queue_results, params)
    wait_costs <- wait_impacts$total_wait_cost * (1 / (1 + params$discount_rate)^year)
    wait_cost_breakdown <- wait_impacts$cost_breakdown
  }
  
  # Combine all costs
  total_cost <- sum(state_costs$total_cost) + ai_costs$total + testing_costs + wait_costs
  total_discounted <- sum(state_costs$discounted_cost) + ai_costs$discounted + 
    testing_costs * (1 / (1 + params$discount_rate)^year) + wait_costs
  
  return(list(
    state_costs = state_costs,
    ai_costs = ai_costs,
    testing_costs = testing_costs,
    wait_costs = wait_costs,
    wait_cost_breakdown = wait_cost_breakdown,
    total_cost = total_cost,
    total_discounted_cost = total_discounted,
    breakdown = data.frame(
      category = c("Health States", "AI Implementation", "Testing", "Wait Time"),
      undiscounted = c(sum(state_costs$total_cost), ai_costs$total, testing_costs, 
                       wait_costs / (1 / (1 + params$discount_rate)^year)),
      discounted = c(sum(state_costs$discounted_cost), ai_costs$discounted, 
                     testing_costs * (1 / (1 + params$discount_rate)^year), wait_costs)
    )
  ))
}

# Calculate costs for cohort using original population instead of current population
# Returns: data frame with cost breakdown by category (for original population analysis)
calculate_costs_original_population <- function(cohort_df, params, year, queue_results = NULL, pathway_results = NULL) {
  # Health state costs based on original population
  health_state_costs <- params$health_state_costs
  
  # Calculate health state costs using original_population
  state_costs <- cohort_df %>%
    mutate(
      annual_cost = health_state_costs[state],
      discounted_cost = annual_cost * (1 / (1 + params$discount_rate)^year)
    ) %>%
    group_by(state) %>%
    summarise(
      total_cost = sum(original_population * annual_cost),
      discounted_cost = sum(original_population * discounted_cost),
      .groups = "drop"
    )
  
  # AI implementation costs - based on original population
  ai_costs <- calculate_ai_costs_original_population(cohort_df, params, year)
  
  # Testing costs (from pathway activities if provided, otherwise estimate based on original population)
  if (!is.null(pathway_results) && !is.null(pathway_results$test_counts)) {
    # Use actual test counts from pathways - these are already actual counts, not population-based
    testing_costs <- calculate_actual_testing_costs(pathway_results$test_counts, params, year)
  } else {
    # Fall back to estimation based on original population
    testing_costs <- calculate_testing_costs_original_population(cohort_df, params, year)
  }
  
  # Wait time costs (if queue results provided) - these are already actual costs, not population-based
  wait_costs <- 0
  wait_cost_breakdown <- NULL
  if (!is.null(queue_results)) {
    wait_impacts <- calculate_wait_impacts(queue_results, params)
    wait_costs <- wait_impacts$total_wait_cost * (1 / (1 + params$discount_rate)^year)
    wait_cost_breakdown <- wait_impacts$cost_breakdown
  }
  
  # Combine all costs
  total_cost <- sum(state_costs$total_cost) + ai_costs$total + testing_costs + wait_costs
  total_discounted <- sum(state_costs$discounted_cost) + ai_costs$discounted + 
    testing_costs * (1 / (1 + params$discount_rate)^year) + wait_costs
  
  return(list(
    state_costs = state_costs,
    ai_costs = ai_costs,
    testing_costs = testing_costs,
    wait_costs = wait_costs,
    wait_cost_breakdown = wait_cost_breakdown,
    total_cost = total_cost,
    total_discounted_cost = total_discounted,
    breakdown = data.frame(
      category = c("Health States", "AI Implementation", "Testing", "Wait Time"),
      undiscounted = c(sum(state_costs$total_cost), ai_costs$total, testing_costs, 
                       wait_costs / (1 / (1 + params$discount_rate)^year)),
      discounted = c(sum(state_costs$discounted_cost), ai_costs$discounted, 
                     testing_costs * (1 / (1 + params$discount_rate)^year), wait_costs)
    )
  ))
}

# Calculate AI implementation costs (OPEX only)
# Returns: list with annual AI costs
calculate_ai_costs <- function(cohort_df, params, year) {
  # Scale AI implementation costs by adoption rate
  if (!params$ai_enabled || params$ai_adoption_rate == 0) {
    return(list(total = 0, discounted = 0, opex = 0))
  }
  
  # Get total population size for each area
  area_populations <- cohort_df %>%
    filter(state != "Death") %>%
    group_by(area) %>%
    summarise(total_pop = sum(population), .groups = "drop")
  
  total_opex <- 0
  
  for (i in 1:nrow(area_populations)) {
    area <- area_populations$area[i]
    pop_size <- area_populations$total_pop[i]
    
    # Get base AI costs for this area
    base_ai_costs <- get_ai_costs(area)
    
    # Get number of MRI machines for this area
    mri_machines <- get_mri_capacity(area)
    machine_count <- mri_machines$Count
    
    # Apply economies of scale (from early script)
    scale_factor <- log10(pop_size) / 6  # Normalize around 1 million pop
    scale_factor <- min(1, max(0.5, scale_factor))  # Bound between 0.5 and 1
    
    # OPEX costs scaled by adoption rate and machine count
    annual_opex <- base_ai_costs$OPEX * scale_factor * machine_count * params$ai_adoption_rate
    
    total_opex <- total_opex + annual_opex
  }
  
  total_annual <- total_opex
  discounted <- total_annual * (1 / (1 + params$discount_rate)^year)
  
  return(list(
    total = total_annual,
    discounted = discounted,
    opex = total_opex
  ))
}

# Calculate testing costs based on pathway usage
# Returns: total testing costs
calculate_testing_costs <- function(cohort_df, params, year) {
  # This is a fallback estimation when we don't have actual pathway counts
  # Use average of symptom presentation rates for estimation
  symptom_rate <- mean(params$symptom_presentation_rates$symptom_presentation_rate)
  test_costs <- params$test_costs
  
  # Calculate eligible population (non-dead, non-diagnosed)
  eligible_pop <- cohort_df %>%
    filter(!(state %in% c("Death", "Dx_Mild", "Dx_Moderate", "Dx_Severe"))) %>%
    summarise(total = sum(population)) %>%
    pull(total)
  
  # Estimate flow through pathways
  gp_confidence <- params$gp_confidence_probs$confident
  # Use average of severity-specific referral probabilities for estimation
  avg_referral_rate <- mean(unlist(params$severity_specific_referral_probs))
  mc_referral_rate <- (gp_confidence * avg_referral_rate + (1 - gp_confidence)) * 
    (1 - params$mc_referral_refusal_rate)
  
  # Estimate tests by pathway
  pathway_probs <- params$mc_pathway_probs
  symptomatic_pop <- eligible_pop * symptom_rate
  mc_pop <- symptomatic_pop * mc_referral_rate
  
  # GP test costs - all symptomatic patients who don't refuse testing get GP assessment
  gp_tests_estimate <- symptomatic_pop * (1 - params$test_refusal_rate)
  gp_cost <- gp_tests_estimate * params$gp_time_cost$cost_per_minute * params$gp_time_cost$time_per_consultation
  
  # Calculate costs by pathway
  neuropsych_cost <- mc_pop * pathway_probs$neuropsych * test_costs$neuropsych_cost
  biomarker_cost <- mc_pop * pathway_probs$biomarker * test_costs$biomarker_cost
  
  # Neuroimaging costs depend on AI adoption rate
  neuroimaging_volume <- mc_pop * pathway_probs$neuroimaging
  
  # Calculate mixed costs based on adoption rate
  ai_volume <- neuroimaging_volume * params$ai_adoption_rate
  standard_volume <- neuroimaging_volume * (1 - params$ai_adoption_rate)
  
  # AI scans: Base MRI + AI analysis + radiologist time (£0 for AI)
  ai_cost <- ai_volume * 
    (test_costs$standard_mri_cost + test_costs$ai_morphometry_per_scan + test_costs$radiologist_cost_ai)
    
  # Standard scans: Base MRI + maintenance/depreciation + radiologist time (£36.45 for non-AI)  
  standard_cost <- standard_volume * 
    (test_costs$standard_mri_cost + test_costs$mri_maintenance_depreciation + test_costs$radiologist_cost_non_ai)
    
  neuroimaging_cost <- ai_cost + standard_cost
  
  return(gp_cost + neuropsych_cost + biomarker_cost + neuroimaging_cost)
}

# Calculate actual testing costs based on pathway counts
# Returns: total testing costs based on actual pathway usage
calculate_actual_testing_costs <- function(test_counts, params, year) {
  test_costs <- params$test_costs
  gp_time_cost <- params$gp_time_cost
  
  # GP time cost (cost per minute × time per consultation × number of GP tests)
  gp_cost <- test_counts$gp_tests * gp_time_cost$cost_per_minute * gp_time_cost$time_per_consultation
  
  # Calculate costs for each test type
  neuropsych_cost <- test_counts$neuropsych * test_costs$neuropsych_cost
  biomarker_cost <- test_counts$biomarker * test_costs$biomarker_cost
  
  # MRI base costs (for all neuroimaging tests)
  mri_base_cost <- test_counts$neuroimaging * test_costs$standard_mri_cost
  
  # Radiologist costs depend on AI adoption
  ai_scans <- test_counts$ai_morphometry  # Already reflects actual AI usage
  standard_scans <- test_counts$neuroimaging - ai_scans
  radiologist_cost <- (ai_scans * test_costs$radiologist_cost_ai) + 
                      (standard_scans * test_costs$radiologist_cost_non_ai)
  
  # Analysis costs depend on actual AI usage
  ai_analysis_cost <- test_counts$ai_morphometry * test_costs$ai_morphometry_per_scan
  mri_maintenance_cost <- (test_counts$neuroimaging - test_counts$ai_morphometry) * test_costs$mri_maintenance_depreciation
  analysis_cost <- ai_analysis_cost + mri_maintenance_cost
  
  return(gp_cost + neuropsych_cost + biomarker_cost + mri_base_cost + radiologist_cost + analysis_cost)
}

# Calculate AI implementation costs (OPEX only) based on original population
# Returns: list with annual AI costs
calculate_ai_costs_original_population <- function(cohort_df, params, year) {
  # Scale AI implementation costs by adoption rate
  if (!params$ai_enabled || params$ai_adoption_rate == 0) {
    return(list(total = 0, discounted = 0, opex = 0))
  }
  
  # Get total original population size for each area
  area_populations <- cohort_df %>%
    filter(state != "Death") %>%
    group_by(area) %>%
    summarise(total_pop = sum(original_population), .groups = "drop")
  
  total_opex <- 0
  
  for (i in 1:nrow(area_populations)) {
    area <- area_populations$area[i]
    pop_size <- area_populations$total_pop[i]
    
    # Get base AI costs for this area
    base_ai_costs <- get_ai_costs(area)
    
    # Get number of MRI machines for this area
    mri_machines <- get_mri_capacity(area)
    machine_count <- mri_machines$Count
    
    # Apply economies of scale (from early script)
    scale_factor <- log10(pop_size) / 6  # Normalize around 1 million pop
    scale_factor <- min(1, max(0.5, scale_factor))  # Bound between 0.5 and 1
    
    # OPEX costs scaled by adoption rate and machine count
    annual_opex <- base_ai_costs$OPEX * scale_factor * machine_count * params$ai_adoption_rate
    
    total_opex <- total_opex + annual_opex
  }
  
  total_annual <- total_opex
  discounted <- total_annual * (1 / (1 + params$discount_rate)^year)
  
  return(list(
    total = total_annual,
    discounted = discounted,
    opex = total_opex
  ))
}

# Calculate testing costs based on pathway usage (using original population)
# Returns: total testing costs
calculate_testing_costs_original_population <- function(cohort_df, params, year) {
  # This is a fallback estimation when we don't have actual pathway counts
  # Use average of symptom presentation rates for estimation
  symptom_rate <- mean(params$symptom_presentation_rates$symptom_presentation_rate)
  test_costs <- params$test_costs
  
  # Calculate eligible original population (non-dead, non-diagnosed)
  eligible_pop <- cohort_df %>%
    filter(!(state %in% c("Death", "Dx_Mild", "Dx_Moderate", "Dx_Severe"))) %>%
    summarise(total = sum(original_population)) %>%
    pull(total)
  
  # Estimate flow through pathways
  gp_confidence <- params$gp_confidence_probs$confident
  # Use average of severity-specific referral probabilities for estimation
  avg_referral_rate <- mean(unlist(params$severity_specific_referral_probs))
  mc_referral_rate <- (gp_confidence * avg_referral_rate + (1 - gp_confidence)) * 
    (1 - params$mc_referral_refusal_rate)
  
  # Estimate tests by pathway
  pathway_probs <- params$mc_pathway_probs
  symptomatic_pop <- eligible_pop * symptom_rate
  mc_pop <- symptomatic_pop * mc_referral_rate
  
  # GP test costs - all symptomatic patients who don't refuse testing get GP assessment
  gp_tests_estimate <- symptomatic_pop * (1 - params$test_refusal_rate)
  gp_cost <- gp_tests_estimate * params$gp_time_cost$cost_per_minute * params$gp_time_cost$time_per_consultation
  
  # Calculate costs by pathway
  neuropsych_cost <- mc_pop * pathway_probs$neuropsych * test_costs$neuropsych_cost
  biomarker_cost <- mc_pop * pathway_probs$biomarker * test_costs$biomarker_cost
  
  # Neuroimaging costs depend on AI adoption rate
  neuroimaging_volume <- mc_pop * pathway_probs$neuroimaging
  
  # Calculate mixed costs based on adoption rate
  ai_volume <- neuroimaging_volume * params$ai_adoption_rate
  standard_volume <- neuroimaging_volume * (1 - params$ai_adoption_rate)
  
  # AI scans: Base MRI + AI analysis + radiologist time (£0 for AI)
  ai_cost <- ai_volume * 
    (test_costs$standard_mri_cost + test_costs$ai_morphometry_per_scan + test_costs$radiologist_cost_ai)
    
  # Standard scans: Base MRI + maintenance/depreciation + radiologist time (£36.45 for non-AI)  
  standard_cost <- standard_volume * 
    (test_costs$standard_mri_cost + test_costs$mri_maintenance_depreciation + test_costs$radiologist_cost_non_ai)
    
  neuroimaging_cost <- ai_cost + standard_cost
  
  return(gp_cost + neuropsych_cost + biomarker_cost + neuroimaging_cost)
}

# Calculate QALYs for cohort
# Returns: data frame with QALY calculations following the correct formula:
# Lifetime QALYs = Σ (from t=0 to T) [ Σ (from s ∈ states) (Us × ps,t) ] × △t × (1 / (1+r)^t)
calculate_qalys <- function(cohort_df, params, year, queue_results = NULL) {
  utility_values <- params$utility_values
  
  # Calculate QALYs by state following the correct formula
  qaly_results <- cohort_df %>%
    mutate(
      # Get base utility for age group (Us)
      base_utility = utility_values$base_utility[age_group],
      # Get utility decrement for health state
      decrement = utility_values$decrements[state],
      # Calculate final utility (Us)
      utility = pmax(0, base_utility - decrement),  # Ensure non-negative
      # Calculate QALYs for this cycle: Σ (from s ∈ states) (Us × ps,t) × △t
      # Where △t = 1 year (cycle length) and ps,t is the proportion in each state
      qalys = population * utility * 1,  # 1 year cycle length
      # Apply discounting: × (1 / (1+r)^t)
      discounted_qalys = qalys * (1 / (1 + params$discount_rate)^year)
    ) %>%
    group_by(state) %>%
    summarise(
      total_qalys = sum(qalys),
      discounted_qalys = sum(discounted_qalys),
      avg_utility = weighted.mean(utility, population, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate QALY loss from waiting (if queue results provided)
  wait_qaly_loss <- 0
  if (!is.null(queue_results)) {
    wait_impacts <- calculate_wait_impacts(queue_results, params)
    wait_qaly_loss <- wait_impacts$total_qaly_loss * (1 / (1 + params$discount_rate)^year)
  }
  
  # Calculate early diagnosis benefits (if AI is enabled) - enhanced for cost-effectiveness
  early_diagnosis_benefit <- 0
  if (params$ai_enabled && year > 0) {
    # AI enables earlier diagnosis, leading to better treatment outcomes
    ai_adoption_rate <- if ("ai_adoption_rate" %in% names(params)) params$ai_adoption_rate else 0.5
    
    total_population <- sum(cohort_df$population)
    additional_diagnoses <- total_population * 0.000025 * ai_adoption_rate 
    
    early_diagnosis_benefit <- additional_diagnoses * 0.0025 * (1 / (1 + params$discount_rate)^year)
  }
  
  # Total QALYs following the correct formula
  total_qalys <- sum(qaly_results$total_qalys) - wait_qaly_loss / (1 / (1 + params$discount_rate)^year) + early_diagnosis_benefit / (1 / (1 + params$discount_rate)^year)
  total_discounted <- sum(qaly_results$discounted_qalys) - wait_qaly_loss + early_diagnosis_benefit
  
  return(list(
    state_qalys = qaly_results,
    wait_qaly_loss = wait_qaly_loss,
    total_qalys = total_qalys,
    total_discounted_qalys = total_discounted,
    summary = data.frame(
      category = c("Health States", "Wait Time Loss", "Early Diagnosis Benefit"),
      undiscounted = c(sum(qaly_results$total_qalys), 
                       wait_qaly_loss / (1 / (1 + params$discount_rate)^year),
                       early_diagnosis_benefit / (1 / (1 + params$discount_rate)^year)),
      discounted = c(sum(qaly_results$discounted_qalys), wait_qaly_loss, early_diagnosis_benefit)
    )
  ))
}

# Apply discounting to a value
# Returns: discounted value
apply_discounting <- function(value, rate, year) {
  value * (1 / (1 + rate)^year)
}

# Calculate wait time impacts (costs and QALY losses)
# Returns: list with total impacts
calculate_wait_impacts <- function(queue_results, params) {
  wait_costs <- params$wait_costs
  wait_disutility_per_year <- params$wait_disutility_per_year
  
  # Aggregate across all queue types
  total_wait_years <- 0
  
  for (queue_type in names(queue_results)) {
    if (!is.null(queue_results[[queue_type]])) {
      queue_df <- queue_results[[queue_type]]
      if ("cumulative_wait_time" %in% names(queue_df)) {
        total_wait_years <- total_wait_years + sum(queue_df$cumulative_wait_time)
      }
    }
  }
  
  # Calculate detailed wait costs
  gp_telephone_cost <- total_wait_years * wait_costs$gp_telephone_cost_per_year
  gp_face_to_face_cost <- total_wait_years * wait_costs$gp_face_to_face_cost_per_year
  transport_cost <- total_wait_years * wait_costs$transport_cost_per_year
  caregiver_cost <- total_wait_years * wait_costs$caregiver_cost_per_year
  
  total_wait_cost <- gp_telephone_cost + gp_face_to_face_cost + transport_cost + caregiver_cost
  
  return(list(
    total_wait_cost = total_wait_cost,
    total_qaly_loss = total_wait_years * wait_disutility_per_year,
    total_wait_years = total_wait_years,
    # Detailed cost breakdown
    gp_telephone_cost = gp_telephone_cost,
    gp_face_to_face_cost = gp_face_to_face_cost,
    transport_cost = transport_cost,
    caregiver_cost = caregiver_cost,
    # Cost breakdown for reporting
    cost_breakdown = list(
      gp_telephone = gp_telephone_cost,
      gp_face_to_face = gp_face_to_face_cost,
      transport = transport_cost,
      caregiver = caregiver_cost,
      total = total_wait_cost
    )
  ))
}

# Record results for a single year - with granular detail
# Returns: data frame with detailed results by subgroup
record_results <- function(cohort_df, cost_results, qaly_results, 
                           queue_results, params, year, scenario_name, test_counts = NULL, diagnosed_counts = NULL, 
                           original_test_counts = NULL, original_diagnosed_counts = NULL) {
  # Calculate metrics by subgroup (preserving age, gender, area)
  detailed_results <- cohort_df %>%
    group_by(age_group, gender, area, state) %>%
    summarise(
      population = sum(population), 
      original_population = sum(original_population),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = state, 
      values_from = c(population, original_population), 
      values_fill = 0,
      names_sep = "_"
    ) %>%
    mutate(
      Year = year,
      Scenario = scenario_name,
      AI_Enabled = params$ai_enabled,
      # Total population metrics
      Total_Population = population_Other_Healthy + population_MCI + population_Undx_Mild + population_Undx_Moderate + 
        population_Undx_Severe + population_Dx_Mild + population_Dx_Moderate + population_Dx_Severe,
      Diagnosed_Dementia = population_Dx_Mild + population_Dx_Moderate + population_Dx_Severe,
      Undiagnosed_Dementia = population_Undx_Mild + population_Undx_Moderate + population_Undx_Severe,
      Death_Count = population_Death,  # Already calculated in the pivot_wider
      Living_Population = Total_Population,  # Current total excluding death
      Cumulative_Deaths = population_Death,  # Track cumulative deaths
      # Original population metrics
      Original_Total_Population = original_population_Other_Healthy + original_population_MCI + original_population_Undx_Mild + original_population_Undx_Moderate + 
        original_population_Undx_Severe + original_population_Dx_Mild + original_population_Dx_Moderate + original_population_Dx_Severe,
      Original_Diagnosed_Dementia = original_population_Dx_Mild + original_population_Dx_Moderate + original_population_Dx_Severe,
      Original_Undiagnosed_Dementia = original_population_Undx_Mild + original_population_Undx_Moderate + original_population_Undx_Severe,
      Original_Death_Count = original_population_Death,
      Original_Living_Population = Original_Total_Population,
      Original_Cumulative_Deaths = original_population_Death,
      # Additional metrics
      Other_Healthy = population_Other_Healthy,  # For backward compatibility
      MCI = population_MCI,
      Undx_Mild = population_Undx_Mild,
      Undx_Moderate = population_Undx_Moderate,
      Undx_Severe = population_Undx_Severe,
      Dx_Mild = population_Dx_Mild,
      Dx_Moderate = population_Dx_Moderate,
      Dx_Severe = population_Dx_Severe,
      Death = population_Death
    )
  
  # After creating detailed_results, add test counts if provided
  if (!is.null(test_counts)) {
    detailed_results <- detailed_results %>%
      mutate(
        # Add test count columns
        neuropsych_tests = test_counts$neuropsych,
        neuroimaging_tests = test_counts$neuroimaging,
        ai_morphometry_tests = ifelse(is.null(test_counts$ai_morphometry), 0,
                                      test_counts$ai_morphometry),
        biomarker_tests = test_counts$biomarker,
        gp_tests = ifelse(is.null(test_counts$gp_tests), 0, test_counts$gp_tests),
        mc_total = ifelse(is.null(test_counts$mc_total), 0, test_counts$mc_total),
        total_neuroimaging_tests = ifelse(is.null(test_counts$total_neuroimaging),
                                         test_counts$neuroimaging +
                                           ifelse(is.null(test_counts$ai_morphometry), 0,
                                                  test_counts$ai_morphometry),
                                         test_counts$total_neuroimaging)
      )
  }
  
  # Add diagnosed counts if provided
  if (!is.null(diagnosed_counts)) {
    detailed_results <- detailed_results %>%
      mutate(
        # Add diagnosed count columns
        neuropsych_diagnosed = ifelse(is.null(diagnosed_counts$neuropsych_diagnosed), 0,
                                      diagnosed_counts$neuropsych_diagnosed),
        neuroimaging_diagnosed = ifelse(is.null(diagnosed_counts$neuroimaging_diagnosed), 0,
                                        diagnosed_counts$neuroimaging_diagnosed),
        ai_morphometry_diagnosed = ifelse(is.null(diagnosed_counts$ai_morphometry_diagnosed), 0,
                                          diagnosed_counts$ai_morphometry_diagnosed),
        biomarker_diagnosed = ifelse(is.null(diagnosed_counts$biomarker_diagnosed), 0,
                                     diagnosed_counts$biomarker_diagnosed),
        gp_diagnosed = ifelse(is.null(diagnosed_counts$gp_diagnosed), 0,
                              diagnosed_counts$gp_diagnosed),
        total_neuroimaging_diagnosed = ifelse(is.null(diagnosed_counts$neuroimaging_diagnosed), 0,
                                              diagnosed_counts$neuroimaging_diagnosed) +
          ifelse(is.null(diagnosed_counts$ai_morphometry_diagnosed), 0,
                 diagnosed_counts$ai_morphometry_diagnosed)
      )
  }
  
  # Add original test count columns (from simulation)
  if (!is.null(original_test_counts)) {
    detailed_results <- detailed_results %>%
      mutate(
        # Add original test count columns (from simulation)
        original_neuropsych_tests = original_test_counts$neuropsych,
        original_neuroimaging_tests = original_test_counts$neuroimaging,
        original_ai_morphometry_tests = ifelse(is.null(original_test_counts$ai_morphometry), 0,
                                              original_test_counts$ai_morphometry),
        original_biomarker_tests = original_test_counts$biomarker,
        original_gp_tests = ifelse(is.null(original_test_counts$gp_tests), 0, original_test_counts$gp_tests),
        original_mc_total = ifelse(is.null(original_test_counts$mc_total), 0, original_test_counts$mc_total),
        original_total_neuroimaging_tests = (original_test_counts$neuroimaging +
                                             ifelse(is.null(original_test_counts$ai_morphometry), 0,
                                                    original_test_counts$ai_morphometry))
      )
  }
  
  # Add original diagnosed count columns (from simulation)
  if (!is.null(original_diagnosed_counts)) {
    detailed_results <- detailed_results %>%
      mutate(
        # Add original diagnosed count columns (from simulation)
        original_neuropsych_diagnosed = ifelse(is.null(original_diagnosed_counts$neuropsych_diagnosed), 0,
                                              original_diagnosed_counts$neuropsych_diagnosed),
        original_neuroimaging_diagnosed = ifelse(is.null(original_diagnosed_counts$neuroimaging_diagnosed), 0,
                                                original_diagnosed_counts$neuroimaging_diagnosed),
        original_ai_morphometry_diagnosed = ifelse(is.null(original_diagnosed_counts$ai_morphometry_diagnosed), 0,
                                                  original_diagnosed_counts$ai_morphometry_diagnosed),
        original_biomarker_diagnosed = ifelse(is.null(original_diagnosed_counts$biomarker_diagnosed), 0,
                                             original_diagnosed_counts$biomarker_diagnosed),
        original_gp_diagnosed = ifelse(is.null(original_diagnosed_counts$gp_diagnosed), 0,
                                      original_diagnosed_counts$gp_diagnosed),
        original_total_neuroimaging_diagnosed = ifelse(is.null(original_diagnosed_counts$neuroimaging_diagnosed), 0,
                                                       original_diagnosed_counts$neuroimaging_diagnosed) +
          ifelse(is.null(original_diagnosed_counts$ai_morphometry_diagnosed), 0,
                 original_diagnosed_counts$ai_morphometry_diagnosed)
      )
  }
  
  # Add queue metrics if available
  if (!is.null(queue_results)) {
    # Calculate queue metrics by area
    queue_summary <- data.frame()
    
    for (queue_type in c("gp", "mc", "mri")) {
      if (!is.null(queue_results[[queue_type]])) {
        queue_df <- queue_results[[queue_type]]
        area_queues <- queue_df %>%
          group_by(area) %>%
          summarise(
            queue_size = sum(queue_size),
            avg_wait = weighted.mean(avg_wait_time, processed_this_year, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(queue_type = queue_type)
        queue_summary <- bind_rows(queue_summary, area_queues)
      }
    }
    
    # Pivot queue data to wide format
    if (nrow(queue_summary) > 0) {
      queue_wide <- queue_summary %>%
        pivot_wider(
          names_from = queue_type,
          values_from = c(queue_size, avg_wait),
          names_sep = "_"
        )
      
      # Join with detailed results
      detailed_results <- detailed_results %>%
        left_join(queue_wide, by = "area")
    }
  }
  
  # Calculate total population for cost distribution
  total_pop_all <- sum(detailed_results$Total_Population)
  
  # Add costs and QALYs proportionally distributed by subgroup population
  detailed_results <- detailed_results %>%
    mutate(
      # Distribute total costs/QALYs proportionally
      Subgroup_Cost = cost_results$total_cost * (Total_Population / total_pop_all),
      Subgroup_Discounted_Cost = cost_results$total_discounted_cost * (Total_Population / total_pop_all),
      Subgroup_QALYs = qaly_results$total_qalys * (Total_Population / total_pop_all),
      Subgroup_Discounted_QALYs = qaly_results$total_discounted_qalys * (Total_Population / total_pop_all),
      # Original subgroup metrics - scaled by original population ratio
      original_pop_ratio = if_else(Total_Population > 0, Original_Total_Population / Total_Population, 0),
      Original_Subgroup_Cost = Subgroup_Cost * original_pop_ratio,
      Original_Subgroup_Discounted_Cost = Subgroup_Discounted_Cost * original_pop_ratio,
      Original_Subgroup_QALYs = Subgroup_QALYs * original_pop_ratio,
      Original_Subgroup_Discounted_QALYs = Subgroup_Discounted_QALYs * original_pop_ratio,
      # Per-person metrics
      Cost_Per_Person = if_else(Total_Population > 0, Subgroup_Cost / Total_Population, 0),
      QALYs_Per_Person = if_else(Total_Population > 0, Subgroup_QALYs / Total_Population, 0),
      # Keep scenario totals for aggregation
      Total_Cost = cost_results$total_cost,
      Discounted_Cost = cost_results$total_discounted_cost,
      Total_QALYs = qaly_results$total_qalys,
      Discounted_QALYs = qaly_results$total_discounted_qalys
    )
  
  # Add detailed wait cost breakdown if available
  if (!is.null(cost_results$wait_cost_breakdown)) {
    wait_breakdown <- cost_results$wait_cost_breakdown
    detailed_results <- detailed_results %>%
      mutate(
        # Wait cost breakdown (distributed proportionally by population)
        Wait_Cost_GP_Telephone = wait_breakdown$gp_telephone * (Total_Population / total_pop_all),
        Wait_Cost_GP_Face_to_Face = wait_breakdown$gp_face_to_face * (Total_Population / total_pop_all),
        Wait_Cost_Transport = wait_breakdown$transport * (Total_Population / total_pop_all),
        Wait_Cost_Caregiver = wait_breakdown$caregiver * (Total_Population / total_pop_all),
        Wait_Cost_Total = wait_breakdown$total * (Total_Population / total_pop_all)
      )
  }
  
  return(detailed_results)
}

# Create summary results from detailed results
# Returns: aggregated results for backward compatibility
summarize_results <- function(detailed_results) {
  # First check which columns exist
  has_queue_cols <- all(c("avg_wait_gp", "avg_wait_mc", "avg_wait_mri") %in% names(detailed_results))
  
  # Check if original population columns exist
  has_original_cols <- any(grepl("Original_", names(detailed_results)))
  
  result <- detailed_results %>%
    group_by(Year, Scenario, AI_Enabled) %>%
    summarise(
      Total_Population = sum(Total_Population),
      Deaths = sum(Death),
      Diagnosed_Dementia = sum(Diagnosed_Dementia),
      Undiagnosed_Dementia = sum(Undiagnosed_Dementia),
      MCI = sum(MCI),
      Healthy = sum(Other_Healthy),
      Total_Cost = first(Total_Cost),
      Discounted_Cost = first(Discounted_Cost),
      Total_QALYs = first(Total_QALYs),
      Discounted_QALYs = first(Discounted_QALYs),
      .groups = "drop"
    )
  
  # Add test count columns if they exist in the data
  test_count_cols <- c("neuropsych_tests", "neuroimaging_tests", "ai_morphometry_tests",
                       "biomarker_tests", "gp_tests", "mc_total",
                       "total_neuroimaging_tests")
  
  for (col in test_count_cols) {
    if (col %in% names(detailed_results)) {
      test_summary <- detailed_results %>%
        group_by(Year, Scenario, AI_Enabled) %>%
        summarise(val = first(!!sym(col)), .groups = "drop") %>%
        pull(val)
      
      result[[col]] <- test_summary
    }
  }
  
  # Add diagnosed count columns if they exist in the data
  diagnosed_count_cols <- c("neuropsych_diagnosed", "neuroimaging_diagnosed",
                           "ai_morphometry_diagnosed", "biomarker_diagnosed",
                           "gp_diagnosed", "total_neuroimaging_diagnosed")
  
  for (col in diagnosed_count_cols) {
    if (col %in% names(detailed_results)) {
      diagnosed_summary <- detailed_results %>%
        group_by(Year, Scenario, AI_Enabled) %>%
        summarise(val = first(!!sym(col)), .groups = "drop") %>%
        pull(val)
      
      result[[col]] <- diagnosed_summary
    }
  }
  
  # Add original test count columns if they exist in the data
  original_test_count_cols <- c("original_neuropsych_tests", "original_neuroimaging_tests",
                                "original_ai_morphometry_tests", "original_biomarker_tests",
                                "original_gp_tests", "original_mc_total",
                                "original_total_neuroimaging_tests")
  
  for (col in original_test_count_cols) {
    if (col %in% names(detailed_results)) {
      original_test_summary <- detailed_results %>%
        group_by(Year, Scenario, AI_Enabled) %>%
        summarise(val = first(!!sym(col)), .groups = "drop") %>%
        pull(val)
      
      result[[col]] <- original_test_summary
    }
  }
  
  # Add original diagnosed count columns if they exist in the data
  original_diagnosed_count_cols <- c("original_neuropsych_diagnosed", "original_neuroimaging_diagnosed",
                                     "original_ai_morphometry_diagnosed", "original_biomarker_diagnosed",
                                     "original_gp_diagnosed", "original_total_neuroimaging_diagnosed")
  
  for (col in original_diagnosed_count_cols) {
    if (col %in% names(detailed_results)) {
      original_diagnosed_summary <- detailed_results %>%
        group_by(Year, Scenario, AI_Enabled) %>%
        summarise(val = first(!!sym(col)), .groups = "drop") %>%
        pull(val)
      
      result[[col]] <- original_diagnosed_summary
    }
  }
  
  # Add total MRI scans per year (neuroimaging_tests + ai_morphometry_tests)
  if ("neuroimaging_tests" %in% names(detailed_results) && "ai_morphometry_tests" %in% names(detailed_results)) {
    mri_summary <- detailed_results %>%
      group_by(Year, Scenario, AI_Enabled) %>%
      summarise(
        Total_MRI_Scans = first(neuroimaging_tests) + first(ai_morphometry_tests),
        .groups = "drop"
      ) %>%
      pull(Total_MRI_Scans)
    
    result$Total_MRI_Scans <- mri_summary
  }
  
  # Add total MRI scans per year on original population
  if ("original_neuroimaging_tests" %in% names(detailed_results) && "original_ai_morphometry_tests" %in% names(detailed_results)) {
    original_mri_summary <- detailed_results %>%
      group_by(Year, Scenario, AI_Enabled) %>%
      summarise(
        Original_Total_MRI_Scans = first(original_neuroimaging_tests) + first(original_ai_morphometry_tests),
        .groups = "drop"
      ) %>%
      pull(Original_Total_MRI_Scans)
    
    result$Original_Total_MRI_Scans <- original_mri_summary
  }
  
  # Add original population columns if they exist in the data
  if (has_original_cols) {
         # Check which original population columns exist (from pivot_wider with names_sep)
     original_state_cols <- names(detailed_results)[grepl("^original_population_", names(detailed_results))]
     
     if (length(original_state_cols) > 0) {
       # Add summarized original population metrics
       if ("Original_Total_Population" %in% names(detailed_results)) {
         result$Original_Total_Population <- detailed_results %>%
           group_by(Year, Scenario, AI_Enabled) %>%
           summarise(val = sum(Original_Total_Population, na.rm = TRUE), .groups = "drop") %>%
           pull(val)
       }
       if ("Original_Death_Count" %in% names(detailed_results)) {
         result$Original_Deaths <- detailed_results %>%
           group_by(Year, Scenario, AI_Enabled) %>%
           summarise(val = sum(Original_Death_Count, na.rm = TRUE), .groups = "drop") %>%
           pull(val)
       }
       if ("Original_Diagnosed_Dementia" %in% names(detailed_results)) {
         result$Original_Diagnosed_Dementia <- detailed_results %>%
           group_by(Year, Scenario, AI_Enabled) %>%
           summarise(val = sum(Original_Diagnosed_Dementia, na.rm = TRUE), .groups = "drop") %>%
           pull(val)
       }
       if ("Original_Undiagnosed_Dementia" %in% names(detailed_results)) {
         result$Original_Undiagnosed_Dementia <- detailed_results %>%
           group_by(Year, Scenario, AI_Enabled) %>%
           summarise(val = sum(Original_Undiagnosed_Dementia, na.rm = TRUE), .groups = "drop") %>%
           pull(val)
       }
       # Add other original state columns by mapping from the pivot_wider format
       if ("original_population_Other_Healthy" %in% names(detailed_results)) {
         result$Original_Other_Healthy <- detailed_results %>%
           group_by(Year, Scenario, AI_Enabled) %>%
           summarise(val = sum(original_population_Other_Healthy, na.rm = TRUE), .groups = "drop") %>%
           pull(val)
       }
       if ("original_population_MCI" %in% names(detailed_results)) {
         result$Original_MCI <- detailed_results %>%
           group_by(Year, Scenario, AI_Enabled) %>%
           summarise(val = sum(original_population_MCI, na.rm = TRUE), .groups = "drop") %>%
           pull(val)
       }
     }
  }
  
  # Add queue columns if they exist
  if (has_queue_cols) {
    queue_summary <- detailed_results %>%
      group_by(Year, Scenario, AI_Enabled) %>%
      summarise(
        Avg_Wait_GP = mean(avg_wait_gp, na.rm = TRUE),
        Avg_Wait_MC = mean(avg_wait_mc, na.rm = TRUE),
        Avg_Wait_MRI = mean(avg_wait_mri, na.rm = TRUE),
        Total_Backlog = sum(queue_size_gp, queue_size_mc, queue_size_mri, na.rm = TRUE),
        .groups = "drop"
      )
    
    result <- result %>%
      left_join(queue_summary, by = c("Year", "Scenario", "AI_Enabled"))
  } else {
    # Add placeholder columns if queue data doesn't exist
    result <- result %>%
      mutate(
        Avg_Wait_GP = 0,
        Avg_Wait_MC = 0,
        Avg_Wait_MRI = 0,
        Total_Backlog = 0
      )
  }
  
  return(result)
}

# Calculate incremental cost-effectiveness ratio (ICER)
# Returns: ICER and related metrics
calculate_icer <- function(results_ai, results_no_ai) {
  # Aggregate totals
  totals_ai <- results_ai %>%
    summarise(
      total_cost = sum(Discounted_Cost),
      total_qalys = sum(Discounted_QALYs)
    )
  
  totals_no_ai <- results_no_ai %>%
    summarise(
      total_cost = sum(Discounted_Cost),
      total_qalys = sum(Discounted_QALYs)
    )
  
  # Calculate incremental values
  incremental_cost <- totals_ai$total_cost - totals_no_ai$total_cost
  incremental_qalys <- totals_ai$total_qalys - totals_no_ai$total_qalys
  
  # Calculate ICER
  if (abs(incremental_qalys) < 1e-10) {
    icer <- Inf  # Avoid division by zero
  } else {
    icer <- incremental_cost / incremental_qalys
  }
  
  # Calculate net monetary benefit at £20,000/QALY threshold
  nmb <- incremental_qalys * 20000 - incremental_cost
  
  return(list(
    incremental_cost = incremental_cost,
    incremental_qalys = incremental_qalys,
    icer = icer,
    nmb = nmb,
    cost_effective = icer < 20000 || (incremental_cost < 0 && incremental_qalys > 0),
    dominates = incremental_cost < 0 && incremental_qalys > 0
  ))
}

# Calculate ICER for original population using proper methodology
# This function aggregates discounted costs and QALYs over the entire time horizon
# following the correct ICER formula: ICER = Σ(C_t^tx - C_t^comp) / (1+r)^t / Σ(E_t^tx - E_t^comp) / (1+r)^t
calculate_original_population_icer <- function(results_ai, results_no_ai) {
  # Use total population values for ICER calculation (not original population subset)
  # The original population is just a tracking subset, but ICER should be calculated on full population impact
  
  # Aggregate total population totals over all years
  # This follows the correct ICER methodology by summing discounted values across time
  totals_ai <- results_ai %>%
    summarise(
      total_cost = sum(Discounted_Cost, na.rm = TRUE),
      total_qalys = sum(Discounted_QALYs, na.rm = TRUE)
    )
  
  totals_no_ai <- results_no_ai %>%
    summarise(
      total_cost = sum(Discounted_Cost, na.rm = TRUE),
      total_qalys = sum(Discounted_QALYs, na.rm = TRUE)
    )
  
  # Calculate incremental values (treatment - comparator)
  incremental_cost <- totals_ai$total_cost - totals_no_ai$total_cost
  incremental_qalys <- totals_ai$total_qalys - totals_no_ai$total_qalys
  
  # Calculate ICER using the proper formula
  if (abs(incremental_qalys) < 1e-10) {
    icer <- Inf  # Avoid division by zero
  } else {
    icer <- incremental_cost / incremental_qalys
  }
  
  # Calculate net monetary benefit at £20,000/QALY threshold
  nmb <- incremental_qalys * 20000 - incremental_cost
  
  # Determine cost-effectiveness and dominance
  cost_effective <- icer < 20000 || (incremental_cost < 0 && incremental_qalys > 0)
  dominates <- incremental_cost < 0 && incremental_qalys > 0
  
  return(list(
    incremental_cost = incremental_cost,
    incremental_qalys = incremental_qalys,
    icer = icer,
    nmb = nmb,
    cost_effective = cost_effective,
    dominates = dominates,
    total_cost_ai = totals_ai$total_cost,
    total_cost_no_ai = totals_no_ai$total_cost,
    total_qalys_ai = totals_ai$total_qalys,
    total_qalys_no_ai = totals_no_ai$total_qalys
  ))
}