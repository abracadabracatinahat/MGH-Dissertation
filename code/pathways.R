# pathways.R - GP confidence pathway and Memory clinic decision tree

library(dplyr)
library(tidyr)

# Calculate who presents with symptoms needing assessment
# Returns: data frame with symptomatic and asymptomatic populations
calculate_symptomatic_presentation <- function(cohort_df, params) {
  # Join with health state and age-specific symptom presentation rates
  cohort_df %>%
    left_join(params$symptom_presentation_rates, by = c("state", "age_group")) %>%
    mutate(
      # Use state/age-specific rate
      symptom_rate = symptom_presentation_rate,
      # Only non-dead, non-diagnosed people present with symptoms
      can_present = !(state %in% c("Death", "Dx_Mild", "Dx_Moderate", "Dx_Severe")),
      symptomatic_pop = if_else(can_present, population * symptom_rate, 0),
      asymptomatic_pop = if_else(can_present, population * (1 - symptom_rate), 0),
      # Track the remaining population in original state
      remaining_pop = population - symptomatic_pop
    ) %>%
    select(-can_present, -symptom_presentation_rate, -symptom_rate)
}

# Run GP pathway with confidence split and test refusal
# Returns: list with diagnosis flags, MC referrals, and updated queue
run_gp_pathway <- function(symptomatic_df, gp_queue_df, params) {
  # Step 1: Prepare demand (symptomatic patients)
  demand_df <- symptomatic_df %>%
    filter(symptomatic_pop > 0) %>%
    select(age_group, gender, area, state, population = symptomatic_pop)
  
  # Step 2: Add demand to queue and process
  gp_queue_df <- add_to_queue(gp_queue_df, demand_df, "gp")
  gp_results <- process_all_queues(gp_queue_df, params)
  gp_queue_df <- gp_results$updated_queue
  processed_pop <- gp_results$processed_population
  
  if (nrow(processed_pop) == 0) {
    # No one was processed
    return(list(
      diagnosed_population = data.frame(
        state = character(),
        age_group = character(),
        gender = character(),
        area = character(),
        diagnosed_count = numeric(),
        severity = character(),
        stringsAsFactors = FALSE
      ),
      mc_referrals = data.frame(
        age_group = character(),
        gender = character(),
        area = character(),
        state = character(),
        population = numeric(),
        stringsAsFactors = FALSE
      ),
      updated_queue = gp_queue_df,
      total_wait_time = 0,
      test_counts = list(gp_tests = 0)
    ))
  }
  
  # Step 3: Process each subgroup
  diagnosed_population <- data.frame()
  mc_referrals <- data.frame()
  gp_tests_performed <- 0
  
  # Process each subgroup
  unique_subgroups <- processed_pop %>%
    select(age_group, gender, area) %>%
    distinct()
  
  for (i in 1:nrow(unique_subgroups)) {
    subgroup <- unique_subgroups[i,]
    
    # Get processed population for this subgroup
    subgroup_pop <- processed_pop %>%
      filter(
        age_group == subgroup$age_group,
        gender == subgroup$gender,
        area == subgroup$area
      ) %>%
      pull(population)
    
    if (subgroup_pop == 0) next
    
    # Apply area-specific test refusal rates
    area_test_refusal_rate <- params$area_specific_test_refusal_rates[[subgroup$area]]
    refusing_pop <- subgroup_pop * area_test_refusal_rate
    accepting_pop <- subgroup_pop * (1 - area_test_refusal_rate)
    
    # Track actual tests performed
    gp_tests_performed <- gp_tests_performed + accepting_pop
    
    #NEW Counter for AI analysis 
    ai_morphometry_analyses <- 0 
    
    # Apply GP confidence pathway to those accepting
    if (accepting_pop > 0) {
      gp_confidence_results <- apply_gp_confidence_logic(
        data.frame(
          age_group = subgroup$age_group,
          gender = subgroup$gender,
          area = subgroup$area,
          population = accepting_pop,
          stringsAsFactors = FALSE
        ),
        params
      )
      
      diagnosed_population <- bind_rows(diagnosed_population, gp_confidence_results$diagnosed_population)
      mc_referrals <- bind_rows(mc_referrals, gp_confidence_results$mc_referrals)
    }
  }
  
  return(list(
    diagnosed_population = diagnosed_population,
    mc_referrals = mc_referrals,
    updated_queue = gp_queue_df,
    total_wait_time = gp_results$total_wait_time,
    test_counts = list(gp_tests = gp_tests_performed)
  ))
}

# Apply GP confidence logic with test performance
# UPDATED: Now handles MCI and dementia separately with severity-specific referral rates
apply_gp_confidence_logic <- function(processed_pop, params) {
  # Get parameters
  gp_confidence_probs <- params$gp_confidence_probs
  p_gp_confident <- gp_confidence_probs$confident
  severity_referral_probs <- params$severity_specific_referral_probs
  
  # Get test performance - now has MCI and dementia sub-objects
  gp_test_perf <- params$test_performance$gp
  
  # Initialize results
  diagnosed_population <- data.frame()
  mc_referrals <- data.frame()
  
  # Process each row in processed_pop
  for (i in 1:nrow(processed_pop)) {
    row <- processed_pop[i, ]
    
    # Get prevalences for this age group and gender
    dementia_prev <- params$dementia_prevalence[[row$age_group]][[row$gender]]
    mci_prev <- params$mci_prevalence[[row$age_group]]
    
    # Split into confident vs not confident
    confident_pop <- row$population * p_gp_confident
    not_confident_pop <- row$population * (1 - p_gp_confident)
    
    # GP Not Confident → 100% referral to Memory Clinic
    if (not_confident_pop > 0) {
      mc_referrals <- bind_rows(mc_referrals, data.frame(
        age_group = row$age_group,
        gender = row$gender,
        area = row$area,
        state = "Mixed",  # Will be resolved at memory clinic
        population = not_confident_pop,
        stringsAsFactors = FALSE
      ))
    }
    
    # GP Confident → Cognitive testing with condition-specific performance
    if (confident_pop > 0) {
      # Split population by underlying condition
      mci_pop <- confident_pop * mci_prev
      dementia_pop <- confident_pop * dementia_prev
      healthy_pop <- confident_pop * (1 - mci_prev - dementia_prev)
      
      # Apply MCI test performance
      tp_mci <- mci_pop * gp_test_perf$mci$sensitivity
      fn_mci <- mci_pop * (1 - gp_test_perf$mci$sensitivity)
      
      # Apply dementia test performance
      tp_dementia <- dementia_pop * gp_test_perf$dementia$sensitivity
      fn_dementia <- dementia_pop * (1 - gp_test_perf$dementia$sensitivity)
      
      # Apply specificity to healthy population
      # Use weighted average specificity based on relative prevalence
      if (mci_prev + dementia_prev > 0) {
        mci_weight <- mci_prev / (mci_prev + dementia_prev)
        dementia_weight <- dementia_prev / (mci_prev + dementia_prev)
        weighted_specificity <- (gp_test_perf$mci$specificity * mci_weight + 
                                   gp_test_perf$dementia$specificity * dementia_weight)
      } else {
        # If no disease prevalence, use average specificity
        weighted_specificity <- (gp_test_perf$mci$specificity + 
                                   gp_test_perf$dementia$specificity) / 2
      }
      
      tn <- healthy_pop * weighted_specificity
      fp <- healthy_pop * (1 - weighted_specificity)
      
      # Get severity distribution for this subgroup
      severity_dist <- params$severity_distributions[[row$gender]][[row$age_group]]
      
      # Split true positive dementia cases by severity
      tp_dementia_by_severity <- list()
      for (severity in names(severity_dist)) {
        tp_dementia_by_severity[[severity]] <- tp_dementia * severity_dist[severity]
      }
      
      # Apply severity-specific referral probabilities
      total_referred <- 0
      total_not_referred_dementia <- 0
      
      # Handle MCI cases (use average referral rate for MCI)
      mci_referral_rate <- mean(unlist(severity_referral_probs))  # Use average of severity rates
      referred_mci <- tp_mci * mci_referral_rate
      not_referred_mci <- tp_mci * (1 - mci_referral_rate)
      total_referred <- total_referred + referred_mci
      
      # Handle false positives (use average referral rate)
      fp_referral_rate <- mean(unlist(severity_referral_probs))  # Use average of severity rates
      referred_fp <- fp * fp_referral_rate
      not_referred_fp <- fp * (1 - fp_referral_rate)
      total_referred <- total_referred + referred_fp
      
      # Handle dementia cases by severity
      for (severity in names(tp_dementia_by_severity)) {
        severity_pop <- tp_dementia_by_severity[[severity]]
        if (severity_pop > 0) {
          severity_referral_rate <- severity_referral_probs[[severity]]
          referred_severity <- severity_pop * severity_referral_rate
          not_referred_severity <- severity_pop * (1 - severity_referral_rate)
          
          total_referred <- total_referred + referred_severity
          total_not_referred_dementia <- total_not_referred_dementia + not_referred_severity
          
          # Add non-referred cases to diagnosed population
          if (not_referred_severity > 0) {
            diagnosed_population <- bind_rows(diagnosed_population, data.frame(
              age_group = row$age_group,
              gender = row$gender,
              area = row$area,
              diagnosed_count = not_referred_severity,
              severity = severity,
              source = "GP",
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Add total referred cases to MC referrals
      if (total_referred > 0) {
        mc_referrals <- bind_rows(mc_referrals, data.frame(
          age_group = row$age_group,
          gender = row$gender,
          area = row$area,
          state = "Mixed",  # Mix of TP_MCI, TP_dementia by severity, and FP
          population = total_referred,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(list(
    diagnosed_population = diagnosed_population,
    mc_referrals = mc_referrals
  ))
}

# Run memory clinic pathway with four distinct routes
# Returns: list with diagnosed population and queue updates
run_memory_clinic_pathway <- function(mc_referrals_df, mc_queue_df, mri_queue_df, params) {
  # Step 1: Apply MC referral refusal (6.7% refuse referral)
  mc_refusal_rate <- params$mc_referral_refusal_rate
  
  refusing_pop <- mc_referrals_df %>%
    mutate(population = population * mc_refusal_rate)
  
  accepting_pop <- mc_referrals_df %>%
    mutate(population = population * (1 - mc_refusal_rate))
  
  # Initialize diagnosed population tracker
  diagnosed_population <- data.frame()
  
  # Step 2: Process MC queue
  mc_queue_df <- add_to_queue(mc_queue_df, accepting_pop, "mc")
  mc_results <- process_all_queues(mc_queue_df, params)
  mc_queue_df <- mc_results$updated_queue
  processed_pop <- mc_results$processed_population
  
  if (nrow(processed_pop) == 0) {
    return(list(
      diagnosed_population = diagnosed_population,
      updated_mc_queue = mc_queue_df,
      updated_mri_queue = mri_queue_df,
      total_wait_time_mc = 0,
      total_wait_time_mri = 0,
      test_counts = list(neuropsych = 0, neuroimaging = 0, ai_morphometry = 0, biomarker = 0, mc_total = 0)
    ))
  }
  
  # Step 3: Split into four pathways and process
  test_counts <- list(neuropsych = 0, neuroimaging = 0, ai_morphometry = 0, biomarker = 0, mc_total = 0)
  total_wait_time_mri <- 0
  
  unique_subgroups <- processed_pop %>%
    select(age_group, gender, area) %>%
    distinct()
  
  for (i in 1:nrow(unique_subgroups)) {
    subgroup <- unique_subgroups[i,]
    
    subgroup_pop <- processed_pop %>%
      filter(
        age_group == subgroup$age_group,
        gender == subgroup$gender,
        area == subgroup$area
      ) %>%
      pull(population)
    
    if (subgroup_pop == 0) next
    
    # Apply MC pathways
    pathway_results <- apply_mc_pathways(
      data.frame(
        age_group = subgroup$age_group,
        gender = subgroup$gender,
        area = subgroup$area,
        population = subgroup_pop,
        stringsAsFactors = FALSE
      ),
      mri_queue_df,
      params
    )
    
    diagnosed_population <- bind_rows(diagnosed_population, pathway_results$diagnosed_population)
    mri_queue_df <- pathway_results$updated_mri_queue
    total_wait_time_mri <- total_wait_time_mri + pathway_results$total_wait_time_mri
    
    # Update test counts
    test_counts$neuropsych <- test_counts$neuropsych + pathway_results$test_counts$neuropsych
    test_counts$neuroimaging <- test_counts$neuroimaging + pathway_results$test_counts$neuroimaging
    test_counts$ai_morphometry <- test_counts$ai_morphometry + pathway_results$test_counts$ai_morphometry
    test_counts$biomarker <- test_counts$biomarker + pathway_results$test_counts$biomarker
  }
  
  test_counts$mc_total <- test_counts$neuropsych + test_counts$neuroimaging +
    test_counts$ai_morphometry + test_counts$biomarker
  # Track total neuroimaging including AI-assisted scans
  test_counts$total_neuroimaging <- test_counts$neuroimaging + test_counts$ai_morphometry
  
  return(list(
    diagnosed_population = diagnosed_population,
    updated_mc_queue = mc_queue_df,
    updated_mri_queue = mri_queue_df,
    total_wait_time_mc = mc_results$total_wait_time,
    total_wait_time_mri = total_wait_time_mri,
    test_counts = test_counts
  ))
}

# Apply memory clinic pathways
# UPDATED: Now uses condition-specific test performance
apply_mc_pathways <- function(processed_pop, mri_queue_df, params) {
  pathway_probs <- params$mc_pathway_probs
  diagnosed_population <- data.frame()
  total_wait_time_mri <- 0
  
  # Initialize test counters
  neuropsych_tests <- 0
  neuroimaging_tests <- 0
  biomarker_tests <- 0
  ai_morphometry_analyses <- 0  # Add this counter
  
  # Process each row
  for (i in 1:nrow(processed_pop)) {
    row <- processed_pop[i, ]
    
    # Get prevalences for this age group and gender
    dementia_prev <- params$dementia_prevalence[[row$age_group]][[row$gender]]
    mci_prev <- params$mci_prevalence[[row$age_group]]
    
    # Split population into pathways
    neuropsych_pop <- row$population * pathway_probs$neuropsych
    neuroimaging_pop <- row$population * pathway_probs$neuroimaging
    biomarker_pop <- row$population * pathway_probs$biomarker
    no_further_pop <- row$population * pathway_probs$no_further
    
    # Process each pathway
    # 1. Neuropsych pathway
    if (neuropsych_pop > 0) {
      neuropsych_tests <- neuropsych_tests + neuropsych_pop
      test_perf <- params$test_performance$neuropsych
      results <- apply_test_with_condition_specific_performance(
        neuropsych_pop, dementia_prev, mci_prev,
        test_perf,
        row$age_group, row$gender, row$area, params, "MC_neuropsych"
      )
      diagnosed_population <- bind_rows(diagnosed_population, results)
    }
    
    # 2. Neuroimaging pathway (requires MRI queue)
    if (neuroimaging_pop > 0) {
      # Add to MRI queue
      mri_demand <- data.frame(
        age_group = row$age_group,
        gender = row$gender,
        area = row$area,
        population = neuroimaging_pop,
        stringsAsFactors = FALSE
      )
      
      mri_queue_df <- add_to_queue(mri_queue_df, mri_demand, "mri")
      
      # Process MRI queue for this subgroup
      mri_result <- process_queue(
        mri_queue_df,
        list(age_group = row$age_group, gender = row$gender, area = row$area),
        params
      )
      
      mri_queue_df <- mri_result$updated_queue
      total_wait_time_mri <- total_wait_time_mri + 
        mri_result$wait_time * mri_result$processed_count
      
      # Apply test to processed population
      if (mri_result$processed_count > 0) {
        processed_mri <- mri_result$processed_count
        neuroimaging_tests <- neuroimaging_tests + processed_mri
        
                # Split scans between AI and non-AI based on adoption rate
        if (processed_mri > 0 && !is.na(params$ai_adoption_rate) && params$ai_adoption_rate > 0) {
          # Round processed_mri to integer for rbinom
          processed_mri_int <- round(processed_mri)
          ai_scans <- rbinom(1, processed_mri_int, params$ai_adoption_rate)
          non_ai_scans <- processed_mri - ai_scans
        } else {
          ai_scans <- 0
          non_ai_scans <- processed_mri
        }
        
        # Track AI analyses
        ai_morphometry_analyses <- ai_morphometry_analyses + ai_scans
        
        # Process AI-enhanced scans
        if (ai_scans > 0) {
          # AI-enhanced test performance values (from parameters.R)
          ai_test_perf <- list(
            mci = list(sensitivity = 0.7714, specificity = 0.8449),
            dementia = list(sensitivity = 0.925, specificity = 0.863)
          )
          ai_results <- apply_test_with_condition_specific_performance(
            ai_scans, dementia_prev, mci_prev,
            ai_test_perf,
            row$age_group, row$gender, row$area, params, "MC_neuroimaging_early_AI"
          )
          diagnosed_population <- bind_rows(diagnosed_population, ai_results)
        }
        
        # Process non-AI scans
        if (non_ai_scans > 0) {
          # Base neuroimaging test performance values (from parameters.R)
          standard_test_perf <- list(
            mci = list(sensitivity = 0.63, specificity = 0.69),
            dementia = list(sensitivity = 0.91, specificity = 0.81)
          )
          standard_results <- apply_test_with_condition_specific_performance(
            non_ai_scans, dementia_prev, mci_prev,
            standard_test_perf,
            row$age_group, row$gender, row$area, params, "MC_neuroimaging"
          )
          diagnosed_population <- bind_rows(diagnosed_population, standard_results)
        }
      }
    }
    
    # 3. Biomarker pathway
    if (biomarker_pop > 0) {
      biomarker_tests <- biomarker_tests + biomarker_pop
      test_perf <- params$test_performance$biomarker
      results <- apply_test_with_condition_specific_performance(
        biomarker_pop, dementia_prev, mci_prev,
        test_perf,
        row$age_group, row$gender, row$area, params, "MC_biomarker"
      )
      diagnosed_population <- bind_rows(diagnosed_population, results)
    }
    
    # 4. No further assessment pathway
    # These people are not diagnosed - no action needed
  }
  
  return(list(
    diagnosed_population = diagnosed_population,
    updated_mri_queue = mri_queue_df,
    total_wait_time_mri = total_wait_time_mri,
    test_counts = list(
      neuropsych = neuropsych_tests,
      neuroimaging = neuroimaging_tests - ai_morphometry_analyses,  # Only non-AI scans
      ai_morphometry = ai_morphometry_analyses,  # Only AI scans
      biomarker = biomarker_tests
    )
  ))
}

# Apply test with condition-specific performance
# Replaces apply_test_and_flag_diagnosis
# Returns: data frame with diagnosed population (MCI and dementia)
apply_test_with_condition_specific_performance <- function(
    population, dementia_prev, mci_prev, 
    test_performance,  # Now contains mci and dementia sub-lists
    age_group, gender, area, params, source
) {
  
  # Split population by underlying condition
  mci_pop <- population * mci_prev
  dementia_pop <- population * dementia_prev
  healthy_pop <- population * (1 - mci_prev - dementia_prev)
  
  # Apply MCI test performance
  tp_mci <- mci_pop * test_performance$mci$sensitivity
  fn_mci <- mci_pop * (1 - test_performance$mci$sensitivity)
  
  # Apply dementia test performance
  tp_dementia <- dementia_pop * test_performance$dementia$sensitivity
  fn_dementia <- dementia_pop * (1 - test_performance$dementia$sensitivity)
  
  # For healthy population, use weighted average specificity based on what we're testing for
  # Weight by the relative prevalence of MCI vs dementia
  if (mci_prev + dementia_prev > 0) {
    mci_weight <- mci_prev / (mci_prev + dementia_prev)
    dementia_weight <- dementia_prev / (mci_prev + dementia_prev)
    weighted_specificity <- (test_performance$mci$specificity * mci_weight + 
                               test_performance$dementia$specificity * dementia_weight)
  } else {
    # If no disease prevalence, use average specificity
    weighted_specificity <- (test_performance$mci$specificity + 
                               test_performance$dementia$specificity) / 2
  }
  
  tn <- healthy_pop * weighted_specificity
  fp <- healthy_pop * (1 - weighted_specificity)
  
  diagnosed_population <- data.frame()
  
  # True positive MCI cases get flagged for MCI diagnosis
  # In the model, MCI diagnosis doesn't change state immediately but affects monitoring
  if (tp_mci > 0) {
    diagnosed_population <- bind_rows(diagnosed_population, data.frame(
      age_group = age_group,
      gender = gender,
      area = area,
      diagnosed_count = tp_mci,
      severity = "MCI",  # Special severity indicator for MCI
      source = source,
      stringsAsFactors = FALSE
    ))
  }
  
  # True positive dementia cases get flagged for dementia diagnosis
  if (tp_dementia > 0) {
    # Get severity distribution for this subgroup
    severity_dist <- params$severity_distributions[[gender]][[age_group]]
    
    # Create diagnosis records for each severity
    for (severity in names(severity_dist)) {
      if (severity_dist[severity] > 0) {
        diagnosed_population <- bind_rows(diagnosed_population, data.frame(
          age_group = age_group,
          gender = gender,
          area = area,
          diagnosed_count = tp_dementia * severity_dist[severity],
          severity = severity,
          source = source,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(diagnosed_population)
}

# Apply diagnosed population to cohort during transitions
# UPDATED: Now handles MCI diagnoses as well
apply_diagnoses_to_cohort <- function(cohort_df, diagnosed_population, params) {
  if (nrow(diagnosed_population) == 0) {
    return(cohort_df)
  }
  
  # Separate MCI and dementia diagnoses
  mci_diagnoses <- diagnosed_population %>%
    filter(severity == "MCI")
  
  dementia_diagnoses <- diagnosed_population %>%
    filter(severity != "MCI")
  
  # Apply MCI diagnoses
  # In the current Markov model, there's no "Diagnosed MCI" state
  # MCI diagnoses are tracked for reporting but don't change state transitions
  # This could be extended in future to affect monitoring or treatment pathways
  if (nrow(mci_diagnoses) > 0) {
    # MCI diagnoses don't change state but could affect future transitions
    # Store as attribute or handle separately based on model requirements
    # For now, we'll just track them without state change
    attr(cohort_df, "mci_diagnoses") <- mci_diagnoses
  }
  
  # Apply dementia diagnoses (existing logic)
  if (nrow(dementia_diagnoses) > 0) {
    # Aggregate diagnoses by subgroup and severity
    diagnoses_summary <- dementia_diagnoses %>%
      group_by(age_group, gender, area, severity) %>%
      summarise(
        total_diagnosed = sum(diagnosed_count),
        .groups = "drop"
      )
    
    # Apply diagnoses: move people from undiagnosed to diagnosed states
    for (i in 1:nrow(diagnoses_summary)) {
      diag <- diagnoses_summary[i,]
      
      # Determine source and target states
      undx_state <- paste0("Undx_", diag$severity)
      dx_state <- paste0("Dx_", diag$severity)
      
      # Find the rows to update
      undx_idx <- which(
        cohort_df$state == undx_state &
          cohort_df$age_group == diag$age_group &
          cohort_df$gender == diag$gender &
          cohort_df$area == diag$area
      )
      
      dx_idx <- which(
        cohort_df$state == dx_state &
          cohort_df$age_group == diag$age_group &
          cohort_df$gender == diag$gender &
          cohort_df$area == diag$area
      )
      
      if (length(undx_idx) > 0 && length(dx_idx) > 0) {
        # Move diagnosed population from undiagnosed to diagnosed
        # But don't exceed available undiagnosed population
        available_undx <- cohort_df$population[undx_idx]
        to_diagnose <- min(diag$total_diagnosed, available_undx)
        
        if (to_diagnose > 0) {
          cohort_df$population[undx_idx] <- cohort_df$population[undx_idx] - to_diagnose
          cohort_df$population[dx_idx] <- cohort_df$population[dx_idx] + to_diagnose
        }
      }
    }
  }
  
  return(cohort_df)
}

# Backward compatibility wrapper for apply_test_and_flag_diagnosis
# This function converts old calls to use the new function
apply_test_and_flag_diagnosis <- function(population, dementia_prev,
                                          sensitivity, specificity,
                                          age_group, gender, area, params, source) {
  
  # Create a test performance object compatible with new function
  # Use same sensitivity/specificity for both MCI and dementia (old behavior)
  test_performance <- list(
    mci = list(sensitivity = sensitivity, specificity = specificity),
    dementia = list(sensitivity = sensitivity, specificity = specificity)
  )
  
  # Get MCI prevalence
  mci_prev <- params$mci_prevalence[[age_group]]
  
  # Call new function
  results <- apply_test_with_condition_specific_performance(
    population, dementia_prev, mci_prev,
    test_performance,
    age_group, gender, area, params, source
  )
  
  # Filter out MCI diagnoses to maintain backward compatibility
  # (old function only diagnosed dementia)
  return(results %>% filter(severity != "MCI"))
}