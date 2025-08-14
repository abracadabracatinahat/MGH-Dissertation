# main.R - Entry point for Dementia Economic Model
# Defines scenarios, runs simulations, and generates results

library(dplyr)
library(tidyr)
library(ggplot2)

# Source all modules
source("parameters.R")
source("cohort_manager.R")
source("queue_manager.R")
source("pathways.R")
source("transitions.R")
source("outcomes.R")
source("simulation.R")

# Main execution function
run_dementia_model <- function(years = 0:50, 
                               verbose = FALSE,
                               save_results = TRUE,
                               output_dir = "results") {
  
  cat("==============================================\n")
  cat("AI Dementia Economic Model - Starting Analysis\n")
  cat("==============================================\n\n")
  
  # Create timestamped output directory if saving results
  if (save_results) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_dir <- file.path(output_dir, timestamp)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }
  
  # Define scenarios
  scenarios <- expand.grid(
    ai_enabled = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      area_types = list(c("Urban", "Rural")),  # Both areas in each scenario
      scenario_name = paste0( #NEW naming 
        ifelse(ai_enabled, "AI_BrainMorphometry", "Standard_Care"), 
        "_All_Areas"
      )
    )
  
  cat("Scenarios to run:\n")
  print(scenarios %>% select(scenario_name, ai_enabled))
  cat("\n")
  
  # Run all scenarios
  all_results <- list()
  
  for (i in 1:nrow(scenarios)) {
    scenario <- scenarios[i, ]
    cat(sprintf("\n--- Running scenario %d/%d: %s ---\n", 
                i, nrow(scenarios), scenario$scenario_name))
    
    # Run simulation with proper error handling
    sim_output <- safe_run_simulation(
      ai_enabled = scenario$ai_enabled,
      area_types = scenario$area_types[[1]],
      years = years,
      verbose = verbose,
      context = scenario$scenario_name,
      ai_mci_sens_override = NULL,
      ai_mci_spec_override = NULL,
      ai_dementia_sens_override = NULL,
      ai_dementia_spec_override = NULL
    )
    
    # Add summarized results for backward compatibility
    if (!sim_output$error && !is.null(sim_output$results) && nrow(sim_output$results) > 0) {
      sim_output$summary_results <- summarize_results(sim_output$results)
    }
    
    # Store results
    all_results[[scenario$scenario_name]] <- sim_output
    
    # Validate results
    if (!sim_output$error && !is.null(sim_output$results) && nrow(sim_output$results) > 0) {
      validation <- validate_simulation_results(sim_output)
      cat(sprintf("  Validation: %s\n", validation$summary))
      if (!validation$is_valid) {
        for (issue in validation$issues) {
          cat(sprintf("    - %s\n", issue))
        }
      }
    }
  }
  
  cat("\n\n=== Calculating Cost-Effectiveness ===\n")
  
  # Extract AI and No-AI results
  results_ai <- all_results[["AI_BrainMorphometry_All_Areas"]]
  results_no_ai <- all_results[["Standard_Care_All_Areas"]]
  
  # Initialize variables for later use
  comparison <- NULL
  original_comparison <- NULL
  area_results <- NULL
  
  # Calculate comparisons
  if (!results_ai$error && !results_no_ai$error &&
      !is.null(results_ai$summary_results) && !is.null(results_no_ai$summary_results) &&
      nrow(results_ai$summary_results) > 0 && nrow(results_no_ai$summary_results) > 0) {
    
    comparison <- compare_scenarios(results_ai, results_no_ai)
    # Generate original population comparison
    original_comparison_result <- compare_scenarios_original(results_ai, results_no_ai)
    original_comparison <- original_comparison_result$comparison_table
    original_icer_results <- original_comparison_result$icer_results
    
    # Print ICER results
    cat("\nIncremental Cost-Effectiveness Results:\n")
    cat(sprintf("  Incremental Cost: £%s\n", 
                format(round(comparison$icer_results$incremental_cost, 2), 
                       big.mark = ",")))
    
    # Calculate area-specific results using detailed data
    area_results <- calculate_area_specific_results(all_results)

    # Memory clinic specific summary
    mc_summary <- calculate_memory_clinic_summary(all_results)
    if (!is.null(mc_summary)) {
      cat("\nMemory Clinic Activity Summary:\n")
      print(mc_summary)
      mc_comp <- compare_memory_clinic_scenarios(mc_summary)
      if (!is.null(mc_comp)) {
        cat(sprintf("\n  Incremental MC Cost: £%s\n",
                    format(round(mc_comp$incremental_cost, 2), big.mark = ",")))
        cat(sprintf("  Incremental MC Diagnoses: %0.2f\n",
                    mc_comp$incremental_diagnoses))
        if (is.finite(mc_comp$cost_per_additional_diagnosis)) {
          cat(sprintf("  Cost per additional MC diagnosis: £%s\n",
                      format(round(mc_comp$cost_per_additional_diagnosis, 2),
                             big.mark = ",")))
        }
      }
    }
    
    # Validate results before proceeding
    for (scenario_name in names(all_results)) {
      if (!all_results[[scenario_name]]$error) {
        validation <- validate_simulation_results(all_results[[scenario_name]])
        if (!validation$is_valid) {
          warning(paste("Validation issues in", scenario_name, ":", 
                        paste(validation$issues, collapse = "; ")))
        }
      }
    }
    
    # Generate reports
    if (save_results) {
      cat("\n\n=== Generating Reports ===\n")
      generate_reports(all_results, comparison, area_results, output_dir, original_comparison, original_icer_results)
    }
    
    # Create visualizations using summary data
    if (save_results) {
      cat("\n=== Creating Visualizations ===\n")
      create_visualizations(all_results, output_dir)
    }
    
  } else {
    warning(paste("Could not calculate comparisons - one or more scenarios failed.",
                  "\nAI scenario error:", if(results_ai$error) results_ai$error_message else "None",
                  "\nNo-AI scenario error:", if(results_no_ai$error) results_no_ai$error_message else "None"))
  }
  
  cat("\n==============================================\n")
  cat("Analysis Complete\n")
  cat("==============================================\n")
  
  # Return all results
  return(list(
    scenarios = scenarios,
    results = all_results,
    comparison = comparison,
    area_results = area_results
  ))
}

# Calculate area-specific results using detailed results
calculate_area_specific_results <- function(all_results) {
  area_summaries <- list()
  
  for (scenario_name in names(all_results)) {
    if (!all_results[[scenario_name]]$error && !is.null(all_results[[scenario_name]]$results)) {
      # Use detailed results for area analysis
      detailed_results <- all_results[[scenario_name]]$results
      
      # Calculate by area - both final year metrics and lifetime totals
      area_summary <- detailed_results %>%
        group_by(area, AI_Enabled) %>%
        summarise(
          # Final year metrics
          final_population = sum(Total_Population[Year == max(Year)]),
          final_diagnosed = sum(Diagnosed_Dementia[Year == max(Year)]),
          final_undiagnosed = sum(Undiagnosed_Dementia[Year == max(Year)]),
          diagnosis_rate = final_diagnosed / (final_diagnosed + final_undiagnosed),
          # Lifetime totals
          total_cost = sum(Discounted_Cost),
          total_qalys = sum(Discounted_QALYs),
          .groups = "drop"
        ) %>%
        mutate(scenario = scenario_name)
      
      area_summaries[[scenario_name]] <- area_summary
    }
  }
  
  return(bind_rows(area_summaries))
}

# Summarize memory clinic activity for each scenario
calculate_memory_clinic_summary <- function(all_results) {
  test_costs <- get_test_costs()
  summaries <- lapply(names(all_results), function(name) {
    res <- all_results[[name]]
    if (!res$error && !is.null(res$results)) {
      df <- res$results
      mc_tests <- sum(df$neuropsych_tests + df$neuroimaging_tests +
                        df$ai_morphometry_tests + df$biomarker_tests,
                      na.rm = TRUE)
      mc_diagnosed <- sum(df$neuropsych_diagnosed + df$total_neuroimaging_diagnosed +
                            df$biomarker_diagnosed, na.rm = TRUE)
      mc_cost <- sum(
        df$neuropsych_tests * test_costs$neuropsych_cost +
          df$neuroimaging_tests * (test_costs$standard_mri_cost +
                                     test_costs$mri_maintenance_depreciation +
                                     test_costs$radiologist_cost_non_ai) +
          df$ai_morphometry_tests * (test_costs$standard_mri_cost +
                                       test_costs$mri_maintenance_depreciation +
                                       test_costs$radiologist_cost_ai +
                                       test_costs$ai_morphometry_per_scan) +
          df$biomarker_tests * test_costs$biomarker_cost,
        na.rm = TRUE
      )
      cost_per_diag <- if (mc_diagnosed > 0) mc_cost / mc_diagnosed else NA
      return(data.frame(
        Scenario = name,
        AI_Enabled = res$parameters$ai_enabled,
        MC_Tests = mc_tests,
        MC_Diagnosed = mc_diagnosed,
        MC_Cost = mc_cost,
        Cost_Per_Diagnosis = cost_per_diag,
        stringsAsFactors = FALSE
      ))
    }
  })
  summaries <- bind_rows(summaries)
  if (nrow(summaries) == 0) return(NULL)
  summaries
}

# Calculate original population memory clinic costs
calculate_original_population_mc_costs <- function(all_results) {
  test_costs <- get_test_costs()
  summaries <- lapply(names(all_results), function(name) {
    res <- all_results[[name]]
    if (!res$error && !is.null(res$results)) {
      df <- res$results
      
      # Use proper filtering approach like original_population_age_group_distribution
      if ("Original_Total_Population" %in% names(df)) {
        # Group by Year and age_group, then filter for Original_Total_Population > 0
        # This ensures we only get the original 65-69 cohort as they age through time
        original_mc_data <- df %>%
          group_by(Year, age_group) %>%
          summarise(
            # Sum original population test counts and costs for each age group
            neuropsych_tests_original = sum(original_neuropsych_tests, na.rm = TRUE),
            neuroimaging_tests_original = sum(original_neuroimaging_tests, na.rm = TRUE),
            ai_morphometry_tests_original = sum(original_ai_morphometry_tests, na.rm = TRUE),
            biomarker_tests_original = sum(original_biomarker_tests, na.rm = TRUE),
            neuropsych_diagnosed_original = sum(original_neuropsych_diagnosed, na.rm = TRUE),
            total_neuroimaging_diagnosed_original = sum(original_total_neuroimaging_diagnosed, na.rm = TRUE),
            biomarker_diagnosed_original = sum(original_biomarker_diagnosed, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          # Filter to only include age groups where the original cohort exists
          filter(neuropsych_tests_original > 0 | neuroimaging_tests_original > 0 | 
                 ai_morphometry_tests_original > 0 | biomarker_tests_original > 0)
        
        if (nrow(original_mc_data) > 0) {
          # Calculate memory clinic costs for original population
          mc_cost_original <- sum(
            original_mc_data$neuropsych_tests_original * test_costs$neuropsych_cost +
              original_mc_data$neuroimaging_tests_original * (test_costs$standard_mri_cost +
                                                               test_costs$mri_maintenance_depreciation +
                                                               test_costs$radiologist_cost_non_ai) +
              original_mc_data$ai_morphometry_tests_original * (test_costs$standard_mri_cost +
                                                                 test_costs$mri_maintenance_depreciation +
                                                                 test_costs$radiologist_cost_ai +
                                                                 test_costs$ai_morphometry_per_scan) +
              original_mc_data$biomarker_tests_original * test_costs$biomarker_cost,
            na.rm = TRUE
          )
          
          # Calculate diagnoses for original population
          mc_diagnosed_original <- sum(
            original_mc_data$neuropsych_diagnosed_original + 
              original_mc_data$total_neuroimaging_diagnosed_original +
              original_mc_data$biomarker_diagnosed_original,
            na.rm = TRUE
          )
          
          return(data.frame(
            Scenario = name,
            AI_Enabled = res$parameters$ai_enabled,
            MC_Cost_Original = mc_cost_original,
            MC_Diagnosed_Original = mc_diagnosed_original,
            stringsAsFactors = FALSE
          ))
        } else {
          # Fallback if no original population data
          return(data.frame(
            Scenario = name,
            AI_Enabled = res$parameters$ai_enabled,
            MC_Cost_Original = 0,
            MC_Diagnosed_Original = 0,
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # Fallback to current population if original not available
        mc_cost_original <- sum(
          df$neuropsych_tests * test_costs$neuropsych_cost +
            df$neuroimaging_tests * (test_costs$standard_mri_cost +
                                       test_costs$mri_maintenance_depreciation +
                                       test_costs$radiologist_cost_non_ai) +
            df$ai_morphometry_tests * (test_costs$standard_mri_cost +
                                         test_costs$mri_maintenance_depreciation +
                                         test_costs$radiologist_cost_ai +
                                         test_costs$ai_morphometry_per_scan) +
            df$biomarker_tests * test_costs$biomarker_cost,
          na.rm = TRUE
        )
        
        mc_diagnosed_original <- sum(
          df$neuropsych_diagnosed + df$total_neuroimaging_diagnosed + df$biomarker_diagnosed,
          na.rm = TRUE
        )
        
        return(data.frame(
          Scenario = name,
          AI_Enabled = res$parameters$ai_enabled,
          MC_Cost_Original = mc_cost_original,
          MC_Diagnosed_Original = mc_diagnosed_original,
          stringsAsFactors = FALSE
        ))
      }
    }
  })
  summaries <- bind_rows(summaries)
  if (nrow(summaries) == 0) return(NULL)
  summaries
}

# Create diagnosis heatmap by state and area (original population - 65-69 cohort tracked through time)
create_diagnosis_heatmap <- function(all_results) {
  # Prepare data for heatmap with cumulative diagnosis counts by source
  heatmap_data <- bind_rows(
    lapply(names(all_results), function(name) {
      res <- all_results[[name]]
      if (!res$error && !is.null(res$results)) {
        df <- res$results
        
        # Use original population data directly (tracks 65-69 cohort through time)
        # Group by age_group and area, then filter for Original_Total_Population > 0
        # This ensures we only get the original 65-69 cohort as they age through time
        df_original <- df %>%
          group_by(Year, age_group, area) %>%
          summarise(
            # Sum original population data for each age group and area
            Original_Total_Population = sum(Original_Total_Population, na.rm = TRUE),
            Undiagnosed_Mild = sum(original_population_Undx_Mild, na.rm = TRUE),
            Undiagnosed_Moderate = sum(original_population_Undx_Moderate, na.rm = TRUE),
            Undiagnosed_Severe = sum(original_population_Undx_Severe, na.rm = TRUE),
            Diagnosed_Mild = sum(original_population_Dx_Mild, na.rm = TRUE),
            Diagnosed_Moderate = sum(original_population_Dx_Moderate, na.rm = TRUE),
            Diagnosed_Severe = sum(original_population_Dx_Severe, na.rm = TRUE),
            MCI = sum(original_population_MCI, na.rm = TRUE),
            Other_Healthy = sum(original_population_Other_Healthy, na.rm = TRUE),
            Death = sum(original_population_Death, na.rm = TRUE),
            # Cumulative diagnosis counts by source
            GP_Diagnoses = sum(original_gp_diagnosed, na.rm = TRUE),
            Neuropsych_Diagnoses = sum(original_neuropsych_diagnosed, na.rm = TRUE),
            Neuroimaging_Diagnoses = sum(original_neuroimaging_diagnosed, na.rm = TRUE),
            AI_Morphometry_Diagnoses = sum(original_ai_morphometry_diagnosed, na.rm = TRUE),
            Biomarker_Diagnoses = sum(original_biomarker_diagnosed, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          # Filter out age groups with zero original population (will be empty for most groups except 65 to 69)
          filter(Original_Total_Population > 0)
        
        if (nrow(df_original) > 0) {
          # Calculate cumulative totals across all years for the original population cohort by area
          df_original %>%
            group_by(area) %>%
            summarise(
              # State counts for original population cohort (cumulative across all years)
              Undiagnosed_Mild = sum(Undiagnosed_Mild, na.rm = TRUE),
              Undiagnosed_Moderate = sum(Undiagnosed_Moderate, na.rm = TRUE),
              Undiagnosed_Severe = sum(Undiagnosed_Severe, na.rm = TRUE),
              Diagnosed_Mild = sum(Diagnosed_Mild, na.rm = TRUE),
              Diagnosed_Moderate = sum(Diagnosed_Moderate, na.rm = TRUE),
              Diagnosed_Severe = sum(Diagnosed_Severe, na.rm = TRUE),
              MCI = sum(MCI, na.rm = TRUE),
              Other_Healthy = sum(Other_Healthy, na.rm = TRUE),
              Death = sum(Death, na.rm = TRUE),
              # Cumulative diagnosis counts by source
              GP_Diagnoses = sum(GP_Diagnoses, na.rm = TRUE),
              Neuropsych_Diagnoses = sum(Neuropsych_Diagnoses, na.rm = TRUE),
              Neuroimaging_Diagnoses = sum(Neuroimaging_Diagnoses, na.rm = TRUE),
              AI_Morphometry_Diagnoses = sum(AI_Morphometry_Diagnoses, na.rm = TRUE),
              Biomarker_Diagnoses = sum(Biomarker_Diagnoses, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(Scenario = name)
        } else {
          # Fallback if no original population data
          df %>%
            group_by(area) %>%
            summarise(
              # State counts (current population)
              Undiagnosed_Mild = sum(Undx_Mild, na.rm = TRUE),
              Undiagnosed_Moderate = sum(Undx_Moderate, na.rm = TRUE),
              Undiagnosed_Severe = sum(Undx_Severe, na.rm = TRUE),
              Diagnosed_Mild = sum(Dx_Mild, na.rm = TRUE),
              Diagnosed_Moderate = sum(Dx_Moderate, na.rm = TRUE),
              Diagnosed_Severe = sum(Dx_Severe, na.rm = TRUE),
              MCI = sum(MCI, na.rm = TRUE),
              Other_Healthy = sum(Other_Healthy, na.rm = TRUE),
              Death = sum(Death, na.rm = TRUE),
              # Diagnosis counts by source
              GP_Diagnoses = sum(gp_diagnosed, na.rm = TRUE),
              Neuropsych_Diagnoses = sum(neuropsych_diagnosed, na.rm = TRUE),
              Neuroimaging_Diagnoses = sum(neuroimaging_diagnosed, na.rm = TRUE),
              AI_Morphometry_Diagnoses = sum(ai_morphometry_diagnosed, na.rm = TRUE),
              Biomarker_Diagnoses = sum(biomarker_diagnosed, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(Scenario = name)
        }
      }
    })
  )
  
  if (nrow(heatmap_data) == 0) return(NULL)
  
  # Reshape data for heatmap with both population states and diagnosis sources
  heatmap_long <- heatmap_data %>%
    pivot_longer(
      cols = c(Undiagnosed_Mild, Undiagnosed_Moderate, Undiagnosed_Severe,
               Diagnosed_Mild, Diagnosed_Moderate, Diagnosed_Severe, MCI, Other_Healthy, Death,
               GP_Diagnoses, Neuropsych_Diagnoses, Neuroimaging_Diagnoses, AI_Morphometry_Diagnoses, Biomarker_Diagnoses),
      names_to = "Category",
      values_to = "Count"
    ) %>%
    mutate(
      Category_Type = case_when(
        Category %in% c("GP_Diagnoses", "Neuropsych_Diagnoses", "Neuroimaging_Diagnoses", "AI_Morphometry_Diagnoses", "Biomarker_Diagnoses") ~ "Diagnosis_Source",
        TRUE ~ "Population_State"
      ),
      Category = factor(Category, levels = c("Other_Healthy", "MCI", "Undiagnosed_Mild", "Undiagnosed_Moderate", 
                                            "Undiagnosed_Severe", "Diagnosed_Mild", "Diagnosed_Moderate", 
                                            "Diagnosed_Severe", "Death",
                                            "GP_Diagnoses", "Neuropsych_Diagnoses", "Neuroimaging_Diagnoses", 
                                            "AI_Morphometry_Diagnoses", "Biomarker_Diagnoses")),
      Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
    )
  
  # Create heatmap
  heatmap_plot <- ggplot(heatmap_long, aes(x = Category, y = area, fill = Count)) +
    geom_tile() +
    geom_text(aes(label = scales::comma(Count, accuracy = 1)), 
              size = 2.5, color = "white", fontface = "bold") +
    facet_wrap(~Scenario, ncol = 1, labeller = labeller(
      Scenario = c(
        "AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
        "Standard_Care_All_Areas" = "Standard Care"
      )
    )) +
    scale_fill_gradient2(
      low = "#4575B4", 
      mid = "#FEE090", 
      high = "#D73027",
      midpoint = median(heatmap_long$Count, na.rm = TRUE)
    ) +
    labs(
      title = "Population States and Cumulative Diagnosis Counts by Source and Area",
      subtitle = "Original 65-69 Cohort - Cumulative Across All Years - Comparison of AI vs Standard Care",
      x = "Category",
      y = "Area",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 10)
    )
  
  return(heatmap_plot)
}

# Calculate original population total diagnosis costs
calculate_original_population_total_costs <- function(all_results) {
  test_costs <- get_test_costs()
  summaries <- lapply(names(all_results), function(name) {
    res <- all_results[[name]]
    if (!res$error && !is.null(res$results)) {
      df <- res$results
      

      
      # Use proper filtering approach like original_population_age_group_distribution
      if ("Original_Total_Population" %in% names(df)) {
        # Group by Year and age_group, then filter for Original_Total_Population > 0
        # This ensures we only get the original 65-69 cohort as they age through time
        original_total_data <- df %>%
          group_by(Year, age_group) %>%
          summarise(
            # Sum original population test counts and costs for each age group
            gp_tests_original = sum(original_gp_tests, na.rm = TRUE),
            neuropsych_tests_original = sum(original_neuropsych_tests, na.rm = TRUE),
            neuroimaging_tests_original = sum(original_neuroimaging_tests, na.rm = TRUE),
            ai_morphometry_tests_original = sum(original_ai_morphometry_tests, na.rm = TRUE),
            biomarker_tests_original = sum(original_biomarker_tests, na.rm = TRUE),
            gp_diagnosed_original = sum(original_gp_diagnosed, na.rm = TRUE),
            neuropsych_diagnosed_original = sum(original_neuropsych_diagnosed, na.rm = TRUE),
            total_neuroimaging_diagnosed_original = sum(original_total_neuroimaging_diagnosed, na.rm = TRUE),
            biomarker_diagnosed_original = sum(original_biomarker_diagnosed, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          # Filter to only include age groups where the original cohort exists
          filter(gp_tests_original > 0 | neuropsych_tests_original > 0 | 
                 neuroimaging_tests_original > 0 | ai_morphometry_tests_original > 0 | 
                 biomarker_tests_original > 0)
        
        if (nrow(original_total_data) > 0) {
          # Calculate total diagnosis costs for original population (including GP costs)
          gp_time_cost <- get_gp_time_cost()
          total_cost_original <- sum(
            # GP costs
            original_total_data$gp_tests_original * gp_time_cost$cost_per_minute * gp_time_cost$time_per_consultation +
              # Neuropsych costs
              original_total_data$neuropsych_tests_original * test_costs$neuropsych_cost +
              # Neuroimaging costs
              original_total_data$neuroimaging_tests_original * (test_costs$standard_mri_cost +
                                                                 test_costs$mri_maintenance_depreciation +
                                                                 test_costs$radiologist_cost_non_ai) +
              # AI morphometry costs
              original_total_data$ai_morphometry_tests_original * (test_costs$standard_mri_cost +
                                                                   test_costs$mri_maintenance_depreciation +
                                                                   test_costs$radiologist_cost_ai +
                                                                   test_costs$ai_morphometry_per_scan) +
              # Biomarker costs
              original_total_data$biomarker_tests_original * test_costs$biomarker_cost,
            na.rm = TRUE
          )
          
          # Calculate total diagnoses for original population
          total_diagnosed_original <- sum(
            original_total_data$gp_diagnosed_original + 
              original_total_data$neuropsych_diagnosed_original + 
              original_total_data$total_neuroimaging_diagnosed_original + 
              original_total_data$biomarker_diagnosed_original,
            na.rm = TRUE
          )
          
          return(data.frame(
            Scenario = name,
            AI_Enabled = res$parameters$ai_enabled,
            Total_Cost_Original = total_cost_original,
            Total_Diagnosed_Original = total_diagnosed_original,
            stringsAsFactors = FALSE
          ))
        } else {
          # Fallback if no original population data
          return(data.frame(
            Scenario = name,
            AI_Enabled = res$parameters$ai_enabled,
            Total_Cost_Original = 0,
            Total_Diagnosed_Original = 0,
            stringsAsFactors = FALSE
          ))
        }
      } else {

        # Fallback to current population if original not available
        gp_time_cost <- get_gp_time_cost()
        total_cost_original <- sum(
          df$gp_tests * gp_time_cost$cost_per_minute * gp_time_cost$time_per_consultation +
            df$neuropsych_tests * test_costs$neuropsych_cost +
            df$neuroimaging_tests * (test_costs$standard_mri_cost +
                                       test_costs$mri_maintenance_depreciation +
                                       test_costs$radiologist_cost_non_ai) +
            df$ai_morphometry_tests * (test_costs$standard_mri_cost +
                                         test_costs$mri_maintenance_depreciation +
                                         test_costs$radiologist_cost_ai +
                                         test_costs$ai_morphometry_per_scan) +
            df$biomarker_tests * test_costs$biomarker_cost,
          na.rm = TRUE
        )
        
        total_diagnosed_original <- sum(
          df$gp_diagnosed + df$neuropsych_diagnosed + df$total_neuroimaging_diagnosed + df$biomarker_diagnosed,
          na.rm = TRUE
        )
        

        
        return(data.frame(
          Scenario = name,
          AI_Enabled = res$parameters$ai_enabled,
          Total_Cost_Original = total_cost_original,
          Total_Diagnosed_Original = total_diagnosed_original,
          stringsAsFactors = FALSE
        ))
      }
    }
  })
  summaries <- bind_rows(summaries)
  if (nrow(summaries) == 0) {
    return(NULL)
  }
  summaries
}

# Compare memory clinic metrics between AI and non-AI scenarios
compare_memory_clinic_scenarios <- function(mc_summary) {
  ai <- mc_summary %>% filter(AI_Enabled)
  no_ai <- mc_summary %>% filter(!AI_Enabled)
  if (nrow(ai) == 0 || nrow(no_ai) == 0) return(NULL)
  incremental_cost <- ai$MC_Cost - no_ai$MC_Cost
  incremental_diag <- ai$MC_Diagnosed - no_ai$MC_Diagnosed
  cost_per_diag <- ifelse(incremental_diag != 0, incremental_cost / incremental_diag, NA)
  data.frame(
    incremental_cost = incremental_cost,
    incremental_diagnoses = incremental_diag,
    cost_per_additional_diagnosis = cost_per_diag
  )
}

# Generate original population focused summary report
generate_original_population_summary <- function(all_results, original_comparison, output_dir, original_icer_results = NULL) {
  cat("  Creating original population summary report...\n")
  original_summary_file <- file.path(output_dir, "summary_report.txt")
  
  sink(original_summary_file)
  cat("AI DEMENTIA ECONOMIC MODEL - ORIGINAL POPULATION SUMMARY REPORT\n")
  cat("================================================================\n\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("REPORT FOCUS: ORIGINAL COHORT POPULATION ONLY\n")
  cat("This report analyzes outcomes for the original population cohort,\n")
  cat("excluding any population inflow effects from the simulation.\n\n")
  
  # Extract original population specific metrics
  if (!is.null(original_comparison)) {
    original_cost_per_person_ai <- original_comparison$AI[3]
    original_cost_per_person_no_ai <- original_comparison$No_AI[3]
    
    cat("KEY FINDINGS - ORIGINAL POPULATION ONLY\n")
    cat("----------------------------------------\n")
    # Show initial population in header, but detailed comparison shows survivors
    # Get initial population from first scenario's initial year data
    first_scenario_name <- names(all_results)[1]
    if (!all_results[[first_scenario_name]]$error && !is.null(all_results[[first_scenario_name]]$results)) {
      initial_year_data <- all_results[[first_scenario_name]]$results %>% 
        filter(Year == min(Year))
      initial_population <- ifelse("Original_Total_Population" %in% names(initial_year_data), 
                                  sum(initial_year_data$Original_Total_Population, na.rm = TRUE), 
                                  sum(initial_year_data$Total_Population))
    } else {
      initial_population <- 0  # fallback
    }
    cat(sprintf("Original population tracked: %s individuals\n", 
                format(round(initial_population), big.mark = ",")))

    
    # Calculate original population incremental metrics (per-person for display)
    incremental_cost_per_person <- original_cost_per_person_ai - original_cost_per_person_no_ai
    
    cat(sprintf("Incremental cost per original person: £%s\n", 
                format(round(incremental_cost_per_person, 2), big.mark = ",")))
    
    cat("\nORIGINAL POPULATION DETAILED COMPARISON\n")
    cat("--------------------------------------\n")
    print(original_comparison, row.names = FALSE)
  }
  
  cat("\n\nMETHODOLOGY NOTE\n")
  cat("----------------\n")
  cat("This analysis focuses exclusively on the original population cohort\n")
  cat("that entered the simulation at Year 0. It excludes:\n")
  cat("- New population inflow from aging\n")
  cat("- Immigration effects\n")
  cat("- Population growth\n\n")
  cat("This provides a pure lifetime cohort analysis showing the long-term\n")
  cat("impact of AI-enhanced dementia care on a fixed population group.\n")
  
  sink()
}

# Generate comprehensive reports
generate_reports <- function(all_results, comparison, area_results, output_dir, original_comparison = NULL, original_icer_results = NULL) {
  
  # 1. Original Population Summary report (primary focus based on user request)
  if (!is.null(original_comparison)) {
          generate_original_population_summary(all_results, original_comparison, output_dir, original_icer_results)
  } else {
    # Fallback: Create standard summary report  
    cat("  Creating standard summary report...\n")
    summary_file <- file.path(output_dir, "summary_report.txt")
    
    sink(summary_file)
    cat("AI DEMENTIA ECONOMIC MODEL - SUMMARY REPORT\n")
    cat("==========================================\n\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    cat("KEY FINDINGS\n")
    cat("------------\n")
    if (!is.null(comparison)) {
      cat(sprintf("- Cost impact: £%s\n", 
                  format(round(comparison$icer_results$incremental_cost, 2), big.mark = ",")))
    }
    
    cat("\nDETAILED COMPARISON\n")
    cat("-------------------\n")
    if (!is.null(comparison)) {
      print(comparison$comparison_table, row.names = FALSE)
    }
    
    cat("\n\nPOPULATION SUMMARY (Final Year)\n")
    cat("--------------------------------\n")
    if (!is.null(comparison)) {
      cat(sprintf("Total population tracked: %s\n", 
                  format(round(comparison$comparison_table$AI[1]), big.mark = ",")))
      cat(sprintf("Diagnosis rate improvement: %+.1f percentage points\n",
                  comparison$comparison_table$Difference[4]))
      cat(sprintf("Relative improvement: %+.1f%%\n",
                  comparison$comparison_table$Percent_Change[4]))
    }
    
    cat("\n\nAREA-SPECIFIC RESULTS\n")
    cat("---------------------\n")
    if (!is.null(area_results)) {
      print(area_results, row.names = FALSE)
    }
    
    sink()
  }
  
  # 2. Detailed results CSV - save both summary and detailed
  cat("  Saving detailed results...\n")
  for (scenario_name in names(all_results)) {
    if (!all_results[[scenario_name]]$error && !is.null(all_results[[scenario_name]]$results)) {
      # Save detailed results
      write.csv(
        all_results[[scenario_name]]$results,
        file.path(output_dir, paste0("detailed_results_", scenario_name, ".csv")),
        row.names = FALSE
      )
      
      # Save summary results for backward compatibility
      if (!is.null(all_results[[scenario_name]]$summary_results)) {
        write.csv(
          all_results[[scenario_name]]$summary_results,
          file.path(output_dir, paste0("summary_results_", scenario_name, ".csv")),
          row.names = FALSE
        )
      }
    }
  }
  
  # 3. Comparison table
  if (!is.null(comparison)) {
    cat("  Saving comparison table...\n")
    write.csv(
      comparison$comparison_table,
      file.path(output_dir, "comparison_table.csv"),
      row.names = FALSE
    )
  }
  
  # 4. Original population comparison table
  if (!is.null(original_comparison)) {
    cat("  Saving original population comparison table...\n")
    write.csv(
      original_comparison,
      file.path(output_dir, "original_comparison_table.csv"),
      row.names = FALSE
    )
  }
  
  cat("  Reports saved to:", output_dir, "\n")
}

# Create visualizations using summary data
create_visualizations <- function(all_results, output_dir) {
  
  # Create AI adoption rate plot first (shows model parameter evolution)
  cat("  Creating AI adoption rate plot...\n")
  
  # Get the year range from the simulation
  years_range <- if (length(all_results) > 0 && !all_results[[1]]$error && !is.null(all_results[[1]]$summary_results)) {
    range(all_results[[1]]$summary_results$Year)
  } else {
    c(0, 10)  # fallback range
  }
  
  years_seq <- seq(years_range[1], years_range[2])
  
  # Generate AI adoption data
  ai_adoption_data <- data.frame(
    Year = years_seq,
    AI_Adoption_Rate = sapply(years_seq, get_ai_adoption_rate)
  )
  
  # Create AI adoption plot
  ai_adoption_plot <- ggplot(ai_adoption_data, aes(x = Year, y = AI_Adoption_Rate)) +
    geom_line(linewidth = 1.5, color = "#2E86AB") +
    geom_point(size = 2.5, color = "#A23B72") +
    geom_area(alpha = 0.3, fill = "#2E86AB") +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2)
    ) +
    theme_minimal() +
    labs(
      title = "AI Technology Adoption Rate Over Time",
      subtitle = "Healthcare AI neuroimaging adoption following logistic growth model",
      x = "Year",
      y = "AI Adoption Rate",
      caption = paste("Model parameters: Initial =", scales::percent(get_capacity_growth_rates()$ai_initial_adoption),
                     "| Growth =", scales::percent(get_capacity_growth_rates()$ai_adoption_rate),
                     "| Maximum =", scales::percent(get_capacity_growth_rates()$ai_max_adoption))
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5, size = 10, color = "gray60"),
      panel.grid.minor = element_blank()
    )
  
  # Save AI adoption plot
  ggsave(
    file.path(output_dir, "ai_adoption_rate.png"),
    ai_adoption_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # Combine summary results for plotting
  combined_results <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$summary_results)) {
        all_results[[name]]$summary_results %>%
          mutate(Scenario = name)
      }
    })
  )
  
  if (nrow(combined_results) == 0) {
    warning("No results to plot")
    return()
  }
  
  # 1. Population trends over time
  cat("  Creating population trends plot...\n")
  pop_plot <- combined_results %>%
    select(Year, Scenario, Healthy, MCI, Diagnosed_Dementia, Undiagnosed_Dementia) %>%
    pivot_longer(
      cols = c(Healthy, MCI, Diagnosed_Dementia, Undiagnosed_Dementia),
      names_to = "State",
      values_to = "Population"
    ) %>%
    mutate(
      Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
    ) %>%
    ggplot(aes(x = Year, y = Population, color = State)) +
    geom_line(size = 1.2) +
    facet_wrap(~Scenario, labeller = labeller(
      Scenario = c(
        "AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
        "Standard_Care_All_Areas" = "Standard Care"
      )
    )) +
    theme_minimal() +
    labs(
      title = "Population Distribution Over Time",
      subtitle = "Total population tracked",
      x = "Year",
      y = "Population",
      color = "Health State"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "bottom")
  
  ggsave(
    file.path(output_dir, "population_trends.png"),
    pop_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # 2. Cost breakdown over time
  cat("  Creating cost breakdown plot...\n")
  cost_plot <- combined_results %>%
    select(Year, Scenario, Total_Cost, Discounted_Cost) %>%
    pivot_longer(
      cols = c(Total_Cost, Discounted_Cost),
      names_to = "Cost_Type",
      values_to = "Cost"
    ) %>%
    mutate(
      Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
    ) %>%
    ggplot(aes(x = Year, y = Cost, color = Cost_Type)) +
    geom_line(size = 1.2) +
    facet_wrap(~Scenario, labeller = labeller(
      Scenario = c(
        "AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
        "Standard_Care_All_Areas" = "Standard Care"
      )
    )) +
    theme_minimal() +
    labs(
      title = "Cost Evolution Over Time",
      x = "Year",
      y = "Cost (£)",
      color = "Cost Type"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = "bottom")
  
    ggsave(
    file.path(output_dir, "cost_evolution.png"),
    cost_plot,
    width = 10,
    height = 6,
    dpi = 300
  )

  # 2b. Original population cost evolution
  cat("  Creating original population cost evolution plot...\n")
  
  # Calculate original population costs for each scenario
  original_cost_results <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$results)) {
        detailed_results <- all_results[[name]]$results
        
        # Use proper filtering approach like original_population_age_group_distribution
        if ("Original_Total_Population" %in% names(detailed_results)) {
          # Group by Year and age_group, then filter for Original_Total_Population > 0
          # This ensures we only get the original 65-69 cohort as they age through time
          original_costs_by_year <- detailed_results %>%
            group_by(Year, age_group) %>%
            summarise(
              # Sum original population costs for each age group
              Original_Total_Cost = sum(Subgroup_Cost * Original_Total_Population / Total_Population, na.rm = TRUE),
              Original_Discounted_Cost = sum(Subgroup_Discounted_Cost * Original_Total_Population / Total_Population, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            # Filter to only include age groups where the original cohort exists
            filter(Original_Total_Cost > 0 | Original_Discounted_Cost > 0)
          
          if (nrow(original_costs_by_year) > 0) {
            # Sum across age groups to get total for the original cohort by year
            original_costs_by_year %>%
              group_by(Year) %>%
              summarise(
                Original_Total_Cost = sum(Original_Total_Cost, na.rm = TRUE),
                Original_Discounted_Cost = sum(Original_Discounted_Cost, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(Scenario = name)
          } else {
            # Fallback if no original population data
            data.frame(
              Year = integer(),
              Original_Total_Cost = numeric(),
              Original_Discounted_Cost = numeric(),
              Scenario = character(),
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Fallback if original population data not available
          data.frame(
            Year = integer(),
            Original_Total_Cost = numeric(),
            Original_Discounted_Cost = numeric(),
            Scenario = character(),
            stringsAsFactors = FALSE
          )
        }
      }
    })
  )
  
  if (nrow(original_cost_results) > 0) {
    # Create the plot using the same structure as the regular cost evolution
    original_cost_plot <- original_cost_results %>%
      select(Year, Scenario, Original_Total_Cost, Original_Discounted_Cost) %>%
      pivot_longer(
        cols = c(Original_Total_Cost, Original_Discounted_Cost),
        names_to = "Cost_Type",
        values_to = "Cost"
      ) %>%
      mutate(
        Cost_Type = case_when(
          Cost_Type == "Original_Total_Cost" ~ "Total_Cost",
          Cost_Type == "Original_Discounted_Cost" ~ "Discounted_Cost",
          TRUE ~ Cost_Type
        ),
        Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
      ) %>%
      ggplot(aes(x = Year, y = Cost, color = Cost_Type)) +
      geom_line(size = 1.2) +
      facet_wrap(~Scenario, labeller = labeller(
        Scenario = c(
          "AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
          "Standard_Care_All_Areas" = "Standard Care"
        )
      )) +
      theme_minimal() +
      labs(
        title = "Cost Evolution Over Time (Original Population Only)",
        subtitle = "Costs calculated for the original cohort population only",
        x = "Year",
        y = "Cost (£)",
        color = "Cost Type"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme(legend.position = "bottom")
    
    ggsave(
      file.path(output_dir, "original_population_cost_evolution.png"),
      original_cost_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
  } else {
    cat("  Warning: No original population cost data available for plotting\n")
  }

  # 2c. Original population survival curve (AI vs non-AI comparison)
  cat("  Creating original population survival curve...\n")
  
  # Calculate survival rates for each scenario
  survival_data <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$results)) {
        detailed_results <- all_results[[name]]$results
        
        # Calculate survival rates for each year
        survival_by_year <- detailed_results %>%
          group_by(Year) %>%
          summarise(
            Total_Original_Population = sum(Original_Total_Population, na.rm = TRUE),
            Living_Original_Population = sum(Original_Living_Population, na.rm = TRUE),
            Dead_Original_Population = sum(Original_Death_Count, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            # Calculate survival rate as percentage of original starting population
            # Use the first year's total as the baseline
            Baseline_Original_Population = first(Total_Original_Population),
            Survival_Rate = ifelse(Baseline_Original_Population > 0, 
                                 Living_Original_Population / Baseline_Original_Population * 100, 100),
            Mortality_Rate = ifelse(Baseline_Original_Population > 0,
                                  Dead_Original_Population / Baseline_Original_Population * 100, 0),
            Scenario = name,
            # Clean scenario names for better display
            Scenario_Clean = case_when(
              grepl("AI_BrainMorphometry", name) ~ "AI-Enhanced Care",
              grepl("Standard_Care", name) ~ "Standard Care",
              TRUE ~ name
            )
          )
        
        return(survival_by_year)
      }
    })
  )
  
  if (nrow(survival_data) > 0) {
    # Create survival curve plot
    survival_plot <- survival_data %>%
      ggplot(aes(x = Year, y = Survival_Rate, color = Scenario_Clean)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2, alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Original Population Survival Curve",
        subtitle = "Comparison of survival rates between AI-enhanced and standard care",
        x = "Year",
        y = "Survival Rate (%)",
        color = "Scenario"
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, 100)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_color_manual(
        values = c("AI-Enhanced Care" = "#2E86C1", "Standard Care" = "#E74C3C")
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray60"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
    
    ggsave(
      file.path(output_dir, "original_population_survival_curve.png"),
      survival_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
    
    cat("  Original population survival curve saved\n")
  } else {
    cat("  Warning: No survival data available for plotting\n")
  }


  
  # 4. Diagnosis per state bar plot - AI vs non-AI comparison
  cat("  Creating diagnosis per state bar plot...\n")
  
  # Get final year data for diagnosis per state comparison
  diagnosis_per_state_data <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$results)) {
        all_results[[name]]$results %>%
          filter(Year == max(Year)) %>%  # Final year data
          group_by(area) %>%
          summarise(
            Dx_Mild = sum(Dx_Mild, na.rm = TRUE),
            Dx_Moderate = sum(Dx_Moderate, na.rm = TRUE),
            Dx_Severe = sum(Dx_Severe, na.rm = TRUE),
            Undx_Mild = sum(Undx_Mild, na.rm = TRUE),
            Undx_Moderate = sum(Undx_Moderate, na.rm = TRUE),
            Undx_Severe = sum(Undx_Severe, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(Scenario = name)
      }
    })
  )
  
  if (nrow(diagnosis_per_state_data) > 0) {
    # Reshape data for plotting
    diagnosis_per_state_long <- diagnosis_per_state_data %>%
      pivot_longer(
        cols = c(Dx_Mild, Dx_Moderate, Dx_Severe, Undx_Mild, Undx_Moderate, Undx_Severe),
        names_to = "State",
        values_to = "Population"
      ) %>%
      mutate(
        Diagnosis_Status = case_when(
          grepl("^Dx_", State) ~ "Diagnosed",
          grepl("^Undx_", State) ~ "Undiagnosed",
          TRUE ~ "Other"
        ),
        Severity = case_when(
          grepl("_Mild$", State) ~ "Mild",
          grepl("_Moderate$", State) ~ "Moderate", 
          grepl("_Severe$", State) ~ "Severe",
          TRUE ~ "Other"
        ),
        State_Label = paste(Severity, Diagnosis_Status, sep = " - ")
      ) %>%
      filter(Diagnosis_Status != "Other") %>%
      mutate(
        State_Label = factor(State_Label, 
                           levels = c("Mild - Undiagnosed", "Moderate - Undiagnosed", "Severe - Undiagnosed",
                                    "Mild - Diagnosed", "Moderate - Diagnosed", "Severe - Diagnosed")),
        Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
      )
    
    # Create bar plot
    diagnosis_per_state_plot <- diagnosis_per_state_long %>%
      ggplot(aes(x = State_Label, y = Population, fill = Scenario)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = sprintf("%.0f", Population)), 
                position = position_dodge(width = 0.7), 
                vjust = -0.5, size = 3) +
      facet_wrap(~area, scales = "free_y") +
      labs(
        title = "Dementia Population by State and Severity (Final Year)",
        subtitle = "Comparison of AI vs Standard Care scenarios",
        x = "Dementia State and Severity",
        y = "Population Count",
        fill = "Scenario"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set1",
                        labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
                                  "Standard_Care_All_Areas" = "Standard Care")) +
      scale_y_continuous(labels = scales::comma)
    

  } else {
    cat("  Warning: No data available for diagnosis per state plot\n")
  }
  
  # 5. Dementia diagnosis counts over time - AI vs non-AI comparison
  cat("  Creating dementia diagnosis counts plot...\n")
  
  # Get summary results for diagnosis counts comparison
  diagnosis_counts_data <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$summary_results)) {
        summary_data <- all_results[[name]]$summary_results
        
        # Check if diagnosed count columns exist, otherwise fall back to test counts
        if ("gp_diagnosed" %in% names(summary_data)) {
          summary_data %>%
            select(Year, Scenario, AI_Enabled, 
                   gp_diagnosed, neuropsych_diagnosed, neuroimaging_diagnosed, 
                   ai_morphometry_diagnosed, biomarker_diagnosed) %>%
            mutate(
              Scenario = name,
              total_neuroimaging_diagnosed = neuroimaging_diagnosed + ai_morphometry_diagnosed
            )
        } else {
          # Fallback to test counts if diagnosed counts not available
          summary_data %>%
            select(Year, Scenario, AI_Enabled, 
                   gp_tests, neuropsych_tests, neuroimaging_tests, 
                   ai_morphometry_tests, biomarker_tests) %>%
            mutate(Scenario = name) %>%
            rename(gp_diagnosed = gp_tests, 
                   neuropsych_diagnosed = neuropsych_tests,
                   neuroimaging_diagnosed = neuroimaging_tests,
                   ai_morphometry_diagnosed = ai_morphometry_tests,
                   biomarker_diagnosed = biomarker_tests) %>%
            mutate(total_neuroimaging_diagnosed = neuroimaging_diagnosed + ai_morphometry_diagnosed)
        }
      }
    })
  )
  
  if (nrow(diagnosis_counts_data) > 0) {
    # Reshape data for plotting
    diagnosis_counts_long <- diagnosis_counts_data %>%
      pivot_longer(
        cols = c(gp_diagnosed, neuropsych_diagnosed, neuroimaging_diagnosed,
                 ai_morphometry_diagnosed, total_neuroimaging_diagnosed,
                 biomarker_diagnosed),
        names_to = "Diagnostic_Node",
        values_to = "Diagnosed_Count"
      ) %>%
      mutate(
        Diagnostic_Node = recode(Diagnostic_Node,
                                "gp_diagnosed" = "GP Node",
                                "neuropsych_diagnosed" = "Neuropsych Test",
                                "neuroimaging_diagnosed" = "Standard Neuroimaging",
                                "ai_morphometry_diagnosed" = "AI Morphometry",
                                "total_neuroimaging_diagnosed" = "Total Neuroimaging",
                                "biomarker_diagnosed" = "Biomarker Test"),
        Diagnostic_Node = factor(Diagnostic_Node,
                               levels = c("GP Node", "Neuropsych Test", "Biomarker Test",
                                          "Standard Neuroimaging", "AI Morphometry", "Total Neuroimaging")),
        Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
      )
    
    # Create faceted line plot
    diagnosis_counts_plot <- diagnosis_counts_long %>%
      ggplot(aes(x = Year, y = Diagnosed_Count, color = Scenario)) +
      geom_line(size = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      facet_wrap(~Diagnostic_Node, scales = "free_y", ncol = 3) +
      theme_minimal() +
      labs(
        title = "Dementia Diagnosis Counts Over Time by Diagnostic Node",
        subtitle = "Comparison of AI vs Standard Care scenarios - Shows diagnosed patients, not test counts",
        x = "Year",
        y = "Diagnosed Count",
        color = "Scenario"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10)
      ) +
      scale_color_brewer(type = "qual", palette = "Set1",
                        labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
                                  "Standard_Care_All_Areas" = "Standard Care")) +
      scale_y_continuous(labels = scales::comma)
    
    ggsave(
      file.path(output_dir, "diagnosis_counts_over_time.png"),
      diagnosis_counts_plot,
      width = 18,
      height = 12,
      dpi = 300
    )
  } else {
    cat("  Warning: No data available for diagnosis counts plot\n")
  }
  
  # 5b. Original population dementia diagnosis counts over time - AI vs non-AI comparison
  cat("  Creating original population dementia diagnosis counts plot...\n")
  
  # Get detailed results to calculate original population diagnosis counts using proper filtering
  original_diagnosis_counts_data <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$results)) {
        detailed_data <- all_results[[name]]$results
        
        # Use proper filtering approach like original_population_age_group_distribution
        if ("Original_Total_Population" %in% names(detailed_data)) {
          # Group by Year and age_group, then filter for Original_Total_Population > 0
          # This ensures we only get the original 65-69 cohort as they age through time
          original_diagnosis_by_year <- detailed_data %>%
            group_by(Year, age_group) %>%
            summarise(
              # Sum original population diagnosis counts for each age group
              gp_diagnosed_original = sum(original_gp_diagnosed, na.rm = TRUE),
              neuropsych_diagnosed_original = sum(original_neuropsych_diagnosed, na.rm = TRUE),
              neuroimaging_diagnosed_original = sum(original_neuroimaging_diagnosed, na.rm = TRUE),
              ai_morphometry_diagnosed_original = sum(original_ai_morphometry_diagnosed, na.rm = TRUE),
              biomarker_diagnosed_original = sum(original_biomarker_diagnosed, na.rm = TRUE),
              original_total_neuroimaging_diagnosed = sum(original_total_neuroimaging_diagnosed, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            # Filter to only include age groups where the original cohort exists
            filter(gp_diagnosed_original > 0 | neuropsych_diagnosed_original > 0 | 
                   neuroimaging_diagnosed_original > 0 | ai_morphometry_diagnosed_original > 0 | 
                   biomarker_diagnosed_original > 0 | original_total_neuroimaging_diagnosed > 0)
          
          if (nrow(original_diagnosis_by_year) > 0) {
            # Sum across age groups to get total for the original cohort by year
            original_diagnosis_by_year %>%
              group_by(Year) %>%
              summarise(
                gp_diagnosed_original = sum(gp_diagnosed_original, na.rm = TRUE),
                neuropsych_diagnosed_original = sum(neuropsych_diagnosed_original, na.rm = TRUE),
                neuroimaging_diagnosed_original = sum(neuroimaging_diagnosed_original, na.rm = TRUE),
                ai_morphometry_diagnosed_original = sum(ai_morphometry_diagnosed_original, na.rm = TRUE),
                biomarker_diagnosed_original = sum(biomarker_diagnosed_original, na.rm = TRUE),
                original_total_neuroimaging_diagnosed = sum(original_total_neuroimaging_diagnosed, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(
                Scenario = name,
                AI_Enabled = all_results[[name]]$parameters$ai_enabled
              )
          } else {
            # Fallback if no original population data
            data.frame(
              Year = integer(),
              gp_diagnosed_original = numeric(),
              neuropsych_diagnosed_original = numeric(),
              neuroimaging_diagnosed_original = numeric(),
              ai_morphometry_diagnosed_original = numeric(),
              biomarker_diagnosed_original = numeric(),
              original_total_neuroimaging_diagnosed = numeric(),
              Scenario = character(),
              AI_Enabled = logical(),
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Fallback to current population if original not available
          data.frame(
            Year = integer(),
            gp_diagnosed_original = numeric(),
            neuropsych_diagnosed_original = numeric(),
            neuroimaging_diagnosed_original = numeric(),
            ai_morphometry_diagnosed_original = numeric(),
            biomarker_diagnosed_original = numeric(),
            original_total_neuroimaging_diagnosed = numeric(),
            Scenario = character(),
            AI_Enabled = logical(),
            stringsAsFactors = FALSE
          )
        }
      }
    })
  )
  
  if (nrow(original_diagnosis_counts_data) > 0) {
    # Reshape data for plotting
    original_diagnosis_counts_long <- original_diagnosis_counts_data %>%
      pivot_longer(
        cols = c(gp_diagnosed_original, neuropsych_diagnosed_original,
                 neuroimaging_diagnosed_original, ai_morphometry_diagnosed_original,
                 original_total_neuroimaging_diagnosed, biomarker_diagnosed_original),
        names_to = "Diagnostic_Node",
        values_to = "Original_Diagnosed_Count"
      ) %>%
      mutate(
        Diagnostic_Node = recode(Diagnostic_Node,
                                "gp_diagnosed_original" = "GP Node",
                                "neuropsych_diagnosed_original" = "Neuropsych Test",
                                "neuroimaging_diagnosed_original" = "Standard Neuroimaging",
                                "ai_morphometry_diagnosed_original" = "AI Morphometry",
                                "original_total_neuroimaging_diagnosed" = "Total Neuroimaging",
                                "biomarker_diagnosed_original" = "Biomarker Test"),
        Diagnostic_Node = factor(Diagnostic_Node,
                               levels = c("GP Node", "Neuropsych Test", "Biomarker Test",
                                          "Standard Neuroimaging", "AI Morphometry", "Total Neuroimaging")),
        Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
      )
    
    # Create faceted line plot for original population (same 6 diagnostic nodes)
    original_diagnosis_counts_plot <- original_diagnosis_counts_long %>%
      ggplot(aes(x = Year, y = Original_Diagnosed_Count, color = Scenario)) +
      geom_line(size = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      facet_wrap(~Diagnostic_Node, scales = "free_y", ncol = 3) +
      theme_minimal() +
      labs(
        title = "Original Population Dementia Diagnosis Counts Over Time by Diagnostic Node",
        subtitle = "Comparison of AI vs Standard Care scenarios - Original population cohort",
        x = "Year",
        y = "Original Population Diagnosed Count",
        color = "Scenario"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10)
      ) +
      scale_color_brewer(type = "qual", palette = "Set1",
                        labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
                                  "Standard_Care_All_Areas" = "Standard Care")) +
      scale_y_continuous(labels = scales::comma)
    
    ggsave(
      file.path(output_dir, "original_diagnosis_counts_over_time.png"),
      original_diagnosis_counts_plot,
      width = 18,
      height = 12,
      dpi = 300
    )
  } else {
    cat("  Warning: No data available for original diagnosis counts plot\n")
  }
  
  # 6. Queue wait times - split by urban/rural areas
  cat("  Creating wait times plot...\n")
  
  # Create wait times data from detailed results to include area information
  wait_times_data <- bind_rows(
    lapply(names(all_results), function(name) {
      if (!all_results[[name]]$error && !is.null(all_results[[name]]$results)) {
        all_results[[name]]$results %>%
          group_by(Year, area) %>%
          summarise(
            Avg_Wait_GP = mean(avg_wait_gp, na.rm = TRUE),
            Avg_Wait_MC = mean(avg_wait_mc, na.rm = TRUE),
            Avg_Wait_MRI = mean(avg_wait_mri, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(Scenario = name)
      }
    })
  )
  
  if (nrow(wait_times_data) > 0) {
    wait_plot <- wait_times_data %>%
      pivot_longer(
        cols = c(Avg_Wait_GP, Avg_Wait_MC, Avg_Wait_MRI),
        names_to = "Queue",
        values_to = "Wait_Time"
      ) %>%
      mutate(
        Queue = recode(Queue,
                       "Avg_Wait_GP" = "GP",
                       "Avg_Wait_MC" = "Memory Clinic",
                       "Avg_Wait_MRI" = "MRI")
      ) %>%
      mutate(
        Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
      ) %>%
      ggplot(aes(x = Year, y = Wait_Time, color = Queue)) +
      geom_line(size = 1) +
      facet_grid(area ~ Scenario, labeller = labeller(
        Scenario = c(
          "AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
          "Standard_Care_All_Areas" = "Standard Care"
        )
      )) +
      theme_minimal() +
      labs(
        title = "Average Wait Times by Service and Area",
        x = "Year",
        y = "Average Wait Time (Years)",
        color = "Service"
      ) +
      theme(legend.position = "bottom")
    
    ggsave(
      file.path(output_dir, "wait_times.png"),
      wait_plot,
      width = 12,
      height = 8,
      dpi = 300
    )
  } else {
    cat("  Warning: No wait times data available for plotting\n")
  }

  # 7. Memory clinic comparison plots
  cat("  Creating memory clinic comparison plots...\n")
  mc_summary <- calculate_memory_clinic_summary(all_results)
  if (!is.null(mc_summary) && nrow(mc_summary) > 0) {
    mc_summary <- mc_summary %>%
      mutate(Scenario = factor(Scenario,
                               levels = c("AI_BrainMorphometry_All_Areas",
                                          "Standard_Care_All_Areas")))

    mc_diag_plot <- ggplot(mc_summary, aes(x = Scenario, y = MC_Diagnosed, fill = Scenario)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = scales::comma(MC_Diagnosed, accuracy = 1)), 
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("AI_BrainMorphometry_All_Areas" = "#2E86C1", "Standard_Care_All_Areas" = "#E74C3C"),
                        labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
      scale_x_discrete(labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Memory Clinic Dementia Diagnoses",
           x = "Scenario", y = "Diagnoses") +
      theme_minimal() +
      theme(legend.position = "none")

    ggsave(file.path(output_dir, "memory_clinic_diagnoses.png"),
           mc_diag_plot, width = 8, height = 6, dpi = 300)

    mc_cost_plot <- ggplot(mc_summary, aes(x = Scenario, y = Cost_Per_Diagnosis, fill = Scenario)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = scales::comma(Cost_Per_Diagnosis, accuracy = 0.01)), 
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("AI_BrainMorphometry_All_Areas" = "#2E86C1", "Standard_Care_All_Areas" = "#E74C3C"),
                        labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
      scale_x_discrete(labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Memory Clinic Cost per Dementia Diagnosis",
           x = "Scenario", y = "Cost per Diagnosis (£)") +
      theme_minimal() +
      theme(legend.position = "none")

    ggsave(file.path(output_dir, "memory_clinic_cost_per_diagnosis.png"),
           mc_cost_plot, width = 8, height = 6, dpi = 300)
    
    # Original population memory clinic costs plot
    mc_original_summary <- calculate_original_population_mc_costs(all_results)
    if (!is.null(mc_original_summary) && nrow(mc_original_summary) > 0) {
      mc_original_summary <- mc_original_summary %>%
        mutate(Scenario = factor(Scenario,
                               levels = c("AI_BrainMorphometry_All_Areas",
                                          "Standard_Care_All_Areas")))
      
      mc_original_cost_plot <- ggplot(mc_original_summary, aes(x = Scenario, y = MC_Cost_Original, fill = Scenario)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = scales::comma(MC_Cost_Original, accuracy = 1)), 
                  vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_manual(values = c("AI_BrainMorphometry_All_Areas" = "#2E86C1", "Standard_Care_All_Areas" = "#E74C3C"),
                          labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
        scale_x_discrete(labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Original Population Memory Clinic Total Costs for Dementia Diagnoses",
             x = "Scenario", y = "Total Cost (£)") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(file.path(output_dir, "original_population_memory_clinic_costs.png"),
             mc_original_cost_plot, width = 8, height = 6, dpi = 300)
    }
    
    # Original population total diagnosis costs plot
    total_original_summary <- calculate_original_population_total_costs(all_results)
    if (!is.null(total_original_summary) && nrow(total_original_summary) > 0) {
      total_original_summary <- total_original_summary %>%
        mutate(Scenario = factor(Scenario,
                               levels = c("AI_BrainMorphometry_All_Areas",
                                          "Standard_Care_All_Areas")))
      
      total_original_cost_plot <- ggplot(total_original_summary, aes(x = Scenario, y = Total_Cost_Original, fill = Scenario)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = scales::comma(Total_Cost_Original, accuracy = 1)), 
                  vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_manual(values = c("AI_BrainMorphometry_All_Areas" = "#2E86C1", "Standard_Care_All_Areas" = "#E74C3C"),
                          labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
        scale_x_discrete(labels = c("AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry", "Standard_Care_All_Areas" = "Standard Care")) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Original Population Total Costs for Dementia Diagnoses",
             x = "Scenario", y = "Total Cost (£)") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(file.path(output_dir, "original_population_total_diagnosis_costs.png"),
             total_original_cost_plot, width = 8, height = 6, dpi = 300)
    }
  } else {
    cat("  Warning: No memory clinic summary available for plotting\n")
  }

  # 8. Diagnostic Pathway Waterfall Chart - AI vs non-AI with rural/urban splits
  cat("  Creating diagnostic pathway waterfall chart...\n")
  
  # Function to calculate pathway percentages from real simulation data (original population)
  calculate_pathway_waterfall_from_simulation <- function(all_results, scenario_name, area_type) {
    # Get the simulation results for this scenario
    scenario_results <- all_results[[scenario_name]]
    
    if (scenario_results$error || is.null(scenario_results$results)) {
      return(NULL)
    }
    
    # Get detailed results for the area
    area_results <- scenario_results$results %>%
      filter(area == area_type) %>%
      group_by(Year) %>%
      summarise(
        # Calculate totals across all subgroups for this area (original population)
        total_population = sum(Original_Total_Population, na.rm = TRUE),
        symptomatic_presentations = sum(Original_Undiagnosed_Dementia + original_population_MCI, na.rm = TRUE), # Approximate symptomatic
        gp_tests = sum(original_neuropsych_tests + original_neuroimaging_tests + original_biomarker_tests, na.rm = TRUE), # Total tests
        neuropsych_tests = sum(original_neuropsych_tests, na.rm = TRUE),
        neuroimaging_tests = sum(original_neuroimaging_tests, na.rm = TRUE),
        ai_morphometry_tests = sum(original_ai_morphometry_tests, na.rm = TRUE),
        biomarker_tests = sum(original_biomarker_tests, na.rm = TRUE),
        diagnosed_dementia = sum(Original_Diagnosed_Dementia, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Use the most recent year for the waterfall (or average across years)
      filter(Year == max(Year)) %>%
      slice(1) # Take first if multiple rows
    
    if (nrow(area_results) == 0) {
      return(NULL)
    }
    
    # Calculate percentages based on actual simulation data
    total_symptomatic <- area_results$symptomatic_presentations
    total_tests <- area_results$gp_tests
    # Combine neuroimaging and AI morphometry for total neuroimaging pathway
    total_neuroimaging_tests <- area_results$neuroimaging_tests + area_results$ai_morphometry_tests
    total_mc_tests <- area_results$neuropsych_tests + total_neuroimaging_tests + area_results$biomarker_tests
    
    # Calculate pathway percentages (normalize to 100% starting point)
    if (total_symptomatic > 0) {
      symptomatic_presentations <- 100
      accept_testing <- (total_tests / total_symptomatic) * 100
      gp_confident <- accept_testing * 0.8 # Assume 80% confident (from parameters)
      gp_not_confident <- accept_testing * 0.2
      
      # Estimate positive vs negative tests (based on diagnosis rate)
      positive_tests <- (area_results$diagnosed_dementia / total_symptomatic) * 100
      negative_tests <- accept_testing - positive_tests
      
      # MC referrals (not confident + some positive tests)
      total_mc_referrals <- gp_not_confident + (positive_tests * 0.9) # Assume 90% referral rate
      not_referred_positive <- positive_tests * 0.1
      
      # MC acceptance (assume 93.3% acceptance rate from parameters)
      mc_accepting <- total_mc_referrals * 0.933
      mc_refusing <- total_mc_referrals * 0.067
      
      # MC pathways (based on actual test counts)
      if (total_mc_tests > 0) {
        neuropsych <- (area_results$neuropsych_tests / total_mc_tests) * mc_accepting
        # Combine neuroimaging (current) with AI morphometry under neuroimaging pathway
        neuroimaging <- (total_neuroimaging_tests / total_mc_tests) * mc_accepting
        biomarker <- (area_results$biomarker_tests / total_mc_tests) * mc_accepting
        no_further <- mc_accepting - neuropsych - neuroimaging - biomarker
      } else {
        neuropsych <- 0
        neuroimaging <- 0
        biomarker <- 0
        no_further <- mc_accepting
      }
    } else {
      # Fallback to theoretical calculations if no data
      symptomatic_presentations <- 100
      accept_testing <- 0
      gp_confident <- 0
      gp_not_confident <- 0
      positive_tests <- 0
      negative_tests <- 0
      total_mc_referrals <- 0
      not_referred_positive <- 0
      mc_accepting <- 0
      mc_refusing <- 0
      neuropsych <- 0
      neuroimaging <- 0
      biomarker <- 0
      no_further <- 0
    }
    
    # Create waterfall data frame
    waterfall_data <- data.frame(
      step = c(
        "Symptomatic\nPresentations",
        "Accept\nTesting",
        "GP Confident",
        "GP Not\nConfident", 
        "Positive\nTests",
        "Negative\nTests",
        "Referred to\nMC",
        "Not Referred\n(Stay at GP)",
        "MC Accepting",
        "MC Refusing",
        "Neuropsych\nPathway",
        "Neuroimaging\nPathway", 
        "Biomarker\nPathway",
        "No Further\nTesting"
      ),
      percentage = c(
        symptomatic_presentations,
        accept_testing,
        gp_confident,
        gp_not_confident,
        positive_tests,
        negative_tests,
        total_mc_referrals,
        not_referred_positive,
        mc_accepting,
        mc_refusing,
        neuropsych,
        neuroimaging,
        biomarker,
        no_further
      ),
      stringsAsFactors = FALSE
    )
    
    return(waterfall_data)
  }
  
  # Calculate waterfall data for AI and non-AI scenarios with rural/urban splits
  # AI scenario
  waterfall_ai_urban <- calculate_pathway_waterfall_from_simulation(all_results, "AI_BrainMorphometry_All_Areas", "Urban")
  waterfall_ai_rural <- calculate_pathway_waterfall_from_simulation(all_results, "AI_BrainMorphometry_All_Areas", "Rural")
  
  # Non-AI scenario
  waterfall_no_ai_urban <- calculate_pathway_waterfall_from_simulation(all_results, "Standard_Care_All_Areas", "Urban")
  waterfall_no_ai_rural <- calculate_pathway_waterfall_from_simulation(all_results, "Standard_Care_All_Areas", "Rural")
  
  # Combine all data (filter out NULL results)
  waterfall_data_list <- list(
    waterfall_ai_urban,
    waterfall_ai_rural,
    waterfall_no_ai_urban,
    waterfall_no_ai_rural
  )
  
  # Add scenario and area information
  if (!is.null(waterfall_ai_urban)) {
    waterfall_ai_urban <- waterfall_ai_urban %>% mutate(area = "Urban", scenario = "AI_BrainMorphometry")
  }
  if (!is.null(waterfall_ai_rural)) {
    waterfall_ai_rural <- waterfall_ai_rural %>% mutate(area = "Rural", scenario = "AI_BrainMorphometry")
  }
  if (!is.null(waterfall_no_ai_urban)) {
    waterfall_no_ai_urban <- waterfall_no_ai_urban %>% mutate(area = "Urban", scenario = "Standard_Care")
  }
  if (!is.null(waterfall_no_ai_rural)) {
    waterfall_no_ai_rural <- waterfall_no_ai_rural %>% mutate(area = "Rural", scenario = "Standard_Care")
  }
  
  # Combine all data
  waterfall_data <- bind_rows(
    waterfall_ai_urban,
    waterfall_ai_rural,
    waterfall_no_ai_urban,
    waterfall_no_ai_rural
  )
  
  if (nrow(waterfall_data) > 0) {
    waterfall_data <- waterfall_data %>%
      mutate(
        step = factor(step, levels = unique(step)),
        area = factor(area, levels = c("Urban", "Rural")),
        scenario = factor(scenario, levels = c("AI_BrainMorphometry", "Standard_Care"))
      )
    
    # Create waterfall chart with AI vs non-AI and rural vs urban splits
    waterfall_plot <- waterfall_data %>%
      ggplot(aes(x = step, y = percentage, fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), 
                position = position_dodge(width = 0.7), 
                vjust = -0.5, size = 2.5) +
      facet_grid(area ~ .) +
      labs(title = "Diagnostic Pathway Waterfall Chart (Real Simulation Data)",
           subtitle = "Percentage flow through each decision point based on actual simulation results",
           x = "Pathway Step", 
           y = "Percentage (%)",
           fill = "Scenario") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold", size = 10)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set1", 
                        labels = c("AI_BrainMorphometry" = "AI Brain Morphometry", 
                                  "Standard_Care" = "Standard Care")) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), 
                         limits = c(0, max(waterfall_data$percentage) * 1.1))
    
    ggsave(
      file.path(output_dir, "queue_diagnostic_pathway_waterfall.png"),
      waterfall_plot,
      width = 18,
      height = 12,
      dpi = 300,
      bg = "white"
    )
  } else {
    cat("  Warning: No simulation data available for waterfall chart\n")
  }
  
  # 8b. Diagnosis heatmap by state and area
  cat("  Creating diagnosis heatmap...\n")
  heatmap_plot <- create_diagnosis_heatmap(all_results)
  if (!is.null(heatmap_plot)) {

  } else {
    cat("  Warning: No data available for diagnosis heatmap\n")
  }
  
  # 8. Original population trends (excluding inflow)
  cat("  Creating original population trends plot...\n")
  if ("Original_Total_Population" %in% names(combined_results)) {
    original_pop_plot <- combined_results %>%
      select(Year, Scenario, starts_with("Original_")) %>%
      # Rename columns for plotting
      rename(
        Healthy = Original_Other_Healthy,
        MCI = Original_MCI,
        Diagnosed_Dementia = Original_Diagnosed_Dementia,
        Undiagnosed_Dementia = Original_Undiagnosed_Dementia
      ) %>%
      select(Year, Scenario, Healthy, MCI, Diagnosed_Dementia, Undiagnosed_Dementia) %>%
      pivot_longer(
        cols = c(Healthy, MCI, Diagnosed_Dementia, Undiagnosed_Dementia),
        names_to = "State",
        values_to = "Population"
      ) %>%
      mutate(
        Scenario = factor(Scenario, levels = c("AI_BrainMorphometry_All_Areas", "Standard_Care_All_Areas"))
      ) %>%
      ggplot(aes(x = Year, y = Population, color = State)) +
      geom_line(size = 1.2) +
      facet_wrap(~Scenario, labeller = labeller(
        Scenario = c(
          "AI_BrainMorphometry_All_Areas" = "AI Brain Morphometry",
          "Standard_Care_All_Areas" = "Standard Care"
        )
      )) +
      theme_minimal() +
      labs(
        title = "Original Population Distribution Over Time (Excluding Inflow)",
        subtitle = "Tracks lifetime outcomes for the original cohort only",
        x = "Year",
        y = "Population",
        color = "Health State"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme(legend.position = "bottom")
    
    ggsave(
      file.path(output_dir, "original_population_trends.png"),
      original_pop_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
  } else {
    cat("  Warning: Original population data not found, skipping original population trends plot\n")
  }

  # 9. Age group distribution over time
  cat("  Creating age group distribution plot...\n")
  
  # Get detailed results for age group analysis
  age_group_data <- list()
  for (scenario_name in names(all_results)) {
    if (!all_results[[scenario_name]]$error && !is.null(all_results[[scenario_name]]$results)) {
      detailed_results <- all_results[[scenario_name]]$results
      
      # Calculate age group totals by year
      age_summary <- detailed_results %>%
        group_by(Year, age_group) %>%
        summarise(
          Total_Population = sum(Total_Population, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Scenario = scenario_name)
      
      age_group_data[[scenario_name]] <- age_summary
    }
  }
  
  if (length(age_group_data) > 0) {
    # Combine all age group data
    combined_age_data <- bind_rows(age_group_data)
    
    # Create stacked barplot with percentages using base R graphics
    # Prepare data for base R
    base_data <- combined_age_data %>%
      group_by(Year, Scenario) %>%
      mutate(
        Total_Year = sum(Total_Population, na.rm = TRUE),
        Percentage = (Total_Population / Total_Year) * 100
      ) %>%
      ungroup() %>%
      # Ensure age groups are in the correct order
      mutate(age_group = factor(age_group, 
                               levels = c("65 to 69", "70-74", "75-79", "80-84", "85+")))
    
    # Create interactive HTML visualization using plotly
    library(plotly)
    
    # Prepare data for plotly
    plotly_data <- base_data %>%
      group_by(Year, Scenario) %>%
      mutate(
        Total_Year = sum(Total_Population, na.rm = TRUE),
        Percentage = (Total_Population / Total_Year) * 100
      ) %>%
      ungroup() %>%
      # Ensure age groups are in the correct order
      mutate(age_group = factor(age_group, 
                               levels = c("65 to 69", "70-74", "75-79", "80-84", "85+")))
    
    # Create separate plots for each scenario to avoid duplication
    scenario_names <- unique(plotly_data$Scenario)
    
    # Create a subplot for each scenario
    subplot_list <- list()
    
    for (i in seq_along(scenario_names)) {
      scenario <- scenario_names[i]
      scenario_data <- plotly_data %>% filter(Scenario == scenario)
      
      # Clean scenario name for display
      scenario_display <- case_when(
        grepl("AI_BrainMorphometry", scenario) ~ "AI-Enhanced Care",
        grepl("Standard_Care", scenario) ~ "Standard Care",
        TRUE ~ scenario
      )
      
      subplot_list[[i]] <- plot_ly() %>%
        # Add traces for each age group (oldest to youngest for proper stacking)
        add_trace(
          data = scenario_data %>% filter(age_group == "85+"),
          x = ~Year, y = ~Percentage, type = 'bar', name = '85+',
          marker = list(color = '#592E83', line = list(color = 'white', width = 1)),
          text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>85+</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<extra></extra>"),
          showlegend = (i == 1)  # Only show legend for first subplot
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "80-84"),
          x = ~Year, y = ~Percentage, type = 'bar', name = '80-84',
          marker = list(color = '#C73E1D', line = list(color = 'white', width = 1)),
          text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>80-84</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "75-79"),
          x = ~Year, y = ~Percentage, type = 'bar', name = '75-79',
          marker = list(color = '#F18F01', line = list(color = 'white', width = 1)),
          text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>75-79</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "70-74"),
          x = ~Year, y = ~Percentage, type = 'bar', name = '70-74',
          marker = list(color = '#A23B72', line = list(color = 'white', width = 1)),
          text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>70-74</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "65 to 69"),
          x = ~Year, y = ~Percentage, type = 'bar', name = '65 to 69',
          marker = list(color = '#2E86AB', line = list(color = 'white', width = 1)),
          text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>65 to 69</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        layout(
          xaxis = list(
            title = if (i == length(scenario_names)) "Year" else "",
            showgrid = TRUE,
            gridcolor = '#E1E5E9',
            zeroline = FALSE
          ),
          yaxis = list(
            title = if (i == 1) "Percentage of Total Population" else "",
            range = c(0, 100),
            tickformat = ',.0%',
            showgrid = TRUE,
            gridcolor = '#E1E5E9',
            zeroline = FALSE
          ),
          barmode = 'stack',
          annotations = list(
            text = scenario_display,
            x = 0.5,
            y = 1.05,
            xref = 'paper',
            yref = 'paper',
            xanchor = 'center',
            yanchor = 'bottom',
            showarrow = FALSE,
            font = list(size = 14, color = 'black')
          )
        )
    }
    
    # Combine subplots
    age_dist_plot <- subplot(
      subplot_list,
      nrows = 1,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    ) %>%
      layout(
        title = list(
          text = "Age Group Distribution Over Time by Scenario",
          x = 0.5,
          font = list(size = 20, color = 'black')
        ),
        showlegend = TRUE,
        legend = list(
          orientation = 'h',
          x = 0.5,
          y = -0.15,
          xanchor = 'center',
          yanchor = 'top',
          font = list(size = 12)
        ),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        margin = list(t = 100, b = 100, l = 80, r = 50),
        hovermode = 'closest'
      )
    
    
    
    # Create count-based age group distribution visualization
    cat("  Creating age group distribution counts plot...\n")
    
    # Create separate plots for each scenario showing counts
    counts_subplot_list <- list()
    
    for (i in seq_along(scenario_names)) {
      scenario <- scenario_names[i]
      scenario_data <- plotly_data %>% filter(Scenario == scenario)
      
      # Clean scenario name for display
      scenario_display <- case_when(
        grepl("AI_BrainMorphometry", scenario) ~ "AI-Enhanced Care",
        grepl("Standard_Care", scenario) ~ "Standard Care",
        TRUE ~ scenario
      )
      
      counts_subplot_list[[i]] <- plot_ly() %>%
        # Add traces for each age group (oldest to youngest for proper stacking)
        add_trace(
          data = scenario_data %>% filter(age_group == "85+"),
          x = ~Year, y = ~Total_Population, type = 'bar', name = '85+',
          marker = list(color = '#592E83', line = list(color = 'white', width = 1)),
          text = ~scales::comma(Total_Population), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>85+</b><br>", scenario_display, "<br>Year: %{x}<br>Population: %{y:,}<extra></extra>"),
          showlegend = (i == 1)  # Only show legend for first subplot
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "80-84"),
          x = ~Year, y = ~Total_Population, type = 'bar', name = '80-84',
          marker = list(color = '#C73E1D', line = list(color = 'white', width = 1)),
          text = ~scales::comma(Total_Population), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>80-84</b><br>", scenario_display, "<br>Year: %{x}<br>Population: %{y:,}<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "75-79"),
          x = ~Year, y = ~Total_Population, type = 'bar', name = '75-79',
          marker = list(color = '#F18F01', line = list(color = 'white', width = 1)),
          text = ~scales::comma(Total_Population), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>75-79</b><br>", scenario_display, "<br>Year: %{x}<br>Population: %{y:,}<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "70-74"),
          x = ~Year, y = ~Total_Population, type = 'bar', name = '70-74',
          marker = list(color = '#A23B72', line = list(color = 'white', width = 1)),
          text = ~scales::comma(Total_Population), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>70-74</b><br>", scenario_display, "<br>Year: %{x}<br>Population: %{y:,}<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        add_trace(
          data = scenario_data %>% filter(age_group == "65 to 69"),
          x = ~Year, y = ~Total_Population, type = 'bar', name = '65 to 69',
          marker = list(color = '#2E86AB', line = list(color = 'white', width = 1)),
          text = ~scales::comma(Total_Population), textposition = 'inside',
          textfont = list(color = 'white', size = 10),
          hovertemplate = paste0("<b>65 to 69</b><br>", scenario_display, "<br>Year: %{x}<br>Population: %{y:,}<extra></extra>"),
          showlegend = (i == 1)
        ) %>%
        layout(
          xaxis = list(
            title = if (i == length(scenario_names)) "Year" else "",
            showgrid = TRUE,
            gridcolor = '#E1E5E9',
            zeroline = FALSE
          ),
          yaxis = list(
            title = if (i == 1) "Population Count" else "",
            showgrid = TRUE,
            gridcolor = '#E1E5E9',
            zeroline = FALSE
          ),
          barmode = 'stack',
          annotations = list(
            text = scenario_display,
            x = 0.5,
            y = 1.05,
            xref = 'paper',
            yref = 'paper',
            xanchor = 'center',
            yanchor = 'bottom',
            showarrow = FALSE,
            font = list(size = 14, color = 'black')
          )
        )
    }
    
    # Combine subplots for counts
    age_dist_counts_plot <- subplot(
      counts_subplot_list,
      nrows = 1,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE
    ) %>%
      layout(
        title = list(
          text = "Age Group Population Counts Over Time by Scenario",
          x = 0.5,
          font = list(size = 20, color = 'black')
        ),
        showlegend = TRUE,
        legend = list(
          orientation = 'h',
          x = 0.5,
          y = -0.15,
          xanchor = 'center',
          yanchor = 'top',
          font = list(size = 12)
        ),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        margin = list(t = 100, b = 100, l = 80, r = 50),
        hovermode = 'closest'
      )
    

    
    # Create age group distribution table
    cat("  Creating age group distribution table...\n")
    age_table <- combined_age_data %>%
      group_by(Year, Scenario) %>%
      mutate(
        Total_Year = sum(Total_Population, na.rm = TRUE),
        Percentage = (Total_Population / Total_Year) * 100
      ) %>%
      ungroup() %>%
      select(Year, Scenario, age_group, Total_Population, Percentage) %>%
      arrange(Year, Scenario, age_group)
    
    write.csv(
      age_table,
      file.path(output_dir, "age_group_distribution.csv"),
      row.names = FALSE
    )
    
    cat("    Age group distribution table saved to:", file.path(output_dir, "age_group_distribution.csv"), "\n")
  } else {
    cat("  Warning: No age group data found, skipping age group distribution plot\n")
  }

  # 10. Original population age group distribution over time
  cat("  Creating original population age group distribution plot...\n")
  
  # Get detailed results for original population age group analysis
  original_age_group_data <- list()
  for (scenario_name in names(all_results)) {
    if (!all_results[[scenario_name]]$error && !is.null(all_results[[scenario_name]]$results)) {
      detailed_results <- all_results[[scenario_name]]$results
      
      # Calculate original population age group totals by year
      original_age_summary <- detailed_results %>%
        group_by(Year, age_group) %>%
        summarise(
          Original_Total_Population = sum(Original_Total_Population, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Scenario = scenario_name)
      
      original_age_group_data[[scenario_name]] <- original_age_summary
    }
  }
  
  if (length(original_age_group_data) > 0) {
    # Combine all original age group data
    combined_original_age_data <- bind_rows(original_age_group_data)
    
    # Filter out age groups with zero original population (will be empty for most groups except 65 to 69)
    combined_original_age_data <- combined_original_age_data %>%
      filter(Original_Total_Population > 0)
    
    if (nrow(combined_original_age_data) > 0) {
      # Prepare data for plotly - calculate percentages
      plotly_original_data <- combined_original_age_data %>%
        group_by(Year, Scenario) %>%
        mutate(
          Total_Year = sum(Original_Total_Population, na.rm = TRUE),
          Percentage = (Original_Total_Population / Total_Year) * 100
        ) %>%
        ungroup() %>%
        # Ensure age groups are in the correct order
        mutate(age_group = factor(age_group, 
                                 levels = c("65 to 69", "70-74", "75-79", "80-84", "85+")))
      
      # Create separate plots for each scenario to avoid duplication
      original_scenario_names <- unique(plotly_original_data$Scenario)
      
      # Create a subplot for each scenario
      original_subplot_list <- list()
      
      for (i in seq_along(original_scenario_names)) {
        scenario <- original_scenario_names[i]
        original_scenario_data <- plotly_original_data %>% filter(Scenario == scenario)
        
        # Clean scenario name for display
        scenario_display <- case_when(
          grepl("AI_BrainMorphometry", scenario) ~ "AI-Enhanced Care",
          grepl("Standard_Care", scenario) ~ "Standard Care",
          TRUE ~ scenario
        )
        
        original_subplot_list[[i]] <- plot_ly() %>%
          # Add traces for each age group (oldest to youngest for proper stacking)
          add_trace(
            data = original_scenario_data %>% filter(age_group == "85+"),
            x = ~Year, y = ~Percentage, type = 'bar', name = '85+',
            marker = list(color = '#592E83', line = list(color = 'white', width = 1)),
            text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>85+</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<br>Population: %{customdata}<extra></extra>"),
            customdata = ~Original_Total_Population,
            showlegend = (i == 1)  # Only show legend for first subplot
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "80-84"),
            x = ~Year, y = ~Percentage, type = 'bar', name = '80-84',
            marker = list(color = '#C73E1D', line = list(color = 'white', width = 1)),
            text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>80-84</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<br>Population: %{customdata}<extra></extra>"),
            customdata = ~Original_Total_Population,
            showlegend = (i == 1)
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "75-79"),
            x = ~Year, y = ~Percentage, type = 'bar', name = '75-79',
            marker = list(color = '#F18F01', line = list(color = 'white', width = 1)),
            text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>75-79</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<br>Population: %{customdata}<extra></extra>"),
            customdata = ~Original_Total_Population,
            showlegend = (i == 1)
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "70-74"),
            x = ~Year, y = ~Percentage, type = 'bar', name = '70-74',
            marker = list(color = '#A23B72', line = list(color = 'white', width = 1)),
            text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>70-74</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<br>Population: %{customdata}<extra></extra>"),
            customdata = ~Original_Total_Population,
            showlegend = (i == 1)
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "65 to 69"),
            x = ~Year, y = ~Percentage, type = 'bar', name = '65 to 69',
            marker = list(color = '#2E86AB', line = list(color = 'white', width = 1)),
            text = ~sprintf("%.1f%%", Percentage), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>65 to 69</b><br>", scenario_display, "<br>Year: %{x}<br>Percentage: %{y:.1f}%<br>Population: %{customdata}<extra></extra>"),
            customdata = ~Original_Total_Population,
            showlegend = (i == 1)
          ) %>%
          layout(
            xaxis = list(
              title = if (i == length(original_scenario_names)) "Year" else "",
              showgrid = TRUE,
              gridcolor = '#E1E5E9',
              zeroline = FALSE
            ),
            yaxis = list(
              title = if (i == 1) "Percentage of Original Population" else "",
              range = c(0, 100),
              tickformat = ',.0%',
              showgrid = TRUE,
              gridcolor = '#E1E5E9',
              zeroline = FALSE
            ),
            barmode = 'stack',
            annotations = list(
              text = scenario_display,
              x = 0.5,
              y = 1.05,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'center',
              yanchor = 'bottom',
              showarrow = FALSE,
              font = list(size = 14, color = 'black')
            )
          )
      }
      
      # Combine subplots
      original_age_dist_plot <- subplot(
        original_subplot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        titleY = TRUE
      ) %>%
        layout(
          title = list(
            text = "Original Population Age Group Distribution Over Time by Scenario",
            x = 0.5,
            font = list(size = 20, color = 'black')
          ),
          showlegend = TRUE,
          legend = list(
            orientation = 'h',
            x = 0.5,
            y = -0.15,
            xanchor = 'center',
            yanchor = 'top',
            font = list(size = 12)
          ),
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          margin = list(t = 100, b = 100, l = 80, r = 50),
          hovermode = 'closest'
        )
      

      
      # Create count-based original population age group distribution visualization
      cat("  Creating original population age group distribution counts plot...\n")
      
      # Create separate plots for each scenario showing original population counts
      original_counts_subplot_list <- list()
      
      for (i in seq_along(original_scenario_names)) {
        scenario <- original_scenario_names[i]
        original_scenario_data <- plotly_original_data %>% filter(Scenario == scenario)
        
        # Clean scenario name for display
        scenario_display <- case_when(
          grepl("AI_BrainMorphometry", scenario) ~ "AI-Enhanced Care",
          grepl("Standard_Care", scenario) ~ "Standard Care",
          TRUE ~ scenario
        )
        
        original_counts_subplot_list[[i]] <- plot_ly() %>%
          # Add traces for each age group (oldest to youngest for proper stacking)
          add_trace(
            data = original_scenario_data %>% filter(age_group == "85+"),
            x = ~Year, y = ~Original_Total_Population, type = 'bar', name = '85+',
            marker = list(color = '#592E83', line = list(color = 'white', width = 1)),
            text = ~scales::comma(Original_Total_Population), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>85+</b><br>", scenario_display, "<br>Year: %{x}<br>Original Population: %{y:,}<extra></extra>"),
            showlegend = (i == 1)  # Only show legend for first subplot
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "80-84"),
            x = ~Year, y = ~Original_Total_Population, type = 'bar', name = '80-84',
            marker = list(color = '#C73E1D', line = list(color = 'white', width = 1)),
            text = ~scales::comma(Original_Total_Population), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>80-84</b><br>", scenario_display, "<br>Year: %{x}<br>Original Population: %{y:,}<extra></extra>"),
            showlegend = (i == 1)
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "75-79"),
            x = ~Year, y = ~Original_Total_Population, type = 'bar', name = '75-79',
            marker = list(color = '#F18F01', line = list(color = 'white', width = 1)),
            text = ~scales::comma(Original_Total_Population), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>75-79</b><br>", scenario_display, "<br>Year: %{x}<br>Original Population: %{y:,}<extra></extra>"),
            showlegend = (i == 1)
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "70-74"),
            x = ~Year, y = ~Original_Total_Population, type = 'bar', name = '70-74',
            marker = list(color = '#A23B72', line = list(color = 'white', width = 1)),
            text = ~scales::comma(Original_Total_Population), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>70-74</b><br>", scenario_display, "<br>Year: %{x}<br>Original Population: %{y:,}<extra></extra>"),
            showlegend = (i == 1)
          ) %>%
          add_trace(
            data = original_scenario_data %>% filter(age_group == "65 to 69"),
            x = ~Year, y = ~Original_Total_Population, type = 'bar', name = '65 to 69',
            marker = list(color = '#2E86AB', line = list(color = 'white', width = 1)),
            text = ~scales::comma(Original_Total_Population), textposition = 'inside',
            textfont = list(color = 'white', size = 10),
            hovertemplate = paste0("<b>65 to 69</b><br>", scenario_display, "<br>Year: %{x}<br>Original Population: %{y:,}<extra></extra>"),
            showlegend = (i == 1)
          ) %>%
          layout(
            xaxis = list(
              title = if (i == length(original_scenario_names)) "Year" else "",
              showgrid = TRUE,
              gridcolor = '#E1E5E9',
              zeroline = FALSE
            ),
            yaxis = list(
              title = if (i == 1) "Original Population Count" else "",
              showgrid = TRUE,
              gridcolor = '#E1E5E9',
              zeroline = FALSE
            ),
            barmode = 'stack',
            annotations = list(
              text = scenario_display,
              x = 0.5,
              y = 1.05,
              xref = 'paper',
              yref = 'paper',
              xanchor = 'center',
              yanchor = 'bottom',
              showarrow = FALSE,
              font = list(size = 14, color = 'black')
            )
          )
      }
      
      # Combine subplots for original population counts
      original_age_dist_counts_plot <- subplot(
        original_counts_subplot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        titleY = TRUE
      ) %>%
        layout(
          title = list(
            text = "Original Population Age Group Counts Over Time by Scenario",
            x = 0.5,
            font = list(size = 20, color = 'black')
          ),
          showlegend = TRUE,
          legend = list(
            orientation = 'h',
            x = 0.5,
            y = -0.15,
            xanchor = 'center',
            yanchor = 'top',
            font = list(size = 12)
          ),
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          margin = list(t = 100, b = 100, l = 80, r = 50),
          hovermode = 'closest'
        )
      

      
      # Create original population age group distribution table
      cat("  Creating original population age group distribution table...\n")
      original_age_table <- combined_original_age_data %>%
        group_by(Year, Scenario) %>%
        mutate(
          Total_Year = sum(Original_Total_Population, na.rm = TRUE),
          Percentage = (Original_Total_Population / Total_Year) * 100
        ) %>%
        ungroup() %>%
        select(Year, Scenario, age_group, Original_Total_Population, Percentage) %>%
        arrange(Year, Scenario, age_group)
      
      write.csv(
        original_age_table,
        file.path(output_dir, "original_population_age_group_distribution.csv"),
        row.names = FALSE
      )
      
      cat("    Original population age group distribution table saved to:", file.path(output_dir, "original_population_age_group_distribution.csv"), "\n")
    } else {
      cat("  Warning: No original population found in any age groups\n")
    }
  } else {
    cat("  Warning: No original population age group data found, skipping original population age group distribution plot\n")
  }

  cat("  All plots saved to:", output_dir, "\n")
  cat("    - ai_adoption_rate.png: AI technology adoption curve\n")
  cat("    - population_trends.png: Population distribution over time\n")
  cat("    - cost_evolution.png: Cost breakdown over time\n") 
  

  cat("    - diagnosis_counts_over_time.png: Dementia diagnosis counts by diagnostic node (AI vs non-AI, includes combined neuroimaging)\n")
  cat("    - original_diagnosis_counts_over_time.png: Original population dementia diagnosis counts over time (AI vs non-AI)\n")
  cat("    - memory_clinic_diagnoses.png: Memory clinic dementia diagnoses by scenario\n")
  cat("    - memory_clinic_cost_per_diagnosis.png: Memory clinic cost per diagnosis by scenario\n")
  cat("    - original_population_memory_clinic_costs.png: Original population memory clinic total costs for dementia diagnoses by scenario\n")
  cat("    - original_population_total_diagnosis_costs.png: Original population total costs for dementia diagnoses by scenario\n")
  cat("    - wait_times.png: Average wait times by service\n")
  cat("    - queue_diagnostic_pathway_waterfall.png: Diagnostic pathway waterfall (AI vs non-AI, rural vs urban)\n")

  cat("    - original_population_trends.png: Original cohort tracking\n")




}

# Convenience function to run with default settings
run_default_analysis <- function() {
  run_dementia_model(
    years = 0:50,
    verbose = FALSE,
    save_results = TRUE,
    output_dir = "results"
  )
}

# If running as script
if (sys.nframe() == 0) {
  cat("Running default analysis...\n")
  results <- run_default_analysis()
}

# OWSA (One-Way Sensitivity Analysis) Functions
# =============================================

# Get valid parameters for OWSA
get_valid_owsa_parameters <- function() {
  c("ai_morphometry_per_scan", "ai_mci_sens", "ai_mci_spec", "ai_dementia_sens", "ai_dementia_spec", 
    "radiologist_cost_non_ai", "ai_neuropsych", "ai_neuroimaging", "ai_biomarker",
    "ai_max_adoption", "ai_adoption_rate", "ai_initial_adoption")
}

# Create modified parameters for OWSA
create_modified_params <- function(base_params, param_name, new_value) {
  modified_params <- base_params
  
  # Modify the specific parameter
  if (param_name == "ai_morphometry_per_scan") {
    modified_params$test_costs$ai_morphometry_per_scan <- new_value
  } else if (param_name == "ai_mci_sens") {
    # This affects the neuroimaging test performance
    # We need to modify the test performance calculation
    modified_params$ai_mci_sens_override <- new_value
  } else if (param_name == "ai_mci_spec") {
    modified_params$ai_mci_spec_override <- new_value
  } else if (param_name == "ai_dementia_sens") {
    modified_params$ai_dementia_sens_override <- new_value
  } else if (param_name == "ai_dementia_spec") {
    modified_params$ai_dementia_spec_override <- new_value
  } else if (param_name == "radiologist_cost_non_ai") {
    modified_params$test_costs$radiologist_cost_non_ai <- new_value
  } else if (param_name == "ai_neuropsych") {
    modified_params$ai_neuropsych_override <- new_value
  } else if (param_name == "ai_neuroimaging") {
    modified_params$ai_neuroimaging_override <- new_value
  } else if (param_name == "ai_biomarker") {
    modified_params$ai_biomarker_override <- new_value
  }
  
  return(modified_params)
}

# Run simulation with modified parameters
run_simulation_with_modified_params <- function(param_name, new_value, years, verbose = FALSE) {
  # Store original functions
  original_get_test_costs <- get_test_costs
  original_get_capacity_growth_rates <- get_capacity_growth_rates
  
  # Override functions temporarily based on parameter
  if (param_name == "ai_morphometry_per_scan") {
    assign("get_test_costs", function() {
      costs <- original_get_test_costs()
      costs$ai_morphometry_per_scan <- new_value
      return(costs)
    }, envir = .GlobalEnv)
  } else if (param_name == "radiologist_cost_non_ai") {
    assign("get_test_costs", function() {
      costs <- original_get_test_costs()
      costs$radiologist_cost_non_ai <- new_value
      return(costs)
    }, envir = .GlobalEnv)
  } else if (param_name == "ai_max_adoption") {
    assign("get_capacity_growth_rates", function() {
      rates <- original_get_capacity_growth_rates()
      rates$ai_max_adoption <- new_value
      return(rates)
    }, envir = .GlobalEnv)
  } else if (param_name == "ai_adoption_rate") {
    assign("get_capacity_growth_rates", function() {
      rates <- original_get_capacity_growth_rates()
      rates$ai_adoption_rate <- new_value
      return(rates)
    }, envir = .GlobalEnv)
  } else if (param_name == "ai_initial_adoption") {
    assign("get_capacity_growth_rates", function() {
      rates <- original_get_capacity_growth_rates()
      rates$ai_initial_adoption <- new_value
      return(rates)
    }, envir = .GlobalEnv)
  }
  
  # Run the simulation with modified parameters
  tryCatch({
    result <- run_dementia_model(
      years = years,
      verbose = verbose,
      save_results = FALSE
    )
    
    # Restore original functions
    assign("get_test_costs", original_get_test_costs, envir = .GlobalEnv)
    assign("get_capacity_growth_rates", original_get_capacity_growth_rates, envir = .GlobalEnv)
    
    return(result)
  }, error = function(e) {
    # Restore original functions on error
    assign("get_test_costs", original_get_test_costs, envir = .GlobalEnv)
    assign("get_capacity_growth_rates", original_get_capacity_growth_rates, envir = .GlobalEnv)
    stop(e)
  })
}

# Run OWSA analysis
run_owsa <- function(parameters_to_vary = NULL, 
                     variation_percentage = 20,
                     years = 0:50,
                     verbose = FALSE,
                     save_results = TRUE,
                     output_dir = "results",
                     base_scenario = "AI_BrainMorphometry_All_Areas") {
  
  # Validate parameters
  valid_params <- get_valid_owsa_parameters()
  if (is.null(parameters_to_vary)) {
    parameters_to_vary <- valid_params
  }
  
  invalid_params <- setdiff(parameters_to_vary, valid_params)
  if (length(invalid_params) > 0) {
    stop(paste("Invalid parameters:", paste(invalid_params, collapse = ", ")))
  }
  
  # Create output directory
  if (save_results) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    owsa_dir <- file.path(output_dir, paste0("owsa_", timestamp))
    if (!dir.exists(owsa_dir)) {
      dir.create(owsa_dir, recursive = TRUE)
    }
  }
  
  cat("==============================================\n")
  cat("One-Way Sensitivity Analysis (OWSA)\n")
  cat("==============================================\n\n")
  
  # Get base case results
  cat("Running base case...\n")
  base_results <- run_dementia_model(
    years = years,
    verbose = verbose,
    save_results = FALSE
  )
  
  # Extract base ICER using the same method as main simulation
  if (!is.null(base_results$comparison)) {
    # Use the same calculation method as main simulation
    # Extract from comparison table like main simulation does
    comparison_table <- base_results$comparison$comparison_table
    
    # Find the incremental cost and QALYs from comparison table
    incremental_cost_row <- comparison_table[comparison_table$Metric == "Total Cost (Lifetime)", ]
    incremental_qalys_row <- comparison_table[comparison_table$Metric == "Total QALYs (Lifetime)", ]
    
    if (nrow(incremental_cost_row) > 0 && nrow(incremental_qalys_row) > 0) {
      incremental_cost <- incremental_cost_row$Difference
      incremental_qalys <- incremental_qalys_row$Difference
      
      # Calculate ICER exactly like main simulation
      if (abs(incremental_qalys) < 1e-10) {
        base_icer <- Inf
      } else {
        base_icer <- incremental_cost / incremental_qalys
      }
    } else {
      # Fallback to icer_results if comparison table method fails
      base_icer <- base_results$comparison$icer_results$icer
    }
    
    # For original population, use the same method
    if (!is.null(base_results$original_comparison)) {
      original_comparison_table <- base_results$original_comparison$comparison_table
      
      # Try to find original population specific metrics
      original_cost_row <- original_comparison_table[original_comparison_table$Metric == "Original Subgroup Discounted Cost", ]
      original_qalys_row <- original_comparison_table[original_comparison_table$Metric == "Original Subgroup Discounted QALYs", ]
      
      if (nrow(original_cost_row) > 0 && nrow(original_qalys_row) > 0) {
        original_incremental_cost <- original_cost_row$Difference
        original_incremental_qalys <- original_qalys_row$Difference
        
        if (abs(original_incremental_qalys) < 1e-10) {
          base_original_icer <- Inf
        } else {
          base_original_icer <- original_incremental_cost / original_incremental_qalys
        }
      } else {
        # Fallback to icer_results
        base_original_icer <- base_results$original_comparison$icer_results$icer
      }
    } else {
      base_original_icer <- base_icer
    }
  } else {
    stop("Base case failed to produce ICER")
  }
  
  # Ensure we have single numeric values
  if (length(base_icer) > 1) {
    base_icer <- as.numeric(base_icer[1])  # Take first value if multiple
  } else {
    base_icer <- as.numeric(base_icer)
  }
  if (length(base_original_icer) > 1) {
    base_original_icer <- as.numeric(base_original_icer[1])  # Take first value if multiple
  } else {
    base_original_icer <- as.numeric(base_original_icer)
  }
  
  # If base_original_icer is empty, use base_icer as fallback
  if (length(base_original_icer) == 0 || is.na(base_original_icer)) {
    base_original_icer <- base_icer
  }
  
  cat(sprintf("Base case ICER: £%.2f per QALY\n", base_icer))
  cat(sprintf("Base case Original Population ICER: £%.2f per QALY\n\n", base_original_icer))
  
  # Initialize results storage
  owsa_results <- list()
  detailed_results <- list()
  
  # Run sensitivity analysis for each parameter
  for (param in parameters_to_vary) {
    cat(sprintf("Analyzing parameter: %s\n", param))
    
    # Get original parameter value
    original_value <- NULL
    if (param == "ai_morphometry_per_scan") {
      original_value <- get_test_costs()$ai_morphometry_per_scan
    } else if (param == "ai_mci_sens") {
      original_value <- 0.7714  # From parameters.R
    } else if (param == "ai_mci_spec") {
      original_value <- 0.8449  # From parameters.R
    } else if (param == "ai_dementia_sens") {
      original_value <- 0.925   # From parameters.R
    } else if (param == "ai_dementia_spec") {
      original_value <- 0.863   # From parameters.R
    } else if (param == "radiologist_cost_non_ai") {
      original_value <- get_test_costs()$radiologist_cost_non_ai
    } else if (param == "ai_neuropsych") {
      original_value <- 0.11    # From documentation
    } else if (param == "ai_neuroimaging") {
      original_value <- 0.95    # From documentation
    } else if (param == "ai_biomarker") {
      original_value <- 0.02    # From documentation
    } else if (param == "ai_max_adoption") {
      original_value <- get_capacity_growth_rates()$ai_max_adoption
    } else if (param == "ai_adoption_rate") {
      original_value <- get_capacity_growth_rates()$ai_adoption_rate
    } else if (param == "ai_initial_adoption") {
      original_value <- get_capacity_growth_rates()$ai_initial_adoption
    }
    
    if (is.null(original_value)) {
      warning(sprintf("Could not find original value for parameter: %s", param))
      next
    }
    
    # Calculate variation bounds
    variation_factor <- variation_percentage / 100
    lower_bound <- original_value * (1 - variation_factor)
    upper_bound <- original_value * (1 + variation_factor)
    
    # Test lower bound
    cat(sprintf("  Testing lower bound: %.4f\n", lower_bound))
    lower_results <- run_simulation_with_modified_params(param, lower_bound, years, verbose)
    
    # Test upper bound
    cat(sprintf("  Testing upper bound: %.4f\n", upper_bound))
    upper_results <- run_simulation_with_modified_params(param, upper_bound, years, verbose)
    
    # Extract ICERs using the same method as main simulation
    lower_icer <- if (!is.null(lower_results$comparison)) {
      comparison_table <- lower_results$comparison$comparison_table
      incremental_cost_row <- comparison_table[comparison_table$Metric == "Total Cost (Lifetime)", ]
      incremental_qalys_row <- comparison_table[comparison_table$Metric == "Total QALYs (Lifetime)", ]
      
      if (nrow(incremental_cost_row) > 0 && nrow(incremental_qalys_row) > 0) {
        incremental_cost <- incremental_cost_row$Difference
        incremental_qalys <- incremental_qalys_row$Difference
        
        if (abs(incremental_qalys) < 1e-10) {
          Inf
        } else {
          incremental_cost / incremental_qalys
        }
      } else {
        # Fallback
        icer <- lower_results$comparison$icer_results$icer
        if (length(icer) > 1) as.numeric(icer[1]) else as.numeric(icer)
      }
    } else NA
    
    upper_icer <- if (!is.null(upper_results$comparison)) {
      comparison_table <- upper_results$comparison$comparison_table
      incremental_cost_row <- comparison_table[comparison_table$Metric == "Total Cost (Lifetime)", ]
      incremental_qalys_row <- comparison_table[comparison_table$Metric == "Total QALYs (Lifetime)", ]
      
      if (nrow(incremental_cost_row) > 0 && nrow(incremental_qalys_row) > 0) {
        incremental_cost <- incremental_cost_row$Difference
        incremental_qalys <- incremental_qalys_row$Difference
        
        if (abs(incremental_qalys) < 1e-10) {
          Inf
        } else {
          incremental_cost / incremental_qalys
        }
      } else {
        # Fallback
        icer <- upper_results$comparison$icer_results$icer
        if (length(icer) > 1) as.numeric(icer[1]) else as.numeric(icer)
      }
    } else NA
    
    lower_original_icer <- if (!is.null(lower_results$original_comparison)) {
      comparison_table <- lower_results$original_comparison$comparison_table
      original_cost_row <- comparison_table[comparison_table$Metric == "Original Subgroup Discounted Cost", ]
      original_qalys_row <- comparison_table[comparison_table$Metric == "Original Subgroup Discounted QALYs", ]
      
      if (nrow(original_cost_row) > 0 && nrow(original_qalys_row) > 0) {
        original_incremental_cost <- original_cost_row$Difference
        original_incremental_qalys <- original_qalys_row$Difference
        
        if (abs(original_incremental_qalys) < 1e-10) {
          Inf
        } else {
          original_incremental_cost / original_incremental_qalys
        }
      } else {
        # Fallback
        icer <- lower_results$original_comparison$icer_results$icer
        if (length(icer) > 1) as.numeric(icer[1]) else as.numeric(icer)
      }
    } else NA
    
    upper_original_icer <- if (!is.null(upper_results$original_comparison)) {
      comparison_table <- upper_results$original_comparison$comparison_table
      original_cost_row <- comparison_table[comparison_table$Metric == "Original Subgroup Discounted Cost", ]
      original_qalys_row <- comparison_table[comparison_table$Metric == "Original Subgroup Discounted QALYs", ]
      
      if (nrow(original_cost_row) > 0 && nrow(original_qalys_row) > 0) {
        original_incremental_cost <- original_cost_row$Difference
        original_incremental_qalys <- original_qalys_row$Difference
        
        if (abs(original_incremental_qalys) < 1e-10) {
          Inf
        } else {
          original_incremental_cost / original_incremental_qalys
        }
      } else {
        # Fallback
        icer <- upper_results$original_comparison$icer_results$icer
        if (length(icer) > 1) as.numeric(icer[1]) else as.numeric(icer)
      }
    } else NA
    
    # Calculate percentage changes
    icer_change_lower <- if (!is.na(lower_icer)) ((lower_icer - base_icer) / base_icer) * 100 else NA
    icer_change_upper <- if (!is.na(upper_icer)) ((upper_icer - base_icer) / base_icer) * 100 else NA
    original_icer_change_lower <- if (!is.na(lower_original_icer)) ((lower_original_icer - base_original_icer) / base_original_icer) * 100 else NA
    original_icer_change_upper <- if (!is.na(upper_original_icer)) ((upper_original_icer - base_original_icer) / base_original_icer) * 100 else NA
    
    # Store results
    owsa_results[[param]] <- list(
      Parameter = param,
      Original_Value = original_value,
      Lower_Bound = lower_bound,
      Upper_Bound = upper_bound,
      Lower_ICER = lower_icer,
      Upper_ICER = upper_icer,
      Lower_Original_ICER = lower_original_icer,
      Upper_Original_ICER = upper_original_icer,
      ICER_Change_Lower = icer_change_lower,
      ICER_Change_Upper = icer_change_upper,
      Original_ICER_Change_Lower = original_icer_change_lower,
      Original_ICER_Change_Upper = original_icer_change_upper
    )
    
    # Store detailed results
    detailed_results[[paste0(param, "_lower")]] <- lower_results
    detailed_results[[paste0(param, "_upper")]] <- upper_results
    
    cat(sprintf("  Lower ICER: £%.2f (%.2f%% change)\n", lower_icer, icer_change_lower))
    cat(sprintf("  Upper ICER: £%.2f (%.2f%% change)\n\n", upper_icer, icer_change_upper))
  }
  
  # Create summary data frame
  if (length(owsa_results) > 0) {
    # Convert list to data frame row by row
    summary_rows <- list()
    for (param_name in names(owsa_results)) {
      x <- owsa_results[[param_name]]
      
      # Create data frame step by step to identify the problematic field
      df_row <- data.frame(
        Parameter = x$Parameter,
        Original_Value = x$Original_Value,
        Lower_Bound = x$Lower_Bound,
        Upper_Bound = x$Upper_Bound,
        Lower_ICER = x$Lower_ICER,
        Upper_ICER = x$Upper_ICER,
        stringsAsFactors = FALSE
      )
      
      # Add the remaining fields one by one
      df_row$Lower_Original_ICER <- x$Lower_Original_ICER
      df_row$Upper_Original_ICER <- x$Upper_Original_ICER
      df_row$ICER_Change_Lower <- x$ICER_Change_Lower
      df_row$ICER_Change_Upper <- x$ICER_Change_Upper
      df_row$Original_ICER_Change_Lower <- x$Original_ICER_Change_Lower
      df_row$Original_ICER_Change_Upper <- x$Original_ICER_Change_Upper
      df_row$Base_Original_ICER <- base_original_icer
      
      summary_rows[[param_name]] <- df_row
    }
    owsa_summary <- do.call(rbind, summary_rows)
  } else {
    # Create empty data frame with correct structure
    owsa_summary <- data.frame(
      Parameter = character(),
      Original_Value = numeric(),
      Lower_Bound = numeric(),
      Upper_Bound = numeric(),
      Lower_ICER = numeric(),
      Upper_ICER = numeric(),
      Lower_Original_ICER = numeric(),
      Upper_Original_ICER = numeric(),
      ICER_Change_Lower = numeric(),
      ICER_Change_Upper = numeric(),
      Original_ICER_Change_Lower = numeric(),
      Original_ICER_Change_Upper = numeric(),
      Base_Original_ICER = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # Save results if requested
  if (save_results) {
    # Save summary CSV
    write.csv(owsa_summary, file.path(owsa_dir, "owsa_summary.csv"), row.names = FALSE)
    
    # Save detailed results
    saveRDS(detailed_results, file.path(owsa_dir, "owsa_detailed_results.rds"))
    
    # Generate report
    report_file <- file.path(owsa_dir, "owsa_report.txt")
    sink(report_file)
    cat("ONE-WAY SENSITIVITY ANALYSIS (OWSA) REPORT\n")
    cat("==========================================\n\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    cat("BASE CASE RESULTS\n")
    cat("-----------------\n")
    cat(sprintf("Base case ICER: £%.2f per QALY\n", base_icer))
    cat(sprintf("Base case Original Population ICER: £%.2f per QALY\n\n", base_original_icer))
    
    cat("METHODOLOGY\n")
    cat("-----------\n")
    cat("This OWSA was conducted deterministically to evaluate the relative importance\n")
    cat("of several key parameters. Each parameter was modified by ±", variation_percentage, "% whilst keeping\n")
    cat("all other parameters constant.\n\n")
    
    cat("PARAMETERS ANALYZED\n")
    cat("-------------------\n")
    for (param in parameters_to_vary) {
      result <- owsa_results[[param]]
      cat(sprintf("%s:\n", param))
      cat(sprintf("  Original value: %.4f\n", result$Original_Value))
      cat(sprintf("  Lower bound (-%d%%): %.4f\n", variation_percentage, result$Lower_Bound))
      cat(sprintf("  Upper bound (+%d%%): %.4f\n", variation_percentage, result$Upper_Bound))
      cat(sprintf("  Lower bound ICER: £%.2f per QALY\n", result$Lower_ICER))
      cat(sprintf("  Upper bound ICER: £%.2f per QALY\n", result$Upper_ICER))
      cat(sprintf("  Lower bound Original Population ICER: £%.2f per QALY\n", result$Lower_Original_ICER))
      cat(sprintf("  Upper bound Original Population ICER: £%.2f per QALY\n\n", result$Upper_Original_ICER))
    }
    
    cat("SUMMARY\n")
    cat("-------\n")
    cat("The OWSA results show the sensitivity of the ICER to variations in key parameters.\n")
    cat("Parameters with larger percentage changes in ICER have greater impact on the model results.\n")
    sink()
    
    cat(sprintf("OWSA results saved to: %s\n", owsa_dir))
  }
  
  return(list(
    summary = owsa_summary,
    detailed_results = detailed_results,
    base_icer = base_icer,
    base_original_icer = base_original_icer,
    output_dir = if (save_results) owsa_dir else NULL
  ))
}