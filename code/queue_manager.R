# queue_manager.R - Queue capacity management with SHARED area-level capacity

library(dplyr)
library(tidyr)

# Internal: Area-level queue state management
# This is the core fix - capacity is managed at area level, not subgroup level
.create_area_state <- function(areas, queue_type, params) {
  area_state <- list()
  
  for (area in areas) {
    # Set total capacity for the entire area
    if (queue_type == "gp") {
      capacity <- params$gp_capacity[[area]]
    } else if (queue_type == "mc") {
      capacity <- params$mc_capacity[[area]]
    } else if (queue_type == "mri") {
      mri_config <- params$mri_machines[[area]]
      capacity <- mri_config$Count * mri_config$Patients_per_machine_per_year
    }
    
    area_state[[area]] <- list(
      total_capacity = capacity,
      total_queue_size = 0,
      cumulative_wait_time = 0,
      processed_this_year = 0
    )
  }
  
  return(area_state)
}

# Initialize queue map for all subgroups (maintains interface compatibility)
# Returns: data frame with queue information for each subgroup
initialize_queue_map <- function(cohort_df, queue_type = c("gp", "mc", "mri"), params) {
  queue_type <- match.arg(queue_type)
  
  # Get unique subgroups from cohort
  subgroups <- cohort_df %>%
    select(age_group, gender, area) %>%
    distinct()
  
  # Create base queue structure
  queue_df <- subgroups %>%
    mutate(
      queue_type = queue_type,
      queue_size = 0,
      cumulative_wait_time = 0,
      avg_wait_time = 0,
      processed_this_year = 0
    )
  
  # Add capacity information (for display only - actual capacity is managed at area level)
  # This maintains backward compatibility with code that expects a capacity column
  queue_df <- queue_df %>%
    mutate(
      capacity = case_when(
        queue_type == "gp" & area == "Urban" ~ params$gp_capacity[["Urban"]],
        queue_type == "gp" & area == "Rural" ~ params$gp_capacity[["Rural"]],
        queue_type == "mc" & area == "Urban" ~ params$mc_capacity[["Urban"]],
        queue_type == "mc" & area == "Rural" ~ params$mc_capacity[["Rural"]],
        queue_type == "mri" ~ 
          params$mri_machines$Count * params$mri_machines$Patients_per_machine_per_year,
        TRUE ~ 0
      )
    )
  
  # Store area-level state as an attribute (hidden from normal use)
  areas <- unique(queue_df$area)
  attr(queue_df, "area_state") <- .create_area_state(areas, queue_type, params)
  
  return(queue_df)
}

# Add demand to queue
# Returns: updated queue data frame
add_to_queue <- function(queue_df, demand_df, queue_type) {
  # demand_df should have columns: age_group, gender, area, population
  
  # Aggregate total demand by subgroup
  demand_summary <- demand_df %>%
    group_by(age_group, gender, area) %>%
    summarise(
      total_demand = sum(population),
      .groups = "drop"
    )
  
  # Add demand to subgroup queues
  queue_df <- queue_df %>%
    left_join(demand_summary, by = c("age_group", "gender", "area")) %>%
    mutate(
      queue_size = queue_size + ifelse(is.na(total_demand), 0, total_demand),
      total_demand = NULL
    )
  
  # Update area-level state
  area_state <- attr(queue_df, "area_state")
  if (!is.null(area_state)) {
    area_demand <- queue_df %>%
      group_by(area) %>%
      summarise(total = sum(queue_size), .groups = "drop")
    
    for (i in 1:nrow(area_demand)) {
      area <- area_demand$area[i]
      area_state[[area]]$total_queue_size <- area_demand$total[i]
    }
    
    attr(queue_df, "area_state") <- area_state
  }
  
  return(queue_df)
}

# Process all queues with SHARED area-level capacity
# This is the main fix - capacity is allocated proportionally within each area
process_all_queues <- function(queue_df, params) {
  # Get area state
  area_state <- attr(queue_df, "area_state")
  use_shared_capacity <- !is.null(area_state)
  
  if (!use_shared_capacity) {
    # Fallback to old behavior if area_state is missing
    warning("Queue system using legacy independent capacity model")
    return(.process_all_queues_legacy(queue_df, params))
  }
  
  # Get unique areas
  areas <- unique(queue_df$area)
  
  # Initialize results
  total_processed <- 0
  total_wait_time <- 0
  processed_population <- data.frame()
  
  # Process each area separately with shared capacity
  for (area_name in areas) {
    area_info <- area_state[[area_name]]
    
    # Get all subgroups in this area
    area_subgroups <- queue_df %>%
      filter(area == area_name, queue_size > 0)
    
    if (nrow(area_subgroups) == 0 || area_info$total_queue_size == 0) next
    
    # Apply attrition for MRI queues
    queue_type <- unique(queue_df$queue_type)
    if (queue_type == "mri" && params$mri_queue_attrition_rate > 0) {
      attrition_rate <- params$mri_queue_attrition_rate
      
      # Apply attrition to each subgroup proportionally
      for (j in 1:nrow(area_subgroups)) {
        idx <- which(queue_df$age_group == area_subgroups$age_group[j] &
                       queue_df$gender == area_subgroups$gender[j] &
                       queue_df$area == area_name)
        queue_df$queue_size[idx] <- queue_df$queue_size[idx] * (1 - attrition_rate)
      }
      
      # Update area total
      area_info$total_queue_size <- sum(queue_df$queue_size[queue_df$area == area_name])
    }
    
    # Calculate how many can be processed at area level
    capacity <- area_info$total_capacity
    total_demand <- area_info$total_queue_size
    to_process_total <- min(capacity, total_demand)
    
    # Calculate wait time for this area
    if (to_process_total > 0) {
      utilization <- total_demand / capacity
      if (utilization <= 1) {
        wait_time <- total_demand / (2 * capacity)
      } else {
        # Queue builds up - average wait is half the time to clear backlog
        wait_time <- (total_demand - capacity) / capacity
      }
    } else {
      wait_time <- 0
    }
    
    # Update area state
    area_info$processed_this_year <- area_info$processed_this_year + to_process_total
    area_info$cumulative_wait_time <- area_info$cumulative_wait_time + wait_time * to_process_total
    area_info$total_queue_size <- total_demand - to_process_total
    
    # Allocate processing PROPORTIONALLY to subgroups based on their queue sizes
    if (to_process_total > 0) {
      total_subgroup_demand <- sum(area_subgroups$queue_size)
      
      for (j in 1:nrow(area_subgroups)) {
        subgroup <- area_subgroups[j,]
        
        # Calculate this subgroup's share
        proportion <- subgroup$queue_size / total_subgroup_demand
        subgroup_processed <- to_process_total * proportion
        
        # Find the index in main queue_df
        idx <- which(queue_df$age_group == subgroup$age_group &
                       queue_df$gender == subgroup$gender &
                       queue_df$area == area_name)
        
        # Update the subgroup
        queue_df$queue_size[idx] <- queue_df$queue_size[idx] - subgroup_processed
        queue_df$processed_this_year[idx] <- queue_df$processed_this_year[idx] + subgroup_processed
        queue_df$cumulative_wait_time[idx] <- queue_df$cumulative_wait_time[idx] + wait_time * subgroup_processed
        
        if (queue_df$processed_this_year[idx] > 0) {
          queue_df$avg_wait_time[idx] <- queue_df$cumulative_wait_time[idx] / queue_df$processed_this_year[idx]
        }
        
        # Build processed population dataframe
        if (subgroup_processed > 0) {
          processed_pop <- data.frame(
            age_group = subgroup$age_group,
            gender = subgroup$gender,
            area = subgroup$area,
            population = subgroup_processed,
            stringsAsFactors = FALSE
          )
          processed_population <- bind_rows(processed_population, processed_pop)
        }
        
        total_processed <- total_processed + subgroup_processed
      }
    }
    
    total_wait_time <- total_wait_time + wait_time * to_process_total
    
    # Update area state in attribute
    area_state[[area_name]] <- area_info
  }
  
  # Update area state attribute
  attr(queue_df, "area_state") <- area_state
  
  # Clean up tiny floating point errors before returning
  queue_df <- queue_df %>%
    mutate(
      # Clean up queue sizes - set tiny values to exactly 0
      queue_size = if_else(abs(queue_size) < 1e-10, 0, queue_size),
      # Ensure non-negative
      queue_size = pmax(0, queue_size),
      # Clean up wait times
      avg_wait_time = if_else(abs(avg_wait_time) < 1e-10, 0, avg_wait_time),
      cumulative_wait_time = if_else(abs(cumulative_wait_time) < 1e-10, 0, cumulative_wait_time)
    )
  
  return(list(
    updated_queue = queue_df,
    total_processed = total_processed,
    total_wait_time = total_wait_time,
    processed_population = processed_population
  ))
}

# Legacy processing function (for backward compatibility)
.process_all_queues_legacy <- function(queue_df, params) {
  # Get all unique subgroups
  subgroups <- queue_df %>%
    select(age_group, gender, area) %>%
    distinct()
  
  # Initialize results
  total_processed <- 0
  total_wait_time <- 0
  processed_population <- data.frame()
  
  # Process each subgroup independently (old behavior)
  for (i in 1:nrow(subgroups)) {
    subgroup <- subgroups[i, ]
    
    result <- process_queue(queue_df, subgroup, params)
    
    queue_df <- result$updated_queue
    total_processed <- total_processed + result$processed_count
    total_wait_time <- total_wait_time + result$wait_time * result$processed_count
    
    if (result$processed_count > 0) {
      processed_pop <- data.frame(
        age_group = subgroup$age_group,
        gender = subgroup$gender,
        area = subgroup$area,
        population = result$processed_count,
        stringsAsFactors = FALSE
      )
      processed_population <- bind_rows(processed_population, processed_pop)
    }
  }
  
  return(list(
    updated_queue = queue_df,
    total_processed = total_processed,
    total_wait_time = total_wait_time,
    processed_population = processed_population
  ))
}

# Process queue for a specific subgroup (legacy function maintained for compatibility)
process_queue <- function(queue_df, subgroup_keys, params) {
  # Extract subgroup row
  subgroup_queue <- queue_df %>%
    filter(
      age_group == subgroup_keys$age_group,
      gender == subgroup_keys$gender,
      area == subgroup_keys$area
    )
  
  if (nrow(subgroup_queue) == 0 || subgroup_queue$queue_size[1] == 0) {
    return(list(
      updated_queue = queue_df,
      processed_count = 0,
      wait_time = 0
    ))
  }
  
  # This function is now only used in legacy mode
  # In shared capacity mode, processing happens in process_all_queues
  
  # Apply attrition for MRI queue
  if (subgroup_queue$queue_type[1] == "mri" && params$mri_queue_attrition_rate > 0) {
    attrition_rate <- params$mri_queue_attrition_rate
    subgroup_queue <- subgroup_queue %>%
      mutate(
        queue_size = queue_size * (1 - attrition_rate)
      )
  }
  
  # Calculate how many can be processed
  capacity <- subgroup_queue$capacity[1]
  current_queue_size <- subgroup_queue$queue_size[1]
  to_process <- min(capacity, current_queue_size)
  
  # Update queue - remove processed population
  subgroup_queue <- subgroup_queue %>%
    mutate(
      queue_size = queue_size - to_process,
      processed_this_year = processed_this_year + to_process
    )
  
  # Calculate wait time (simplified)
  wait_time <- if (to_process > 0 && current_queue_size > capacity) {
    (current_queue_size - to_process) / (2 * capacity)
  } else {
    0
  }
  
  # Update cumulative wait metrics
  subgroup_queue <- subgroup_queue %>%
    mutate(
      cumulative_wait_time = cumulative_wait_time + wait_time * to_process,
      avg_wait_time = if_else(
        processed_this_year > 0,
        cumulative_wait_time / processed_this_year,
        0
      )
    )
  
  # Update main queue dataframe
  queue_df <- queue_df %>%
    filter(!(age_group == subgroup_keys$age_group & 
               gender == subgroup_keys$gender & 
               area == subgroup_keys$area)) %>%
    bind_rows(subgroup_queue)
  
  return(list(
    updated_queue = queue_df,
    processed_count = to_process,
    wait_time = wait_time
  ))
}

# Calculate queue metrics for reporting
calculate_queue_metrics <- function(queue_df) {
  queue_df %>%
    group_by(queue_type, area) %>%
    summarise(
      total_backlog = sum(queue_size),
      avg_wait_time = weighted.mean(avg_wait_time, processed_this_year, na.rm = TRUE),
      total_processed = sum(processed_this_year),
      utilization_rate = sum(processed_this_year) / first(capacity),
      .groups = "drop"
    ) %>%
    mutate(
      avg_wait_time = if_else(is.nan(avg_wait_time), 0, avg_wait_time)
    )
}

# Calculate wait impacts (updated to use detailed wait costs)
calculate_wait_impacts <- function(queue_list, params) {
  wait_costs <- params$wait_costs
  wait_disutility_per_year <- params$wait_disutility_per_year
  
  # Calculate total population-years of waiting across all queue types
  total_wait_years <- 0
  
  if (is.data.frame(queue_list)) {
    total_wait_years <- sum(queue_list$cumulative_wait_time)
  } else if (is.list(queue_list)) {
    for (queue_name in names(queue_list)) {
      if (is.data.frame(queue_list[[queue_name]])) {
        total_wait_years <- total_wait_years + sum(queue_list[[queue_name]]$cumulative_wait_time)
      }
    }
  }
  
  # Calculate detailed wait costs
  gp_telephone_cost <- total_wait_years * wait_costs$gp_telephone_cost_per_year
  gp_face_to_face_cost <- total_wait_years * wait_costs$gp_face_to_face_cost_per_year
  transport_cost <- total_wait_years * wait_costs$transport_cost_per_year
  caregiver_cost <- total_wait_years * wait_costs$caregiver_cost_per_year
  
  total_wait_cost <- gp_telephone_cost + gp_face_to_face_cost + transport_cost + caregiver_cost
  
  # Calculate impacts
  list(
    total_wait_cost = total_wait_cost,
    total_qaly_loss = total_wait_years * wait_disutility_per_year,
    total_wait_years = total_wait_years,
    # Detailed cost breakdown
    gp_telephone_cost = gp_telephone_cost,
    gp_face_to_face_cost = gp_face_to_face_cost,
    transport_cost = transport_cost,
    caregiver_cost = caregiver_cost,
    avg_wait_by_type = if (is.data.frame(queue_list)) {
      queue_list %>%
        group_by(queue_type) %>%
        summarise(
          avg_wait = weighted.mean(avg_wait_time, processed_this_year, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      NULL
    }
  )
}

# Apply disease progression to people waiting in queues (unchanged)
apply_queue_progression <- function(queue_df, params) {
  # For now, just return the queue unchanged
  # Disease progression in queues could be implemented here if needed
  return(queue_df)
}

# Reset annual counters (unchanged)
reset_annual_counters <- function(queue_df) {
  # Reset subgroup counters
  queue_df <- queue_df %>%
    mutate(
      processed_this_year = 0,
      cumulative_wait_time = 0
    )
  
  # Reset area state if present
  area_state <- attr(queue_df, "area_state")
  if (!is.null(area_state)) {
    for (area in names(area_state)) {
      area_state[[area]]$processed_this_year <- 0
      area_state[[area]]$cumulative_wait_time <- 0
    }
    attr(queue_df, "area_state") <- area_state
  }
  
  return(queue_df)
}