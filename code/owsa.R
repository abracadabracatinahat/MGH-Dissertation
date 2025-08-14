# Run OWSA with AI adoption parameters
source("main.R")
cat("\n=== Example 1: OWSA with AI Adoption Parameters ===\n")
results1 <- run_owsa(
  parameters_to_vary = c("ai_morphometry_per_scan", "radiologist_cost_non_ai", "ai_max_adoption", "ai_adoption_rate", "ai_initial_adoption"),  # Test AI adoption parameters
  variation_percentage = 10,
  years = 0:50,  # Full time horizon for AI adoption analysis
  verbose = FALSE,
  save_results = TRUE
)

# Print results
cat("\nOWSA Results Summary:\n")
print(results1$summary)

# Show the most impactful parameter
cat("\n=== Parameter Impact Analysis ===\n")
if (nrow(results1$summary) > 0) {
  # Calculate maximum absolute change for each parameter
  results1$summary$Max_Abs_Change <- pmax(abs(results1$summary$ICER_Change_Lower), 
                                         abs(results1$summary$ICER_Change_Upper))
  
  # Sort by impact
  sorted_results <- results1$summary[order(-results1$summary$Max_Abs_Change), ]
  
  cat("Parameters ranked by impact on ICER:\n")
  for (i in 1:nrow(sorted_results)) {
    param <- sorted_results$Parameter[i]
    max_change <- sorted_results$Max_Abs_Change[i]
    cat(sprintf("%d. %s: %.2f%% maximum change\n", i, param, max_change))
  }
}