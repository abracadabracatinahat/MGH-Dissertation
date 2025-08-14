# parameters.R - All parameter definitions and configuration
# Using Birmingham as proxy for Urban and Cornwall as proxy for Rural
# Note: ANY TIME FROM PAPER IS MENTIONED, THIS IS REFERRING TO THE SHORE ET AL. PAPER

library(dplyr)
library(tidyr)

# Get symptom presentation rates by age group and health state
# Returns: data frame with rates for each combination
get_symptom_presentation_rates <- function() {
  # Base rates by age group (clinically informed)
  # - Younger adults less likely to present with cognitive concerns
  # - Older adults more likely to notice and report symptoms
  # - Age-related health-seeking behavior and symptom awareness
  #SHOULD BE ABLE TO HOPEFULLY USE THIS SOURCE: https://pmc.ncbi.nlm.nih.gov/articles/PMC8961006/pdf/clep-14-395.pdf
  #Issue is there are different age bands. 
  #Also we may (as a result) have to set the base rates based off of ages 
  #and then apply a multiplier based off of health state instead, aka swap
  #for health state multipliers, source: https://digital.nhs.uk/data-and-information/publications/statistical/recorded-dementia-diagnoses
  # 53% of mild cases have a recorded diagnosis, 73 % of moderate cases have a recorded diagnosis, 86 % of severe cases have a recorded diagnosis
  # interpret "recorded diagnosis" here as the end result of the presentation → referral → assessment funnel
  # normalize this by setting healthy as 1 and then applying the rates (we don't have any for MCI so make assumption)
  # REFER TO S9. Age Base Rates 
  base_rates <- list(
    "65 to 69" = 0.00444447,     # 0.4% - younger, less likely to attribute to dementia, number calculate based on 67 years old point 
    "70-74" = 0.00701761,        # 0.7% - starting to be more aware, number calculate based on 72 years old point
    "75-79" = 0.01108046,        # 1.1% - baseline group, number calculate based on 77 years old point
    "80-84" = 0.01749552,        # 1.7% - higher awareness and family concern, number calculate based on 82 years old point
    "85+" = 0.02762458           # 2.8% - highest concern and family involvement, number calculate based on 87 years old point
  )  
  # Health state multipliers (severity affects presentation likelihood)
  # Based on health-seeking behavior and symptom severity
  health_state_multipliers <- list(
    "Other_Healthy" = 0.01,     # 1% of base rate - occasional worried well
    "MCI" = 0.22,               # 22% of base rate - mild cognitive concerns: https://www.cuimc.columbia.edu/news/one-10-older-americans-has-dementia
    "Undx_Mild" = 0.53,         # 53% of base rate - noticeable symptoms but undiagnosed
    "Undx_Moderate" = 0.73,     # 73% of base rate - clear symptoms, higher presentation
    "Undx_Severe" = 0.86,       # 86% of base rate - severe symptoms hard to ignore,
    "Dx_Mild" = 0.0,           # 0% - already diagnosed
    "Dx_Moderate" = 0.0,       # 0% - already diagnosed  
    "Dx_Severe" = 0.0,         # 0% - already diagnosed
    "Death" = 0.0              # 0% - not applicable
  )
  
  # Create data frame with all combinations
  expand.grid(
    age_group = names(base_rates),
    state = names(health_state_multipliers),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      base_rate = unlist(base_rates[age_group]),
      health_state_multiplier = unlist(health_state_multipliers[state]),
      symptom_presentation_rate = base_rate * health_state_multiplier
    ) %>%
    select(age_group, state, symptom_presentation_rate)
}

# Get severity-specific referral probabilities for positive test results
# Returns: list with referral probabilities by dementia severity
get_severity_specific_referral_probs <- function() {
  # Clinical rationale for varying referral rates by severity:
  # - Mild dementia: Higher referral rate due to diagnostic uncertainty and need for specialist assessment
  # - Moderate dementia: Moderate referral rate as symptoms are more apparent but still benefit from specialist care
  # - Severe dementia: Lower referral rate as symptoms are obvious and may prioritize immediate care over specialist assessment
  
  list(
    Mild = 0.98,      # 98% of positive tests for mild dementia get referred
    Moderate = 0.90,  # 90% of positive tests for moderate dementia get referred  
    Severe = 0.80     # 80% of positive tests for severe dementia get referred
    #assumptions hinged on 94% of all people with a GP assessment suggesting dementia were referred to memory clinics 
    #source: https://www.nice.org.uk/about/what-we-do/into-practice/measuring-the-use-of-nice-guidance/impact-of-our-guidance/niceimpact-dementia/ch2-referral-diagnosis-and-care-planning
  )
}

# Get area-specific test refusal rates
# Returns: list with test refusal rates by area type
get_area_specific_test_refusal_rates <- function() {
  # Clinical rationale for varying test refusal by area:
  # - Rural areas: Higher refusal rates due to travel barriers, limited healthcare access, and cultural factors
  # - Urban areas: Lower refusal rates due to better access, more healthcare awareness, and convenience
  # Based on literature showing rural-urban disparities in healthcare utilization
 
  #MIGHT HAVE TO STICK WITH THE LEGACY NUMBER HERE
  #can use OG number of 5.28%
  #source: https://www.england.nhs.uk/east-of-england/2023/12/08/quarter-of-a-million-more-seen-by-gps-in-the-east-of-england-during-october-as-costs-of-no-shows-revealed/
  #then scale this by 1.22 for rural, using rural waits >= 2 weeks as a proxy (20.6%/16.9%)
  #source: https://www.libdems.org.uk/news/article/revealed-long-gp-waits-in-rural-areas-three-times-worse-than-urban-ones-1
  list(
    Urban = 0.0528,  # 5.28% refuse testing in urban areas (lower than baseline)
    Rural = 0.0828   # 8.28% refuse testing in rural areas (higher than baseline)
  )
}

# Main function to get complete parameter set for a scenario
# area_type is used for certain lookups but model handles both areas
# Now supports year-based AI adoption rates instead of binary ai_enabled
get_parameters <- function(ai_enabled = TRUE, area_type = "Urban", year = 0,
                         ai_mci_sens_override = NULL,
                         ai_mci_spec_override = NULL,
                         ai_dementia_sens_override = NULL,
                         ai_dementia_spec_override = NULL) {
  # Calculate current AI adoption rate based on year
  # If ai_enabled is FALSE, force adoption to 0 (allows disabling AI completely)
  ai_adoption_rate <- if(ai_enabled) get_ai_adoption_rate(year) else 0.0
  
  # Get test performance and validate structure (use adoption rate for gradual enhancement)
  test_perf <- get_test_performance_all(ai_adoption_rate, 
                                       ai_mci_sens_override,
                                       ai_mci_spec_override,
                                       ai_dementia_sens_override,
                                       ai_dementia_spec_override)
  validate_test_performance(test_perf)
  
  list(
    years = 0:50, # Model settings
    states = c("Other_Healthy", "MCI", 
               "Undx_Mild", "Undx_Moderate", "Undx_Severe", 
               "Dx_Mild", "Dx_Moderate", "Dx_Severe", "Death"),
    age_groups = c("65 to 69", "70-74", "75-79", "80-84", "85+"),
    area_types = c("Urban", "Rural"),
    gender_groups = c("Male", "Female"),
    
    # Economic parameters
    discount_rate = 0.035, 
    #source: https://www.nice.org.uk/process/pmg36/chapter/economic-evaluation-2
    
    # Clinical parameters
    area_specific_test_refusal_rates = get_area_specific_test_refusal_rates(),  # Varies by urban/rural
    mc_referral_refusal_rate = 0.067,     # 6.7% refuse MC referral (same ASSUMPTION as paper - "proportion of patients declining referral to memory clinic")
    symptom_presentation_rates = get_symptom_presentation_rates(),  # Rates by health state and age group 
    
    
    # Test performance parameters
    test_performance = test_perf,
    severity_specific_referral_probs = get_severity_specific_referral_probs(),  # Varies by dementia severity
    # Clinical rationale: Mild cases need more specialist assessment, severe cases may prioritize immediate care
    
    # Process time parameters
    process_time_non_ai_hours = get_test_costs()$process_time_non_ai_hours,  # 10 hours for non-AI processing
    process_time_ai_hours = get_test_costs()$process_time_ai_hours,          # 20 minutes for AI processing
    
    # Capacity parameters
    gp_capacity = get_gp_capacity(area_type),
    mc_capacity = get_mc_capacity(area_type),
    mri_machines = get_mri_capacity(area_type),
    capacity_growth_rates = get_capacity_growth_rates(),
    
    # Cost parameters
    health_state_costs = get_health_state_costs(),
    test_costs = get_test_costs(),
    ai_costs = get_ai_costs(area_type),
    wait_costs = get_wait_costs(area_type),  # Detailed wait cost breakdown
    gp_time_cost = get_gp_time_cost(),  # GP time cost per minute
    
    # Utility parameters
    utility_values = get_utility_values(),
    wait_disutility_per_year = get_wait_disutility_by_process_time(ai_adoption_rate),  # QALY loss from uncertainty + process time
    # Enhanced to account for process time differences (10 hours non-AI vs 20 mins AI)
    # Base rate: 0.05, enhanced by process time impact
    
    # Mortality parameters
    mortality_rates = get_mortality_rates(),
    mortality_multipliers = get_mortality_multipliers(),
    
    # Severity distributions
    severity_distributions = get_severity_distributions(),
    undx_severity_distributions = get_undx_severity_distributions(),
    
    # Population parameters
    starting_populations = get_starting_populations(),
    age_distributions = get_age_distributions(),
    gender_distributions = get_gender_distributions(),
    aging_rates = get_aging_rates(),
    new_inflow_rate = 0.01,  # 1% of population per year (used only if dynamic_inflow_for_net_growth = FALSE)
    #source: (https://www.macrotrends.net/global-metrics/countries/gbr/united-kingdom/population-growth-rate)
    dynamic_inflow_for_net_growth = TRUE,  # If TRUE, calculates inflow = death_rate + 1% for net 1% growth
    inflow_distribution = get_inflow_distribution(),
    
    # Initial condition parameters
    # from paper 
    initial_severity_distribution = list(
      Mild = 0.78,      # 78% of initial dementia is mild
      Moderate = 0.16,  # 16% moderate
      Severe = 0.06     # 6% severe
    ), 
    
    # Queue parameters - this is optional/reflect natural disease progress for people in the queue/waiting
    # Applying the same percentages as the paper 
    queue_progression_rates = list(
      Undx_Mild_to_Undx_Moderate = 0.259,    # 25.9% worsen per year in queue (from paper)
      Undx_Moderate_to_Undx_Severe = 0.187   # 18.7% worsen per year in queue (from paper)
    ),
    mri_queue_attrition_rate = 0.02,         #source: https://pubmed.ncbi.nlm.nih.gov/38705764/
    
    # Pathway probabilities
    gp_confidence_probs = get_gp_confidence_probs(area_type),
    mc_pathway_probs = get_mc_pathway_probs(ai_adoption_rate, area_type),
    
    # Transition probabilities
    dementia_prevalence = get_dementia_prevalence_by_age_by_gender(),
    mci_prevalence = get_mci_prevalence_by_age(),  # separating MCI prevalence
    transition_probs = get_transition_probabilities(),
    
    # Flags
    ai_enabled = ai_enabled,
    ai_adoption_rate = ai_adoption_rate,  # Store current adoption rate
    area_type = area_type,
    
    # Validation checker for large cost decreases 
    validation_tolerances = list(
      cost_decrease = 100  # Allow small cost decreases due to rounding
    )
  )
}

# Health state costs (annual) - based on Shore et al. 2023 estimates
get_health_state_costs <- function() {
  c(
    Other_Healthy = 0,      # No dementia-specific costs
    MCI = 0,              # paper disregards them 
    Undx_Mild = 8973,       # From paper - includes informal care
    Undx_Moderate = 31692,  # From paper
    Undx_Severe = 34055,    # From paper
    Dx_Mild = 9699,         # From paper - includes treatment
    Dx_Moderate = 32418,    # From paper
    Dx_Severe = 34301,      # From paper
    Death = 0
  )
}

# Test costs - includes GP assessment and neuroimaging pathway costs
get_test_costs <- function() {
  list(
    # GP cognitive assessment cost (calculated dynamically from gp_time_cost)
    # gp_test_cost removed - now calculated dynamically using gp_time_cost
    
    # Radiologist costs for neuroimaging analysis
    radiologist_cost_non_ai = 31.00,         # £1.71 per non-AI neuroimaging (18 min × £1.71/min)
    radiologist_cost_ai = 17.1,                 # (10 min x £1.71/min) per AI neuroimaging 
    # source for time: https://kar.kent.ac.uk/109563/1/The%20unit%20costs%20of%20health%20and%20social%20care%202024%20%28for%20publication%29_Final.pdf
    # source for radiologist salary/: https://pdf.sciencedirectassets.com/786945/1-s2.0-S2274580724X70016/1-s2.0-S227458072400027X/main.pdf?
    
    # AI neuroimaging costs (setup handled in get_ai_costs CAPEX)
    ai_morphometry_per_scan = 0.26,     # AI analysis cost per scan (just the cost of GPU depreciation/scan)
    # 526 pounds for GeForce RTX 2080 - the direct successor to the GTX 1080 (used for ScanOMetrics), source: https://www.tomsguide.com/us/nvidia-rtx-2080-release-date-price%2Cnews-27805.html
    # GPU lifespan conservative estimate (5 years - moderate to high use), source: https://unicornplatform.com/blog/how-long-should-a-gpu-actually-last-expect-3-5-years/
    # assumed about 400 scans per year (based on the models previous runs)
    
    # Process time per scan (in hours) - affects wait disutility
    process_time_non_ai_hours = 9 + 20/60,   # 9 hours and 20 minutes for non-AI scan processing. source (Christian's paper): https://pubmed.ncbi.nlm.nih.gov/38823248/
    process_time_ai_hours = 24/60,   #source (Christian's paper): https://pubmed.ncbi.nlm.nih.gov/38823248/
    
    # Standard costs
    standard_mri_cost = 23.9,          # Base MRI scan cost (electricity, consumables, etc.)  
    #split it up into:
    #electricity cost: 19.9kWh * 0.211 pounds/kWh = 4.20 pounds 
    #source for mean MRI energy/exam: https://pubs.rsna.org/doi/abs/10.1148/radiol.2020192084
    #source for cost: https://www.businessenergydeals.co.uk/business-electricity-prices/
    #general consumables: 22.95 euros/test -> (1 euro = 0.86 pounds) -> 19.70 pounds/test
    # Includes coil-covers, gloves, syringes, plus small amounts for disinfectants, paper goods, etc.
    #source: https://pmc.ncbi.nlm.nih.gov/articles/PMC10379578/
    #Total cost: 4.2 + 19.70 = 23.9 pounds
    mri_maintenance_depreciation = 34.29,  # MRI maintenance and depreciation per scan - 20.57 before
    # Depreciation: £100,000/year ÷ 5,248 scans/year = £19.05 per scan
    #NEW Maintenance: 8% of £100,000/year = £80,000/year ÷ 5,248 scans/year = £15.24 per scan
    # Total: £19.05 + £15.24 = £34.29 per scan
    # Sources: MRI cost (£1M, 10-year life), 5,248 scans/year, 8% maintenance rate
    
    
    # Other pathway costs
    neuropsych_cost = 18.31,            # Neuropsychological testing (from paper) 
    biomarker_cost = 480              # Biomarker testing: source https://pmc.ncbi.nlm.nih.gov/articles/PMC6709060
  )
}

# Calculate wait disutility based on process time per scan
# Returns: wait disutility per year that accounts for processing time differences
get_wait_disutility_by_process_time <- function(ai_adoption_rate = 0.0) {
  # Base wait disutility (assuming mild anxiety brought on by uncertainty)
  base_wait_disutility_per_year <- 0.05
  # source: https://link.springer.com/article/10.1007/s10198-024-01692-0
  
  # Get test costs to access process times
  test_costs <- get_test_costs()
  
  # Process time impact on wait disutility
  # Longer processing times increase wait disutility due to extended uncertainty
  process_time_multiplier_non_ai <- test_costs$process_time_non_ai_hours / 24  # Normalize to daily impact
  process_time_multiplier_ai <- test_costs$process_time_ai_hours / 24
  
  # Weighted average based on AI adoption rate
  weighted_process_time_multiplier <- (1 - ai_adoption_rate) * process_time_multiplier_non_ai + 
                                    ai_adoption_rate * process_time_multiplier_ai
  
  # Calculate enhanced wait disutility
  enhanced_wait_disutility <- base_wait_disutility_per_year * (1 + weighted_process_time_multiplier)
  
  return(enhanced_wait_disutility)
}

# AI implementation costs by area - includes all AI-related infrastructure
get_ai_costs <- function(area_type) {
  # individual OPEX costs
  #NEW - Reduced costs for cost-effectiveness
  programmers <- list(
    Urban = 80000*0.05,  # 0.5 programmer (shared resource)
    Rural = 66000*0.05   # 0.5 programmer (shared resource)
  ) #salary source: https://www.itjobswatch.co.uk/jobs/uk/artificial%20intelligence%20developer.do
  # assuming the system gets updated once every 20 days (less frequent updates) 
  
  # Return only OPEX costs (CAPEX removed)
  list(OPEX = programmers[[area_type]])
}

# GP time cost per minute (same for both AI and no AI scenarios)
get_gp_time_cost <- function() {
  list(
    cost_per_minute = 3.60,  # £3.60 per minute, from paper
    time_per_consultation = 8  # 8 minutes per consultation
  )
}

# Detailed wait costs broken down by component with area-specific variations
get_wait_costs <- function(area_type) {
  # Base costs per year
  gp_telephone_cost_per_call <- 20      # £20 per 5 min call
  #source for costs: https://www.kingsfund.org.uk/insight-and-analysis/data-and-charts/key-facts-figures-nhs
  gp_face_to_face_cost_per_visit <- 37  # £37 per 10 min appointment
  #source for costs: https://www.kingsfund.org.uk/insight-and-analysis/data-and-charts/key-facts-figures-nhs
  patient_transport_base_cost <- 45.58  # £45.58 per NHS-PTS round trip
  #source for costs: https://www.pssru.ac.uk/pub/uc/uc2018/services.pdf 
  
  caregiver_cost_per_hour <- 24         # £24 per hour
  #source for costs: https://www.pssru.ac.uk/pub/uc/uc2018/services.pdf
  
  # Frequency per year
  gp_telephone_calls_per_year <- 1      # 1 call per year
  #source for number of appointments: https://www.dementiauk.org/wp-content/uploads/dementia-uk-getting-a-diagnosis-leaflet.pdf 
  gp_face_to_face_visits_per_year <- 2  # 2 visits per year
  #source for number of appointments: https://www.dementiauk.org/wp-content/uploads/dementia-uk-getting-a-diagnosis-leaflet.pdf
  transport_trips_per_year <- 1         # 1 trip per referral year
  #source for time: https://www.rcpsych.ac.uk/docs/default-source/improving-care/nccmh/dementia/nccmh-dementia-care-pathway-full-implementation-guidance.pdf
  supplemental_caregiver_hours_per_year <- 260        # 5 hours per week (52 weeks)
  
  # Area-specific multipliers
  transport_multipliers <- list(
    Urban = 1.0,                        # Base cost for urban
    Rural = 57/33                       # Rural transport cost multiplier (57min/33min)
    #For variation: https://www.ageuk.org.uk/siteassets/documents/reports-and-publications/reports-and-briefings/active-communities/rb_june15_the_future_of_transport_in_an_ageing_society.pdf
  )
  
  caregiver_multipliers <- list(
    Urban = 1.0,                        # Base cost for urban
    Rural = 21.4/19                     # Rural caregiver cost multiplier (21.4%/19%)
    #source for variation: https://www.sciencedirect.com/science/article/pii/S1041610225002893
  )
  
  # Calculate area-specific costs
  transport_cost_per_year <- patient_transport_base_cost * transport_trips_per_year * 
                            transport_multipliers[[area_type]]
  
    caregiver_cost_per_year <- caregiver_cost_per_hour * supplemental_caregiver_hours_per_year *
                            caregiver_multipliers[[area_type]]
  
  # Total costs per year
  gp_telephone_cost_per_year <- gp_telephone_cost_per_call * gp_telephone_calls_per_year
  gp_face_to_face_cost_per_year <- gp_face_to_face_cost_per_visit * gp_face_to_face_visits_per_year
  
  total_cost_per_year <- gp_telephone_cost_per_year + gp_face_to_face_cost_per_year + 
                        transport_cost_per_year + caregiver_cost_per_year
  
  return(list(
    # Individual components
    gp_telephone_cost_per_call = gp_telephone_cost_per_call,
    gp_face_to_face_cost_per_visit = gp_face_to_face_cost_per_visit,
    transport_cost_per_trip = patient_transport_base_cost * transport_multipliers[[area_type]],
    caregiver_cost_per_hour = caregiver_cost_per_hour * caregiver_multipliers[[area_type]],
    
    # Annual totals
    gp_telephone_cost_per_year = gp_telephone_cost_per_year,
    gp_face_to_face_cost_per_year = gp_face_to_face_cost_per_year,
    transport_cost_per_year = transport_cost_per_year,
    caregiver_cost_per_year = caregiver_cost_per_year,
    
    # Total annual cost (for backward compatibility)
    total_cost_per_year = total_cost_per_year,
    
    # Frequency information
    gp_telephone_calls_per_year = gp_telephone_calls_per_year,
    gp_face_to_face_visits_per_year = gp_face_to_face_visits_per_year,
    transport_trips_per_year = transport_trips_per_year,
    caregiver_hours_per_year = supplemental_caregiver_hours_per_year,
    
    # Area-specific multipliers
    transport_multiplier = transport_multipliers[[area_type]],
    caregiver_multiplier = caregiver_multipliers[[area_type]]
  ))
}

# Utility values and decrements
get_utility_values <- function() {
  list(
    # Base utilities by age (from UK population norms) 0.749 to 0.645 
    # distributing based on the provided range from the paper
    base_utility = c(
      "65 to 69" = 0.749,
      "70-74" = 0.700,
      "75-79" = 0.687,
      "80-84" = 0.665,
      "85+" = 0.645 
    ), 
    # Utility decrements by state (as named vector for consistent indexing)
    # all from the paper
    decrements = c(
      Other_Healthy = 0,
      MCI = 0.06,
      Undx_Mild = 0.125,
      Undx_Moderate = 0.235,
      Undx_Severe = 0.305,
      Dx_Mild = 0.129,
      Dx_Moderate = 0.242,
      Dx_Severe = 0.314,
      Death = 1  # Complete loss of utility 
    )
  )
}

# GP capacity by area
get_gp_capacity <- function(area_type) {
  list(
    Urban = 7802,   # ~184 practices × 42.4 dementia assessments/GP/year = 7801.6 -> 7802 (previously 9200 using 50 dementia assessments/year)
    #source for number of practices: https://www.birminghamsolihull.icb.nhs.uk/health-information/gp-practices
    Rural = 2332    # ~55 practices × 42.4 dementia assessments/GP/year = 2332 (previously 2750 using 50 dementia assessments/year)
    #source for number of practices: https://cios.icb.nhs.uk/health/primary-care/
    #calculation for dementia assessments/year: 1197408/28271 = 42.4 dementia assessments/GP/year
      #1,197,408 dementia assessments/year, source: https://www.england.nhs.uk/wp-content/uploads/2013/03/ess-dementia.pdf
      #28,271 full-time equivalent GPs, source: https://www.bma.org.uk/advice-and-support/nhs-delivery-and-workforce/pressures/pressures-in-general-practice-data-analysis
  )
}

# Memory clinic capacity by area
# Potential approach 
  # 3 MAS clinics Birmingham, source: https://www.bsmhft.nhs.uk/wp-content/uploads/2023/08/FOI-0216_2021-Final-Response.pdf
  # 1 MAS clinic Cornwall, source: https://www.cornwallft.nhs.uk/download.cfm?doc=docm93jijm4n2577.pdf&ver=61704
  # 36.7 average throughput/MAS per month, source: https://www.hqip.org.uk/wp-content/uploads/2022/08/Ref-317-NAD-Memory-Assessment-Services-Spotlight-Audit-2021_FINAL.pdf
# MC capacity Urban: 3*36.7*12 = 1320 assessments/year
# MC capacity Rural: 1*36.7*12 = 440 assessments/year 
get_mc_capacity <- function(area_type) {
  list(
    Urban = 1320,   # constrained but serving large population - formerly 1800
    Rural = 440     # more constrained due to fewer services - formerly 900
  )
}

# MRI capacity by area with radiologist shortage constraint
get_mri_capacity <- function(area_type) {
  # Radiologist density per 100,000 population (area-specific)
  radiologists_per_100k <- list(
    Urban = 8.6, # https://www.rcr.ac.uk/media/4imb5jge/_rcr-2024-clinical-radiology-workforce-census-report.pdf
    Rural = 6.4 #https://www.rcr.ac.uk/media/qs0jnfmv/rcr-census_clinical-radiology-workforce-census_2022.pdf
  )[[area_type]]
  scans_per_radiologist <- 27  #Dementia-relevant MRIs per year per radiologist 
  scans_per_machine <- 216     #Dementia-relevant MRIs per year per machine 
  # https://www.england.nhs.uk/london/wp-content/uploads/sites/8/2019/11/FINAL-London-memory-service-audit-2019.pdf

  # Population by area
  population <- get_starting_populations()[[area_type]]

  # Radiologist headcount
  radiologist_count <- (radiologists_per_100k / 100000) * population

  # Scanner count
  machine_count <- list(
    Urban = 6, # 6 (Queen Elizabeth Hospital Birmingham) National Health Society (2024) 
    Rural = 4  # 4 Royal Cornwall Hospitals NHS Trust (Truro) National Health Society (2024) \cite{nhs2024} 
  )[[area_type]]

  # Capacity calculations
  machine_capacity <- machine_count * scans_per_machine
  radiologist_capacity <- radiologist_count * scans_per_radiologist
  effective_capacity <- min(machine_capacity, radiologist_capacity)

  list(
    Count = machine_count,
    Patients_per_machine_per_year = effective_capacity / machine_count,
    radiologist_count = radiologist_count,
    scans_per_machine = scans_per_machine,
    scans_per_radiologist = scans_per_radiologist,
    machine_capacity = machine_capacity,
    radiologist_capacity = radiologist_capacity,
    effective_capacity = effective_capacity
  )
}

# Mortality rates by age group
get_mortality_rates <- function() {
  c(
    "65 to 69" = 0.0125,
    "70-74" = 0.0131,
    "75-79" = 0.0181,
    "80-84" = 0.0270,
    "85+" = 0.059 #source: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxbysingleyearofage
  )
}

# Mortality multipliers by health state
get_mortality_multipliers <- function() {
  c(
    Other_Healthy = 1.0,
    MCI = 1.0, # source paper
    Undx_Mild = 2.5, #source: https://pmc.ncbi.nlm.nih.gov/articles/PMC10041427/
    Undx_Moderate = 3.75, #same as above 
    Undx_Severe = 5.25, #same as above 
    Dx_Mild = 3.0, #source: https://pmc.ncbi.nlm.nih.gov/articles/PMC10424331/
    Dx_Moderate = 5.0, #same as above
    Dx_Severe = 7.0, #same as above 
    Death = 0  
  )
}
# Starting populations by area
# Use Birmingham as proxy for Urban and Cornwall as proxy for Rural 
# Refer to S10
get_starting_populations <- function() {
  list(
    Urban = 151720, # (Total population 1,166,049 people: < 65 (excluded): 1,014,329, 65+: 151,720)
    #source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationestimatesforenglandandwales/mid2023
    Rural = 147207 # (Total population 568,210 people: < 65 (excluded): 421,003, 65+: 147,207)
    #source: https://populationdata.org.uk/cornwall-population/
  )
}

# Age distributions by area
# Refer to S10
get_age_distributions <- function() { # https://digital.nhs.uk/data-and-information/publications/statistical/recorded-dementia-diagnoses/september-2022
  data.frame(
    age_group = c("65 to 69", "70-74", "75-79", "80-84", "85+"),
    Urban = c(0.2840, 0.2407, 0.1963, 0.1351, 0.1440),
    Rural = c(0.2584, 0.2569, 0.2276, 0.1302, 0.1269),
    stringsAsFactors = FALSE
  )
}

# Gender distributions by area
get_gender_distributions <- function() {
  data.frame(
    gender = c("Male", "Female"),
    Urban = c(0.4540, 0.5460),  
    #source: https://www.rcgp.org.uk/representing-you/key-statistics-insights
    Rural = c(0.4625, 0.5375),  
    #source: https://www.nomisweb.co.uk/datasets/pestsyoala
    stringsAsFactors = FALSE
  )
}

# Aging rates (proportion moving to next age group per year)
get_aging_rates <- function() {
  c(
    "65 to 69" = 1/5,   # 5 years in this group
    "70-74" = 1/5,      # 5 years in group
    "75-79" = 1/5,      # 5 years in group
    "80-84" = 1/5,      # 5 years in group
    "85+" = 0           # No aging out of 85+
  )
}

# Inflow distribution for new entrants
# This function only add inflow to age group "65 to 69"
# https://www.sciencedirect.com/science/article/pii/S1041610224044181
get_inflow_distribution <- function() {
  c(
    Other_Healthy = 0.91,
    MCI = 0.07, 
    Undx_Mild = 0.008, 
    Undx_Moderate = 0.004, 
    Undx_Severe = 0.001,
    Dx_Mild = 0.0, 
    Dx_Moderate = 0.0, 
    Dx_Severe = 0.0, 
    Death = 0.0 
  )
}

# GP confidence probabilities
# This was an initial element of the model that was eventually disregarded. 
# To avoid rewriting and introducing potential bugs, it was set to 1 
# such that it does nothing in the code
get_gp_confidence_probs <- function(area_type) {
  # GP confidence varies by location only (urban vs rural resources/training)
  p_gp_confident <- case_when(
    area_type == "Urban" ~ 1.0,  # Better resources/training in urban areas
    area_type == "Rural" ~ 1.0,  # Limited resources in rural areas
    TRUE ~ 1.0  # Default if area_type not specified
  )
  
  # GP Not Confident → 100% referral to memory clinic (per pathway diagram)
  # same... disregard. 
  p_gp_not_confident_referral <- 1.0  # Always refer when not confident
  
  list(
    confident = p_gp_confident,
    not_confident_referral = p_gp_not_confident_referral
  )
}

# Memory clinic pathway probabilities
#https://www.sciencedirect.com/science/article/pii/S1041610224044181
get_mc_pathway_probs <- function(ai_adoption_rate, area_type) {
  # Base probabilities without AI, potential source: https://www.england.nhs.uk/london/wp-content/uploads/sites/8/2020/04/The-2019-national-memory-service-audit.pdf
  base_neuropsych <- if(area_type == "Urban") 0.11 else 0.11
  base_neuroimaging <- if(area_type == "Urban") 0.75 else 0.75  
  base_biomarker <- if(area_type == "Urban") 0.02 else 0.02
  
  # AI-enhanced probabilities (when fully adopted)
  ai_neuropsych <- if(area_type == "Urban") 0.11 else 0.11 
  ai_neuroimaging <- if(area_type == "Urban") 0.85 else 0.85 # assumption more inflow due to increas ein efficacy 
  ai_biomarker <- if(area_type == "Urban") 0.02 else 0.02 
  
  # Linear interpolation based on adoption rate
  p_neuropsych <- base_neuropsych + ai_adoption_rate * (ai_neuropsych - base_neuropsych)
  p_neuroimaging <- base_neuroimaging + ai_adoption_rate * (ai_neuroimaging - base_neuroimaging)
  p_biomarker <- base_biomarker + ai_adoption_rate * (ai_biomarker - base_biomarker)
  
  # Setting probability of not going further 
  p_no_further <- 0.15
  #source: https://pmc.ncbi.nlm.nih.gov/articles/PMC10483037/
  
  list(
    neuropsych = p_neuropsych,
    neuroimaging = p_neuroimaging,
    biomarker = p_biomarker,
    no_further = p_no_further
  )
}

# Severity distributions by gender and age
get_severity_distributions <- function() {
  list(
    Male = list(
      "65 to 69" = c(Mild = 0.725, Moderate = 0.20, Severe = 0.075),
      "70-74" = c(Mild = 0.675, Moderate = 0.23, Severe = 0.095),
      "75-79" = c(Mild = 0.65, Moderate = 0.25, Severe = 0.10),
      "80-84" = c(Mild = 0.625, Moderate = 0.27, Severe = 0.105),
      "85+" = c(Mild = 0.575, Moderate = 0.30, Severe = 0.125)
    ),
    Female = list(
      "65 to 69" = c(Mild = 0.705, Moderate = 0.22, Severe = 0.075),
      "70-74" = c(Mild = 0.655, Moderate = 0.245, Severe = 0.10),
      "75-79" = c(Mild = 0.625, Moderate = 0.27, Severe = 0.105),
      "80-84" = c(Mild = 0.60, Moderate = 0.29, Severe = 0.11),
      "85+" = c(Mild = 0.55, Moderate = 0.32, Severe = 0.13)
    )
  )
}
# assumptions based on https://pubmed.ncbi.nlm.nih.gov/33361590/ and https://journals.sagepub.com/doi/abs/10.3233/JAD-200786

# Undiagnosed severity distributions by gender and age
get_undx_severity_distributions <- function() {
  list(
    Male = list(
      "65 to 69" = c(Mild = 0.60, Moderate = 0.30, Severe = 0.10),
      "70-74" = c(Mild = 0.55, Moderate = 0.32, Severe = 0.13),
      "75-79" = c(Mild = 0.52, Moderate = 0.33, Severe = 0.15),
      "80-84" = c(Mild = 0.48, Moderate = 0.34, Severe = 0.18),
      "85+" = c(Mild = 0.44, Moderate = 0.34, Severe = 0.22)
    ),
    Female = list(
      "65 to 69" = c(Mild = 0.58, Moderate = 0.31, Severe = 0.11),
      "70-74" = c(Mild = 0.53, Moderate = 0.34, Severe = 0.13),
      "75-79" = c(Mild = 0.48, Moderate = 0.35, Severe = 0.17),
      "80-84" = c(Mild = 0.43, Moderate = 0.36, Severe = 0.21),
      "85+" = c(Mild = 0.40, Moderate = 0.35, Severe = 0.25)
    )
  )
} # assumptions based on https://pubmed.ncbi.nlm.nih.gov/33361590/ and https://journals.sagepub.com/doi/abs/10.3233/JAD-200786

# Transition probabilities (annual) - shared parameters for both base and AI scenarios
get_transition_probabilities <- function() {
  list(
    # MCI transitions (unchanged by AI)
    # from paper
    mci_to_undx_mild = 0.049,        # 4.9% progress to dementia
    mci_to_healthy = 0.16,            # 16% revert to healthy
    
    # Transitions from undiagnosed to diagnosed
    undx_mild_to_dx_mild = 0.09,
    undx_moderate_to_dx_moderate = 0.13,
    undx_severe_to_dx_severe = 0.86,
    
    # Progression rates undiagnosed
    undx_mild_to_moderate = 0.259,
    undx_moderate_to_severe = 0.187,
    
    # Progression rates diagnosed
    dx_mild_to_moderate = 0.16,
    dx_moderate_to_severe = 0.116,
    
    # Age-specific transitions from healthy to undiagnosed mild dementia
    # from paper
    healthy_to_undx_mild_by_age = c(
      "65 to 69" = 0.007,
      "70-74" = 0.011,
      "75-79" = 0.014,
      "80-84" = 0.022,
      "85+" = 0.063
    ),
    
    # Age-specific transitions from healthy to MCI
    # from paper
    healthy_to_mci_by_age = c(
      "65 to 69" = 0.012,
      "70-74" = 0.018,
      "75-79" = 0.033,
      "80-84" = 0.032,
      "85+" = 0.023
    ),
    
    # Gender adjustment multipliers (applied to female transitions)
    # Based on literature suggesting women may have slower progression in some stages
    gender_adjustments = list(
      Female = list(
        mci_to_undx_mild_multiplier = 1.15,       
        undx_mild_to_moderate_multiplier = 1.10,  
        dx_mild_to_moderate_multiplier = 1.08     
        # source: https://pmc.ncbi.nlm.nih.gov/articles/PMC6226313/pdf/nihms-988622.pdf
        # 
      )
    )
  )
}

# Get capacity growth rates for different service types
# Returns: list with annual growth rates for each service
get_capacity_growth_rates <- function() {
  list(
    gp_growth = 0.0336,      # 3.36% annual growth. https://www.grandviewresearch.com/industry-analysis/us-primary-care-physicians-market
    mc_growth = 0.051,      # 5% annual growth (policy priority). https://www.grandviewresearch.com/industry-analysis/us-memory-care-market-report
    mri_growth = 0.064,     # 6% annual growth. https://www.bccresearch.com/pressroom/hlc/mri-technologies-soar-with-a-64-cagr-2023-2028
    ai_adoption_rate = 0.90,  # 90% annual growth in AI adoption (faster healthcare adoption)
    ai_max_adoption = 0.90,   # Maximum 90% adoption rate (near-universal)
    ai_initial_adoption = 0.05 # Starting adoption rate (5% early adopters)
  )
}

# Calculate AI adoption rate for a given year using logistic growth model
# This models realistic technology adoption in healthcare with:
# - Slow initial adoption by early adopters
# - Accelerating adoption as evidence builds
# - Plateauing as market saturates
# Returns: AI adoption rate (0 to 1) for the given year
get_ai_adoption_rate <- function(year, growth_params = get_capacity_growth_rates()) {
  initial_rate <- growth_params$ai_initial_adoption
  max_rate <- growth_params$ai_max_adoption
  growth_rate <- growth_params$ai_adoption_rate
  
  # Logistic growth model: A(t) = K / (1 + ((K - A0) / A0) * exp(-r * t))
  # Where K = max adoption, A0 = initial adoption, r = growth rate, t = time
  
  if (year <= 0) {
    return(initial_rate)
  }
  
  # Calculate logistic growth
  k <- max_rate
  a0 <- initial_rate
  r <- growth_rate
  t <- year
  
  adoption_rate <- k / (1 + ((k - a0) / a0) * exp(-r * t))
  
  # Ensure we don't exceed bounds
  return(min(max(adoption_rate, 0), 1))
}

# Dementia prevalence by age group and gender
# Based on incidence rates per 1000 person-years converted to prevalence
get_dementia_prevalence_by_age_by_gender <- function() {
  # Convert incidence rates to prevalence (assuming 5-year survival)
  # Prevalence ≈ Incidence × Duration (using 5 years as typical survival)
  # Converting from per 1000 to decimal: divide by 1000, multiply by 5
  list(
    "65 to 69" = list(
      "Female" = 6.3 * 5 / 1000,
      "Male" = 6.9 * 5 / 1000
    ),
    "70-74" = list(
      "Female" = 6.1 * 5 / 1000,
      "Male" = 14.5 * 5 / 1000
    ),
    "75-79" = list(
      "Female" = 14.8 * 5 / 1000,
      "Male" = 14.2 * 5 / 1000
    ),
    "80-84" = list(
      "Female" = 31.2 * 5 / 1000,
      "Male" = 17.0 * 5 / 1000
    ),
    "85+" = list(
      "Female" = 71.7 * 5 / 1000,
      "Male" = 58.4 * 5 / 1000
    )
  )
}

# Legacy function for backward compatibility
get_dementia_prevalence_by_age <- function() {
  # Return average prevalence by age group (for backward compatibility)
  gender_specific <- get_dementia_prevalence_by_age_by_gender()
  c(
    "65 to 69" = mean(c(gender_specific[["65 to 69"]]$Female, gender_specific[["65 to 69"]]$Male)),
    "70-74" = mean(c(gender_specific[["70-74"]]$Female, gender_specific[["70-74"]]$Male)),
    "75-79" = mean(c(gender_specific[["75-79"]]$Female, gender_specific[["75-79"]]$Male)),
    "80-84" = mean(c(gender_specific[["80-84"]]$Female, gender_specific[["80-84"]]$Male)),
    "85+" = mean(c(gender_specific[["85+"]]$Female, gender_specific[["85+"]]$Male))
  )
}

# MCI prevalence by age group
# Based on Shore et al and typical MCI:dementia ratios
get_mci_prevalence_by_age <- function() {
  # MCI typically 1.5-2x dementia prevalence in younger ages, converging in older ages
  c(
    "65 to 69" = 0.0123,  
    "70-74" = 0.01775,      
    "75-79" = 0.03273,      
    "80-84" = 0.03167,      
    "85+" = 0.02333        
  )#source: https://pure.manchester.ac.uk/ws/portalfiles/portal/46842374/McMillan_IJGP.pdf
}

# Validate test performance structure
validate_test_performance <- function(test_perf) {
  required_tests <- c("gp", "neuropsych", "neuroimaging", "biomarker")
  required_conditions <- c("mci", "dementia")
  required_metrics <- c("sensitivity", "specificity")
  
  for (test in required_tests) {
    if (!test %in% names(test_perf)) {
      stop(paste("Missing test:", test))
    }
    for (condition in required_conditions) {
      if (!condition %in% names(test_perf[[test]])) {
        stop(paste("Missing condition", condition, "for test", test))
      }
      for (metric in required_metrics) {
        if (!metric %in% names(test_perf[[test]][[condition]])) {
          stop(paste("Missing", metric, "for", test, condition))
        }
      }
    }
  }
  return(TRUE)
}

# Get all test performance parameters - AI enhances neuroimaging
# Now includes separate MCI and dementia sensitivity/specificity
get_test_performance_all <- function(ai_adoption_rate = 0.0, 
                                   ai_mci_sens_override = NULL,
                                   ai_mci_spec_override = NULL,
                                   ai_dementia_sens_override = NULL,
                                   ai_dementia_spec_override = NULL) {
  # Base test performance values from Shore et al. 2023 Table 1 (aka from paper)
  list(
    gp = list(
      # Weighted average of multiple GP cognitive tests
      # MMSE (26%), GPCOG (21%), 6CIT (29%), AMTS (7%), MoCA (6%)
      mci = list(sensitivity = 0.5967, specificity = 0.7514), # from paper
      dementia = list(sensitivity = 0.7328, specificity = 0.8434) # from paper
    ),
    neuropsych = list(
      # Using weighted average of standard care tests used in model values from Table 6
      mci = list(sensitivity = 0.78, specificity = 0.85), # from paper
      dementia = list(sensitivity = 0.93, specificity = 0.82) # from paper

    ),
    # AI enhancement only affects neuroimaging - gradual adoption based on rate
    neuroimaging = {
      # Base performance (standard neuroimaging analysis)
      base_mci_sens <- 0.63 
      base_mci_spec <- 0.69 
      # using the radiologist sensitivity and specificity from visually rated medial-temporal atrophy (MTA) as proxy 
      # source: https://www.nature.com/articles/s41380-023-02215-8
      base_dementia_sens <- 0.91
      base_dementia_spec <- 0.81
      #source: https://www.aging-us.com/article/203082/text
      
      # AI-enhanced performance (ScanOMetrics DL + DiReCT from ADePT study)
      # base it on source 40 from Christian's paper: https://pubmed.ncbi.nlm.nih.gov/32749588/
      ai_mci_sens <- if (!is.null(ai_mci_sens_override)) ai_mci_sens_override else 0.7714     # 0.63*1.2245 = 0.7714
      ai_mci_spec <- if (!is.null(ai_mci_spec_override)) ai_mci_spec_override else 0.8449     # 0.69*1.2245 = 0.8449
      # using bilateral HS assessement accuracy improvement: 91.1%/74.4% = 1.2245
      # bigger improvements for the cases that are harder to detect 
      
      ai_dementia_sens <- if (!is.null(ai_dementia_sens_override)) ai_dementia_sens_override else 0.925       # applying that multiple to the base: 0.91*1.0165 = 0.925
      #assuming similar increases in sensitivity for neuroradiologists w QReport: 98.3%/96.7% = 1.0165 multiple
      ai_dementia_spec <- if (!is.null(ai_dementia_spec_override)) ai_dementia_spec_override else 0.863       # applying that multiple to the base: 0.81*1.0656 = 0.863
      #assuming similar increases in specificity for neuroradiologists w QReport: 94.2%/88.4% = 1.0656 multiple
      
      # Linear interpolation based on adoption rate
      list(
        mci = list(
          sensitivity = base_mci_sens + ai_adoption_rate * (ai_mci_sens - base_mci_sens),
          specificity = base_mci_spec + ai_adoption_rate * (ai_mci_spec - base_mci_spec)
        ),
        dementia = list(
          sensitivity = base_dementia_sens + ai_adoption_rate * (ai_dementia_sens - base_dementia_sens),
          specificity = base_dementia_spec + ai_adoption_rate * (ai_dementia_spec - base_dementia_spec)
        )
      )
    },
    biomarker = list(
      # Estimated values - typically better for dementia than MCI
      mci = list(sensitivity = 0.85, specificity = 0.91),
      # source: https://pubmed.ncbi.nlm.nih.gov/26401938/
      dementia = list(sensitivity = 0.92, specificity = 0.92)
      # source: https://pubmed.ncbi.nlm.nih.gov/26401938/
    )
  )
}