results_dir <- file.path("../results")

ai_file <- file.path(results_dir, "detailed_results_AI_BrainMorphometry_All_Areas.csv")
noai_file <- file.path(results_dir, "detailed_results_Standard_Care_All_Areas.csv")

stopifnot(file.exists(ai_file), file.exists(noai_file))

read_all <- function(path) {
  df <- tryCatch(read.csv(path, check.names = TRUE), error = function(e) {
    stop(sprintf("Failed to read %s: %s", path, e$message))
  })
  if (!("Year" %in% names(df))) stop("Expected column 'Year' not found")
  df
}

summarise_by_area <- function(df) {
  required <- c("area", "Subgroup_Discounted_Cost", "Subgroup_Discounted_QALYs",
                "Dx_Mild", "Dx_Moderate", "Dx_Severe",
                "Original_Subgroup_Discounted_QALYs", "Original_Total_Population", "Year")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  df$Diagnosed_Dementia <- df$Dx_Mild + df$Dx_Moderate + df$Dx_Severe

  # Aggregate across ALL years by area (lifetime totals)
  agg <- aggregate(
    cbind(Subgroup_Discounted_Cost, Subgroup_Discounted_QALYs, Diagnosed_Dementia,
          Original_Subgroup_Discounted_QALYs) ~ area,
    data = df,
    sum
  )

  # Rename for clarity
  names(agg) <- c(
    "Area",
    "Discounted_Cost",
    "Discounted_QALYs",
    "Diagnosed_Dementia",
    "Original_Discounted_QALYs"
  )

  # Baseline original population by area from Year 0
  base <- aggregate(Original_Total_Population ~ area,
                    data = df[df$Year == 0, , drop = FALSE],
                    FUN = function(x) as.numeric(max(x, na.rm = TRUE)))
  names(base) <- c("Area", "Original_Baseline_Pop")
  agg <- merge(agg, base, by = "Area", all.x = TRUE)
  # Scenario-specific QALYs per original person by area (based on original discounted QALYs)
  agg$QALYs_per_original_person <- with(agg, ifelse(Original_Baseline_Pop > 0,
                                                    Original_Discounted_QALYs / Original_Baseline_Pop,
                                                    NA_real_))
  agg
}

ai_all <- read_all(ai_file)
noai_all <- read_all(noai_file)

ai_area <- summarise_by_area(ai_all)
noai_area <- summarise_by_area(noai_all)

# Join AI and No-AI by Area
merged <- merge(ai_area, noai_area, by = "Area", suffixes = c("_AI", "_NoAI"), all = TRUE)

# Compute metrics
merged$Delta_Cost <- merged$Discounted_Cost_AI - merged$Discounted_Cost_NoAI
merged$Delta_QALY <- merged$Discounted_QALYs_AI - merged$Discounted_QALYs_NoAI

# ICER per area (incremental cost-effectiveness ratio). Guard against divide-by-zero.
merged$ICER <- with(merged, ifelse(abs(Delta_QALY) < .Machine$double.eps, NA_real_, Delta_Cost / Delta_QALY))

# Cost per dementia diagnosis for each scenario per area
merged$Cost_per_Dx_AI   <- with(merged, ifelse(Diagnosed_Dementia_AI   > 0, Discounted_Cost_AI   / Diagnosed_Dementia_AI, NA_real_))
merged$Cost_per_Dx_NoAI <- with(merged, ifelse(Diagnosed_Dementia_NoAI > 0, Discounted_Cost_NoAI / Diagnosed_Dementia_NoAI, NA_real_))

# Build output in the requested layout: rows for AI Urban/Rural (with QALY gains vs No AI)
# and No AI Urban/Rural (QALY gains set to 0)
make_rows <- function(area_row) {
  rbind(
    data.frame(
      Scenario = "AI",
      Area = area_row$Area,
      ICER = area_row$ICER,
      Costs_per_Dementia_Diagnosis = area_row$Cost_per_Dx_AI,
      QALY_gains = area_row$Original_Discounted_QALYs_AI - area_row$Original_Discounted_QALYs_NoAI,
      QALYs_per_original_person = area_row$QALYs_per_original_person_AI,
      stringsAsFactors = FALSE
    ),
    data.frame(
      Scenario = "No AI",
      Area = area_row$Area,
      ICER = NA_real_,
      Costs_per_Dementia_Diagnosis = area_row$Cost_per_Dx_NoAI,
      QALY_gains = 0,
      QALYs_per_original_person = area_row$QALYs_per_original_person_NoAI,
      stringsAsFactors = FALSE
    )
  )
}

rows <- do.call(rbind, lapply(split(merged, merged$Area), make_rows))

# Add QALY_gains to QALYs_per_original_person for AI rows to show total QALYs
rows$QALYs_per_original_person[rows$Scenario == "AI"] <- 
  rows$QALYs_per_original_person[rows$Scenario == "AI"] + rows$QALY_gains[rows$Scenario == "AI"]

# --- Add Combined (not area specific) row ---
# This is a "total" row, not a per-area row. It is always added.
Tot_Delta_Cost <- sum(merged$Discounted_Cost_AI, na.rm = TRUE) - sum(merged$Discounted_Cost_NoAI, na.rm = TRUE)
Tot_Delta_QALY <- sum(merged$Discounted_QALYs_AI, na.rm = TRUE) - sum(merged$Discounted_QALYs_NoAI, na.rm = TRUE)
Tot_ICER <- ifelse(abs(Tot_Delta_QALY) < .Machine$double.eps, NA_real_, Tot_Delta_Cost / Tot_Delta_QALY)

Tot_AI_Cost <- sum(merged$Discounted_Cost_AI, na.rm = TRUE)
Tot_NoAI_Cost <- sum(merged$Discounted_Cost_NoAI, na.rm = TRUE)
Tot_AI_Dx <- sum(merged$Diagnosed_Dementia_AI, na.rm = TRUE)
Tot_NoAI_Dx <- sum(merged$Diagnosed_Dementia_NoAI, na.rm = TRUE)
Tot_AI_Cost_per_Dx <- ifelse(Tot_AI_Dx > 0, Tot_AI_Cost / Tot_AI_Dx, NA_real_)
Tot_NoAI_Cost_per_Dx <- ifelse(Tot_NoAI_Dx > 0, Tot_NoAI_Cost / Tot_NoAI_Dx, NA_real_)

Tot_Orig_Q_NoAI <- mean(merged$Original_Discounted_QALYs_NoAI, na.rm = TRUE)
Tot_Orig_Q_AI <- mean(merged$Original_Discounted_QALYs_AI, na.rm = TRUE)
Tot_BasePop <- mean(merged$Original_Baseline_Pop_NoAI, na.rm = TRUE)
Tot_Qpp_NoAI <- ifelse(Tot_BasePop > 0, Tot_Orig_Q_NoAI / Tot_BasePop, NA_real_)
Tot_QALY_gain_per_person <- ifelse(Tot_BasePop > 0, (Tot_Orig_Q_AI - Tot_Orig_Q_NoAI), NA_real_)

# Fix: QALY_gains for combined should be total QALY gain (not per person)
Tot_QALY_gain_total <- Tot_Orig_Q_AI - Tot_Orig_Q_NoAI

# Add the combined row (not area specific)
rows <- rbind(
  rows,
  data.frame(Scenario = "AI", Area = "Combined", ICER = Tot_ICER,
             Costs_per_Dementia_Diagnosis = Tot_AI_Cost_per_Dx,
             QALY_gains = Tot_QALY_gain_total,  # <-- total QALY gain, not per person
             QALYs_per_original_person = Tot_Qpp_NoAI + Tot_QALY_gain_per_person, # show total QALYs for AI
             stringsAsFactors = FALSE),
  data.frame(Scenario = "No AI", Area = "Combined", ICER = NA_real_,
             Costs_per_Dementia_Diagnosis = Tot_NoAI_Cost_per_Dx,
             QALY_gains = 0,
             QALYs_per_original_person = Tot_Qpp_NoAI,
             stringsAsFactors = FALSE)
)

# Sort rows by Scenario then Area as Urban, Rural, Combined
rows$Area <- factor(rows$Area, levels = c("Urban", "Rural", "Combined"))
rows$Scenario <- factor(rows$Scenario, levels = c("AI", "No AI"))
rows <- rows[order(rows$Scenario, rows$Area), ]
row.names(rows) <- NULL

# Write raw output
raw_out <- file.path(results_dir, "urban_rural_table_raw.csv")
write.csv(rows, raw_out, row.names = FALSE)

# Pretty formatting helpers
fmt_currency <- function(x) {
  ifelse(is.na(x), "-", paste0("£", formatC(x, format = "f", digits = 2, big.mark = ",")))
}

fmt_icer <- function(x) {
  ifelse(is.na(x), "-", paste0("£", formatC(x, format = "f", digits = 2, big.mark = ",")))
}

fmt_qaly <- function(x) {
  formatC(x, format = "f", digits = 1, big.mark = ",")
}

formatted <- rows
formatted$ICER <- fmt_icer(formatted$ICER)
formatted$Costs_per_Dementia_Diagnosis <- fmt_currency(formatted$Costs_per_Dementia_Diagnosis)
formatted$QALY_gains <- fmt_qaly(formatted$QALY_gains)
formatted$QALYs_per_original_person <- fmt_qaly(formatted$QALYs_per_original_person)

formatted_out <- file.path(results_dir, "urban_rural_table_formatted.csv")
write.csv(formatted, formatted_out, row.names = FALSE)
cat(sprintf("Wrote: %s\n", raw_out))
cat(sprintf("Wrote: %s\n", formatted_out))
