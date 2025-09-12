#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 12/09/2025                                                                           #
# Purpose: process study quality assessment results                                          #
#============================================================================================#

# load required packages
library(tidyverse)

# read study quality data
qa_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_randomised.rds"
qa_randomised_raw <- readRDS(qa_randomised_raw_location)

qa_non_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_non_randomised.rds"
qa_non_randomised_raw <- readRDS(qa_non_randomised_raw_location)

#-------------------------------------------------------------------------------
# 1. Process Randomised Quality Assessment to get summary measure
#-------------------------------------------------------------------------------

qa_randomised_clean <- qa_randomised_raw |>
  # drop constructs that are irrelevant to ALMP RCTs
  select(
    -question_4, # participant blinding
    -question_5, # delivery blinding
    -question_11 # ITT use (we deal with that in other ways)
  ) |>
  # calculate summary measure
  mutate(
    summary_quality_assessment = case_when(
      # any "No" across retained items
      rowSums(across(starts_with("question_"), ~ .x == "No"), na.rm = TRUE) >=
        1 ~
        "Low quality",
      # more than 3 "Unsure" across retained items
      rowSums(
        across(starts_with("question_"), ~ .x == "Unsure"),
        na.rm = TRUE
      ) >
        3 ~
        "Low quality",
      TRUE ~ "Acceptable quality"
    )
  ) |>
  # subset study_id and summary_measure
  select(
    study_id,
    summary_quality_assessment
  )

#-------------------------------------------------------------------------------
# 2. Process Non-randomised Quality Assessment to get summary measure
#-------------------------------------------------------------------------------

qa_non_randomised_clean <- qa_non_randomised_raw |>
  # drop constructs that are irrelevant to ALMP QEDs
  select(
    -question_5, # pre/post testing
  ) |>
  # calculate summary measure
  mutate(
    summary_quality_assessment = case_when(
      # any "No" across retained items
      rowSums(across(starts_with("question_"), ~ .x == "No"), na.rm = TRUE) >=
        1 ~
        "Low quality",
      # more than 3 "Unsure" across retained items
      rowSums(
        across(starts_with("question_"), ~ .x == "Unsure"),
        na.rm = TRUE
      ) >
        3 ~
        "Low quality",
      TRUE ~ "Acceptable quality"
    )
  ) |>
  # subset study_id and summary_measure
  select(
    study_id,
    summary_quality_assessment
  )

#-------------------------------------------------------------------------------
# 3. Merge, process and export data
#-------------------------------------------------------------------------------

# combine data
combined_study_quality_assessments <- bind_rows(
  qa_randomised_clean,
  qa_non_randomised_clean
) |>
  # convert to binary
  mutate(
    low_study_quality = case_when(
      summary_quality_assessment == "Low quality" ~ 1,
      TRUE ~ 0
    ),
    low_study_quality = as.numeric(low_study_quality)
  ) |>
  # subset data
  select(
    study_id,
    low_study_quality
  )

# export
saveRDS(
  combined_study_quality_assessments,
  "./data_cleaning/outputs/almp_nma_study_quality_assessments.RDS"
)
