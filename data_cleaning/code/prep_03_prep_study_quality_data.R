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
  rename(
    qa_randomised_q1 = question_1,
    qa_randomised_q2 = question_2,
    qa_randomised_q3 = question_3,
    qa_randomised_q4 = question_4,
    qa_randomised_q5 = question_5,
    qa_randomised_q6 = question_6,
    qa_randomised_q7 = question_7,
    qa_randomised_q8 = question_8,
    qa_randomised_q9 = question_9,
    qa_randomised_q10 = question_10,
    qa_randomised_q11 = question_11,
    qa_randomised_q12 = question_12,
    qa_randomised_q13 = question_13
  )

qa_randomised_summary <- qa_randomised_raw |>
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
  rename(
    qa_non_randomised_q1 = question_1,
    qa_non_randomised_q2 = question_2,
    qa_non_randomised_q3 = question_3,
    qa_non_randomised_q4 = question_4,
    qa_non_randomised_q5 = question_5,
    qa_non_randomised_q6 = question_6,
    qa_non_randomised_q7 = question_7,
    qa_non_randomised_q8 = question_8,
    qa_non_randomised_q9 = question_9
  )

qa_non_randomised_summary <- qa_non_randomised_raw |>
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
  qa_randomised_summary,
  qa_non_randomised_summary
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
  ) |>
  # add detailed randomised quality assessments
  left_join(
    qa_randomised_clean,
    by = "study_id"
  ) |>
  # add detailed non-randomised quality assessments
  left_join(
    qa_non_randomised_clean,
    by = "study_id"
  ) |>
  # convert any "Unsure" to "Unclear" across all columns
  mutate(across(
    starts_with("qa_non_randomised"),
    ~ str_replace_all(.x, "Unsure", "Unclear")
  ))

# export
saveRDS(
  combined_study_quality_assessments,
  "./data_cleaning/outputs/almp_nma_study_quality_assessments.RDS"
)
