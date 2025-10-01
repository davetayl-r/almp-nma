#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 1/10/2025                                                                            #
# Purpose: Evidence profile for each component                                               #
#============================================================================================#

# load model data
almp_nma_additive_model_data_location <- "./analysis/output/almp_nma_additive_model_data.RDS"
almp_nma_additive_model_data <- readRDS(almp_nma_additive_model_data_location)

# load component data
almp_nma_combined_data_clean_location <- "./data_cleaning/outputs/almp_nma_pooled_analysis_data.rds"
almp_nma_combined_data_clean <- readRDS(almp_nma_combined_data_clean_location)

# load custom functions
source("./visualisation/code/visualisation_functions.R")

#-------------------------------------------------------------------------------
# 1. Subset model data
#-------------------------------------------------------------------------------

almp_model_data <- almp_nma_additive_model_data |>
  select(
    study,
    outcome,
    outcome_domain,
    study_design_type,
    low_study_quality
  )

#-------------------------------------------------------------------------------
# 2. Subset component data
#-------------------------------------------------------------------------------

almp_nma_component_data <- almp_nma_combined_data_clean |>
  select(
    study_id,
    starts_with("int_"),
    starts_with("com_")
  ) |>
  mutate(
    across(
      .cols = starts_with("int_"),
      .fns = ~ case_when(
        .x == 1 ~ 1,
        get(paste0("com_", cur_column() |> str_remove("^int_"))) == 1 ~ 1,
        TRUE ~ 0
      ),
      .names = "{str_remove(.col, '^int_')}"
    )
  ) |>
  select(
    -starts_with("int_"),
    -starts_with("com_")
  ) |>
  rename(
    study = study_id
  ) |>
  distinct()

#-------------------------------------------------------------------------------
# 3. Merge data
#-------------------------------------------------------------------------------

almp_nma_component_summary_data <- left_join(
  almp_model_data,
  almp_nma_component_data,
  by = "study"
) |>
  ungroup()

#-------------------------------------------------------------------------------
# 1. Create evidence profile for each component
#-------------------------------------------------------------------------------

num_studies <- almp_nma_component_summary_data |>
  select(
    -outcome_domain,
    -outcome
  ) |>
  filter(
    basic_skills_training == 1
  ) |>
  distinct() |>
  nrow()

num_es <- almp_nma_component_summary_data |>
  filter(
    basic_skills_training == 1
  ) |>
  distinct() |>
  nrow()

num_study_design <- almp_nma_component_summary_data |>
  filter(
    basic_skills_training == 1
  ) |>
  group_by(
    study_design_type
  ) |>
  distinct() |>
  tally()

num_study_quality <- almp_nma_component_summary_data |>
  filter(
    basic_skills_training == 1
  ) |>
  group_by(
    low_study_quality
  ) |>
  distinct() |>
  tally()

num_study_design_x_quality <- almp_nma_component_summary_data |>
  filter(
    basic_skills_training == 1
  ) |>
  group_by(
    study_design_type,
    low_study_quality
  ) |>
  distinct() |>
  tally()

num_outcomes <- almp_nma_component_summary_data |>
  filter(
    basic_skills_training == 1
  ) |>
  group_by(
    outcome_domain,
    outcome
  ) |>
  distinct() |>
  tally()


# Basic Skills Training
create_evidence_profile(
  almp_nma_component_summary_data,
  "basic_skills_training"
)
