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
  # combine required components
  mutate(
    int_self_employment_support = case_when(
      int_business_skills_training |
        int_business_advisory_and_mentoring |
        int_financial_and_start_up_support == 1 ~
        1,
      TRUE ~ 0
    ),
    com_self_employment_support = case_when(
      com_business_skills_training |
        com_business_advisory_and_mentoring |
        com_financial_and_start_up_support == 1 ~
        1,
      TRUE ~ 0
    )
  ) |>
  # drop components no longer required
  select(
    -int_business_skills_training,
    -int_business_advisory_and_mentoring,
    -int_financial_and_start_up_support,
    -com_business_skills_training,
    -com_business_advisory_and_mentoring,
    -com_financial_and_start_up_support
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

# Basic Skills Training
evidence_profile_basic_skills_training <- create_evidence_profile(
  almp_nma_component_summary_data,
  basic_skills_training
)

View(evidence_profile_basic_skills_training$outcomes)

# Soft Skills Training
evidence_profile_soft_skills_training <- create_evidence_profile(
  almp_nma_component_summary_data,
  soft_skills_training
)

View(evidence_profile_soft_skills_training$outcomes)

# Behavioural  Skills Training
evidence_profile_behavioural_skills_training <- create_evidence_profile(
  almp_nma_component_summary_data,
  behavioural_skills_training
)

View(evidence_profile_behavioural_skills_training$outcomes)

# Job Specific Technical Skills (Off-Job Training)
evidence_profile_job_specific_technical_skills_off_job_training <- create_evidence_profile(
  almp_nma_component_summary_data,
  job_specific_technical_skills_off_job_training
)

View(evidence_profile_job_specific_technical_skills_off_job_training$outcomes)

# Self-Employment Support
evidence_profile_self_employment_support <- create_evidence_profile(
  almp_nma_component_summary_data,
  self_employment_support
)

View(evidence_profile_self_employment_support$outcomes)

# Job Search Preparation
evidence_profile_job_search_preparation <- create_evidence_profile(
  almp_nma_component_summary_data,
  job_search_preparation
)

View(evidence_profile_job_search_preparation$outcomes)

# Job Search Assistance
evidence_profile_job_search_assistance <- create_evidence_profile(
  almp_nma_component_summary_data,
  job_search_assistance
)

View(evidence_profile_job_search_assistance$outcomes)

# Employment Counselling
evidence_profile_employment_counselling <- create_evidence_profile(
  almp_nma_component_summary_data,
  employment_counselling
)

View(evidence_profile_employment_counselling$outcomes)


# Employment Coaching
evidence_profile_employment_coaching <- create_evidence_profile(
  almp_nma_component_summary_data,
  employment_coaching
)

View(evidence_profile_employment_coaching$outcomes)

# Financial Assistance
evidence_profile_financial_assistance <- create_evidence_profile(
  almp_nma_component_summary_data,
  financial_assistance
)

View(evidence_profile_financial_assistance$outcomes)


# Job-Specific Technical Skills (on the job training)
evidence_profile_job_specific_technical_skills_on_job_training <- create_evidence_profile(
  almp_nma_component_summary_data,
  job_specific_technical_skills_on_job_training
)

View(evidence_profile_job_specific_technical_skills_on_job_training$outcomes)

# Paid Temporary Work Experience
evidence_profile_paid_temporary_work_experience <- create_evidence_profile(
  almp_nma_component_summary_data,
  paid_temporary_work_experience
)

View(evidence_profile_paid_temporary_work_experience$outcomes)

# Unpaid Temporary Work Experience
evidence_profile_unpaid_temporary_work_experience <- create_evidence_profile(
  almp_nma_component_summary_data,
  unpaid_temporary_work_experience
)

View(evidence_profile_unpaid_temporary_work_experience$outcomes)

# Wage Subsidies
evidence_profile_wage_subsidies <- create_evidence_profile(
  almp_nma_component_summary_data,
  wage_subsidies
)

View(evidence_profile_wage_subsidies$outcomes)

# Public Works
evidence_profile_public_works <- create_evidence_profile(
  almp_nma_component_summary_data,
  public_works
)

View(evidence_profile_public_works$outcomes)
