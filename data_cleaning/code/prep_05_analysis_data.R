#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: clean up and prepare analysis data set                                            #
#============================================================================================#

# load required packages
library(tidyverse)

# load data
almp_nma_combined_data_clean_location <- "./data_cleaning/outputs/almp_nma_pooled_analysis_data.rds"
almp_nma_combined_data_clean <- readRDS(almp_nma_combined_data_clean_location)

#-------------------------------------------------------------------------------
# 1. Export summary data for quality assessment for visualisation
#-------------------------------------------------------------------------------

almp_nma_analysis_data_quality_assessment <- almp_nma_combined_data_clean |>
  # subset quality assessment information
  select(
    study_id,
    low_study_quality,
    qa_randomised_q1,
    qa_randomised_q2,
    qa_randomised_q3,
    qa_randomised_q4,
    qa_randomised_q5,
    qa_randomised_q6,
    qa_randomised_q7,
    qa_randomised_q8,
    qa_randomised_q9,
    qa_randomised_q10,
    qa_randomised_q11,
    qa_randomised_q12,
    qa_randomised_q13,
    qa_non_randomised_q1,
    qa_non_randomised_q2,
    qa_non_randomised_q3,
    qa_non_randomised_q4,
    qa_non_randomised_q5,
    qa_non_randomised_q6,
    qa_non_randomised_q7,
    qa_non_randomised_q8,
    qa_non_randomised_q9
  ) |>
  # drop duplicates
  distinct()

saveRDS(
  almp_nma_analysis_data_quality_assessment,
  "./visualisation/inputs/almp_nma_analysis_data_quality_assessment.RDS"
)

#-------------------------------------------------------------------------------
# 2. Export summary data for intervention components
#-------------------------------------------------------------------------------

almp_nma_analysis_data_intervention_components <- almp_nma_combined_data_clean |>
  # subset quality assessment information
  select(
    study_id,
    int_basic_skills_training,
    int_soft_skills_training,
    int_behavioural_skills_training,
    int_job_specific_technical_skills_off_job_training,
    int_business_skills_training,
    int_business_advisory_and_mentoring,
    int_financial_and_start_up_support,
    int_job_search_preparation,
    int_job_search_assistance,
    int_employment_counselling,
    int_employment_coaching,
    int_financial_assistance,
    int_job_specific_technical_skills_on_job_training,
    int_paid_temporary_work_experience,
    int_unpaid_temporary_work_experience,
    int_wage_subsidies,
    int_public_works,
    int_other_active_component_nec,
    com_services_as_usual,
    com_basic_skills_training,
    com_soft_skills_training,
    com_behavioural_skills_training,
    com_job_specific_technical_skills_off_job_training,
    com_business_skills_training,
    com_business_advisory_and_mentoring,
    com_financial_and_start_up_support,
    com_job_search_preparation,
    com_job_search_assistance,
    com_employment_counselling,
    com_employment_coaching,
    com_financial_assistance,
    com_job_specific_technical_skills_on_job_training,
    com_paid_temporary_work_experience,
    com_unpaid_temporary_work_experience,
    com_wage_subsidies,
    com_public_works,
    com_other_active_component_nec
  ) |>
  # drop duplicates
  distinct()

saveRDS(
  almp_nma_analysis_data_intervention_components,
  "./visualisation/inputs/almp_nma_analysis_data_intervention_components.RDS"
)

#-------------------------------------------------------------------------------
# 3. Export summary data for other summary information
#-------------------------------------------------------------------------------

almp_nma_analysis_data_summary_information <- almp_nma_combined_data_clean |>
  # subset quality assessment information
  select(
    study_id,
    year_published,
    source,
    peer_reviewed,
    study_design_type,
    study_design_detail,
    proportion_female_treatment,
    study_age_min,
    study_age_max,
    study_age_mean,
    location,
    year_start,
    intervention_length_n,
    youth_focused,
    intervention_developer,
    intervention_implementor,
    program_scope,
    intervention_funding,
    conditionality,
    incentives,
    certification,
    study_funding,
    evaluator
  ) |>
  # drop duplicates
  distinct()

saveRDS(
  almp_nma_analysis_data_summary_information,
  "./visualisation/inputs/almp_nma_analysis_data_summary_information.RDS"
)

#-------------------------------------------------------------------------------
# 4. Final cleaning steps
#-------------------------------------------------------------------------------

almp_nma_analysis_data <- almp_nma_combined_data_clean |>
  # impute missing average age data from reported minimum and maximum age ranges
  mutate(
    study_age_mean = case_when(
      is.na(study_age_mean) & !is.na(study_age_min) & !is.na(study_age_max) ~
        (study_age_min + study_age_max) / 2,
      TRUE ~ study_age_mean
    )
  ) |>
  # convert location into a USA binary
  mutate(
    united_states = case_when(
      location == "United States" ~ 1,
      TRUE ~ 0
    )
  ) |>
  # drop data that is not required
  select(
    -study_age_min,
    -study_age_max,
    -location,
    -intervention_intensity_n,
    -qa_randomised_q1,
    -qa_randomised_q2,
    -qa_randomised_q3,
    -qa_randomised_q4,
    -qa_randomised_q5,
    -qa_randomised_q6,
    -qa_randomised_q7,
    -qa_randomised_q8,
    -qa_randomised_q9,
    -qa_randomised_q10,
    -qa_randomised_q11,
    -qa_randomised_q12,
    -qa_randomised_q13,
    -qa_non_randomised_q1,
    -qa_non_randomised_q2,
    -qa_non_randomised_q3,
    -qa_non_randomised_q4,
    -qa_non_randomised_q5,
    -qa_non_randomised_q6,
    -qa_non_randomised_q7,
    -qa_non_randomised_q8,
    -qa_non_randomised_q9
  )

saveRDS(
  almp_nma_analysis_data,
  "./analysis/inputs/almp_nma_analysis_data.RDS"
)
