#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: clean up and prepare analysis data set                                            #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./analysis/code/analysis_functions.R")

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
    study_design_type,
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
# 4. Consolidate component data
#-------------------------------------------------------------------------------

almp_nma_consolidated_component_data <- almp_nma_combined_data_clean |>
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
  # consolidate self employment support
  mutate(
    int_self_employment_support = case_when(
      int_business_skills_training == 1 |
        int_business_advisory_and_mentoring == 1 |
        int_financial_and_start_up_support == 1 ~
        1,
      TRUE ~ 0
    ),
    com_self_employment_support = case_when(
      com_business_skills_training == 1 |
        com_business_advisory_and_mentoring == 1 |
        com_financial_and_start_up_support == 1 ~
        1,
      TRUE ~ 0
    )
  ) |>
  select(
    -int_business_skills_training,
    -int_business_advisory_and_mentoring,
    -int_financial_and_start_up_support,
    -com_business_skills_training,
    -com_business_advisory_and_mentoring,
    -com_financial_and_start_up_support
  ) |>
  distinct() |>
  # convert treatment components to long format
  pivot_longer(
    cols = starts_with("int_"),
    names_to = "int_type",
    values_to = "int_flag",
    names_prefix = "int_"
  ) |>
  # drop rows with no components
  filter(
    int_flag == 1
  ) |>
  # group by study_id and combine treatments with "+"
  group_by(
    study_id
  ) |>
  summarise(
    intervention = paste(int_type, collapse = "+"),
    # keep all comparison columns
    across(starts_with("com_"), first),
    .groups = "drop"
  ) |>
  # convert comparison components to long format
  pivot_longer(
    cols = starts_with("com_"),
    names_to = "comp_type",
    values_to = "comp_flag",
    names_prefix = "com_"
  ) |>
  # drop rows with no components
  filter(
    comp_flag == 1
  ) |>
  # group by study_id and treatment components, combine comparison components
  group_by(
    study_id,
    intervention
  ) |>
  summarise(
    comparison = paste(comp_type, collapse = "+"),
    .groups = "drop"
  )

#-------------------------------------------------------------------------------
# 5. Centre study-level variables
#-------------------------------------------------------------------------------

almp_nma_centred_data <- almp_nma_combined_data_clean |>
  distinct(
    study_id,
    proportion_female_treatment,
    study_age_min,
    study_age_max,
    study_age_mean
  ) |>
  group_by(
    study_id
  ) |>
  arrange(
    study_age_mean
  ) |>
  mutate(
    appearence_flag = row_number()
  ) |>
  filter(
    appearence_flag == 1
  ) |>
  ungroup() |>
  mutate(
    # impute missing average age data from reported minimum and maximum age ranges
    study_age_mean = case_when(
      is.na(study_age_mean) & !is.na(study_age_min) & !is.na(study_age_max) ~
        (study_age_min + study_age_max) / 2,
      TRUE ~ study_age_mean
    ),
    # centre age around mean
    prop_female_centred = proportion_female_treatment -
      mean(proportion_female_treatment, na.rm = TRUE),
    # centre age around mean
    study_age_mean_centred = study_age_mean -
      mean(study_age_mean, na.rm = TRUE),
    # centre age from 18
    study_age_mean_centred_18 = study_age_mean - 18
  ) |>
  # drop redundant vars
  select(
    -study_age_min,
    -study_age_max,
    -appearence_flag
  )

#-------------------------------------------------------------------------------
# 6. Merge component data and centred study-level vars
#-------------------------------------------------------------------------------

almp_nma_analysis_data <- almp_nma_combined_data_clean |>
  # join consolidated component data
  left_join(
    almp_nma_consolidated_component_data,
    by = "study_id"
  ) |>
  # drop vars that will duplicate from centred data
  select(
    -proportion_female_treatment,
    -study_age_min,
    -study_age_max,
    -study_age_mean
  ) |>
  # join centred vars
  left_join(
    almp_nma_centred_data,
    by = "study_id"
  ) |>
  mutate(
    # convert location into a USA binary
    united_states = case_when(
      location == "United States" ~ 1,
      TRUE ~ 0
    )
  ) |>
  # drop data that is not required
  select(
    -treatment_n,
    -comparison_n,
    -intervention_intensity_n,
    -int_basic_skills_training,
    -int_soft_skills_training,
    -int_behavioural_skills_training,
    -int_job_specific_technical_skills_off_job_training,
    -int_business_skills_training,
    -int_business_advisory_and_mentoring,
    -int_financial_and_start_up_support,
    #-int_self_employment_support,
    -int_job_search_preparation,
    -int_job_search_assistance,
    -int_employment_counselling,
    -int_employment_coaching,
    -int_financial_assistance,
    -int_job_specific_technical_skills_on_job_training,
    -int_paid_temporary_work_experience,
    -int_unpaid_temporary_work_experience,
    -int_wage_subsidies,
    -int_public_works,
    -int_other_active_component_nec,
    -com_services_as_usual,
    -com_basic_skills_training,
    -com_soft_skills_training,
    -com_behavioural_skills_training,
    -com_job_specific_technical_skills_off_job_training,
    -com_business_skills_training,
    -com_business_advisory_and_mentoring,
    -com_financial_and_start_up_support,
    #-com_self_employment_support,
    -com_job_search_preparation,
    -com_job_search_assistance,
    -com_employment_counselling,
    -com_employment_coaching,
    -com_financial_assistance,
    -com_job_specific_technical_skills_on_job_training,
    -com_paid_temporary_work_experience,
    -com_unpaid_temporary_work_experience,
    -com_wage_subsidies,
    -com_public_works,
    -com_other_active_component_nec,
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

# export data to use for summary visualisation
saveRDS(
  almp_nma_analysis_data,
  "./visualisation/inputs/almp_nma_summary_visualisation_data.RDS"
)

#-------------------------------------------------------------------------------
# 7. Prepare additive modelling data
#-------------------------------------------------------------------------------

# Create component matrix
almp_component_matrix <- create_component_matrix(almp_nma_analysis_data)

# Create working copy of data
almp_working_data <- almp_nma_analysis_data

# Add component variables
for (component in colnames(almp_component_matrix)) {
  comp_col_name <- paste0("comp_", component)
  almp_working_data[[comp_col_name]] <- sapply(
    1:nrow(almp_nma_analysis_data),
    function(i) {
      intervention <- almp_nma_analysis_data$intervention[i]
      comparison <- almp_nma_analysis_data$comparison[i]
      intervention_comp <- almp_component_matrix[intervention, component]
      comparison_comp <- almp_component_matrix[comparison, component]
      return(intervention_comp - comparison_comp)
    }
  )
}

# clean up data
almp_nma_component_model_data <- almp_working_data |>
  # drop redundant vars
  select(
    -intervention,
    -comparison
  ) |>
  # rename vars
  rename(
    study = study_id,
    delta = g,
    delta_se = g_se
  )

#-------------------------------------------------------------------------------
# 8. Select single timepoint for analysis
#-------------------------------------------------------------------------------

# specify time point anchors for 24 months +/- 6 months
outcome_domain_targets <- tibble::tribble(
  ~outcome_domain,
  ~target,
  ~window,
  "Labour Force Status",
  24,
  6,
  "Education and Skills",
  24,
  6,
  "Employment Duration",
  24,
  6,
  "Employment compensation",
  24,
  6,
  "Hours Worked",
  24,
  6,
  "Total Income",
  24,
  6,
  "Labour Market Transitions",
  24,
  6
)

# run outcome timing function
almp_nma_timing_model_data <- almp_nma_component_model_data |>
  select_outcome_timepoint(
    targets = outcome_domain_targets,
    study_var = "study",
    domain_var = "outcome_domain",
    time_var = "outcome_timing",
    default_target = 24,
    default_window = 6
  ) |>
  mutate(
    timepoint_outside_anchor_window = case_when(
      selected_outside_window == TRUE ~ 1
    )
  )

# coverage table to see how much you keep
outcome_coverage <- almp_nma_timing_model_data |>
  filter(!is.na(outcome_timing)) |>
  summarise(
    n_studies = n_distinct(study),
    n_within = n_distinct(study[within_anchor_window]),
    prop_within = n_within / n_studies,
    .by = outcome_domain
  ) |>
  arrange(desc(prop_within))
outcome_coverage

#-------------------------------------------------------------------------------
# 9. Clean up data for export
#-------------------------------------------------------------------------------

almp_nma_additive_model_data <- almp_nma_timing_model_data |>
  # drop redundant vars
  select()

saveRDS(
  almp_nma_additive_model_data,
  "./analysis/inputs/almp_nma_additive_model_data.RDS"
)
