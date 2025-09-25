#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 25/09/2025                                                                           #
# Purpose: Visualise subgroup results x age NMA model #13                                    #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggdist)
library(scales)

# load custom functions
source("./visualisation/code/visualisation_functions.R")

#-------------------------------------------------------------------------------
# 1. Load and clean data
#-------------------------------------------------------------------------------

# load plot data
almp_nma_model_thirteen_study_level_subgroup_age_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_study_level_subgroup_age_draws.RDS"
almp_nma_model_thirteen_study_level_subgroup_age_draws <- readRDS(
  almp_nma_model_thirteen_study_level_subgroup_age_draws_location
)

almp_nma_model_thirteen_study_level_subgroup_age_summary_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_study_level_subgroup_age_summary.RDS"
almp_nma_model_thirteen_study_level_subgroup_age_summary <- readRDS(
  almp_nma_model_thirteen_study_level_subgroup_age_summary_location
)

# load component results to exclude where posterior_different_prior_flag == "No"
almp_nma_model_thirteen_component_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_component_draws.RDS"
almp_nma_model_thirteen_component_draws <- readRDS(
  almp_nma_model_thirteen_component_draws_location
)

# identify outcomes to exclude
posterior_flags <- almp_nma_model_thirteen_component_draws |>
  select(
    outcome,
    component,
    posterior_different_prior_flag
  ) |>
  distinct()

# join flags to subgroup plotting data and filter
almp_nma_model_thirteen_study_level_subgroup_age_draws_filtered <- almp_nma_model_thirteen_study_level_subgroup_age_draws |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    # not reporting other components
    !component == "Other Active Components",
    # filter outcomes where posterior is not different to prior
    posterior_different_prior_flag == "Yes",
    # drop outcome's that are otherwise sparse
    !outcome %in%
      c(
        "Currently Not in the Labour Force",
        "Employed Since Baseline"
      ),
    # drop outcome domains that are included for network stability
    !outcome_domain %in%
      c(
        "Total Income",
        "Labour Market Transitions"
      )
  ) |>
  # order outcomes
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Currently Employed",
        "Recent Employment",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Currently NEET",
        "Hours Worked",
        "Period Employed",
        "Labour Earnings",
        "Wages"
      ),
      ordered = TRUE
    )
  )

# join flags to subgroup summary data and filter
almp_nma_model_thirteen_study_level_subgroup_age_summary_filtered <- almp_nma_model_thirteen_study_level_subgroup_age_summary |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    # not reporting other components
    !component == "Other Active Components",
    # filter outcomes where posterior is not different to prior
    posterior_different_prior_flag == "Yes",
    # drop outcome's that are otherwise sparse
    !outcome %in%
      c(
        "Currently Not in the Labour Force",
        "Employed Since Baseline"
      ),
    # drop outcome domains that are included for network stability
    !outcome_domain %in%
      c(
        "Total Income",
        "Labour Market Transitions"
      )
  ) |>
  # order outcomes
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Currently Employed",
        "Recent Employment",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Currently NEET",
        "Hours Worked",
        "Period Employed",
        "Labour Earnings",
        "Wages"
      ),
      ordered = TRUE
    )
  )

#-------------------------------------------------------------------------------
# 2. Basic Skills Training x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Basic Skills Training",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Basic Skills Training",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Basic Skills Training",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Basic Skills Training",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Basic Skills Training",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_basic_skills_training_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 3. Behavioural Skills Training x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Behavioural Skills Training",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Behavioural Skills Training",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Behavioural Skills Training",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Behavioural Skills Training",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Behavioural Skills Training",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_behavioural_skills_training_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 4. Employment Coaching x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Employment Coaching",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Employment Coaching",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Employment Coaching",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Employment Coaching",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Employment Coaching",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_coaching_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 5. Employment Counselling x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Employment Counselling",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Employment Counselling",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Employment Counselling",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Employment Counselling",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Employment Counselling",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_employment_counselling_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 6. Financial Assistance x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Financial Assistance",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Financial Assistance",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Financial Assistance",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Financial Assistance",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Financial Assistance",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_financial_assistance_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 7. Job Search Assistance x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Job Search Assistance",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Job Search Assistance",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Job Search Assistance",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Job Search Assistance",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Job Search Assistance",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_assistance_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 8. Job Search Preparation x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Job Search Preparation",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Job Search Preparation",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Job Search Preparation",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Job Search Preparation",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Job Search Preparation",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_job_search_preparation_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 9. Paid Temporary Work Experience x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Paid Temporary Work Experience",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Paid Temporary Work Experience",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Paid Temporary Work Experience",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Paid Temporary Work Experience",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Paid Temporary Work Experience",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_paid_temporary_work_experience_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 10. Public Works x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Public Works",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
#almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_education_skills <- create_subgroup_age_forest_plot(
#  component_name = "Public Works",
#  outcome_domain_name = "Education and Skills"
#)

#ggsave(
#  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_education_skills,
#  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_education_skills.png",
#  height = 11,
#  width = 7,
#  device = "png",
#  type = "cairo-png"
#)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Public Works",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Public Works",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
#almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_hours_worked <- create_subgroup_age_forest_plot(
#  component_name = "Public Works",
#  outcome_domain_name = "Hours Worked"
#)

#ggsave(
#  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_hours_worked,
#  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_public_works_hours_worked.png",
#  height = 3,
#  width = 7,
#  device = "png",
#  type = "cairo-png"
#)

#-------------------------------------------------------------------------------
# 11. Self-Employment Support x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Self-Employment Support",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Self-Employment Support",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
#almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_employment_compensation <- create_subgroup_age_forest_plot(
#  component_name = "Self-Employment Support",
#  outcome_domain_name = "Employment Compensation"
#)

#ggsave(
#  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_employment_compensation,
#  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_employment_compensation.png",
#  height = 4,
#  width = 7,
#  device = "png",
#  type = "cairo-png"
#)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Self-Employment Support",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
#almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_hours_worked <- create_subgroup_age_forest_plot(
#  component_name = "Self-Employment Support",
#  outcome_domain_name = "Hours Worked"
#)

#ggsave(
#  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_hours_worked,
#  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_self_employment_support_hours_worked.png",
#  height = 3,
#  width = 7,
#  device = "png",
#  type = "cairo-png"
#)

#-------------------------------------------------------------------------------
# 12. Soft Skills Training x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Soft Skills Training",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Soft Skills Training",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Soft Skills Training",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Soft Skills Training",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Soft Skills Training",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_soft_skills_training_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 13. Technical Skills Training (Off-the-Job) x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (Off-the-Job)",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (Off-the-Job)",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (Off-the-Job)",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (Off-the-Job)",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (Off-the-Job)",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_off_the_job_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 14. Technical Skills Training (On-the-Job) x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (On-the-Job)",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (On-the-Job)",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (On-the-Job)",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (On-the-Job)",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Technical Skills Training (On-the-Job)",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_technical_skills_training_on_the_job_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 15. Unpaid Temporary Work Experience x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Unpaid Temporary Work Experience",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Unpaid Temporary Work Experience",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Unpaid Temporary Work Experience",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Unpaid Temporary Work Experience",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Unpaid Temporary Work Experience",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_unpaid_temporary_work_experience_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 16. Wage Subsidies x study-level age subgroup
#-------------------------------------------------------------------------------

# Labour Force outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_labour_force_status_outcomes <- create_subgroup_age_forest_plot(
  component_name = "Wage Subsidies",
  outcome_domain_name = "Labour Force Status"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_labour_force_status_outcomes,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_labour_force_status_outcomes.png",
  height = 6,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Education and Skills outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_education_skills <- create_subgroup_age_forest_plot(
  component_name = "Wage Subsidies",
  outcome_domain_name = "Education and Skills"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_education_skills,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_education_skills.png",
  height = 11,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Compensation outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_employment_compensation <- create_subgroup_age_forest_plot(
  component_name = "Wage Subsidies",
  outcome_domain_name = "Employment Compensation"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_employment_compensation,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_employment_compensation.png",
  height = 4,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Employment Duration outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_employment_duration <- create_subgroup_age_forest_plot(
  component_name = "Wage Subsidies",
  outcome_domain_name = "Employment Duration"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_employment_duration,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_employment_duration.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)

# Hours worked outcomes
almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_hours_worked <- create_subgroup_age_forest_plot(
  component_name = "Wage Subsidies",
  outcome_domain_name = "Hours Worked"
)

ggsave(
  plot = almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_hours_worked,
  filename = "./visualisation/output/subgroup_age/almp_nma_additive_model_study_level_subgroup_age_forest_plot_wage_subsidies_hours_worked.png",
  height = 3,
  width = 7,
  device = "png",
  type = "cairo-png"
)
