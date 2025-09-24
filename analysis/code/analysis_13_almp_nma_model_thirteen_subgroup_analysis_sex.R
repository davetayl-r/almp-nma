#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 24/09/2025                                                                           #
# Purpose: NMA model #13 — subgroup analysis x sex                                           #
#============================================================================================#

# load required packages
library(tidyverse)
library(tidybayes)
library(brms)
library(posterior)

# load custom functions
source("./analysis/code/analysis_functions.R")

# load model data and results
almp_nma_model_thirteen_data_location <- "./analysis/output/almp_nma_model_thirteen_data.RDS"
almp_nma_model_thirteen_data <- readRDS(almp_nma_model_thirteen_data_location)

almp_nma_model_thirteen_results_location <- "./analysis/output/almp_nma_model_thirteen.RDS"
almp_nma_model_thirteen_results <- readRDS(
  almp_nma_model_thirteen_results_location
)

# set seed
set.seed(2204)

#-------------------------------------------------------------------------------
# 1. Establish inputs & constants
#-------------------------------------------------------------------------------

# Component dummies present in the fitted model’s data
component_vars <- grep(
  "^comp_",
  names(almp_nma_model_thirteen_results$data),
  value = TRUE
)

# Outcome & design levels
outcome_levels <- levels(almp_nma_model_thirteen_results$data$outcome)
almp_nma_model_thirteen_results$data$study_design_type <- factor(
  almp_nma_model_thirteen_results$data$study_design_type
)
study_design_levels <- levels(
  almp_nma_model_thirteen_results$data$study_design_type
)

# We need the raw female share to recover the centering constant
study_mean_proportion_female <- mean(
  almp_nma_model_thirteen_data$prop_female, # this needs to be changed for the final model
  na.rm = TRUE
)

# Convert desired raw values (0 and 1) to the centred scale used in the model
proportion_female_0_centred_scale <- 0 - study_mean_proportion_female # raw 0%
proportion_female_1_centred_scale <- 1 - study_mean_proportion_female # raw 100%

# A reasonable placeholder for delta_se (required because y|se(delta_se))
delta_se_placeholder <- mean(
  almp_nma_model_thirteen_results$data$delta_se,
  na.rm = TRUE
)

# create base grid with one row per outcome and all variables used by the model so the prediction won’t shit itself
base_outcome_grid <- tibble(
  outcome = factor(outcome_levels, levels = outcome_levels),
  outcome_timing_centred_24 = 0,
  study_design_type = factor("Randomised design", levels = study_design_levels),
  study_design_soo = 0,
  study_design_dbi = 0,
  low_study_quality = 0,
  # placeholder will be overwritten
  prop_female_centred = 0,
  study_age_mean_centred = 0,
  # placeholder
  delta = 0,
  # placeholder required since included in model formula
  delta_se = delta_se_placeholder
) |>
  # add all comp_* columns, initialised to 0 (SAU)
  mutate(!!!setNames(rep(list(0), length(component_vars)), component_vars)) |>
  as.data.frame()

#-------------------------------------------------------------------------------
# 2. Extract draws for study-level subgroup effects x sex
#-------------------------------------------------------------------------------

# where proportion_female = 0 i.e., male
study_level_subgroup_sex_male_draws <- component_effect_x_proportion_female(
  proportion_female_0_centred_scale,
  draw_ids = NULL
)

# where proportion_female = 1 i.e., male
study_level_subgroup_sex_female_draws <- component_effect_x_proportion_female(
  proportion_female_1_centred_scale,
  draw_ids = NULL
)

almp_nma_model_thirteen_study_level_subgroup_sex_draws <- bind_rows(
  study_level_subgroup_sex_male_draws,
  study_level_subgroup_sex_female_draws
) |>
  rename(
    theta = estimate
  ) |>
  mutate(
    subgroup = case_when(
      prop_female_raw == 1 ~ "Female",
      TRUE ~ "Male"
    )
  ) |>
  group_by(
    outcome,
    component,
    subgroup
  ) |>
  mutate(
    probability_greater_zero = mean(theta > 0, na.rm = TRUE),
    probability_low_impact = mean(theta > 0 & theta <= 0.1, na.rm = TRUE),
    probability_medium_impact = mean(
      theta > 0.1 & theta < 0.2,
      na.rm = TRUE
    ),
    probability_high_impact = mean(theta >= 0.2, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    outcome = recode(
      outcome,
      "Apprenticeship participation" = "Apprenticeship Participation",
      "Bachelors or equivalent (ISCED 6) completion" = "Bachelors Degree (ISCED 6) Completion",
      "Bachelors or equivalent (ISCED 6) participation" = "Bachelors Degree (ISCED 6) Participation",
      "Currently Employed" = "Currently Employed",
      "Currently NEET" = "Currently NEET",
      "Currently Not in the Labour Force" = "Currently Not in the Labour Force",
      "Currently Self-Employed" = "Currently Self-Employed",
      "Currently Unemployed" = "Currently Unemployed",
      "Employed since baseline" = "Employed Since Baseline",
      "Entries into Employment" = "Entries into Employment",
      "Exits from Unemployment" = "Exits from Unemployment",
      "Hours Worked" = "Hours Worked",
      "Labour Earnings" = "Labour Earnings",
      "Occupational licence obtained" = "Occupational Licence Obtained",
      "Period Employed" = "Period Employed",
      "Period Unemployed" = "Period Unemployed",
      "Post-secondary non-tertiary (ISCED 4) completion" = "Post-Secondary Non-Tertiary (ISCED 4) Completion",
      "Post-secondary non-tertiary (ISCED 4) participation" = "Post-Secondary Non-Tertiary (ISCED 4) Participation",
      "Recent Employment" = "Recent Employment",
      "Secondary school or equivalent (ISCED 3) completion" = "Secondary School (ISCED 3) Completion",
      "Secondary school or equivalent (ISCED 3) participation" = "Secondary School (ISCED 3) Participation",
      "Short-cycle tertiary (ISCED 5) completion" = "Short-Cycle Tertiary (ISCED 5) Completion",
      "Short-cycle tertiary (ISCED 5) participation" = "Short-Cycle Tertiary (ISCED 5) Participation",
      "Total Income" = "Total Income",
      "Total individual income" = "Total Individual Income",
      "Wages" = "Wages"
    ),
    component = case_when(
      component == "comp_basic_skills_training" ~ "Basic Skills Training",
      component == "comp_soft_skills_training" ~ "Soft Skills Training",
      component == "comp_behavioural_skills_training" ~
        "Behavioural Skills Training",
      component == "comp_self_employment_support" ~ "Self-Employment Support",
      component == "comp_job_specific_technical_skills_off_job_training" ~
        "Technical Skills Training (Off-the-Job)",
      component == "comp_job_search_preparation" ~ "Job Search Preparation",
      component == "comp_job_search_assistance" ~ "Job Search Assistance",
      component == "comp_employment_counselling" ~ "Employment Counselling",
      component == "comp_employment_coaching" ~ "Employment Coaching",
      component == "comp_financial_assistance" ~ "Financial Assistance",
      component == "comp_job_specific_technical_skills_on_job_training" ~
        "Technical Skills Training (On-the-Job)",
      component == "comp_paid_temporary_work_experience" ~
        "Paid Temporary Work Experience",
      component == "comp_unpaid_temporary_work_experience" ~
        "Unpaid Temporary Work Experience",
      component == "comp_wage_subsidies" ~ "Wage Subsidies",
      component == "comp_public_works" ~ "Public Works",
      component == "comp_other_active_component_nec" ~
        "Other Active Components",
      TRUE ~ NA_character_
    ),
    outcome_domain = case_when(
      outcome %in%
        c(
          "Currently Employed",
          "Currently Unemployed",
          "Currently NEET",
          "Currently Not in the Labour Force",
          "Currently Self-Employed",
          "Recent Employment",
          "Employed Since Baseline"
        ) ~
        "Labour Force Status",
      outcome %in% c("Labour Earnings", "Wages") ~ "Employment Compensation",
      outcome %in% c("Total Income", "Total Individual Income") ~
        "Total Income",
      outcome %in% c("Period Employed", "Period Unemployed") ~
        "Employment Duration",
      outcome == "Hours Worked" ~ "Hours Worked",
      outcome %in%
        c(
          "Apprenticeship Participation",
          "Bachelors Degree (ISCED 6) Participation",
          "Bachelors Degree (ISCED 6) Completion",
          "Secondary School (ISCED 3) Completion",
          "Secondary School (ISCED 3) Participation",
          "Occupational Licence Obtained",
          "Short-Cycle Tertiary (ISCED 5) Participation",
          "Short-Cycle Tertiary (ISCED 5) Completion",
          "Post-Secondary Non-Tertiary (ISCED 4) Participation",
          "Post-Secondary Non-Tertiary (ISCED 4) Completion"
        ) ~
        "Education and Skills",
      outcome %in% c("Entries into Employment", "Exits from Unemployment") ~
        "Labour Market Transitions"
    ),
    # Flip the sign on negative outcomes
    theta = case_when(
      outcome %in%
        c(
          "Currently Unemployed",
          "Currently NEET",
          "Currently Not in the Labour Force",
          "Period Not in the Labour Force"
        ) ~
        theta * -1,
      TRUE ~ theta
    )
  )

# Posterior summaries for subgroup effects
almp_nma_model_thirteen_study_level_subgroup_sex_summary <- almp_nma_model_thirteen_study_level_subgroup_sex_draws |>
  group_by(
    outcome,
    outcome_domain,
    component,
    subgroup
  ) |>
  median_qi(
    theta,
    .width = .95
  ) |>
  ungroup() |>
  select(
    -.width,
    -.point,
    -.interval
  )

#-------------------------------------------------------------------------------
# 3. Extract draws for study-level differential treatment effect x sex
#-------------------------------------------------------------------------------

almp_nma_model_thirteen_differential_treatment_effect_sex_draws <- almp_nma_model_thirteen_study_level_subgroup_sex_draws |>
  select(
    .draw,
    outcome,
    outcome_domain,
    component,
    prop_female_raw,
    theta
  ) |>
  pivot_wider(
    names_from = prop_female_raw,
    values_from = theta,
    names_prefix = "proportion_female_"
  ) |>
  group_by(
    .draw,
    outcome,
    component
  ) |>
  mutate(
    contrast_1_minus_0 = proportion_female_1 - proportion_female_0
  ) |>
  ungroup() |>
  group_by(
    outcome,
    component
  ) |>
  mutate(
    probability_greater_for_female = mean(contrast_1_minus_0 > 0, na.rm = TRUE),
    probability_greater_for_male = mean(contrast_1_minus_0 < 0, na.rm = TRUE)
  )

almp_nma_model_thirteen_differential_treatment_effect_sex_summary <- almp_nma_model_thirteen_differential_treatment_effect_sex_draws |>
  group_by(
    outcome,
    outcome_domain,
    component,
    probability_greater_for_female,
    probability_greater_for_male
  ) |>
  median_qi(
    contrast_1_minus_0,
    .width = .95
  ) |>
  ungroup() |>
  select(
    -.width,
    -.point,
    -.interval
  )

#-------------------------------------------------------------------------------
# 4. Export results for visualisation
#-------------------------------------------------------------------------------

saveRDS(
  almp_nma_model_thirteen_study_level_subgroup_sex_draws,
  "./visualisation/inputs/almp_nma_model_thirteen_study_level_subgroup_sex_draws.RDS"
)

saveRDS(
  almp_nma_model_thirteen_study_level_subgroup_sex_summary,
  "./visualisation/inputs/almp_nma_model_thirteen_study_level_subgroup_sex_summary.RDS"
)

saveRDS(
  almp_nma_model_thirteen_differential_treatment_effect_sex_draws,
  "./visualisation/inputs/almp_nma_model_thirteen_differential_treatment_effect_sex_draws.RDS"
)

saveRDS(
  almp_nma_model_thirteen_differential_treatment_effect_sex_summary,
  "./visualisation/inputs/almp_nma_model_thirteen_differential_treatment_effect_sex_summary.RDS"
)
