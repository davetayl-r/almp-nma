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
  mutate(
    subgroup = case_when(
      prop_female_raw == 1 ~ "female",
      TRUE ~ "male"
    )
  ) |>
  group_by(
    outcome,
    component,
    subgroup
  ) |>
  mutate(
    probability_greater_zero = mean(estimate > 0, na.rm = TRUE),
    probability_low_impact = mean(estimate > 0 & estimate <= 0.1, na.rm = TRUE),
    probability_medium_impact = mean(
      estimate > 0.1 & estimate < 0.2,
      na.rm = TRUE
    ),
    probability_high_impact = mean(estimate >= 0.2, na.rm = TRUE)
  ) |>
  ungroup()

# Posterior summaries for subgroup effects
almp_nma_model_thirteen_study_level_subgroup_sex_summary <- almp_nma_model_thirteen_study_level_subgroup_sex_draws |>
  median_qi(
    estimate,
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
    component,
    prop_female_raw,
    estimate
  ) |>
  pivot_wider(
    names_from = prop_female_raw,
    values_from = estimate,
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
