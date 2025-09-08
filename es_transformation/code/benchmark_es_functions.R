#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 08/09/2025                                                                           #
# Purpose: test effect size transformation formulas against the Campbell calculator          #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare test data
test_data <- outcome_data |>
  filter(
    # exclude outcomes with missing data
    is.na(exclude_missing_data) | exclude_missing_data != "Yes",
    # exclude outcomes that report duplicate constructs
    is.na(exclude_duplicate_construct) | exclude_duplicate_construct != "Yes",
    # exclude outcomes that have been deprioritised for other reasons (for example, they are not relevant to this analysis)
    is.na(deprioritised) | deprioritised != "Yes"
  ) |>
  # convert list vars to numeric
  mutate(
    across(
      c(
        outcome_timing,
        treatment_n,
        comparison_n,
        treatment_proportion,
        comparison_proportion,
        treatment_mean,
        treatment_sd,
        treatment_se,
        comparison_mean,
        comparison_sd,
        comparison_se,
        pooled_sd,
        odds_ratio,
        se,
        totaln,
        chisq,
        t,
        t_pvalue,
        treatment_effect,
        treatment_effect_se,
        treatment_effect_ci_low,
        treatment_effect_ci_high,
        treatment_effect_p_value
      ),
      convert_input_data_to_numeric
    )
  ) |>
  # round sample sizes to whole numbers
  mutate(
    treatment_n = round(treatment_n, 0),
    comparison_n = round(comparison_n, 0)
  )

# test binary proportions by selecting one at random, and running function
binary_proportions <- test_data |>
  filter(
    esc_type == "Binary proportions",
    study_id == "aeberhardt2022conditionalcashtransfers",
    outcome == "Currently Employed"
  ) |>
  slice(1) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement binary proportions function
    mutate(
      .,
      !!!proportion_to_smd(
        .$treatment_n,
        .$comparison_n,
        .$treatment_proportion,
        .$comparison_proportion,
        method = "cox_logit",
        mask = .$esc_type == "Binary proportions"
      )
    )
  })()
View(binary_proportions)

# result from function: g = 0.0594, g_se = 0.0416
# result from Campbell: g = 0.0594, g_se = 0.0416

# test mean sd by selecting one at random, and running function
mean_sd <- test_data |>
  filter(
    esc_type == "Mean SD",
    study_id == "cmar2022impactsummerwork",
    outcome == "Hours Worked"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!mean_sd_to_smd(
        .$treatment_n,
        .$comparison_n,
        .$treatment_mean,
        .$comparison_mean,
        .$treatment_sd,
        .$comparison_sd,
        mask = .$esc_type == "Mean SD"
      )
    )
  })()
View(mean_sd)

# result from function: g = 0.03385, g_se = 0.1585
# result from Campbell: g = 0.0339, g_se = 0.1585

# test mean se by selecting one at random, and running function
mean_se <- test_data |>
  filter(
    esc_type == "Mean SE",
    study_id == "ibarraran2006impactevaluationjob_a",
    outcome == "Labour Earnings"
  ) |>
  slice_sample(n = 1) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!mean_se_to_smd(
        .$treatment_n,
        .$comparison_n,
        .$treatment_mean,
        .$comparison_mean,
        .$treatment_se,
        .$comparison_se,
        mask = .$esc_type == "Mean SE"
      )
    )
  })()
View(mean_se)

# result from function: g = 0.1285, g_se: 0.1019
# result from Campbell: g = 0.1285, g_se: 0.1019

# test mean pooled sd by selecting one at random, and running function
mean_sd_pooled <- test_data |>
  filter(
    esc_type == "Mean SD (Pooled)",
    study_id == "aeberhardt2022conditionalcashtransfers",
    outcome == "Total individual income"
  ) |>
  slice_sample(n = 1) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!mean_pooled_sd_to_smd(
        .$treatment_n,
        .$comparison_n,
        .$treatment_mean,
        .$comparison_mean,
        .$pooled_sd,
        mask = .$esc_type == "Mean SD (Pooled)"
      )
    )
  })()
View(mean_sd_pooled)

# result from function: g = -0.0282, g_se = 0.0416
# result from Campbell: g = -0.0282, g_se = 0.0416

# test treatment effect binary by comparing with a study that reports binary proportions and a treatment effect
treatment_effect_binary <- test_data |>
  filter(
    study_id == "aeberhardt2022conditionalcashtransfers",
    outcome == "Currently Employed"
  ) |>
  mutate(
    esc_type = case_when(
      esc_type == "Binary proportions" ~ "Treatment Effect (Binary)"
    )
  ) |>
  mutate(
    treatment_effect = treatment_effect / 100,
    treatment_effect_se = treatment_effect_se / 100
  ) |>
  slice(1) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!treatment_effect_binary_to_smd(
        .$treatment_n,
        .$comparison_n,
        .$treatment_effect,
        .$treatment_effect_se,
        mask = .$esc_type == "Treatment Effect (Binary)"
      )
    )
  })()
View(treatment_effect_binary)

# result from function: g = 0.0518, g_se = 0.0343
# result from binary proportions function (they should be similar): g = 0.0594, g_se = 0.0416

# test t-value by comparing with a study that reports binary proportions and a treatment effect
t_value <- test_data |>
  filter(
    study_id == "ehlert2012temporaryworkactive",
    outcome == "Currently Employed"
  ) |>
  slice(1) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!t_value_to_smd(
        .$t,
        .$treatment_n,
        .$comparison_n,
        mask = .$esc_type == "T-value"
      )
    )
  })()
View(t_value)

# result from function: g = 0.3126, g_se = 0.1349
# result from Campbell: g = 0.3126, g_se = 0.1349
