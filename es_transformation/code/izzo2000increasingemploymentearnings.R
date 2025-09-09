#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 09/09/2025                                                                           #
# Purpose: transform reported results to a common effect size                                #
# Study ID: izzo2000increasingemploymentearnings                                             #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare data for transformation
izzo2000increasingemploymentearnings_outcome_data <- outcome_data |>
  filter(
    # filter data by study id
    study_id == "izzo2000increasingemploymentearnings",
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

# filter results reported as mean and sd and run function
izzo2000increasingemploymentearnings_mean_sd <- izzo2000increasingemploymentearnings_outcome_data |>
  filter(
    esc_type == "Mean SD"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!mean_sd_to_smd(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_mean = .$treatment_mean,
        comparison_mean = .$comparison_mean,
        treatment_sd = .$treatment_sd,
        comparison_sd = .$comparison_sd,
        mask = .$esc_type == "Mean SD"
      )
    )
  })()

# filter results reported as other (chi-squared)
izzo2000increasingemploymentearnings_chi_squared <- izzo2000increasingemploymentearnings_outcome_data |>
  filter(
    esc_type == "Other"
  ) |>
  mutate(
    total_n = treatment_n + comparison_n,
    # assume 1 df chi-sq (2x2 test)
    df = 1,
    # t = sqrt(chi2) when df=1
    t_value = sqrt(chisq),
    d = t_value * sqrt(1 / treatment_n + 1 / comparison_n),
    d_var = (treatment_n + comparison_n) /
      (treatment_n * comparison_n) +
      d^2 / (2 * (total_n - 2)),
    d_se = sqrt(d_var),
    # Hedges’ correction
    J = 1 - 3 / (4 * (total_n - 2) - 1),
    g = J * d,
    g_var = (J^2) * d_var,
    g_se = sqrt(g_var)
  ) |>
  select(
    study_id,
    outcome_domain,
    outcome,
    outcome_source,
    favourable_direction,
    outcome_timing,
    estimand,
    intention_to_treat,
    conditional,
    d,
    d_se,
    d_var,
    g,
    g_se,
    g_var
  )

# merge seperate data back together and filter for export
izzo2000increasingemploymentearnings_export <- izzo2000increasingemploymentearnings_mean_sd |>
  select(
    study_id,
    outcome_domain,
    outcome,
    outcome_source,
    favourable_direction,
    outcome_timing,
    estimand,
    intention_to_treat,
    conditional,
    d,
    d_se,
    d_var,
    g,
    g_se,
    g_var
  ) |>
  bind_rows(
    izzo2000increasingemploymentearnings_chi_squared
  )

# export data
saveRDS(
  izzo2000increasingemploymentearnings_export,
  file = "./es_transformation/output/izzo2000increasingemploymentearnings.RDS"
)
