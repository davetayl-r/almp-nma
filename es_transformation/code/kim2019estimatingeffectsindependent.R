#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 09/09/2025                                                                           #
# Purpose: transform reported results to a common effect size                                #
# Study ID: kim2019estimatingeffectsindependent                                              #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare data for transformation
kim2019estimatingeffectsindependent_outcome_data <- outcome_data |>
  filter(
    # filter data by study id
    study_id == "kim2019estimatingeffectsindependent",
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

# filter results reported as odds ratios and run function
kim2019estimatingeffectsindependent_odds_ratio <- kim2019estimatingeffectsindependent_outcome_data |>
  filter(
    esc_type == "Odds Ratio"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement binary proportions function
    mutate(
      .,
      !!!or_to_smd(
        n1 = .$treatment_n,
        n2 = .$comparison_n,
        odds_ratio = .$odds_ratio,
        se_or = .$se,
        or_is_log = FALSE,
        se_is_log = TRUE,
        method = "logit",
        mask = .$esc_type == "Odds Ratio"
      )
    )
  })()

# merge seperate data back together and filter for export
kim2019estimatingeffectsindependent_export <- kim2019estimatingeffectsindependent_odds_ratio |>
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
    treatment_n,
    comparison_n,
    d,
    d_se,
    d_var,
    g,
    g_se,
    g_var
  )

# export data
saveRDS(
  kim2019estimatingeffectsindependent_export,
  file = "./es_transformation/output/kim2019estimatingeffectsindependent.RDS"
)
