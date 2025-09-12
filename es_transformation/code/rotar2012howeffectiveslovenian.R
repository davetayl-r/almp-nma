#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 11/09/2025                                                                           #
# Purpose: transform reported results to a common effect size                                #
# Study ID: rotar2012howeffectiveslovenian                                                        #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare data for transformation
rotar2012howeffectiveslovenian_outcome_data <- outcome_data |>
  filter(
    # filter data by study id
    study_id == "rotar2012howeffectiveslovenian",
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

# filter results reported as treatment effect continuous and run function
rotar2012howeffectiveslovenian_te_continuous <- rotar2012howeffectiveslovenian_outcome_data |>
  filter(
    esc_type == "Treatment Effect (Continuous)"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!treatment_effect_continuous_to_smd(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_effect = .$treatment_effect,
        pooled_sd = rep_len(NA_real_, nrow(.)),
        treatment_effect_se = .$treatment_effect_se,
        mask = .$esc_type == "Treatment Effect (Continuous)"
      )
    )
  })()

# merge seperate data back together and filter for export
rotar2012howeffectiveslovenian_export <- rotar2012howeffectiveslovenian_te_continuous |>
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
  rotar2012howeffectiveslovenian_export,
  file = "./es_transformation/output/rotar2012howeffectiveslovenian.RDS"
)
