#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 09/09/2025                                                                           #
# Purpose: transform reported results to a common effect size                                #
# Study ID: almp_nma_study_identifier                                                        #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare data for transformation
almp_nma_study_identifier_outcome_data <- outcome_data |>
  filter(
    # filter data by study id
    study_id == "almp_nma_study_identifier",
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

# filter results reported as binary proportions and run function
almp_nma_study_identifier_binary_proportions <- almp_nma_study_identifier_outcome_data |>
  filter(
    esc_type == "Binary proportions"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement binary proportions function
    mutate(
      .,
      !!!proportion_to_smd(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_proportion = .$treatment_proportion,
        comparison_proportion = .$comparison_proportion,
        method = "cox_logit",
        mask = .$esc_type == "Binary proportions"
      )
    )
  })()

# filter results reported as mean and sd and run function
almp_nma_study_identifier_mean_sd <- almp_nma_study_identifier_outcome_data |>
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

# filter results reported as mean and se and run function
almp_nma_study_identifier_mean_se <- almp_nma_study_identifier_outcome_data |>
  filter(
    esc_type == "Mean SE"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!mean_se_to_smd(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_mean = .$treatment_mean,
        comparison_mean = .$comparison_mean,
        treatment_se = .$treatment_se,
        comparison_se = .$comparison_se,
        mask = .$esc_type == "Mean SE"
      )
    )
  })()

# filter results reported as mean and pooled SD and run function
almp_nma_study_identifier_mean_pooled_sd <- almp_nma_study_identifier_outcome_data |>
  filter(
    esc_type == "Mean SD (Pooled)"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!mean_pooled_sd_to_smd(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_mean = .$treatment_mean,
        comparison_mean = .$comparison_mean,
        pooled_sd = .$pooled_sd,
        mask = .$esc_type == "Mean SD (Pooled)"
      )
    )
  })()

# filter results reported as treatment effect binary and run function
almp_nma_study_identifier_te_binary <- almp_nma_study_identifier_outcome_data |>
  filter(
    esc_type == "Treatment Effect (Binary)"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!treatment_effect_binary_to_smd(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_effect = .$treatment_effect,
        treatment_effect_se = .$treatment_effect_se,
        mask = .$esc_type == "Treatment Effect (Binary)"
      )
    )
  })()

# filter results reported as treatment effect continuous and run function
almp_nma_study_identifier_te_continuous <- almp_nma_study_identifier_outcome_data |>
  filter(
    esc_type == "Treatment Effect (Continuous)"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!treatment_effect_continuous_to_smdI(
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        treatment_effect = .$treatment_effect,
        pooled_sd = rep_len(NA_real_, nrow(.)),
        treatment_effect_se = .$treatment_effect_se,
        mask = .$esc_type == "Treatment Effect (Continuous)"
      )
    )
  })()

almp_nma_study_identifier_t_value <- almp_nma_study_identifier_outcome_data |>
  filter(
    esc_type == "T-value"
  ) |>
  # random custom function to allow custom functions to vectorise
  (\(.) {
    # implement mean and pooled sd function
    mutate(
      .,
      !!!t_value_to_smd(
        t_value = .$t,
        treatment_n = .$treatment_n,
        comparison_n = .$comparison_n,
        mask = .$esc_type == "T-value"
      )
    )
  })()

# merge seperate data back together and filter for export
almp_nma_study_identifier_export <- bind_rows(
  almp_nma_study_identifier_binary_proportions,
  almp_nma_study_identifier_mean_se,
  almp_nma_study_identifier_mean_sd,
  almp_nma_study_identifier_mean_pooled_sd,
  almp_nma_study_identifier_te_binary,
  almp_nma_study_identifier_te_continuous,
  almp_nma_study_identifier_t_value
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

# export data
saveRDS(
  almp_nma_study_identifier_export,
  file = "./es_transformation/output/almp_nma_study_identifier.RDS"
)
