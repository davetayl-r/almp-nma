#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 05/09/2025                                                                           #
# Purpose: transform reported results to a common effect size                                #
# Study ID: aeberhardt2022conditionalcashtransfers                                           #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare data for transformation
aeberhardt2022conditionalcashtransfers_outcome_data <- outcome_data |>
  filter(
    # filter data by study id
    study_id == "aeberhardt2022conditionalcashtransfers",
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
        grp1m,
        grp1sd,
        grp1se,
        grp2m,
        grp2sd,
        grp2se,
        pre1mean,
        pre1sd,
        post1mean,
        post1sd,
        pre2mean,
        pre2sd,
        post2mean,
        post2sd,
        pooled_sd,
        gain1mean,
        gain1se,
        gain2mean,
        gain2se,
        or,
        f,
        se,
        totaln,
        b,
        beta,
        sdy,
        chisq,
        t,
        t_pvalue,
        diff,
        diff_lower,
        diff_upper,
        diff_se,
        te,
        te_se,
        te_ci_low,
        te_ci_high,
        te_p_value
      ),
      convert_input_data_to_numeric
    )
  ) |>
  # round sample sizes to whole numbers
  mutate(
    treatment_n = round(treatment_n, 0),
    comparison_n = round(comparison_n, 0)
  )

test <- aeberhardt2022conditionalcashtransfers_outcome_data |>
  mutate(
    !!!proportion_to_smd(
      .$treatment_n,
      .$comparison_n,
      .$treatment_proportion,
      .$comparison_proportion,
      method = "cox_logit",
      mask = .$esc_type == "binary_proportions"
    )
  )


View(aeberhardt2022conditionalcashtransfers_outcome_data)
