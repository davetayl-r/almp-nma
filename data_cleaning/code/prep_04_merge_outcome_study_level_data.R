#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 09/09/2025                                                                           #
# Purpose: consolidate effect size data with study-level data                                #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read all relevant data
almp_study_level_data_location <- "./data_cleaning/outputs/almp_nma_study_details.rds"
almp_study_level_data <- readRDS(almp_study_level_data_location)

almp_effect_size_data_location <- "./es_transformation/output/almp_nma_combined_effect_size_data.rds"
almp_effect_size_data <- readRDS(almp_effect_size_data_location)

almp_intervention_components_data_location <- "./data_cleaning/outputs/almp_nma_intervention_components.rds"
almp_intervention_components_data <- readRDS(
  almp_intervention_components_data_location
)


#almp_study_quality_data_location <- ""
#almp_study_quality_data <- readRDS(almp_study_quality_data_location)

# start with effect size data
almp_nma_combined_data <- almp_effect_size_data |>
  # add study level data for those studies for which we managed to get an ES
  left_join(
    almp_study_level_data,
    by = "study_id"
  ) |>
  # add intervention components in too
  left_join(
    almp_intervention_components_data,
    by = "study_id"
  ) |>
  # drop unrequired variables
  select(
    -proportion_female_comparison,
    -study_age_median,
    -disability_proportion,
    -ever_employed_proportion,
    -high_school_complete_proportion,
    -care_experienced_proportion,
    -pregnant_or_child_proportion,
    -homeless_proportion,
    -ever_arrested_proportion,
    -incarcerated_proportion
  ) |>
  # convert list vars to numeric
  mutate(
    across(
      c(
        proportion_female_treatment,
        study_age_min,
        study_age_max,
        study_age_mean,
        year_start,
        year_end,
        intervention_length_n,
        intervention_intensity_n,
      ),
      convert_input_data_to_numeric
    )
  )
