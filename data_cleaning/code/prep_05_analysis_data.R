#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: clean up and prepare analysis data set                                            #
#============================================================================================#

# load required packages
library(tidyverse)

# load data
almp_nma_combined_data_clean_location <- "./data_cleaning/outputs/almp_nma_analysis_data.rds"
almp_nma_combined_data_clean <- readRDS(almp_nma_combined_data_clean_location)

#-------------------------------------------------------------------------------
# 1. Final cleaning steps
#-------------------------------------------------------------------------------

almp_nma_analysis_data <- almp_nma_combined_data_clean |>
  # impute missing average age data from reported minimum and maximum age ranges
  mutate(
    mean_age = case_when(
      is.na(study_age_mean) & !is.na(study_age_min) & !is.na(study_age_max) ~
        (study_age_min + study_age_max) / 2,
      TRUE ~ study_age_mean
    )
  ) |>
  # drop data that is not required
  select(
    -study_age_min,
    -study_age_max,
    -intervention_intensity_n
  )
