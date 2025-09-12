#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 11/09/2025                                                                           #
# Purpose: visualise study quality results                                                   #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)

# read study quality data
qa_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_randomised.rds"
qa_randomised_raw <- readRDS(qa_randomised_raw_location)

qa_non_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_non_randomised.rds"
qa_non_randomised_raw <- readRDS(qa_non_randomised_raw_location)

#-------------------------------------------------------------------------------
# 1. Visualise Randomised Quality Assessment results
#-------------------------------------------------------------------------------

qa_randomised_raw |>
  # drop constructs that are irrelevant to ALMP RCTs
  select(
    -question_4, # participant blinding
    -question_5, # delivery blinding
    -question_11 # ITT use (we deal with that in other ways)
  ) |>
  pivot_longer(
    cols = -study_id,
    names_to = "domain",
    values_to = "response"
  ) |>
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(~domain, scales = "free") + # Separate histogram for each domain
  labs(
    title = "Distribution of Responses by Domain",
    x = "Response",
    y = "Count"
  )

#-------------------------------------------------------------------------------
# 2. Visualise Non-randomised Quality Assessment results
#-------------------------------------------------------------------------------

qa_non_randomised_raw |>
  # drop constructs that are irrelevant to ALMP RCTs
  select(
    -question_5,
  ) |>
  pivot_longer(
    cols = -study_id,
    names_to = "domain",
    values_to = "response"
  ) |>
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(~domain, scales = "free") + # Separate histogram for each domain
  labs(
    title = "Distribution of Responses by Domain",
    x = "Response",
    y = "Count"
  )

# export
