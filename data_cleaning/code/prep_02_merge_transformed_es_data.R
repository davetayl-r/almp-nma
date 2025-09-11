#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 09/09/2025                                                                           #
# Purpose: read and consolidate all transformed effect size data                             #
#============================================================================================#

# load required packages
library(tidyverse)

# effect_size_location to your folder with effect size data
effect_size_location <- "./es_transformation/output"

# list all .rds files in the folder
effect_size_files <- list.files(
  effect_size_location,
  pattern = "\\.RDS$",
  full.names = TRUE
)

# read and combine into one data frame
combined_effect_sizes <- effect_size_files %>%
  map(readRDS) %>%
  bind_rows()

# number of effect sizes
effect_size_count <- dim(combined_effect_sizes)[1]

# number of studies
study_count <- combined_effect_sizes |> distinct(study_id) |> dim()

# summary
effect_size_count
study_count[1]

# per cent complete
est_es <- 2210
percent_es_complete <- effect_size_count / est_es
percent_es_complete

est_studies <- (288 - 5) - 40
percent_complete <- study_count[1] / est_studies
percent_complete
