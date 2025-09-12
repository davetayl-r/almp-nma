#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 11/09/2025                                                                           #
# Purpose: run, read and consolidate all transformed effect size code and data               #
#============================================================================================#

# load required packages
library(tidyverse)

#-------------------------------------------------------------------------------
# 1. [Optional] Run all individual R code files
#-------------------------------------------------------------------------------

# code location
effect_size_transformation_code_location <- "./es_transformation/code"

# list all .rds files in the folder
effect_size_code_files <- list.files(
  effect_size_transformation_code_location,
  pattern = "\\.R$",
  full.names = TRUE
) |>
  discard(~ basename(.x) == "benchmark_es_functions.R") |>
  discard(~ basename(.x) == "effect_size_functions.R") |>
  discard(~ basename(.x) == "backup_functions.R")

# run each code file
effect_size_code_files |>
  walk(source)

#-------------------------------------------------------------------------------
# 2. Read all effect size output
#-------------------------------------------------------------------------------

# effect size location
effect_size_output_location <- "./es_transformation/output"

# list all .rds files in the folder
effect_size_output_files <- list.files(
  effect_size_output_location,
  pattern = "\\.RDS$",
  full.names = TRUE
)

# read and combine into one data frame
combined_effect_size_data <- effect_size_output_files %>%
  map(readRDS) %>%
  bind_rows()

# export data
saveRDS(
  combined_effect_size_data,
  file = "./es_transformation/output/almp_nma_combined_effect_size_data.rds"
)
