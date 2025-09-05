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
outcome_data_location <- ""
outcome_data <- readRDS(outcome_data_location)

# filter data by study id
aeberhardt2022conditionalcashtransfers_outcome_data <- outcome_data |>
  filter(
    study_id == "aeberhardt2022conditionalcashtransfers"
  )
