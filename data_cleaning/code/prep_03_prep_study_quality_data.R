#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 11/09/2025                                                                           #
# Purpose: process study quality assessment results                                          #
#============================================================================================#

# load required packages
library(tidyverse)

# read study quality data
qa_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_randomised.rds"
qa_randomised_raw <- readRDS(qa_randomised_raw_location)

qa_non_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_non_randomised.rds"
qa_non_randomised_raw <- readRDS(qa_non_randomised_raw_location)

#-------------------------------------------------------------------------------
# 1. Process Randomised Assessment
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2. Process Non-randomised Assessment
#-------------------------------------------------------------------------------

# export
