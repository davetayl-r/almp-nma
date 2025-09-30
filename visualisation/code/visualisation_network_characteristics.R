#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 27/09/2025                                                                           #
# Purpose: Network characteristics                                                           #
#============================================================================================#

# load required packages
library(tidyverse)
library(netmeta)

# read model data
additive_model_input_data_location <- "./analysis/output/almp_nma_additive_model_data.RDS"
additive_model_input_data <- readRDS(additive_model_input_data_location)

network_data_location <- "./data_cleaning/outputs/almp_nma_pooled_analysis_data.RDS"
network_data_raw <- readRDS(network_data_location)

# filter data for studies includes in the model
network_data_clean <- network_data_raw |>
  filter(
    study_id %in% additive_model_input_data$study
  ) |>
  select(
    delta,
    delta_se,
    study_id,
    starts_with("comp_")
  ) |>
  distinct()

# create bogus meta for the plot
bogus_meta <-
  netmeta(
    TE = delta,
    seTE = delta_se,
    treat1 = ,
    treat2 = ,
    data = network_data_clean
  )

# create netgraph
netmeta::netgraph(
  data = network_data_clean
)

# get network details
ntgrp. > data(smokingcessation)

w1 <- pairwise(
  list(treat1, treat2, treat3),
  ntgrp. + event = list(event1, event2, event3),
  n = list(n1, n2, n3),
  ntgrp. + data = smokingcessation,
  sm = "OR"
)
