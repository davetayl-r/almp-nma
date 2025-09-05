#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 28/08/2025                                                                           #         
# Study ID: dwp2025sectorbasedworkacademy                                                    #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
chart_3_3_raw_location <- "./data_extraction/juicr_outputs/dwp2025sectorbasedworkacademy_chart_3_3_juicr_extracted_points.csv" 
chart_3_3_raw <- read_csv(chart_3_3_raw_location)

# clean up chart 3.3
chart_3_3_clean <- chart_3_3_raw |>
  # rename vars
  rename(
     proportion = x.calibrated,
     age_group = y.calibrated
  ) |>
  mutate(
    # round age group
    age_group = round(age_group, 1)
  ) |>
  # select useful vars
  select(
    age_group,
    proportion,
    group
  ) |>
  mutate(
    # name groups
    group = case_when(
      group == "orangeGrp" ~ "intervention",
      group == "cherryGrp" ~ "comparison"),
    # assign study id
    study_id = case_when(
      age_group == 1 ~ "dwp2025sectorbasedworkacademy_a",
      age_group == 2 ~ "dwp2025sectorbasedworkacademy_b"),
    # assign age groups
    age_group = case_when(
      age_group == 1 ~ "20_24",
      age_group == 2 ~ "25_29")
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = group,
    values_from = proportion
  ) |>
  # reorder vars for export
  select(
    study_id,
    age_group,
    intervention,
    comparison
  )

# export
write_csv(
  chart_3_3_clean,
  "./data_extraction/outputs/dwp2025sectorbasedworkacademy_chart_3_3.csv"
)