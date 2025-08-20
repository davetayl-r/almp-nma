#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 05/08/2025                                                                           #         
# Study ID: geckeler2017helpingdropoutyouth                                                  #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
exhibit_v2_raw_location <- "./data_extraction/juicr_outputs/geckeler2017helpingdropoutyouth_exhibit_v2_juicr_extracted_points.csv" 
exhibit_v2_raw <- read_csv(exhibit_v2_raw_location)

# clean up exhibit v2
exhibit_v2_clean <- exhibit_v2_raw |>
  # rename vars
  rename(
    quarter = x.calibrated,
    probability = y.calibrated
  ) |>
  # select useful vars
  select(
    quarter,
    probability,
    group
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    group = case_when(
      group == "orangeGrp" ~ "intervention",
      group == "cherryGrp" ~ "comparison"
    ),
    # round months
    quarter = round(quarter, 0)
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = group,
    values_from = probability
  ) |>
  arrange(
    quarter
  ) |>
  mutate(
    month = quarter * 3
  ) |>
  select(
    month,
    intervention,
    comparison
  )

# export
write_csv(
  exhibit_v2_clean,
  "./data_extraction/outputs/geckeler2017helpingdropoutyouth_exhibit_v2.csv"
)
