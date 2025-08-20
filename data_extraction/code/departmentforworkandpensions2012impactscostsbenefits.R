#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 14/08/2025                                                                           #         
# Study ID: departmentforworkandpensions2012impactscostsbenefits                             #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
figure_4_7_raw_location <- "./data_extraction/juicr_outputs/departmentforworkandpensions2012impactscostsbenefits_figure_4_7_juicr_extracted_points.csv" 
figure_4_7_raw <- read_csv(figure_4_7_raw_location)

# clean up exhibit v2
figure_4_7_clean <- figure_4_7_raw |>
  # rename vars
  rename(
    week = x.calibrated,
    percent = y.calibrated
  ) |>
  mutate(
    # convert weeks to months
    month = week / 4.3,
    #month = week / 4,  
    # round months
    month = round(month, 0),
    # convert percent to proportion
    proportion = percent / 100,
  ) |>
  # select useful vars
  select(
    month,
    proportion,
    group
  ) |>
  mutate(
    # name groups
    group = case_when(
      group == "orangeGrp" ~ "intervention",
      group == "cherryGrp" ~ "comparison"
    )
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = group,
    values_from = proportion
  ) |>
  arrange(
    month
  ) 

# export
write_csv(
  figure_4_7_clean,
  "./data_extraction/outputs/departmentforworkandpensions2012impactscostsbenefits_figure_4_7.csv"
)