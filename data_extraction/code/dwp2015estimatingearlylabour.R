#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 20/08/2025                                                                           #         
# Study ID: dwp2015estimatingearlylabour                                                     #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
figure_14_raw_location <- "./data_extraction/juicr_outputs/dwp2015estimatingearlylabour_figure_14_juicr_extracted_points.csv" 
figure_14_raw <- read_csv(figure_14_raw_location)

# clean up exhibit v2
figure_14_clean <- figure_14_raw |>
  # rename vars
  rename(
    days = x.calibrated,
    proportion = y.calibrated
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    group = case_when(
      group == "orangeGrp" ~ "intervention",
      group == "cherryGrp" ~ "comparison"
    ),
    # round months
    months = round(days/30, 0)
  ) |>
  # select useful vars
  select(
    months,
    proportion,
    group
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = group,
    values_from = proportion
  ) |>
  arrange(
    months
  ) 

# export
write_csv(
  figure_14_clean,
  "./data_extraction/outputs/dwp2015estimatingearlylabour_figure_14.csv"
)
