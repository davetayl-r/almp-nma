#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 20/08/2025                                                                           #         
# Study ID: boockmann2016mentoringdisadvantagedyouths                                        #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
figure_3A_raw_location <- "./data_extraction/juicr_outputs/boockmann2016mentoringdisadvantagedyouths_figure_3A_juicr_extracted_points.csv" 
figure_3A_raw <- read_csv(figure_3A_raw_location)

# clean up exhibit v2
figure_3A_clean <- figure_3A_raw |>
  # rename vars
  rename(
    month = x.calibrated,
    proportion = y.calibrated
  ) |>
  mutate(
    # round months
    month = round(month, 0),
    # add 24 months to each month observation to account for time from baseline
    month = month + 24
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
      group == "orangeGrp" ~ "comparison",
      group == "cherryGrp" ~ "intervention"
    )
  ) |>
  # manually edit a month
  mutate(
    month = case_when(
      month == 46 ~ 45, # change 26 to 25
      TRUE ~ month
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
  figure_3A_clean,
  "./data_extraction/outputs/boockmann2016mentoringdisadvantagedyouths_figure_3A.csv"
)