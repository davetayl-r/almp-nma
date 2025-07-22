#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 21/07/2025                                                                           #         
# Study ID: rosholm2019bridginggapwelfare                                          #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
figure_4_raw_location <- "./data_extraction/juicr_outputs/rosholm2019bridginggapwelfare_figure_4_juicr_extracted_points.csv" 
figure_4_raw <- read_csv(figure_4_raw_location)

figure_8_raw_location <- "./data_extraction/juicr_outputs/rosholm2019bridginggapwelfare_figure_8_juicr_extracted_points.csv" 
figure_8_raw <- read_csv(figure_8_raw_location)

# clean up figure 4
figure_4_clean <- figure_4_raw |>
  # rename vars
  rename(
    te = y.calibrated
  ) |>
  # rename groups
  mutate(
    outcome = case_when(
      group == "orangeGrp" ~ "High school",
      group == "cherryGrp" ~ "High school",
      group == "kiwiGrp" ~ "Vocational education (main track)",
      group == "grapeGrp" ~ "Vocational education (main track)",
      group == "berryGrp" ~ "Further education",
      group == "plumGrp" ~ "Further education"),
    exposure = case_when(
      group == "orangeGrp" ~ "treatment",
      group == "cherryGrp" ~ "comparison",
      group == "kiwiGrp" ~ "treatment",
      group == "grapeGrp" ~ "comparison",
      group == "berryGrp" ~ "treatment",
      group == "plumGrp" ~ "comparison")
  ) |>
  # select useful vars
  select(
    te,
    outcome,
    exposure
  ) |>
  # drop na values
  na.omit()

# export
write_csv(
  figure_4_clean,
  "./data_extraction/outputs/rosholm2019bridginggapwelfare_figure_4.csv"
)

# clean up figure 8
figure_8_clean <- figure_8_raw |>
  # rename vars
  rename(
    week = x.calibrated,
    per_cent = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    week,
    per_cent,
    estimate
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    estimate = case_when(
      estimate == "orangeGrp" ~ "te",
      estimate == "cherryGrp" ~ "te_95_high",
      estimate == "kiwiGrp" ~ "te_95_low"
    ),
    week = round(week/5, 0) * 5
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = per_cent
  ) |>
  arrange(
    week
  ) |>
  mutate(
    # estimate se from 95% CI
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975)),
    # convert quarters to months
    months = week/4
  ) |>
  # select final vars
  select(
    months,
    te,
    te_se
  ) |>
  # force non-scientific notation
  mutate(
    across(
      where(is.numeric), ~ format(., scientific = FALSE))
  )

# export
write_csv(
  figure_8_clean,
  "./data_extraction/outputs/rosholm2019bridginggapwelfare_figure_8.csv"
)

