#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 22/07/2025                                                                           #         
# Study ID: canzian2020activelabourmarket                                                    #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
figure_3_raw_location <- "./data_extraction/juicr_outputs/canzian2020activelabourmarket_figure_3_juicr_extracted_points.csv" 
figure_3_raw <- read_csv(figure_3_raw_location)

figure_4_raw_location <- "./data_extraction/juicr_outputs/canzian2020activelabourmarket_figure_4_juicr_extracted_points.csv" 
figure_4_raw <- read_csv(figure_4_raw_location)

figure_5_raw_location <- "./data_extraction/juicr_outputs/canzian2020activelabourmarket_figure_5_juicr_extracted_points.csv" 
figure_5_raw <- read_csv(figure_5_raw_location)

figure_6_raw_location <- "./data_extraction/juicr_outputs/canzian2020activelabourmarket_figure_6_juicr_extracted_points.csv" 
figure_6_raw <- read_csv(figure_6_raw_location)

# clean up figure 3
figure_3_clean <- figure_3_raw |>
  # rename vars
  rename(
    month = x.calibrated,
    probability = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    month,
    probability,
    estimate
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    estimate = case_when(
      estimate == "orangeGrp" ~ "te",
      estimate == "cherryGrp" ~ "te_95_high",
      estimate == "kiwiGrp" ~ "te_95_low"
    ),
    month = round(month, 0)
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = probability
  ) |>
  arrange(
    month
  ) |>
  # estimate se from 95% CI
  mutate(
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975))
  ) |>
  # select final vars
  select(
    month,
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
  figure_3_clean,
  "./data_extraction/outputs/canzian2020activelabourmarket_figure_3.csv"
)

# clean up figure 4
figure_4_clean <- figure_4_raw |>
  # rename vars
  rename(
    month = x.calibrated,
    probability = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    month,
    probability,
    estimate
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    estimate = case_when(
      estimate == "orangeGrp" ~ "te",
      estimate == "cherryGrp" ~ "te_95_high",
      estimate == "kiwiGrp" ~ "te_95_low"
    ),
    month = round(month, 0)
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = probability
  ) |>
  arrange(
    month
  ) |>
  # estimate se from 95% CI
  mutate(
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975))
  ) |>
  # select final vars
  select(
    month,
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
  figure_4_clean,
  "./data_extraction/outputs/canzian2020activelabourmarket_figure_4.csv"
)

# clean up figure 5
figure_5_clean <- figure_5_raw |>
  # rename vars
  rename(
    month = x.calibrated,
    probability = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    month,
    probability,
    estimate
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    estimate = case_when(
      estimate == "orangeGrp" ~ "te",
      estimate == "cherryGrp" ~ "te_95_high",
      estimate == "kiwiGrp" ~ "te_95_low"
    ),
    month = round(month, 0)
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = probability
  ) |>
  arrange(
    month
  ) |>
  # estimate se from 95% CI
  mutate(
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975))
  ) |>
  # select final vars
  select(
    month,
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
  figure_5_clean,
  "./data_extraction/outputs/canzian2020activelabourmarket_figure_5.csv"
)

# clean up figure 6
figure_6_clean <- figure_6_raw |>
  # rename vars
  rename(
    month = x.calibrated,
    probability = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    month,
    probability,
    estimate
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    estimate = case_when(
      estimate == "orangeGrp" ~ "te",
      estimate == "cherryGrp" ~ "te_95_high",
      estimate == "kiwiGrp" ~ "te_95_low"
    ),
    month = round(month, 0)
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = probability
  ) |>
  arrange(
    month
  ) |>
  # estimate se from 95% CI
  mutate(
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975))
  ) |>
  # select final vars
  select(
    month,
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
  figure_6_clean,
  "./data_extraction/outputs/canzian2020activelabourmarket_figure_6.csv"
)