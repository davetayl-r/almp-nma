#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 19/11/2024                                                                           #         
# Study ID: alegre2015impacttrainingintensivelabour                                          #        
#============================================================================================#

# load required packages
library(tidyverse)

# read output from juicr
figure_1_raw_location <- "./data_extraction/juicr_outputs/alegre2015impacttrainingintensivelabour_figure_1_juicr_extracted_points.csv" 
figure_1_raw <- read_csv(figure_1_raw_location)

figure_3_raw_location <- "./data_extraction/juicr_outputs/alegre2015impacttrainingintensivelabour_figure_3_juicr_extracted_points.csv" 
figure_3_raw <- read_csv(figure_3_raw_location)

figure_5_raw_location <- "./data_extraction/juicr_outputs/alegre2015impacttrainingintensivelabour_figure_5_juicr_extracted_points.csv" 
figure_5_raw <- read_csv(figure_5_raw_location)

figure_7_raw_location <- "./data_extraction/juicr_outputs/alegre2015impacttrainingintensivelabour_figure_7_juicr_extracted_points.csv" 
figure_7_raw <- read_csv(figure_7_raw_location)

# clean up figure 1
figure_1_clean <- figure_1_raw |>
  # rename vars
  rename(
    quarter = x.calibrated,
    per_cent = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    quarter,
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
    quarter = round(quarter, 0)
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = per_cent
  ) |>
  arrange(
    quarter
  ) |>
  mutate(
    # estimate se from 95% CI
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975)),
    # convert quarters to months
    months = quarter * 3
  ) |>
  # select final vars
  select(
    months,
    te,
    te_se
  ) |>
  # drop 0 timepoint
  filter(
    months != 0
  ) |>
  # force non-scientific notation
  mutate(
    across(
      where(is.numeric), ~ format(., scientific = FALSE))
  )

# export
write_csv(
  figure_1_clean,
  "./data_extraction/outputs/alegre2015impacttrainingintensivelabour_figure_1.csv"
)

# clean up figure 5
figure_5_clean <- figure_5_raw |>
  # rename vars
  rename(
    months = x.calibrated,
    per_cent = y.calibrated,
    estimate = group
  ) |>
  # select useful vars
  select(
    months,
    per_cent,
    estimate
  ) |>
  # convert months to quarters
  mutate(
    quarter = months / 3
  ) |>
  # name groups and round estimates to nearest quarter
  mutate(
    estimate = case_when(
      estimate == "orangeGrp" ~ "te",
      estimate == "cherryGrp" ~ "te_95_high",
      estimate == "kiwiGrp" ~ "te_95_low"
    ),
    quarter = round(quarter, 0)
  ) |>
  select(
    -months
  ) |>
  # convert to wide format
  pivot_wider(
    names_from = estimate,
    values_from = per_cent
  ) |>
  mutate(
    # estimate se from 95% CI
    te_se = (te_95_high - te_95_low) / (2 * qnorm(0.975)),
    # convert quarters to months
    months = quarter * 3
  ) |>
  # select final vars
  select(
    months,
    te,
    te_se
  ) |>
  # drop 0 timepoint
  filter(
    months != 0
  ) |>
  # force non-scientific notation
  mutate(
    across(
      where(is.numeric), ~ format(., scientific = FALSE))
  )

# export
write_csv(
  figure_5_clean,
  "./data_extraction/outputs/alegre2015impacttrainingintensivelabour_figure_5.csv"
)

# clean up figure 3
figure_3_clean <- figure_3_raw |>
  # rename vars
  rename(
    te = y.calibrated
  ) |>
  # rename groups
  mutate(
    group = case_when(
      group == "orangeGrp" ~ "enrolment_ESO",
      group == "cherryGrp" ~ "graduation_ESO",
      group == "kiwiGrp" ~ "enrolment_CGFM",
      group == "grapeGrp" ~ "graduation_CGFM")
  ) |>
  # select useful vars
  select(
    te,
    group
  )

# export
write_csv(
  figure_3_clean,
  "./data_extraction/outputs/alegre2015impacttrainingintensivelabour_figure_3.csv"
)

# clean up figure 7
figure_7_clean <- figure_7_raw |>
  # rename vars
  rename(
    te = y.calibrated
  ) |>
  # rename groups
  mutate(
    group = case_when(
      group == "orangeGrp" ~ "enrolment_ESO",
      group == "cherryGrp" ~ "graduation_ESO",
      group == "kiwiGrp" ~ "enrolment_CGFM",
      group == "grapeGrp" ~ "graduation_CGFM")
  ) |>
  # select useful vars
  select(
    te,
    group
  )

# export
write_csv(
  figure_7_clean,
  "./data_extraction/outputs/alegre2015impacttrainingintensivelabour_figure_7.csv"
)
