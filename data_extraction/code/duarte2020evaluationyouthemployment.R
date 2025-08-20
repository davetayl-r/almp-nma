#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 19/11/2024                                                                           #         
# Study ID: duarte2020evaluationyouthemployment                                              #        
#============================================================================================#

# this code estimates the average intervention length from the reported intervals using data extracted from Table 1 of the paper

# load required packages
library(tidyverse)

# data from table 1
table_one <- data.frame(
  type = c("Internship 1-6 months", "Internship 7-12 months", "Internship 13-18 months",
           "Hiring support 1-6 months", "Hiring support 7-12 months", "Hiring support 13-18 months",
           "Internship + Hiring Support"),
  n_participants = c(5576, 36468, 2315, 7003, 11277, 3651, 16325),
  col_percent = c(6.7, 44.1, 2.8, 8.5, 13.7, 4.4, 19.8)
)

# estimate duration using midpoint of reported
intervention_length <- table_one |>
  mutate(
    duration_months = case_when(
      type == "Internship 1-6 months" ~ 3.5, # midpoint of 1-6 months
      type == "Internship 7-12 months" ~ 9.5, # midpoint of 7-12 months
      type == "Internship 13-18 months" ~ 15.5, # midpoint of 13-18 months
      type == "Hiring support 1-6 months" ~ 3.5, # midpoint of 1-6 months
      type == "Hiring support 7-12 months" ~ 9.5, # midpoint of 7-12 months
      type == "Hiring support 13-18 months" ~ 15.5, # midpoint of 13-18 months
      type == "Internship + Hiring Support" ~ 24 # use midpoints from above
    ),
    intervention_category = case_when(
      str_detect(type, "^Internship [0-9]") ~ "Internship",
      str_detect(type, "^Hiring support") ~ "Hiring Support", 
      str_detect(type, "Internship \\+ Hiring") ~ "Internship + Hiring Support"
    )
  ) |>
  # calculate weighted average duration by intervention category
  group_by(
    intervention_category
  ) |>
  summarise(
    total_participants = sum(n_participants),
    weighted_avg_duration = sum(duration_months * n_participants) / sum(n_participants),
    .groups = "drop"
  )

intervention_length