#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 19/11/2024                                                                           #         
# Study ID: kopecna2016counterfactualimpactevaluation                                        #        
#============================================================================================#

# this code calculates the weighted average, variance and standard deviation of the treatment, comparison and study population using data extracted from figure 4 of the paper

# load required packages
library(tidyverse)

# study details
intervention_group_sample_size <- 772
comparison_group_sample_size <- 731

# extracted from the chart
intervention_group_age_distribution <- c(
  `19` = 0.01, 
  `20` = 0.08, 
  `21` = 0.10, 
  `22` = 0.20, 
  `23` = 0.11, 
  `24` = 0.14, 
  `25` = 0.23, 
  `26` = 0.17, 
  `27` = 0.10, 
  `28` = 0.03, 
  `29` = 0.008, 
  `30` = 0.002)

comparison_group_age_distribution <- c(
  `19` = 0.01,
  `20` = 0.04, 
  `21` = 0.07, 
  `22` = 0.04, 
  `23` = 0.11, 
  `24` = 0.16, 
  `25` = 0.25, 
  `26` = 0.17, 
  `27` = 0.11, 
  `28` = 0.03, 
  `29` = 0.006, 
  `30` = 0.002)

# convert to counts
intervention_group_counts <- intervention_group_age_distribution * intervention_group_sample_size
comparison_group_counts <- comparison_group_age_distribution * comparison_group_sample_size

# combine into data frame
age_distribution <- tibble(
  age = as.numeric(names(intervention_group_age_distribution)),
  intervention_group = intervention_group_counts,
  comparison_group = comparison_group_counts,
  study_population = intervention_group_counts + comparison_group_counts
)

# calculate weighted means and variance for intervention_group, comparison_group, and study population
results <- age_distribution |>
  mutate(
    intervention_group_weighted_age = age * intervention_group,
    comparison_group_weighted_age = age * comparison_group,
    study_population_weighted_age = age * study_population 
  ) |>
  summarise(
    # weighted averages
    intervention_group_average_age = sum(intervention_group_weighted_age) / sum(intervention_group),
    comparison_group_average_age = sum(comparison_group_weighted_age) / sum(comparison_group),
    study_population_average_age = sum(study_population_weighted_age) / sum(study_population),
    # weighted variances
    intervention_group_variance = sum(intervention_group * (age - intervention_group_average_age)^2) / sum(intervention_group),
    comparison_group_variance = sum(comparison_group * (age - comparison_group_average_age)^2) / sum(comparison_group),
    study_population_variance = sum(study_population * (age - study_population_average_age)^2) / sum(study_population),
    # weighted standard deviations
    intervention_group_sd_age = sqrt(intervention_group_variance),
    comparison_group_sd_age = sqrt(comparison_group_variance),
    study_population_sd_age = sqrt(study_population_variance)
  )

# print results
results
