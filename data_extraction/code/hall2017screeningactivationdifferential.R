#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 06/08/2025                                                                           #         
# Study ID: hall2017screeningactivationdifferential                                          #        
#============================================================================================#

# this code estimates treatment/control group sizes using the age distribution from Table 1, it assumes ages is distributed normally

# load required packages
library(tidyverse)

# set seed for reproducibility
set.seed(268)

# specify data from Table 1 (Age at spellstart + 90 days)
age_mean <- 25.06
age_sd <- 2.698
total_sample <- 335521

# specify data from Table 3 (Age bandwidths and sample sizes within bandwidths)
models <- tribble(
  ~model, ~outcome, ~bandwidth, ~n_within_bandwidth,
  1, "Threat effect (90 days)", 1.605, 117202,
  2, "Effect within 180 days", 1.970, 133473,
  3, "Effect within 270 days", 1.549, 87848,
  4, "Effect within 365 days", 2.215, 105595
)

# declare function to estimate treatment/control split for each model
estimate_group_sizes <- function(bandwidth, n_within_bandwidth, age_mean, age_sd) {
  
  # age range for this bandwidth (centered on 25)
  age_lower <- 25 - bandwidth
  age_upper <- 25 + bandwidth
  
  # probability of being in the bandwidth
  p_in_bandwidth <- pnorm(age_upper, age_mean, age_sd) - pnorm(age_lower, age_mean, age_sd)
  
  # probability of being both in bandwidth AND below 25
  p_in_bandwidth_and_below_25 <- pnorm(25, age_mean, age_sd) - pnorm(age_lower, age_mean, age_sd)
  
  # conditional probability: P(age < 25 | in bandwidth)
  p_below_25_given_in_bandwidth <- p_in_bandwidth_and_below_25 / p_in_bandwidth
  
  # estimated group sizes
  n_treatment <- round(n_within_bandwidth * p_below_25_given_in_bandwidth)
  n_control <- n_within_bandwidth - n_treatment
  
  return(list(
    bandwidth = bandwidth,
    n_total = n_within_bandwidth,
    n_treatment = n_treatment,
    n_control = n_control,
    prop_treatment = p_below_25_given_in_bandwidth,
    age_range_lower = age_lower,
    age_range_upper = age_upper
  ))
}

# apply to each model
results <- pmap(models, function(model, outcome, bandwidth, n_within_bandwidth) {
  cat(sprintf("MODEL %d: %s\n", model, outcome))
  result <- estimate_group_sizes(bandwidth, n_within_bandwidth, age_mean, age_sd)
  result$model <- model
  result$outcome <- outcome
  return(result)
})

# create summary table
summary_table <- map_dfr(results, function(x) {
  tibble(
    model = x$model,
    outcome = x$outcome,
    bandwidth = x$bandwidth,
    age_range = sprintf("%.2f - %.2f", x$age_range_lower, x$age_range_upper),
    n_total = x$n_total,
    n_treatment = x$n_treatment,
    n_control = x$n_control,
    pct_treatment = round(100 * x$prop_treatment, 1)
  )
})

summary_table