#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #  
# Date: 14/08/2025                                                                           #         
# Study ID: ghirelli2019doesonthejobtraining                                                 #        
#============================================================================================#

# this code aims to estimate the median follow-up time from the start of the intervention to the measurement date in December 2015

# load required packages
library(tidyverse)
library(lubridate)

# set seed for reproducibility
set.seed(2204)

# assumptions:
# - all participants commence between May 2013 and March 2014 (6 months from end) 
# - intervention lasts 6 months 
# - outcomes are measured in December 2015

# fefine enrolment window — earliest and latest possible start dates
start_date_min <- ymd("2013-05-01")
start_date_max <- ymd("2014-03-31")

# set the measurement date for mid-December
measurement_date <- ymd("2015-12-15")

# use weakly-informative priors on the enrolment distribution shape: alpha and beta parameters are drawn from Gamma(2,1)
sample <- 20000
alpha <- rgamma(sample, shape = 2, rate = 1)
beta  <- rgamma(sample, shape = 2, rate = 1)

# calculate length of enrolment window (number of days between first and last start)
span_possible_start_days <- as.integer(
  difftime(
    start_date_max, 
    start_date_min, 
    units = "days")
  )

# for each (alpha, beta), compute the median enrolment date i.e., the 50th percentile of the assumed enrolment distribution, expressed as a fraction of the window (0 = earliest start, 1 = latest start)
start_median_fraction <- qbeta(
  0.5, 
  alpha, 
  beta)

# scale median fraction to an actual date within the enrolment window
start_med_date <- start_date_min + days(
  round(
    start_median_fraction * span_possible_start_days)
  )

# estimate follow-up time from baseline in months
months_from_baseline <- time_length(
  interval(
    start_med_date, 
    measurement_date), 
  unit = "month")

# summarise posterior distribution of follow-up time
median_estimate <- quantile(months_from_baseline, 0.5)
lower_estimate <- quantile(months_from_baseline, 0.025)
upper_estimate <- quantile(months_from_baseline, 0.975)

