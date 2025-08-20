#============================================================================================#
# Project: ALMP NMA                                                                          #
# Aim: Convert reported result into standardised effect size                                 #
# Author: David Taylor                                                                       #  
# Date: 19/08/2025                                                                           #         
# Study ID: yin2023vocationalrehabilitationservices                                          #        
#============================================================================================#

# load packages
library(tidyverse)

# ensure custom functions are available
source("./es_transformation/es_functions.R")

# load data
almp_pre_transformation_data_location <- "./es_transformation/inputs/almp_pre_transformation_data.RDS"
almp_pre_transformation_data <- readRDS(almp_pre_transformation_data_location)

# filter data
yin2023vocationalrehabilitationservices_data <- almp_pre_transformation_data |>
  filter(study_id == "yin2023vocationalrehabilitationservices") 

# Inputs from Yin et al.
late_p  <- 0.154          # IV effect on quarterly employment (post-closure)
late_ci <- c(0.023, 0.285)# AR 95% CI for that effect
pi_c    <- 0.36           # share on the margin (compliers)
pbar    <- 0.435          # full-sample mean employment (post-closure window)

# ITT and Cohen's d
itt_p   <- late_p * pi_c
denom   <- sqrt(pbar * (1 - pbar))
d_hat   <- itt_p / denom

# SEs via delta method, approximating SE from the AR CI
se_late <- (diff(late_ci) / 2) / 1.96
se_itt  <- pi_c * se_late
se_d    <- se_itt / denom
ci_d    <- d_hat + c(-1,1) * 1.96 * se_d

list(ITT_pp = itt_p, d = d_hat, d_CI = ci_d)

# export data
saveRDS(
  "yin2023vocationalrehabilitationservices.RDS"
)