#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 12/09/2025                                                                           #
# Purpose: combine studies that stratify results by demographic subgroups                    #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read combined data
almp_nma_combined_data_raw_location <- "./data_cleaning/outputs/almp_nma_combined_study_data.RDS"
almp_nma_combined_data_raw <- readRDS(almp_nma_combined_data_raw_location)

#-------------------------------------------------------------------------------
# 1. Filter studies to be pooled
#-------------------------------------------------------------------------------

# subset combined data
almp_nma_combined_data_subset <- almp_nma_combined_data_raw |>
  # exclude study ids that will subsequently be pooled
  filter(
    str_detect(study_id, "stefanik2024supportingrightworkplace"),
    str_detect(study_id, "bloom1993nationaljtpastudy")
  )

#-------------------------------------------------------------------------------
# 2. Pool studies across subgroups
#-------------------------------------------------------------------------------

# pool studies of 'JPTA (classroom training) from bloom1993nationaljtpastudy
bloom1993nationaljtpastudy_a <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "bloom1993nationaljtpastudy_a",
    "bloom1993nationaljtpastudy_d"
  ),
  output_study_id = "bloom1993nationaljtpastudy_a"
)

# pool studies of 'JPTA (OJT/JSA) from bloom1993nationaljtpastudy
bloom1993nationaljtpastudy_b <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "bloom1993nationaljtpastudy_b",
    "bloom1993nationaljtpastudy_e"
  ),
  output_study_id = "bloom1993nationaljtpastudy_b"
)

# pool studies of 'JPTA (Other services) from bloom1993nationaljtpastudy
bloom1993nationaljtpastudy_c <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "bloom1993nationaljtpastudy",
    "bloom1993nationaljtpastudy"
  ),
  output_study_id = "bloom1993nationaljtpastudy_c"
)

# pool studies of 'graduate practice' from stefanik2024supportingrightworkplace
stefanik2024supportingrightworkplace_a <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "stefanik2024supportingrightworkplace_a",
    "stefanik2024supportingrightworkplace_b",
    "stefanik2024supportingrightworkplace_c",
    "stefanik2024supportingrightworkplace_d",
    "stefanik2024supportingrightworkplace_e",
    "stefanik2024supportingrightworkplace_f"
  ),
  output_study_id = "stefanik2024supportingrightworkplace_a"
)

# pool studies of 'activation works' from stefanik2024supportingrightworkplace
stefanik2024supportingrightworkplace_b <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "stefanik2024supportingrightworkplace_g",
    "stefanik2024supportingrightworkplace_h",
    "stefanik2024supportingrightworkplace_i",
    "stefanik2024supportingrightworkplace_j",
    "stefanik2024supportingrightworkplace_k",
    "stefanik2024supportingrightworkplace_l"
  ),
  output_study_id = "stefanik2024supportingrightworkplace_b"
)

# pool studies of 'voluntary activation works' from stefanik2024supportingrightworkplace
stefanik2024supportingrightworkplace_c <- pool_studies(
  dat = almp_nma_combined_data_raw,
  study_ids = c(
    "stefanik2024supportingrightworkplace_m",
    "stefanik2024supportingrightworkplace_n",
    "stefanik2024supportingrightworkplace_o",
    "stefanik2024supportingrightworkplace_p",
    "stefanik2024supportingrightworkplace_q",
    "stefanik2024supportingrightworkplace_e"
  ),
  output_study_id = "stefanik2024supportingrightworkplace_c"
)

#-------------------------------------------------------------------------------
# 3. Combine studies again and export
#-------------------------------------------------------------------------------

# combine pooled studies with study data
almp_nma_combined_data_clean <- bind_rows(
  almp_nma_combined_data_subset,
  bloom1993nationaljtpastudy_a,
  bloom1993nationaljtpastudy_b,
  bloom1993nationaljtpastudy_c,
  stefanik2024supportingrightworkplace_a,
  stefanik2024supportingrightworkplace_b,
  stefanik2024supportingrightworkplace_c
)

# export
saveRDS(
  almp_nma_combined_data_clean,
  file = "./data_cleaning/outputs/almp_nma_analysis_data.rds"
)
