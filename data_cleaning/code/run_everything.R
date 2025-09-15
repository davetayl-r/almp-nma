#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: run all steps to create analysis-ready dataset                                    #
#============================================================================================#

# this file lays out each of steps in data cleaning in order

#-------------------------------------------------------------------------------
# 1. Download and save data from google sheet
#-------------------------------------------------------------------------------

source(
  "./data_cleaning/code/prep_01_extraction_sheet_download.R"
)

#-------------------------------------------------------------------------------
# 2. Transform reported results into effect sizes
#-------------------------------------------------------------------------------

source(
  "./data_cleaning/code/prep_02_merge_transformed_es_data.R"
)

#-------------------------------------------------------------------------------
# 3. Prepare study quality data
#-------------------------------------------------------------------------------

source(
  "./data_cleaning/code/prep_03_prep_study_quality_data.R"
)

#-------------------------------------------------------------------------------
# 4. Merge outcome and study-level data
#-------------------------------------------------------------------------------

source(
  "./data_cleaning/code/prep_04_merge_outcome_study_level_data.R"
)

#-------------------------------------------------------------------------------
# 5. Pool results from reporting results across subgroups
#-------------------------------------------------------------------------------

source(
  "./es_transformation/code/es_transformation_pool_demographic_subgroups.R"
)

#-------------------------------------------------------------------------------
# 6. Final analysis data preparation
#-------------------------------------------------------------------------------

source(
  "./data_cleaning/code/prep_05_analysis_data.R"
)
