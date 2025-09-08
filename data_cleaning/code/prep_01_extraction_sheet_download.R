#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 05/09/2025                                                                           #
# Purpose: download and process data extraction template from Google Sheets                  #
#============================================================================================#

# load required packages
library(tidyverse)
library(googlesheets4)

# download the data from the google drive
data_extraction_google_sheet <- "https://docs.google.com/spreadsheets/d/1Z6sACsWkbwKFrdbBC8dF5RPvKwyv0msnSYRi5zghpgU/edit?gid=0#gid=0"

#-------------------------------------------------------------------------------
# 1. Intervention components
#-------------------------------------------------------------------------------

# read, subset and clean component data
intervention_components <- read_sheet(
  data_extraction_google_sheet,
  sheet = "component_details",
  skip = 2
) |>
  # remove instructions
  slice(-1) |>
  # select relevant columns
  select(
    -other_detail
  ) |>
  # convert yes and no to binary indicators
  mutate(across(
    starts_with(c("int_", "com_")),
    ~ case_when(
      . == "Yes" ~ 1,
      . == "No" ~ 0
    )
  ))

# export data
saveRDS(
  intervention_components,
  file = "./data_cleaning/outputs/almp_nma_intervention_components.rds"
)

#-------------------------------------------------------------------------------
# 2. Study-level details
#-------------------------------------------------------------------------------

# read, subset and clean data
study_level_details <- read_sheet(
  data_extraction_google_sheet,
  sheet = "study_details"
) |>
  # remove instructions
  slice(-1) |>
  # select relevant columns
  select(
    study_id,
    year_published,
    source,
    peer_reviewed,
    study_design_type,
    study_design_detail,
    study_population_treatment,
    study_population_comparison,
    study_age_min,
    study_age_max,
    study_age_mean,
    study_age_median,
    #low_income,
    #low_income_prop,
    #mental_or_physical_disability,
    disability_proportion,
    #elevated_risks,
    ever_employed_proportion,
    high_school_complete_proportion,
    #limited_literacy_numeracy_proportion,
    care_experienced_proportion,
    pregnant_or_child_proportion,
    homeless_proportion,
    ever_arrested_proportion,
    incarcerated_proportion,
    #receives_public_assistance_proportion,
    location,
    year_start,
    year_end,
    intervention_length_n,
    #intervention_length_desc,
    intervention_intensity_n,
    #intervention_intensity_description,
    youth_focused,
    intervention_developer,
    intervention_implementor,
    program_scope,
    intervention_funding,
    conditionality,
    incentives,
    certification,
    study_funding,
    evaluator
  ) |>
  # rename columns
  rename(
    proportion_female_treatment = study_population_treatment,
    proportion_female_comparison = study_population_comparison
  ) |>
  # format data
  mutate(
    year_published = as.numeric(year_published)
  )

# export data
saveRDS(
  study_level_details,
  file = "./data_cleaning/outputs/almp_nma_study_details.rds"
)

#-------------------------------------------------------------------------------
# 3. Outcome data
#-------------------------------------------------------------------------------

# read, subset and clean data
outcome_data <- read_sheet(
  data_extraction_google_sheet,
  sheet = "outcome_data"
) |>
  slice(
    -1
  ) |>
  # rename vars to intuitive names
  rename(
    outcome = outcome_construct,
    treatment_n = grp1n,
    comparison_n = grp2n,
    treatment_proportion = prop1event,
    comparison_proportion = prop2event,
    treatment_mean = grp1m,
    treatment_sd = grp1sd,
    treatment_se = grp1se,
    comparison_mean = grp2m,
    comparison_sd = grp2sd,
    comparison_se = grp2se,
    odds_ratio = or,
    treatment_effect = te,
    treatment_effect_se = te_se,
    treatment_effect_ci_low = te_ci_low,
    treatment_effect_ci_high = te_ci_high,
    treatment_effect_p_value = te_p_value
  ) |>
  # drop redundant vars
  select(
    -baseline_rate,
    -missing_data,
    -missing_data_can_be_derived,
    -required_information,
    -pre1mean,
    -pre1sd,
    -post1mean,
    -post1sd,
    -pre2mean,
    -pre2sd,
    -post2mean,
    -post2sd,
    -gain1mean,
    -gain1se,
    -gain2mean,
    -gain2se,
    -f,
    -b,
    -beta,
    -sdy,
    -diff,
    -diff_lower,
    -diff_upper,
    -diff_se
  )

# export data
saveRDS(
  outcome_data,
  file = "./es_transformation/inputs/almp_nma_outcome_data.rds"
)

#-------------------------------------------------------------------------------
# 4. Quality assessment for randomised studies
#-------------------------------------------------------------------------------

# read, subset and clean data
qa_randomised <- read_sheet(
  data_extraction_google_sheet,
  sheet = "rct_quality_assessment",
  skip = 4
) |>
  slice(-1) |>
  select(
    study_id,
    question_1,
    question_2,
    question_3,
    question_4,
    question_5,
    question_6,
    question_7,
    question_8,
    question_9,
    question_10,
    question_11,
    question_12,
    question_13
  )

# export data
saveRDS(
  qa_randomised,
  file = "./data_cleaning/outputs/almp_nma_qa_randomised.rds"
)

#-------------------------------------------------------------------------------
# 5. Quality assessment for non-randomised studies
#-------------------------------------------------------------------------------

# read, subset and clean data
qa_non_randomised <- read_sheet(
  data_extraction_google_sheet,
  sheet = "qed_quality_assessment",
  skip = 4
) |>
  slice(-1) |>
  select(
    study_id,
    question_1,
    question_2,
    question_3,
    question_4,
    question_5,
    question_6,
    question_7,
    question_8,
    question_9
  )

# export data
saveRDS(
  qa_non_randomised,
  file = "./data_cleaning/outputs/almp_nma_qa_non_randomised.rds"
)
