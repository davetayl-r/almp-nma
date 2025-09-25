#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 18/09/2025                                                                           #
# Purpose: NMA model #5 — introduce correlated study random effects                          #
#============================================================================================#

# load required packages
library(tidyverse)
library(stringr)
library(cmdstanr)
library(brms)
library(tidybayes)
library(posterior)

# load custom functions
source("./analysis/code/analysis_functions.R")

# load data
almp_nma_additive_model_data_location <- "./analysis/inputs/almp_nma_additive_model_data.RDS"
almp_nma_additive_model_data <- readRDS(almp_nma_additive_model_data_location)

#-------------------------------------------------------------------------------
# 1. Subset data for model
#-------------------------------------------------------------------------------

almp_nma_model_five_data <- almp_nma_additive_model_data |>
  # select closest data point to 24 month window
  filter(
    selected_primary_timepoint == 1
  ) |>
  filter(
    timepoint_outside_anchor_window != 1.00 |
      is.na(timepoint_outside_anchor_window)
  ) |>
  # drop the rarest outcomes
  filter(
    !outcome %in%
      c(
        # these outcomes appear once
        "Apprenticeship Duration",
        "Bachelors or equivalent (ISCED 6) application",
        "Current Unemployment",
        "Currently Employed Temporarily",
        "Currently Employment Permanently",
        "EET since baseline",
        "Entires to Not in the Labour Force",
        "Foundation skills (ISCED 1) completion",
        "Lower secondary school (ISCED 2) completion",
        "Not in the Labour Force since baseline",
        "Recent EET",
        "Re-employment probability",
        "Quarters employed",
        "Recent Unemployment",
        "Unemployed since baseline",
        # these outcomes appear twice
        "Masters or equivalent (ISCED 7) completion",
        "Period Not in the Labour Force"
      )
  ) |>
  # create flags for study design type
  mutate(
    study_design_soo = case_when(
      study_design_type == "Selection on observables" ~ 1,
      TRUE ~ 0
    ),
    study_design_soo = as.numeric(study_design_soo),
    study_design_dbi = case_when(
      study_design_type == "Design-based identification" ~ 1,
      TRUE ~ 0
    ),
    study_design_dbi = as.numeric(study_design_dbi)
  )

#-------------------------------------------------------------------------------
# 2. Specify model formula
#-------------------------------------------------------------------------------

almp_nma_model_five_formula <- bf(
  delta | se(delta_se) ~
    # remove the intercept so each outcome:component coefficient is estimable relative to SAU
    0 +
      # component × outcome effects: let each component have a different effect per outcome
      outcome:comp_basic_skills_training +
      outcome:comp_soft_skills_training +
      outcome:comp_behavioural_skills_training +
      outcome:comp_self_employment_support +
      outcome:comp_job_specific_technical_skills_off_job_training +
      outcome:comp_job_search_preparation +
      outcome:comp_job_search_assistance +
      outcome:comp_employment_counselling +
      outcome:comp_employment_coaching +
      outcome:comp_financial_assistance +
      outcome:comp_job_specific_technical_skills_on_job_training +
      outcome:comp_paid_temporary_work_experience +
      outcome:comp_unpaid_temporary_work_experience +
      outcome:comp_wage_subsidies +
      outcome:comp_public_works +
      outcome:comp_other_active_component_nec +
      # additive study-design adjustments using RCT as a baseline
      study_design_soo +
      study_design_dbi +
      # study quality penalty with design specific interaction
      low_study_quality +
      low_study_quality:study_design_soo +
      low_study_quality:study_design_dbi +
      # random effects varying by outcome
      (0 + outcome | p | study)
)

#-------------------------------------------------------------------------------
# 3. Specify priors
#-------------------------------------------------------------------------------

almp_nma_model_five_priors <- c(
  # Default prior for all fixed effects (components + design): moderately sceptical Normal(0, 0.4) on the delta scale
  prior(normal(0, 0.4), class = "b"),
  # Study-level heterogeneity: weakly informative Normal(0, 0.25) (half-Normal implied)
  prior(normal(0, 0.25), class = "sd", group = "study"),
  # lkj prior for correlated outcomes within studies
  prior(lkj(2), class = "cor", group = "study"),
  # Tighter priors for the two design adjustments: tighter, sceptical Normals centred at 0 (e.g., SD = 0.15) suggested that differences may exist, but shouldn't dominate
  prior(normal(0, 0.15), class = "b", coef = "study_design_soo"),
  prior(normal(0, 0.15), class = "b", coef = "study_design_dbi"),
  # Tight prior for low study quality
  prior(normal(0, 0.12), class = "b", coef = "low_study_quality"),
  # Even tighter prior for design interactions
  prior(
    normal(0, 0.10),
    class = "b",
    coef = "study_design_soo:low_study_quality"
  ),
  prior(
    normal(0, 0.10),
    class = "b",
    coef = "study_design_dbi:low_study_quality"
  )
)

#-------------------------------------------------------------------------------
# 3. Fit the Bayesian additive CNMA model
#-------------------------------------------------------------------------------

almp_nma_model_five <- brm(
  formula = almp_nma_model_five_formula,
  data = almp_nma_model_five_data,
  prior = almp_nma_model_five_priors,
  family = gaussian(),
  chains = 2,
  iter = 4000,
  warmup = 2000,
  cores = 8,
  threads = threading(2),
  backend = "cmdstanr",
  init = "random",
  control = list(adapt_delta = 0.95, max_treedepth = 13),
  refresh = 250,
  seed = 12345
)

#-------------------------------------------------------------------------------
# 4. Inspect the results
#-------------------------------------------------------------------------------

# inspect the results
summary(almp_nma_model_five)

# inspect diagnostic plots
#plot(almp_nma_model_five)

pp_check(almp_nma_model_five)

#-------------------------------------------------------------------------------
# 5. Clean up model output
#-------------------------------------------------------------------------------

# extract study-level heterogeneity
almp_nma_model_five_tau_draws <- almp_nma_model_five |>
  gather_draws(
    `sd_.*`,
    regex = TRUE
  ) |>
  mutate(
    outcome = str_remove(.variable, "^sd_study__outcome")
  ) |>
  select(
    .draw,
    outcome,
    tau = .value
  )

# Extract component effects (the b_ parameters)
almp_nma_model_five_component_draws <- almp_nma_model_five |>
  gather_draws(
    `b_.*:comp_.*`,
    regex = TRUE
  ) |>
  # Parse the parameter names to extract outcome and component
  mutate(
    # Extract outcome (everything before the colon)
    outcome = str_extract(
      .variable,
      "^[^:]+"
    ),
    outcome = str_remove(
      outcome,
      "^b_outcome"
    ),
    # Extract component (everything after comp_)
    component = str_extract(
      .variable,
      "(?<=:).*"
    ),
    component = str_remove(
      component,
      "^comp_"
    ),
    component = recode(
      component,
      "basic_skills_training" = "Basic Skills Training",
      "behavioural_skills_training" = "Behavioral Skills Training",
      "employment_coaching" = "Employment Coaching",
      "employment_counselling" = "Employment Counseling",
      "financial_assistance" = "Financial Assistance",
      "job_search_assistance" = "Job Search Assistance",
      "job_search_preparation" = "Job Search Preparation",
      "job_specific_technical_skills_off_job_training" = "Technical Skills Training (Off-the-Job)",
      "job_specific_technical_skills_on_job_training" = "Technical Skills Training (On-the-Job)",
      "other_active_component_nec" = "Other Active Components",
      "paid_temporary_work_experience" = "Paid Work Experience",
      "public_works" = "Public Works",
      "self_employment_support" = "Self-Employment Support",
      "soft_skills_training" = "Soft Skills Training",
      "unpaid_temporary_work_experience" = "Unpaid Work Experience",
      "wage_subsidies" = "Wage Subsidies"
    ),
    component = factor(
      component,
      levels = c(
        "Basic Skills Training",
        "Soft Skills Training",
        "Behavioral Skills Training",
        "Technical Skills Training (Off-the-Job)",
        "Self-Employment Support",
        "Job Search Assistance",
        "Job Search Preparation",
        "Employment Coaching",
        "Employment Counseling",
        "Financial Assistance",
        "Technical Skills Training (On-the-Job)",
        "Paid Work Experience",
        "Unpaid Work Experience",
        "Wage Subsidies",
        "Public Works",
        "Other Active Components"
      ),
      ordered = TRUE
    ),
    outcome = recode(
      outcome,
      "Apprenticeshipparticipation" = "Apprenticeship Participation",
      "BachelorsorequivalentISCED6completion" = "Bachelors Degree (ISCED 6) Completion",
      "BachelorsorequivalentISCED6participation" = "Bachelors Degree (ISCED 6) Participation",
      "CurrentlyEmployed" = "Currently Employed",
      "CurrentlyNEET" = "Currently NEET",
      "CurrentlyNotintheLabourForce" = "Currently Not in the Labour Force",
      "CurrentlySelfMEmployed" = "Currently Self-Employed",
      "CurrentlyUnemployed" = "Currently Unemployed",
      "Employedsincebaseline" = "Employed Since Baseline",
      "Employmentcompensation" = "Employment Compensation",
      "ExitsfromUnemployment" = "Exits from Unemployment",
      "HoursWorked" = "Hours Worked",
      "LabourEarnings" = "Labour Earnings",
      "Occupationallicenceobtained" = "Occupational Licence Obtained",
      "PeriodEmployed" = "Period Employed",
      "PeriodUnemployed" = "Period Unemployed",
      "PostMsecondarynonMtertiaryISCED4completion" = "Post-Secondary Non-Tertiary (ISCED 4) Completion",
      "PostMsecondarynonMtertiaryISCED4participation" = "Post-Secondary Non-Tertiary (ISCED 4) Participation",
      "RecentEmployment" = "Recent Employment",
      "SecondaryschoolorequivalentISCED3completion" = "Secondary School (ISCED 3) Completion",
      "SecondaryschoolorequivalentISCED3participation" = "Secondary School (ISCED 3) Participation",
      "ShortMcycletertiaryISCED5completion" = "Short-Cycle Tertiary (ISCED 5) Completion",
      "ShortMcycletertiaryISCED5participation" = "Short-Cycle Tertiary (ISCED 5) Participation",
      "Totalindividualincome" = "Total Individual Income",
      "Wages" = "Wages"
    ),
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Bachelors Degree (ISCED 6) Participation",
        "Currently Employed",
        "Currently NEET",
        "Currently Not in the Labour Force",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Employed Since Baseline",
        "Employment Compensation",
        "Exits from Unemployment",
        "Hours Worked",
        "Labour Earnings",
        "Occupational Licence Obtained",
        "Period Employed",
        "Period Unemployed",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Recent Employment",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Total Individual Income",
        "Wages"
      ),
      ordered = TRUE
    )
  ) |>
  ungroup() |>
  select(
    .draw,
    outcome,
    component,
    effect = .value
  ) |>
  mutate(
    outcome_domain = case_when(
      outcome %in%
        c(
          "Currently Employed",
          "Currently Unemployed",
          "Currently NEET",
          "Currently Not in the Labour Force",
          "Currently Self-Employed",
          "Recent Employment",
          "Employed Since Baseline"
        ) ~
        "Labour Force Status",
      outcome %in% c("Labour Earnings", "Employment Compensation", "Wages") ~
        "Employment Compensation",
      outcome == "Total Individual Income" ~ "Total Income",
      outcome %in% c("Period Employed", "Period Unemployed") ~
        "Employment Duration",
      outcome == "Hours Worked" ~ "Hours Worked",
      outcome %in%
        c(
          "Apprenticeship Participation",
          "Bachelors Degree (ISCED 6) Participation",
          "Bachelors Degree (ISCED 6) Completion",
          "Secondary School (ISCED 3) Completion",
          "Secondary School (ISCED 3) Participation",
          "Occupational Licence Obtained",
          "Short-Cycle Tertiary (ISCED 5) Participation",
          "Short-Cycle Tertiary (ISCED 5) Completion",
          "Post-Secondary Non-Tertiary (ISCED 4) Participation",
          "Post-Secondary Non-Tertiary (ISCED 4) Completion"
        ) ~
        "Education and Skills",
      outcome == "Exits from Unemployment" ~ "Labour Market Transitions"
    )
  )

# Create summary statistics for labels
almp_nma_model_five_component_summary <- almp_nma_model_five_component_draws |>
  group_by(
    outcome,
    component
  ) |>
  median_qi(
    effect,
    .width = c(0.8, 0.95)
  ) |>
  filter(
    .width == 0.95
  ) |>
  mutate(
    component = factor(component)
  ) |>
  mutate(
    effect = format(round(effect, 2), nsmall = 2),
    .lower = format(round(.lower, 2), nsmall = 2),
    .upper = format(round(.upper, 2), nsmall = 2)
  ) |>
  mutate(
    outcome_domain = case_when(
      outcome %in%
        c(
          "Currently Employed",
          "Currently Unemployed",
          "Currently NEET",
          "Currently Not in the Labour Force",
          "Currently Self-Employed",
          "Recent Employment",
          "Employed Since Baseline"
        ) ~
        "Labour Force Status",
      outcome %in% c("Labour Earnings", "Employment Compensation", "Wages") ~
        "Employment Compensation",
      outcome == "Total Individual Income" ~ "Total Income",
      outcome %in% c("Period Employed", "Period Unemployed") ~
        "Employment Duration",
      outcome == "Hours Worked" ~ "Hours Worked",
      outcome %in%
        c(
          "Apprenticeship Participation",
          "Bachelors Degree (ISCED 6) Participation",
          "Bachelors Degree (ISCED 6) Completion",
          "Secondary School (ISCED 3) Completion",
          "Secondary School (ISCED 3) Participation",
          "Occupational Licence Obtained",
          "Short-Cycle Tertiary (ISCED 5) Participation",
          "Short-Cycle Tertiary (ISCED 5) Completion",
          "Post-Secondary Non-Tertiary (ISCED 4) Participation",
          "Post-Secondary Non-Tertiary (ISCED 4) Completion"
        ) ~
        "Education and Skills",
      outcome == "Exits from Unemployment" ~ "Labour Market Transitions"
    )
  )

#-------------------------------------------------------------------------------
# 6. Export results for visualisation
#-------------------------------------------------------------------------------

saveRDS(
  almp_nma_model_five_component_summary,
  "./visualisation/inputs/prototype_models/almp_nma_model_five_component_summary.RDS"
)

saveRDS(
  almp_nma_model_five_component_draws,
  "./visualisation/inputs/prototype_models/almp_nma_model_five_component_draws.RDS"
)

saveRDS(
  almp_nma_model_five_tau_draws,
  "./visualisation/inputs/prototype_models/almp_nma_model_five_tau_draws.RDS"
)
