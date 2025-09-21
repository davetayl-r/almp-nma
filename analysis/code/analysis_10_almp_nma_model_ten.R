#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 20/09/2025                                                                           #
# Purpose: NMA model #9 — include sex, age and simplify RE                                   #
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

almp_nma_model_ten_data <- almp_nma_additive_model_data |>
  # select closest data point to 24 month window
  filter(
    selected_primary_timepoint == 1
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
        "Post-secondary non-tertiary (ISCED 4) application",
        "Quarters employed",
        "Re-employment probability",
        "Recent EET",
        "Recent Unemployment",
        "Short-cycle tertiary (ISCED 5) application",
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
    study_design_dbi = as.numeric(study_design_dbi),
    outcome_domain = factor(outcome_domain)
  )

#-------------------------------------------------------------------------------
# 2. Specify model formula
#-------------------------------------------------------------------------------

almp_nma_model_ten_formula <- bf(
  delta | se(delta_se) ~
    0 +
      # component main effects — shared across outcomes
      comp_basic_skills_training +
      comp_soft_skills_training +
      comp_behavioural_skills_training +
      comp_self_employment_support +
      comp_job_specific_technical_skills_off_job_training +
      comp_job_search_preparation +
      comp_job_search_assistance +
      comp_employment_counselling +
      comp_employment_coaching +
      comp_financial_assistance +
      comp_job_specific_technical_skills_on_job_training +
      comp_paid_temporary_work_experience +
      comp_unpaid_temporary_work_experience +
      comp_wage_subsidies +
      comp_public_works +
      comp_other_active_component_nec +
      # Correlated random slopes across components by outcome (deviations around component means)
      (0 +
        comp_basic_skills_training +
        comp_soft_skills_training +
        comp_behavioural_skills_training +
        comp_self_employment_support +
        comp_job_specific_technical_skills_off_job_training +
        comp_job_search_preparation +
        comp_job_search_assistance +
        comp_employment_counselling +
        comp_employment_coaching +
        comp_financial_assistance +
        comp_job_specific_technical_skills_on_job_training +
        comp_paid_temporary_work_experience +
        comp_unpaid_temporary_work_experience +
        comp_wage_subsidies +
        comp_public_works +
        comp_other_active_component_nec |
        outcome) +
      # Outcome-specific time slopes: where 24 months == 0
      0 +
      outcome:outcome_timing_centred_24 +
      # Per-component slopes for prop_female for subgroup analysis
      0 +
      prop_female_centred:(comp_basic_skills_training +
        comp_soft_skills_training +
        comp_behavioural_skills_training +
        comp_self_employment_support +
        comp_job_specific_technical_skills_off_job_training +
        comp_job_search_preparation +
        comp_job_search_assistance +
        comp_employment_counselling +
        comp_employment_coaching +
        comp_financial_assistance +
        comp_job_specific_technical_skills_on_job_training +
        comp_paid_temporary_work_experience +
        comp_unpaid_temporary_work_experience +
        comp_wage_subsidies +
        comp_public_works +
        comp_other_active_component_nec) +
      # Per-component slopes for study_age_mean for subgroup analysis
      0 +
      study_age_mean_centred:(comp_basic_skills_training +
        comp_soft_skills_training +
        comp_behavioural_skills_training +
        comp_self_employment_support +
        comp_job_specific_technical_skills_off_job_training +
        comp_job_search_preparation +
        comp_job_search_assistance +
        comp_employment_counselling +
        comp_employment_coaching +
        comp_financial_assistance +
        comp_job_specific_technical_skills_on_job_training +
        comp_paid_temporary_work_experience +
        comp_unpaid_temporary_work_experience +
        comp_wage_subsidies +
        comp_public_works +
        comp_other_active_component_nec) +
      # Design × quality bias structure — RCT baseline == 0
      0 +
      study_design_soo +
      study_design_dbi +
      0 +
      low_study_quality +
      0 +
      low_study_quality:study_design_soo +
      low_study_quality:study_design_dbi +
      # Design-specific study-level heterogeneity (allowing tau to vary by design)
      (1 | gr(study, by = study_design_type))
)

#-------------------------------------------------------------------------------
# 3. Specify priors
#-------------------------------------------------------------------------------

# inspect available names
possible_prior_names <- get_prior(
  almp_nma_model_ten_formula,
  data = almp_nma_model_ten_data,
  family = gaussian()
)

# Base priors — apply to everything unless overridden
prior_base <- list(
  prior(normal(0, 0.40), class = "b"),
  prior(normal(0, 0.20), class = "sd", group = "study")
)

# outcome-level priors — LKJ correlation structure + generic SD on outcome-level deviations
prior_outcome <- list(
  prior(lkj(2), class = "cor", group = "outcome"),
  prior(normal(0, 0.20), class = "sd", group = "outcome")
)

# outcome-specific time slopes (24m = 0): match all b_outcome<...>:outcome_timing_centred_24
prior_time_outcome <- make_coef_priors(
  possible_prior_names,
  "^b_outcome.*:outcome_timing_centred_24$",
  0.10
)

# per-component subgroup slopes
prior_sex_subgroup <- make_coef_priors(
  possible_prior_names,
  "^b_prop_female_centred:comp_",
  0.05
)

prior_age_subgroup <- make_coef_priors(
  possible_prior_names,
  "^b_study_age_mean_centred:comp_",
  0.03
)

# Global design × quality
prior_study_design_quality <- make_coef_priors(
  possible_prior_names,
  "^(study_design_(soo|dbi):low_study_quality|low_study_quality:study_design_(soo|dbi))$",
  0.10
)

# Combine priors and flatten for model to digest
almp_nma_model_ten_priors <- do.call(
  c,
  c(
    prior_base,
    prior_outcome,
    prior_time_outcome,
    prior_sex_subgroup,
    prior_age_subgroup,
    prior_study_design_quality
  )
)

#-------------------------------------------------------------------------------
# 4. Fit the Bayesian additive CNMA model
#-------------------------------------------------------------------------------

almp_nma_model_ten <- brm(
  formula = almp_nma_model_ten_formula,
  data = almp_nma_model_ten_data,
  prior = almp_nma_model_ten_priors,
  family = gaussian(),
  chains = 2,
  iter = 3000,
  warmup = 1500,
  cores = 8,
  threads = threading(2),
  backend = "cmdstanr",
  init = "random",
  control = list(adapt_delta = 0.95, max_treedepth = 13),
  refresh = 250,
  seed = 12345,
  save_pars = save_pars(all = TRUE)
)

#-------------------------------------------------------------------------------
# 5. Inspect the results
#-------------------------------------------------------------------------------

# inspect the results
summary(almp_nma_model_ten)

# inspect diagnostic plots
#plot(almp_nma_model_ten)

pp_check(almp_nma_model_ten, type = "dens_overlay", ndraws = 200)
pp_check(almp_nma_model_ten, type = "intervals")

# examine influence
#almp_nma_model_ten_loo <- add_criterion(
#  almp_nma_model_ten,
#  "loo",
#  reloo = FALSE
#)
#summary(almp_nma_model_ten_loo$criteria$loo) # check Pareto k

#-------------------------------------------------------------------------------
# 6. Clean up model output
#-------------------------------------------------------------------------------

x <- factor(almp_nma_model_ten_tau_draws$.variable) |> levels()

# extract heterogeneity
almp_nma_model_ten_tau_draws <- almp_nma_model_ten |>
  # extract tau for study-level REs by outcome × design
  gather_draws(
    `^sd_outcome.*$`,
    regex = TRUE
  ) |>
  transmute(
    .draw,
    # rename vars
    outcome = case_when(
      .variable == "sd_outcome__comp_basic_skills_training" ~
        "Basic Skills Training",
      .variable == "sd_outcome__comp_behavioural_skills_training" ~
        "Behavioural Skills Training",
      .variable == "sd_outcome__comp_employment_coaching" ~
        "Employment Coaching",
      .variable == "sd_outcome__comp_employment_counselling" ~
        "Employment Counselling",
      .variable == "sd_outcome__comp_financial_assistance" ~
        "Financial Assistance",
      .variable == "sd_outcome__comp_job_search_assistance" ~
        "Job Search Assistance",
      .variable == "sd_outcome__comp_job_search_preparation" ~
        "Job Search Preparation",
      .variable ==
        "sd_outcome__comp_job_specific_technical_skills_off_job_training" ~
        "Off-Job Training",
      .variable ==
        "sd_outcome__comp_job_specific_technical_skills_on_job_training" ~
        "On-Job Training",
      .variable == "sd_outcome__comp_other_active_component_nec" ~
        "Other active component",
      .variable == "sd_outcome__comp_paid_temporary_work_experience" ~
        "Paid Temporary Work Experience",
      .variable == "sd_outcome__comp_public_works" ~ "Public Works",
      .variable == "sd_outcome__comp_self_employment_support" ~
        "Self-employment Support",
      .variable == "sd_outcome__comp_soft_skills_training" ~
        "Soft Skills Training",
      .variable == "sd_outcome__comp_unpaid_temporary_work_experience" ~
        "Unpaid Temporary Work Experience",
      .variable == "sd_outcome__comp_wage_subsidies" ~ "Wage Subsidies"
    ),
    tau = .value
  ) |>
  select(
    outcome,
    tau
  )

# Extract component effects (the b_ parameters)
almp_nma_model_ten_component_draws <- almp_nma_model_ten |>
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
almp_nma_model_ten_component_summary <- almp_nma_model_ten_component_draws |>
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
# 7. Export results for visualisation
#-------------------------------------------------------------------------------

saveRDS(
  almp_nma_model_ten_component_summary,
  "./visualisation/inputs/prototype_models/almp_nma_model_ten_component_summary.RDS"
)

saveRDS(
  almp_nma_model_ten_component_draws,
  "./visualisation/inputs/prototype_models/almp_nma_model_ten_component_draws.RDS"
)

saveRDS(
  almp_nma_model_ten_tau_draws,
  "./visualisation/inputs/prototype_models/almp_nma_model_ten_tau_draws.RDS"
)
