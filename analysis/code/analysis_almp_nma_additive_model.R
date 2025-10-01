#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 25/09/2025                                                                           #
# Purpose: NMA additive model                                                                #
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

almp_nma_additive_model_input_data <- almp_nma_additive_model_data |>
  # select closest data point to 24 month window
  filter(
    selected_primary_timepoint == 1
  ) |>
  # drop the rarest outcomes
  mutate(
    outcome = as.character(outcome)
  ) |>
  filter(
    !outcome %in%
      c(
        # these outcomes appear once
        "Apprenticeship Duration",
        "Bachelors or equivalent (ISCED 6) application",
        "EET since baseline",
        "Entires to Not in the Labour Force",
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
        "Foundation skills (ISCED 1) completion",
        "Lower secondary school (ISCED 2) completion",
        "Currently Employment Permanently",
        "Period Not in the Labour Force",
        # these outcomes appear three times
        "Currently Employed Temporarily",
        # these outcomes appear four times
        "Currently EET",
        "Period Unemployed"
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
  ) |>
  # convert negative outcomes to positive
  mutate(
    delta = case_when(
      outcome %in%
        c(
          "Current Unemployment",
          "Currently Unemployed",
          "Currently NEET",
          "Currently Not in the Labour Force",
          #"Entires to Not in the Labour Force",
          #"Not in the Labour Force since baseline",
          "Period Not in the Labour Force" #,
          #"Period Unemployed",
          #"Recent Unemployment",
          #"Unemployed since baseline"
        ) ~
        delta * -1,
      TRUE ~ delta
    )
  ) |>
  mutate(
    outcome = as.factor(outcome)
  )

saveRDS(
  almp_nma_additive_model_input_data,
  "./analysis/output/almp_nma_additive_model_data.RDS"
)

#-------------------------------------------------------------------------------
# 2. Specify model formula
#-------------------------------------------------------------------------------

almp_nma_additive_model_formula <- bf(
  delta | se(delta_se) ~
    0 +
      # fixed effects for each outcome × component (no pooling across outcomes)
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
      # time by outcome
      0 +
      outcome:outcome_timing_centred_24 +
      # component-level moderators for subgroup analysis x female
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
      # component-level moderators for subgroup analysis x age
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
      # heterogeneity: tau by design
      #(1 | gr(study, by = study_design_type)) +
      # heterogeneity: tau by component x design
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
        comp_other_active_component_nec ||
        gr(study, by = study_design_type))
)

#-------------------------------------------------------------------------------
# 3. Specify priors
#-------------------------------------------------------------------------------

# inspect available names
possible_prior_names <- get_prior(
  almp_nma_additive_model_formula,
  data = almp_nma_additive_model_input_data,
  family = gaussian()
)

# mild ridge for everything not otherwise targeted
prior_base <- list(
  prior(normal(0, 0.35), class = "b")
)

# outcome × component fixed effects
prior_outcome_component <- make_coef_priors(
  possible_prior_names,
  pattern = "^outcome.*:comp_",
  sd = 0.30
)

# main slopes: change in 1 unit = 1 year of mean age
prior_time_outcome <- make_coef_priors(
  possible_prior_names,
  pattern = "^outcome.*:outcome_timing_centred_24$",
  sd = 0.1
)

# female subgroup
prior_sex_subgroup <- make_coef_priors(
  possible_prior_names,
  pattern = "^prop_female_centred:comp_",
  sd = 0.10
)

# age subgroup
prior_age_subgroup <- make_coef_priors(
  possible_prior_names,
  pattern = "^study_age_mean_centred:comp_",
  sd = 0.05
)

# design × quality mains
prior_design_quality_mains <- make_coef_priors(
  possible_prior_names,
  pattern = "^(study_design_(soo|dbi)|low_study_quality)$",
  sd = 0.30
)

# design × quality interactions
prior_design_quality_interactions <- make_coef_priors(
  possible_prior_names,
  pattern = "^low_study_quality:study_design_(soo|dbi)$",
  sd = 0.25
)

# heterogeneity SDs: component-specific by design (diagonal)
prior_tau_design_component <- make_sd_priors(
  possible_prior_names,
  coef_pattern = "^comp_",
  group_pattern = "^study$",
  dist = "student_t(3, 0, %s)",
  scale = 0.25
)

almp_nma_additive_model_priors <- do.call(
  c,
  c(
    prior_base,
    prior_outcome_component,
    prior_time_outcome,
    prior_sex_subgroup,
    prior_age_subgroup,
    prior_design_quality_mains,
    prior_design_quality_interactions,
    prior_tau_design_component
  )
)

#-------------------------------------------------------------------------------
# 4. Fit the Bayesian additive CNMA model
#-------------------------------------------------------------------------------

almp_nma_additive_model <- brm(
  formula = almp_nma_additive_model_formula,
  data = almp_nma_additive_model_input_data,
  prior = almp_nma_additive_model_priors,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 2000,
  threads = threading(1),
  backend = "cmdstanr",
  init = 0,
  sample_prior = "no",
  control = list(adapt_delta = 0.99, max_treedepth = 16),
  refresh = 100,
  seed = 2204,
  file = "analysis/output/almp_nma_additive_model.RDS",
  file_refit = "on_change",
  save_pars = save_pars(all = TRUE)
)

#-------------------------------------------------------------------------------
# 5. Inspect the results
#-------------------------------------------------------------------------------

# inspect the results
summary(almp_nma_additive_model)

# inspect diagnostic plots
#plot(almp_nma_additive_model)

pp_check(almp_nma_additive_model, type = "dens_overlay", ndraws = 200)
pp_check(almp_nma_additive_model, type = "intervals")

#-------------------------------------------------------------------------------
# 6. Extract model draws
#-------------------------------------------------------------------------------

# load model if required
almp_nma_additive_model_results_location <- "./analysis/output/almp_nma_additive_model.RDS"
almp_nma_additive_model <- readRDS(
  almp_nma_additive_model_results_location
)

# get all model draws as a data frame
almp_nma_additive_model_draws <- as_draws_df(almp_nma_additive_model)

#-------------------------------------------------------------------------------
# 7. Extract heterogeneity stats
#-------------------------------------------------------------------------------

# extract heterogeneity by component x design
almp_nma_additive_model_tau_component_design_draws <- almp_nma_additive_model_draws |>
  # extract tau for study-level REs by component × design
  select(.draw, starts_with("sd_study__comp_")) |>
  pivot_longer(
    cols = -.draw,
    names_to = ".variable",
    values_to = "tau"
  ) %>%
  mutate(
    # pull out raw keys using string substitution
    component_key = sub("^sd_study__(comp_[^:]+):.*$", "\\1", .variable),
    design_key = sub("^.*:study_design_type", "", .variable),
    # rename pretty labels
    component = case_when(
      component_key == "comp_basic_skills_training" ~ "Basic Skills Training",
      component_key == "comp_soft_skills_training" ~ "Soft Skills Training",
      component_key == "comp_behavioural_skills_training" ~
        "Behavioural Skills Training",
      component_key == "comp_self_employment_support" ~
        "Self-Employment Support",
      component_key == "comp_job_specific_technical_skills_off_job_training" ~
        "Technical Skills Training (Off-the-Job)",
      component_key == "comp_job_search_preparation" ~ "Job Search Preparation",
      component_key == "comp_job_search_assistance" ~ "Job Search Assistance",
      component_key == "comp_employment_counselling" ~ "Employment Counselling",
      component_key == "comp_employment_coaching" ~ "Employment Coaching",
      component_key == "comp_financial_assistance" ~ "Financial Assistance",
      component_key == "comp_job_specific_technical_skills_on_job_training" ~
        "Technical Skills Training (On-the-Job)",
      component_key == "comp_paid_temporary_work_experience" ~
        "Paid Temporary Work Experience",
      component_key == "comp_unpaid_temporary_work_experience" ~
        "Unpaid Temporary Work Experience",
      component_key == "comp_wage_subsidies" ~ "Wage Subsidies",
      component_key == "comp_public_works" ~ "Public Works",
      component_key == "comp_other_active_component_nec" ~
        "Other Active Components",
      TRUE ~ NA_character_
    ),
    design = recode(
      design_key,
      "Randomiseddesign" = "Randomised design",
      "Selectiononobservables" = "Selection on observables",
      "Design-basedidentification" = "Design-based identification",
      .default = NA_character_
    )
  ) %>%
  filter(!is.na(component), !is.na(design)) %>%
  transmute(component, design, tau, .draw)

# summarise heterogeneity by component x design
almp_nma_additive_model_tau_component_design_summary <- almp_nma_additive_model_tau_component_design_draws |>
  group_by(component, design) |>
  summarise(
    .lower = quantile(tau, 0.05),
    .upper = quantile(tau, 0.95),
    tau = median(tau),
    .groups = "drop"
  )

# summarise heterogeneity by design
almp_nma_additive_model_tau_design_summary <- almp_nma_additive_model_tau_component_design_draws |>
  group_by(design) |>
  summarise(
    .lower = quantile(tau, 0.05),
    .upper = quantile(tau, 0.95),
    tau = median(tau),
    .groups = "drop"
  )


#-------------------------------------------------------------------------------
# 8. Export heterogenity results for visualisation
#-------------------------------------------------------------------------------

saveRDS(
  almp_nma_additive_model_tau_component_design_draws,
  "./visualisation/inputs/almp_nma_additive_model_tau_component_design_draws.RDS"
)

saveRDS(
  almp_nma_additive_model_tau_component_design_summary,
  "./visualisation/inputs/almp_nma_additive_model_tau_component_design_summary.RDS"
)

saveRDS(
  almp_nma_additive_model_tau_design_summary,
  "./visualisation/inputs/almp_nma_additive_model_tau_design_summary.RDS"
)

#-------------------------------------------------------------------------------
# 9. Explore what component estimates were actually updated
#-------------------------------------------------------------------------------

# Extract component effects (the b_ parameters)
almp_nma_additive_model_component_draws_flagged <- almp_nma_additive_model_draws |>
  select(.draw, matches("^b_outcome.*:comp_")) |>
  pivot_longer(
    cols = -.draw,
    names_to = "param",
    values_to = "theta"
  ) |>
  # parse outcome and component from the parameter name
  mutate(
    outcome = str_remove(str_extract(param, "^b_outcome[^:]+"), "^b_outcome"),
    component = str_remove(str_extract(param, "comp_.*$"), "^comp_"),
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
      "EntriesintoEmployment" = "Entries into Employment",
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
      "TotalIncome" = "Total Income",
      "Totalindividualincome" = "Total Individual Income",
      "Wages" = "Wages"
    ),
    component = recode(
      component,
      "basic_skills_training" = "Basic Skills Training",
      "behavioural_skills_training" = "Behavioural Skills Training",
      "employment_coaching" = "Employment Coaching",
      "employment_counselling" = "Employment Counselling",
      "financial_assistance" = "Financial Assistance",
      "job_search_assistance" = "Job Search Assistance",
      "job_search_preparation" = "Job Search Preparation",
      "job_specific_technical_skills_off_job_training" = "Technical Skills Training (Off-the-Job)",
      "job_specific_technical_skills_on_job_training" = "Technical Skills Training (On-the-Job)",
      "other_active_component_nec" = "Other Active Components",
      "paid_temporary_work_experience" = "Paid Temporary Work Experience",
      "public_works" = "Public Works",
      "self_employment_support" = "Self-Employment Support",
      "soft_skills_training" = "Soft Skills Training",
      "unpaid_temporary_work_experience" = "Unpaid Temporary Work Experience",
      "wage_subsidies" = "Wage Subsidies"
    )
  ) |>
  group_by(outcome, component) |>
  summarise(
    theta_sd = sd(theta),
    theta = mean(theta),
    .groups = "drop"
  ) |>
  # use custom functions to determine if posterior summary x component different from the prior
  mutate(
    kld = kullback_leibler_divergence_normal(
      theta,
      theta_sd,
      prior_sd = 0.40
    ),
    ppo = mapply(
      prior_posterior_overlap,
      theta,
      theta_sd,
      MoreArgs = list(prior_sd = 0.40)
    ),
    # signal gate: moved if PPO ≤ 0.60 OR KLD ≥ 0.10 nats
    posterior_different_prior = (ppo <= 0.60) | (kld >= 0.10),
    # flag outcomes where the posterior does not differ from the prior
    posterior_different_prior_flag = case_when(
      posterior_different_prior == TRUE ~ "Yes",
      posterior_different_prior == FALSE ~ "No"
    )
  ) |>
  select(
    -kld,
    -ppo,
    -posterior_different_prior,
    -theta,
    -theta_sd
  )

#-------------------------------------------------------------------------------
# 10. Extract component effects
#-------------------------------------------------------------------------------

# Extract component effects
almp_nma_additive_model_component_draws <- almp_nma_additive_model_draws |>
  select(.draw, matches("^b_outcome.*:comp_")) |>
  pivot_longer(
    cols = -.draw,
    names_to = "param",
    values_to = "theta"
  ) |>
  # parse outcome and component from the parameter name
  mutate(
    outcome = str_remove(str_extract(param, "^b_outcome[^:]+"), "^b_outcome"),
    component = str_remove(str_extract(param, "comp_.*$"), "^comp_"),
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
      "EntriesintoEmployment" = "Entries into Employment",
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
      "TotalIncome" = "Total Income",
      "Totalindividualincome" = "Total Individual Income",
      "Wages" = "Wages"
    ),
    component = recode(
      component,
      "basic_skills_training" = "Basic Skills Training",
      "behavioural_skills_training" = "Behavioural Skills Training",
      "employment_coaching" = "Employment Coaching",
      "employment_counselling" = "Employment Counselling",
      "financial_assistance" = "Financial Assistance",
      "job_search_assistance" = "Job Search Assistance",
      "job_search_preparation" = "Job Search Preparation",
      "job_specific_technical_skills_off_job_training" = "Technical Skills Training (Off-the-Job)",
      "job_specific_technical_skills_on_job_training" = "Technical Skills Training (On-the-Job)",
      "other_active_component_nec" = "Other Active Components",
      "paid_temporary_work_experience" = "Paid Temporary Work Experience",
      "public_works" = "Public Works",
      "self_employment_support" = "Self-Employment Support",
      "soft_skills_training" = "Soft Skills Training",
      "unpaid_temporary_work_experience" = "Unpaid Temporary Work Experience",
      "wage_subsidies" = "Wage Subsidies"
    )
  ) |>
  select(
    -param
  ) |>
  mutate(
    probability_greater_zero = mean(theta > 0, na.rm = TRUE),
    probability_low_impact = mean(theta > 0 & theta <= 0.1, na.rm = TRUE),
    probability_medium_impact = mean(theta > 0.1 & theta < 0.2, na.rm = TRUE),
    probability_high_impact = mean(theta >= 0.2, na.rm = TRUE),
    .by = c(outcome, component)
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
      outcome %in% c("Labour Earnings", "Wages") ~ "Employment Compensation",
      outcome %in% c("Total Income", "Total Individual Income") ~
        "Total Income",
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
      outcome %in% c("Entries into Employment", "Exits from Unemployment") ~
        "Labour Market Transitions"
    ),
    # Flip the sign on negative outcomes
    theta = case_when(
      outcome %in%
        c(
          "Currently Unemployed",
          "Currently NEET",
          "Currently Not in the Labour Force",
          "Period Not in the Labour Force"
        ) ~
        theta * -1,
      TRUE ~ theta
    )
  )

# merge draws with flags
almp_nma_additive_model_component_draws_combined <- left_join(
  almp_nma_additive_model_component_draws,
  almp_nma_additive_model_component_draws_flagged,
  by = c(
    "outcome",
    "component"
  )
) |>
  mutate(
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
        "Entries into Employment",
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
        "Total Income",
        "Total Individual Income",
        "Wages"
      ),
      ordered = TRUE
    ),
    component = factor(
      component,
      levels = c(
        "Basic Skills Training",
        "Soft Skills Training",
        "Behavioural Skills Training",
        "Technical Skills Training (Off-the-Job)",
        "Self-Employment Support",
        "Job Search Assistance",
        "Job Search Preparation",
        "Employment Coaching",
        "Employment Counselling",
        "Financial Assistance",
        "Technical Skills Training (On-the-Job)",
        "Paid Temporary Work Experience",
        "Unpaid Temporary Work Experience",
        "Wage Subsidies",
        "Public Works",
        "Other Active Components"
      ),
      ordered = TRUE
    )
  )

# Create summary statistics for labels
almp_nma_additive_model_component_summary <- almp_nma_additive_model_component_draws_combined |>
  group_by(
    outcome,
    component,
    posterior_different_prior_flag,
    probability_greater_zero,
    probability_low_impact,
    probability_medium_impact,
    probability_high_impact
  ) |>
  median_qi(
    theta,
    .width = c(0.8, 0.95)
  ) |>
  filter(
    .width == 0.95
  ) |>
  mutate(
    component = factor(component)
  ) |>
  mutate(
    theta = format(round(theta, 2), nsmall = 2),
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
      outcome %in% c("Labour Earnings", "Wages") ~ "Employment Compensation",
      outcome %in% c("Total Income", "Total Individual Income") ~
        "Total Income",
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
      outcome %in% c("Entries into Employment", "Exits from Unemployment") ~
        "Labour Market Transitions"
    )
  )

#-------------------------------------------------------------------------------
# 11. Export component results for visualisation
#-------------------------------------------------------------------------------

saveRDS(
  almp_nma_additive_model_component_summary,
  "./visualisation/inputs/almp_nma_additive_model_component_summary.RDS"
)

saveRDS(
  almp_nma_additive_model_component_draws_combined,
  "./visualisation/inputs/almp_nma_additive_model_component_draws.RDS"
)
