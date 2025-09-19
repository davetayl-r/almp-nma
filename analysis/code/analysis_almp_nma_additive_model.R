#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 19/09/2025                                                                           #
# Purpose: Additive NMA model                                                                #
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

almp_nma_additive_model_data <- almp_nma_additive_model_data |>
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

almp_nma_additive_model_formula <- bf(
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
      # consider outcome timing
      outcome_timing_centred_24 +
      outcome_domain:outcome_timing_centred_24 +
      prop_female_centred +
      outcome_domain:prop_female_centred +
      study_age_mean_centred +
      outcome_domain:study_age_mean_centred +
      # additive study-design adjustments using RCT as a baseline
      study_design_soo +
      study_design_dbi +
      # study quality penalty with design specific interaction
      low_study_quality +
      low_study_quality:study_design_soo +
      low_study_quality:study_design_dbi +
      # domain-varying penalties
      outcome_domain:study_design_soo +
      outcome_domain:study_design_dbi +
      outcome_domain:low_study_quality +
      # random effects
      (0 + outcome_domain || gr(study, by = study_design_type)) +
      (1 | gr(study, by = study_design_type))
)

#-------------------------------------------------------------------------------
# 3. Specify priors
#-------------------------------------------------------------------------------

# inspect available names
possible_prior_names <- get_prior(
  almp_nma_additive_model_formula,
  data = almp_nma_additive_model_data,
  family = gaussian()
)

# Base priors — apply to everything unless overridden
prior_base <- list(
  prior(normal(0, 0.40), class = "b"),
  prior(normal(0, 0.20), class = "sd", group = "study")
)

# Main effects
prior_main_study_design <- make_coef_priors(
  possible_prior_names,
  "^study_design_(soo|dbi)$",
  0.15
)
prior_main_quality <- make_coef_priors(
  possible_prior_names,
  "^low_study_quality$",
  0.12
)

# main slopes: change in 1 unit = 1 year from 24 months
prior_time_main <- make_coef_priors(
  possible_prior_names,
  "^outcome_timing_centred_24$",
  0.10
)

# domain-specific time slopes
prior_time_domain <- make_coef_priors(
  possible_prior_names,
  "^(outcome_domain.*:outcome_timing_centred_24|outcome_timing_centred_24:outcome_domain[A-Z].*)",
  0.12
)

# Global design × quality (order-robust)
prior_global_interactions <- make_coef_priors(
  possible_prior_names,
  "^(study_design_(soo|dbi):low_study_quality|low_study_quality:study_design_(soo|dbi))$",
  0.10
)

# main slopes: change in 1 unit = 10 percentage points female
prior_sex_main <- make_coef_priors(
  possible_prior_names,
  "^prop_female_centred$",
  0.05
)

# main slopes: change in 1 unit = 1 year of mean age
prior_age_main <- make_coef_priors(
  possible_prior_names,
  "^study_age_mean_centred$",
  0.03
)

# domain-specific modifiers
prior_sex_outcome_domain <- make_coef_priors(
  possible_prior_names,
  "^(outcome_domain.*:prop_female_centred|prop_female_centred:outcome_domain[A-Z].*)",
  0.10
)
prior_age_outcome_domain <- make_coef_priors(
  possible_prior_names,
  "^(outcome_domain.*:study_age_mean_centred|study_age_mean_centred:outcome_domain[A-Z].*)",
  0.08
)

# Domain-varying penalties
prior_domain_quality <- make_coef_priors(
  possible_prior_names,
  "^(low_study_quality:outcome_domain[A-Z].*|outcome_domain[A-Z].*:low_study_quality)",
  0.10
)

prior_domain_design <- make_coef_priors(
  possible_prior_names,
  "^(study_design_(soo|dbi):outcome_domain[A-Z].*|outcome_domain[A-Z].*:study_design_(soo|dbi))",
  0.12
)

# Combine & flatten
almp_nma_additive_model_priors <- do.call(
  c,
  c(
    prior_base,
    prior_main_study_design,
    prior_main_quality,
    prior_time_main,
    prior_time_domain,
    prior_global_interactions,
    prior_sex_main,
    prior_age_main,
    prior_sex_outcome_domain,
    prior_age_outcome_domain,
    prior_domain_quality,
    prior_domain_design
  )
)

#-------------------------------------------------------------------------------
# 3. Fit the Bayesian additive CNMA model
#-------------------------------------------------------------------------------

almp_nma_additive_model <- brm(
  formula = almp_nma_additive_model_formula,
  data = almp_nma_additive_model_data,
  prior = almp_nma_additive_model_priors,
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
# 4. Inspect model results
#-------------------------------------------------------------------------------

# inspect the results
summary(almp_nma_additive_model)

# inspect diagnostic plots
#plot(almp_nma_additive_model)

pp_check(almp_nma_additive_model, type = "dens_overlay", ndraws = 200)
pp_check(almp_nma_additive_model, type = "intervals")

# examine influence
almp_nma_additive_model_loo <- add_criterion(
  almp_nma_additive_model,
  "loo",
  reloo = FALSE
)
summary(almp_nma_additive_model_loo$criteria$loo)

#-------------------------------------------------------------------------------
# 5. Extract and export heterogeneity
#-------------------------------------------------------------------------------

# extract tau
almp_nma_additive_model_tau_draws_raw <- almp_nma_additive_model |>
  # extract tau for study-level REs by outcome_domain × design
  gather_draws(
    `^sd_study__outcome_domain.*:study_design_type.*$`,
    regex = TRUE
  ) |>
  transmute(
    .draw,
    # pull outcome between "__outcome_domain" and the colon that precedes "study_design_type"
    outcome = str_match(.variable, "^sd_study__outcome_domain([^:]+):")[, 2],
    # pull design label after ":study_design_type"
    design = str_match(.variable, ":study_design_type(.+)$")[, 2],
    tau = .value
  ) |>
  mutate(
    design = recode(
      design,
      "Randomiseddesign" = "Randomised design",
      "Selectiononobservables" = "Selection on observables",
      "Design-basedidentification" = "Design-based identification"
    )
  )

# extract overall tau (pool across outcome domains & study designs)
almp_nma_additive_model_tau_overall_draws <- almp_nma_additive_model_tau_draws_raw |>
  group_by(.draw) |>
  summarise(tau = median(tau), .groups = "drop") |>
  mutate(
    group = "Overall"
  )

# extract tau by study design (median across outcome domains within draw)
almp_nma_additive_model_tau_by_design_draws <- almp_nma_additive_model_tau_draws_raw |>
  group_by(.draw, design) |>
  summarise(tau = median(tau), .groups = "drop") |>
  rename(
    group = design
  ) |>
  select(
    .draw,
    tau,
    group
  )

# merge tau draws
almp_nma_additive_model_tau_draws <- bind_rows(
  almp_nma_additive_model_tau_overall_draws,
  almp_nma_additive_model_tau_by_design_draws
) |>
  mutate(
    group = factor(
      group,
      levels = c(
        "Overall",
        "Randomised design",
        "Selection on observables",
        "Design-based identification"
      ),
      ordered = TRUE
    )
  )

# extract and merge summaries
almp_nma_additive_model_tau_overall <- almp_nma_additive_model_tau_overall_draws |>
  median_qi(tau, .width = c(0.8, 0.95)) |>
  filter(.width == 0.95) |>
  mutate(
    group = "Overall"
  ) |>
  select(
    group,
    tau,
    .lower,
    .upper
  )

almp_nma_additive_model_tau_by_design <- almp_nma_additive_model_tau_by_design_draws |>
  group_by(
    group
  ) |>
  median_qi(
    tau,
    .width = c(0.8, 0.95)
  ) |>
  filter(
    .width == 0.95
  ) |>
  ungroup() |>
  select(
    group,
    tau,
    .lower,
    .upper
  )

almp_nma_additive_model_tau_summary <- bind_rows(
  almp_nma_additive_model_tau_overall,
  almp_nma_additive_model_tau_by_design
) |>
  mutate(
    group = factor(
      group,
      levels = c(
        "Overall",
        "Randomised design",
        "Selection on observables",
        "Design-based identification"
      ),
      ordered = TRUE
    )
  )

#-------------------------------------------------------------------------------
# 7. Extract component effects
#-------------------------------------------------------------------------------

# Extract component effects (the b_ parameters)
almp_nma_additive_model_component_draws <- almp_nma_additive_model |>
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
almp_nma_additive_model_component_summary <- almp_nma_additive_model_component_draws |>
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
# 6. Explore what component estimates were actually updated
#-------------------------------------------------------------------------------

# extract posterior summaries for b_ component × outcome terms
almp_nma_additive_model_component_updated_flag <- almp_nma_additive_model |>
  gather_draws(`b_.*:comp_.*`, regex = TRUE) |>
  mutate(
    outcome = str_remove(str_extract(.variable, "^[^:]+"), "^b_outcome"),
    component = str_remove(str_extract(.variable, "(?<=:).*"), "^comp_")
  ) |>
  group_by(outcome, component) |>
  summarise(
    post_mean = mean(.value),
    post_sd = sd(.value),
    .groups = "drop"
  ) |>
  # use custom functions to determine if posterior summary x component different from the prior
  mutate(
    kld = kullback_leibler_divergence_normal(
      post_mean,
      post_sd,
      prior_sd = 0.40
    ),
    ppo = mapply(
      prior_posterior_overlap,
      post_mean,
      post_sd,
      MoreArgs = list(prior_sd = 0.40)
    ),
    # signal gate: moved if PPO ≤ 0.60 OR KLD ≥ 0.10 nats
    posterior_different_prior = (ppo <= 0.60) | (kld >= 0.10)
  )

# apply filters to component draws
almp_nma_additive_model_component_draws_flagged <- almp_nma_additive_model_component_draws |>
  left_join(
    almp_nma_additive_model_component_updated_flag |>
      transmute(outcome, component, kld, ppo, posterior_different_prior),
    by = c("outcome", "component")
  ) |>
  # flag outcomes where the posterior does not differ from the prior
  mutate(
    posterior_different_prior_flag = case_when(
      posterior_different_prior == TRUE ~ "Yes",
      posterior_different_prior == FALSE ~ "No"
    )
  ) |>
  select(
    -posterior_different_prior
  )

almp_nma_additive_model_component_summary_flagged <- almp_nma_additive_model_component_summary |>
  left_join(
    almp_nma_additive_model_component_updated_flag |>
      transmute(outcome, component, kld, ppo, posterior_different_prior),
    by = c("outcome", "component")
  ) |>
  # flag outcomes where the posterior does not differ from the prior
  mutate(
    posterior_different_prior_flag = case_when(
      posterior_different_prior == TRUE ~ "Yes",
      posterior_different_prior == FALSE ~ "No"
    )
  ) |>
  select(
    -posterior_different_prior
  )

#-------------------------------------------------------------------------------
# 6. Export results for visualisation
#-------------------------------------------------------------------------------

saveRDS(
  almp_nma_additive_model_component_summary_flagged,
  "./visualisation/inputs/almp_nma_additive_model_component_summary.RDS"
)

saveRDS(
  almp_nma_additive_model_component_draws_flagged,
  "./visualisation/inputs/almp_nma_additive_model_component_draws.RDS"
)

saveRDS(
  almp_nma_additive_model_tau_summary,
  "./visualisation/inputs/almp_nma_additive_model_tau_summary.RDS"
)

saveRDS(
  almp_nma_additive_model_tau_draws,
  "./visualisation/inputs/almp_nma_additive_model_tau_draws.RDS"
)
