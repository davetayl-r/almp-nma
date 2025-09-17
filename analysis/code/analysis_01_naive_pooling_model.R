#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: NMA model #1                                                                      #
#============================================================================================#

# load required packages
library(tidyverse)
library(stringr)
library(cmdstanr)
library(brms)
library(tidybayes)
library(posterior)
library(ggplot2)
library(ggdist)
library(scales)
library(ggh4x)

# load custom functions
source("./analysis/code/analysis_functions.R")

# load data
almp_nma_additive_model_data_location <- "./analysis/inputs/almp_nma_additive_model_data.RDS"
almp_nma_additive_model_data <- readRDS(almp_nma_additive_model_data_location)

#-------------------------------------------------------------------------------
# 1. Subset data for model
#-------------------------------------------------------------------------------

almp_nma_model_one_data <- almp_nma_additive_model_data |>
  # select closest data point to 24 month window
  filter()

#-------------------------------------------------------------------------------
# 2. Specify model formula
#-------------------------------------------------------------------------------

# specify formula for the additive component model
almp_nma_model_one_formula <- bf(
  delta | se(delta_se) ~
    0 +
      # component x outcome effects
      outcome:comp_basic_skills_training +
      outcome:comp_soft_skills_training +
      outcome:comp_behavioural_skills_training +
      outcome:comp_business_skills_training +
      outcome:comp_business_advisory_and_mentoring +
      outcome:comp_financial_and_start_up_support +
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
      # random effects for each study
      (0 + outcome | p | study)
)

#-------------------------------------------------------------------------------
# 3. Fit the Bayesian additive CNMA model
#-------------------------------------------------------------------------------

almp_nma_model_one <- brm(
  formula = almp_nma_model_one_formula,
  data = almp_nma_model_one_data,
  prior = c(
    # Component effects
    prior(normal(0, 0.4), class = "b"),
    # Study-level heterogeneity
    prior(normal(0, 0.25), class = "sd", group = "study")
  ),
  chains = 2,
  warmup = 2000,
  iter = 4000,
  cores = 8,
  backend = "cmdstanr",
  refresh = 250,
  # Additional control parameters for stability
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  ),
  silent = FALSE,
  seed = 12345
)

#-------------------------------------------------------------------------------
# 4. Inspect the results
#-------------------------------------------------------------------------------

# inspect the results
summary(almp_nma_model_one)

# inspect diagnostic plots
#plot(almp_nma_model_one)

pp_check(almp_nma_model_one)

#-------------------------------------------------------------------------------
# 5. Clean up output
#-------------------------------------------------------------------------------

# The component effects are the b_ parameters (excluding intercept if any)
component_draws <- multivariate_hpsb_cnma_model |>
  gather_draws(`b_.*:comp_.*`, regex = TRUE) |>
  # Parse the parameter names to extract outcome and component
  mutate(
    # Extract outcome (everything before the colon)
    outcome = str_extract(.variable, "^[^:]+"),
    outcome = str_remove(outcome, "^b_outcome"),
    # Extract component (everything after comp_)
    component = str_extract(.variable, "(?<=:).*"),
    component = str_remove(component, "^comp_"),
    component = case_when(
      component == "cognitive_behaviour_therapy_group" ~
        "Cognitive Behaviour Therapy (Group)",
      component == "cognitive_behaviour_therapy_individual" ~
        "Cognitive Behaviour Therapy (Individual)",
      component == "family_therapy" ~ "Family Therapy",
      component == "sex_education" ~ "Sex Education",
      component == "relapse_prevention" ~ "Relapse Prevention",
      component == "social_skills_training" ~ "Social Skills Training",
      component == "multisystemtic_therapy" ~ "Multisystemtic Therapy",
      component == "individual_therapy" ~ "Individual Therapy",
      component == "group_therapy" ~ "Group Therapy",
      component == "adventure_therapy" ~ "Adventure Therapy",
      component == "exercise" ~ "Exercise",
      component == "mode_deactivation_therapy" ~ "Mode Deactivation Therapy",
      component == "play_therapy" ~ "Play Therapy",
      component == "other" ~ "Other"
    ),
    component = factor(
      component,
      levels = c(
        "Cognitive Behaviour Therapy (Group)",
        "Cognitive Behaviour Therapy (Individual)",
        "Family Therapy",
        "Multisystemtic Therapy",
        "Individual Therapy",
        "Group Therapy",
        "Mode Deactivation Therapy",
        "Adventure Therapy",
        "Sex Education",
        "Relapse Prevention",
        "Social Skills Training",
        "Exercise",
        "Play Therapy",
        "Other"
      ),
      ordered = TRUE
    ),
    outcome = case_when(
      outcome == "SexualRiskBehaviors" ~ "Sexual Risk Behaviours",
      outcome == "SexualCognitiveDistortions" ~ "Sexual Cognitive Distortions",
      outcome == "HealthySexualKnowledgeDAttitudes" ~
        "Healthy Sexual Knowledge or Attitudes",
      outcome == "Frequencyofsexualoffending" ~ "Frequency of Sexual Offending",
      outcome == "Anyviolentsexualoffending" ~ "Any Violent Sexual Offending",
      outcome == "Anysexualoffending" ~ "Any Sexual Offending"
    ),
    outcome = factor(
      outcome,
      levels = c(
        "Sexual Risk Behaviours",
        "Sexual Cognitive Distortions",
        "Frequency of Sexual Offending",
        "Any Violent Sexual Offending",
        "Any Sexual Offending",
        "Healthy Sexual Knowledge or Attitudes"
      ),
      ordered = TRUE
    )
  ) |>
  ungroup() |>
  select(.draw, outcome, component, effect = .value)

# Create summary statistics for labels
component_summary <- component_draws |>
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
  #mutate(
  #  effect = round(effect, 2),
  #  .lower = round(.lower, 2),
  #  .upper = round(.upper, 2)
  #)
  mutate(
    effect = format(round(effect, 2), nsmall = 2),
    .lower = format(round(.lower, 2), nsmall = 2),
    .upper = format(round(.upper, 2), nsmall = 2)
  )

#-------------------------------------------------------------------------------
# 6. Visualise results
#-------------------------------------------------------------------------------

# create forest plot
forest_plot_harmful_sexual_behaviour_outcomes <- component_draws |>
  ggplot(
    aes(
      x = effect,
      y = outcome #,
      #fill = outcome
    )
  ) +
  # Zero reference line
  geom_vline(
    xintercept = 0,
    linewidth = 0.25,
    linetype = "dashed",
    alpha = 0.5,
    color = "gray50"
  ) +
  # Half-eye plots showing posterior distributions
  stat_halfeye(
    data = . %>% filter(outcome == "Healthy Sexual Knowledge or Attitudes"),
    aes(
      fill = after_stat(ifelse(
        x <= -0.0,
        "negative_outcome",
        "positive_outcome"
      ))
    ),
    .width = c(0.95),
    colour = "#000000",
    alpha = 0.7,
    point_interval = "median_qi"
  ) +
  stat_halfeye(
    data = . %>% filter(!outcome == "Healthy Sexual Knowledge or Attitudes"),
    aes(
      fill = after_stat(ifelse(
        x >= -0.0,
        "negative_outcome",
        "positive_outcome"
      ))
    ),
    .width = c(0.95),
    colour = "#000000",
    alpha = 0.7,
    point_interval = "median_qi"
  ) +
  # Add summary text labels
  geom_text(
    data = mutate_if(component_summary, is.numeric, round, 3),
    aes(
      label = str_glue("{effect} [{.lower},{.upper}]"),
      x = 0
    ),
    hjust = "centre",
    nudge_y = -0.2,
    size = 3,
    color = "black"
  ) +
  # wrap y-axis labels
  scale_y_discrete(
    labels = label_wrap(20)
  ) +
  scale_x_continuous(
    limits = c(-2, 2),
    breaks = c(-1, 0, 1)
  ) +
  # wrap facets
  facet_grid(
    . ~ component,
    labeller = label_wrap_gen(width = 15)
  ) +
  # specify colour scheme
  scale_fill_manual(
    values = c(
      "positive_outcome" = "#008744",
      "negative_outcome" = "#d62d20"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of therapeutic interventions for children and young people who have exhibited harmful sexual behaviour from a\nBayesian CNMA for Harmful Sexual Behaviour Outcomes",
    subtitle = "Posterior distributions with 95% credible intervals",
    x = "Effect Size (Hedges' g)",
    y = "Outcome",
    caption = "Values show median effect size [95% Cr I]"
  ) +
  # set theme
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11
    ),
    axis.title = element_text(
      size = 12
    ),
    axis.text = element_text(
      size = 10
    ),
    strip.text = element_text(
      size = 10
    ),
    legend.position = "bottom"
  )

forest_plot_harmful_sexual_behaviour_outcomes

# export plot
#ggsave(
#  plot = forest_plot_harmful_sexual_behaviour_outcomes,
#  filename = "./output/figures/hpsb_cnma_forest_plot_harmful_sexual_behaviour_outcomes.png",
#  height = 7,
#  width = 16,
#  device = "png",
#  type = "cairo-png"
#)
