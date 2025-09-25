#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 24/09/2025                                                                           #
# Purpose: Visualise subgroup results x sex NMA model #13                                    #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggdist)
library(scales)
#library(ggh4x)

# load custom functions
source("./visualisation/code/visualisation_functions.R")

#-------------------------------------------------------------------------------
# 1. Load and clean data
#-------------------------------------------------------------------------------

# load plot data
almp_nma_model_thirteen_study_level_subgroup_sex_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_study_level_subgroup_sex_draws.RDS"
almp_nma_model_thirteen_study_level_subgroup_sex_draws <- readRDS(
  almp_nma_model_thirteen_study_level_subgroup_sex_draws_location
)

almp_nma_model_thirteen_study_level_subgroup_sex_summary_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_study_level_subgroup_sex_summary.RDS"
almp_nma_model_thirteen_study_level_subgroup_sex_summary <- readRDS(
  almp_nma_model_thirteen_study_level_subgroup_sex_summary_location
)

almp_nma_model_thirteen_differential_treatment_effect_sex_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_differential_treatment_effect_sex_draws.RDS"
almp_nma_model_thirteen_differential_treatment_effect_sex_draws <- readRDS(
  almp_nma_model_thirteen_differential_treatment_effect_sex_draws_location
)

almp_nma_model_thirteen_differential_treatment_effect_sex_summary_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_differential_treatment_effect_sex_summary.RDS"
almp_nma_model_thirteen_differential_treatment_effect_sex_summary <- readRDS(
  almp_nma_model_thirteen_differential_treatment_effect_sex_summary_location
)

# load component results to exclude where posterior_different_prior_flag == "No"
almp_nma_model_thirteen_component_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_component_draws.RDS"
almp_nma_model_thirteen_component_draws <- readRDS(
  almp_nma_model_thirteen_component_draws_location
)

# identify outcomes to exclude
posterior_flags <- almp_nma_model_thirteen_component_draws |>
  select(
    outcome,
    component,
    posterior_different_prior_flag
  ) |>
  distinct()

# join flags to subgroup plotting data and filter
almp_nma_model_thirteen_study_level_subgroup_sex_draws_filtered <- almp_nma_model_thirteen_study_level_subgroup_sex_draws |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    # not reporting other components
    !component == "Other Active Components",
    # filter outcomes where posterior is not different to prior
    posterior_different_prior_flag == "Yes",
    # drop outcome's that are otherwise sparse
    !outcome %in%
      c(
        "Currently Not in the Labour Force",
        "Employed Since Baseline"
      ),
    # drop outcome domains that are included for network stability
    !outcome_domain %in%
      c(
        "Total Income",
        "Labour Market Transitions"
      )
  ) |>
  # order outcomes
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Currently Employed",
        "Recent Employment",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Currently NEET",
        "Hours Worked",
        "Period Employed",
        "Labour Earnings",
        "Wages"
      ),
      ordered = TRUE
    )
  )

# join flags to subgroup summary data and filter
almp_nma_model_thirteen_study_level_subgroup_sex_summary_filtered <- almp_nma_model_thirteen_study_level_subgroup_sex_summary |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    # not reporting other components
    !component == "Other Active Components",
    # filter outcomes where posterior is not different to prior
    posterior_different_prior_flag == "Yes",
    # drop outcome's that are otherwise sparse
    !outcome %in%
      c(
        "Currently Not in the Labour Force",
        "Employed Since Baseline"
      ),
    # drop outcome domains that are included for network stability
    !outcome_domain %in%
      c(
        "Total Income",
        "Labour Market Transitions"
      )
  ) |>
  # order outcomes
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Currently Employed",
        "Recent Employment",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Currently NEET",
        "Hours Worked",
        "Period Employed",
        "Labour Earnings",
        "Wages"
      ),
      ordered = TRUE
    )
  )

almp_nma_model_thirteen_differential_treatment_effect_sex_draws_filtered <- almp_nma_model_thirteen_differential_treatment_effect_sex_draws |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    # not reporting other components
    !component == "Other Active Components",
    # filter outcomes where posterior is not different to prior
    posterior_different_prior_flag == "Yes",
    # drop outcome's that are otherwise sparse
    !outcome %in%
      c(
        "Currently Not in the Labour Force",
        "Employed Since Baseline"
      ),
    # drop outcome domains that are included for network stability
    !outcome_domain %in%
      c(
        "Total Income",
        "Labour Market Transitions"
      )
  ) |>
  # order outcomes
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Currently Employed",
        "Recent Employment",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Currently NEET",
        "Hours Worked",
        "Period Employed",
        "Labour Earnings",
        "Wages"
      ),
      ordered = TRUE
    )
  ) |>
  ungroup()

almp_nma_model_thirteen_differential_treatment_effect_sex_summary_filtered <- almp_nma_model_thirteen_differential_treatment_effect_sex_summary |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    # not reporting other components
    !component == "Other Active Components",
    # filter outcomes where posterior is not different to prior
    posterior_different_prior_flag == "Yes",
    # drop outcome's that are otherwise sparse
    !outcome %in%
      c(
        "Currently Not in the Labour Force",
        "Employed Since Baseline"
      ),
    # drop outcome domains that are included for network stability
    !outcome_domain %in%
      c(
        "Total Income",
        "Labour Market Transitions"
      )
  ) |>
  # order outcomes
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Completion",
        "Secondary School (ISCED 3) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion",
        "Currently Employed",
        "Recent Employment",
        "Currently Self-Employed",
        "Currently Unemployed",
        "Currently NEET",
        "Hours Worked",
        "Period Employed",
        "Labour Earnings",
        "Wages"
      ),
      ordered = TRUE
    )
  )

#-------------------------------------------------------------------------------
# 1. Basic Skills Training x study-level subgroup
#-------------------------------------------------------------------------------

# subset summary data for basic skills training and labour force status outcomes
almp_nma_model_thirteen_study_level_subgroup_sex_summary_data_basic_skills_training <- almp_nma_model_thirteen_study_level_subgroup_sex_summary_filtered |>
  filter(
    component == "Basic Skills Training",
    outcome_domain == "Labour Force Status"
  ) |>
  rename(
    effect = theta
  ) |>
  mutate(
    outcome = maintain_factor_order(outcome)
  )

# subset summary contrast data for basic skills training and labour force status outcomes
almp_nma_model_thirteen_differential_treatment_effect_sex_summary_data_basic_skills_training <- almp_nma_model_thirteen_differential_treatment_effect_sex_summary_filtered |>
  filter(
    component == "Basic Skills Training",
    outcome_domain == "Labour Force Status"
  ) |>
  rename(
    effect = contrast_1_minus_0
  ) |>
  mutate(
    outcome = maintain_factor_order(outcome)
  )

# subset plot data for basic skills training and labour force status outcomes
almp_nma_model_thirteen_study_level_subgroup_sex_plot_data_basic_skills_training <- almp_nma_model_thirteen_study_level_subgroup_sex_draws_filtered |>
  filter(
    component == "Basic Skills Training",
    outcome_domain == "Labour Force Status"
  ) |>
  rename(
    effect = theta
  ) |>
  mutate(
    outcome = maintain_factor_order(outcome)
  )

# subset contrast plot data for basic skills training and labour force status outcomes
almp_nma_model_thirteen_differential_treatment_effect_sex_plot_data_basic_skills_training <- almp_nma_model_thirteen_differential_treatment_effect_sex_draws_filtered |>
  filter(
    component == "Basic Skills Training",
    outcome_domain == "Labour Force Status"
  ) |>
  rename(
    effect = contrast_1_minus_0
  ) |>
  mutate(
    outcome = maintain_factor_order(outcome)
  )

# create subgroup x sex forest plot for basic skills training for labour force status
almp_nma_model_thirteen_study_level_subgroup_sex_forest_plot_basic_skills_training_labour_force_status_outcomes <- almp_nma_model_thirteen_study_level_subgroup_sex_plot_data_basic_skills_training |>
  ggplot(
    aes(
      x = effect,
      y = fct_rev(outcome)
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
    data = . %>%
      filter(
        outcome %in%
          c(
            "Currently Unemployed",
            "Currently NEET"
          )
      ),
    aes(
      fill = after_stat(ifelse(
        x >= -0.0,
        "negative_outcome",
        "positive_outcome"
      ))
    ),
    .width = c(0.95),
    colour = "#000000",
    alpha = 0.8,
    point_interval = "median_qi"
  ) +
  stat_halfeye(
    data = . %>%
      filter(
        !outcome %in%
          c(
            "Currently Unemployed",
            "Currently NEET"
          )
      ),
    aes(
      fill = after_stat(ifelse(
        x <= -0.0,
        "negative_outcome",
        "positive_outcome"
      ))
    ),
    .width = c(0.95),
    colour = "#000000",
    alpha = 0.8,
    point_interval = "median_qi"
  ) +
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_study_level_subgroup_sex_summary_data_basic_skills_training,
      is.numeric,
      round,
      2
    ),
    aes(
      label = str_glue("{effect} [{.lower},{.upper}]"),
      x = effect
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
    limits = c(-1.1, 1.1),
    breaks = c(-1, 0, 1)
  ) +
  # wrap facets
  facet_grid(
    . ~ subgroup,
    labeller = label_wrap_gen(width = 15),
    scales = "free_y",
    space = "free_y"
  ) +
  # specify colour scheme
  scale_fill_manual(
    values = c(
      "positive_outcome" = "#008744",
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # hide colour from legend
  guides(
    fill = guide_legend(
      override.aes = list(
        colour = NA
      )
    )
  ) +
  # specify labels
  labs(
    subtitle = "",
    x = "Posterior distributions with 95% credible intervals (Hedges' g)",
    y = ""
  ) +
  # set theme
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(
      size = 10,
      colour = "#000000"
    ),
    axis.text = element_text(
      size = 10,
      colour = "#000000"
    ),
    strip.text = element_text(
      size = 10,
      face = "bold"
    ),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    strip.clip = "off",
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    plot.subtitle = element_blank(),
    plot.title = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(t = 2, r = 15, b = 2, l = 2, unit = "pt")
  )

# create subgroup x sex contrast forest plot for basic skills training for labour force status
almp_nma_model_thirteen_differential_treatment_effect_sex_forest_plot_basic_skills_training_labour_force_status_outcomes <- almp_nma_model_thirteen_differential_treatment_effect_sex_plot_data_basic_skills_training |>
  ggplot(
    aes(
      x = effect,
      y = fct_rev(outcome)
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
    data = . %>%
      filter(
        outcome %in%
          c(
            "Currently Unemployed",
            "Currently NEET"
          )
      ),
    aes(
      fill = after_stat(ifelse(
        x >= -0.0,
        "better_male",
        "better_female"
      ))
    ),
    .width = c(0.95),
    colour = "#000000",
    alpha = 0.8,
    point_interval = "median_qi"
  ) +
  stat_halfeye(
    data = . %>%
      filter(
        !outcome %in%
          c(
            "Currently Unemployed",
            "Currently NEET"
          )
      ),
    aes(
      fill = after_stat(ifelse(
        x <= -0.0,
        "better_male",
        "better_female"
      ))
    ),
    .width = c(0.95),
    colour = "#000000",
    alpha = 0.8,
    point_interval = "median_qi"
  ) +
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_differential_treatment_effect_sex_summary_data_basic_skills_training,
      is.numeric,
      round,
      2
    ),
    aes(
      label = str_glue("{effect} [{.lower},{.upper}]"),
      x = effect
    ),
    hjust = "centre",
    nudge_y = -0.2,
    size = 3,
    color = "black"
  ) +
  # wrap facets
  facet_wrap(
    . ~ "Contrast (Female - Male)"
  ) +
  # wrap y-axis labels
  scale_y_discrete(
    labels = label_wrap(20)
  ) +
  scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.5, 0, 0.5)
  ) +
  # specify colour scheme
  scale_fill_manual(
    values = c(
      "better_female" = "#d9a637",
      "better_male" = "#542437"
    ),
    name = "Outcome Direction",
    labels = c(
      "better_female" = "Favours studies with all females",
      "better_male" = "Favours studies with all males"
    )
  ) +
  # hide colour from legend
  guides(
    fill = guide_legend(
      override.aes = list(
        colour = NA
      )
    )
  ) +
  # specify labels
  labs(
    subtitle = "",
    x = "Posterior distributions with 95% credible intervals (Hedges' g)",
    y = ""
  ) +
  # set theme
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(
      size = 10,
      colour = "#000000"
    ),
    axis.text = element_text(
      size = 10,
      colour = "#000000"
    ),
    strip.text = element_text(
      size = 10,
      face = "bold"
    ),
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.title = element_blank(),
    strip.clip = "off",
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    plot.subtitle = element_blank(),
    plot.title = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(t = 2, r = 15, b = 2, l = 2, unit = "pt"),
    axis.ticks = element_blank()
  )

# Combine the plots
almp_nma_model_thirteen_subgroup_sex_forest_plot_basic_skills_training_labour_force_status_outcomes <- almp_nma_model_thirteen_study_level_subgroup_sex_forest_plot_basic_skills_training_labour_force_status_outcomes +
  almp_nma_model_thirteen_differential_treatment_effect_sex_forest_plot_basic_skills_training_labour_force_status_outcomes +
  # aligns axis titles
  plot_layout(
    axis_titles = "collect",
    widths = c(2, 1)
  ) +
  plot_annotation(
    caption = "Values report median effect size [95% Credible Interval]",
    theme = theme(
      plot.caption = element_text(
        size = 10,
        margin = margin(t = 5)
      )
    )
  )
almp_nma_model_thirteen_subgroup_sex_forest_plot_basic_skills_training_labour_force_status_outcomes
