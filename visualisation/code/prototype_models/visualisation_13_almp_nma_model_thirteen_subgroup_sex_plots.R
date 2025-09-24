#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 24/09/2025                                                                           #
# Purpose: Visualise subgroup results x sex NMA model #13                                    #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)
library(ggdist)
library(scales)
library(ggh4x)

#-------------------------------------------------------------------------------
# 1. Load and clean data
#-------------------------------------------------------------------------------

# load plot data
almp_nma_model_thirteen_study_level_subgroup_sex_draws_location <- "./visualisation/inputs/almp_nma_model_thirteen_study_level_subgroup_sex_draws.RDS"
almp_nma_model_thirteen_study_level_subgroup_sex_draws <- readRDS(
  almp_nma_model_thirteen_study_level_subgroup_sex_draws_location
)

almp_nma_model_thirteen_study_level_subgroup_sex_summary_location <- "./visualisation/inputs/almp_nma_model_thirteen_study_level_subgroup_sex_summary.RDS"
almp_nma_model_thirteen_study_level_subgroup_sex_summary <- readRDS(
  almp_nma_model_thirteen_study_level_subgroup_sex_summary_location
)

almp_nma_model_thirteen_differential_treatment_effect_sex_draws_location <- "./visualisation/inputs/almp_nma_model_thirteen_differential_treatment_effect_sex_draws.RDS"
almp_nma_model_thirteen_differential_treatment_effect_sex_draws <- readRDS(
  almp_nma_model_thirteen_differential_treatment_effect_sex_draws_location
)

almp_nma_model_thirteen_differential_treatment_effect_sex_summary_location <- "./visualisation/inputs/almp_nma_model_thirteen_differential_treatment_effect_sex_summary.RDS"
almp_nma_model_thirteen_differential_treatment_effect_sex_summary <- readRDS(
  almp_nma_model_thirteen_differential_treatment_effect_sex_summary_location
)

# load component results to exclude where posterior_different_prior_flag == "No"
almp_nma_model_thirteen_component_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_thirteen_component_draws.RDS"
almp_nma_model_thirteen_component_draws <- readRDS(
  almp_nma_model_thirteen_component_draws_location
)

main_model_names <- levels(factor(
  almp_nma_model_thirteen_component_draws$component
))
subgroup_model_names <- levels(factor(
  almp_nma_model_thirteen_study_level_subgroup_sex_draws$component
))

posterior_flags <- almp_nma_model_thirteen_component_draws |>
  select(
    outcome,
    component,
    posterior_different_prior_flag
  ) |>
  distinct()

almp_nma_model_thirteen_study_level_subgroup_sex_draws_flagged <- almp_nma_model_thirteen_study_level_subgroup_sex_draws |>
  left_join(
    posterior_flags,
    by = c(
      "outcome",
      "component"
    )
  ) |>
  # drop data we're not interested in
  filter(
    !component == "Other Active Component"
  )

#-------------------------------------------------------------------------------
# 1. Basic Skills Training x study-level subgroup
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_thirteen_forest_plot_data_labour_market_outcomes <- almp_nma_model_thirteen_study_level_subgroup_sex_draws |>
  filter(
    outcome_domain == "Labour Force Status",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes",
    outcome %in% c("Currently Employed", "Currently Unemployed")
  ) |>
  mutate(
    #outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        "Currently Employed",
        #"Recent Employment",
        #"Employed Since Baseline",
        #"Currently Not in the Labour Force",
        "Currently Unemployed" #,
        #"Currently NEET",
        #"Currently Self-Employed"
      ),
      ordered = TRUE
    )
  ) |>
  rename(
    effect = theta
  )

almp_nma_model_thirteen_forest_plot_labels_labour_market_outcomes <- almp_nma_model_thirteen_component_summary |>
  filter(
    outcome_domain == "Labour Force Status",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes",
    outcome %in% c("Currently Employed", "Currently Unemployed")
  ) |>
  mutate(
    #outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        "Currently Employed",
        #"Recent Employment",
        #"Employed Since Baseline",
        #"Currently Not in the Labour Force",
        "Currently Unemployed" #,
        #"Currently NEET",
        #"Currently Self-Employed"
      ),
      ordered = TRUE
    )
  ) |>
  rename(
    effect = theta
  )

# create forest plot
almp_nma_model_thirteen_forest_plot_labour_market_outcomes <- almp_nma_model_thirteen_forest_plot_data_labour_market_outcomes |>
  ggplot(
    aes(
      x = effect,
      y = reorder(outcome, as.numeric(outcome), decreasing = TRUE)
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
    alpha = 0.7,
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
    alpha = 0.7,
    point_interval = "median_qi"
  ) +
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_forest_plot_labels_labour_market_outcomes,
      is.numeric,
      round,
      3
    ),
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
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months on Labour Force Status outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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
    legend.position = "bottom",
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = almp_nma_model_thirteen_forest_plot_labour_market_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_forest_plot_labour_market_outcomes.png",
  height = 5,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 3. Visualise Employment compensation outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_thirteen_forest_plot_data_employment_compensation_outcomes <- almp_nma_model_thirteen_component_draws |>
  filter(
    outcome_domain == "Employment Compensation",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

almp_nma_model_thirteen_forest_plot_labels_employment_compensation_outcomes <- almp_nma_model_thirteen_component_summary |>
  filter(
    outcome_domain == "Employment Compensation",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

# create forest plot
almp_nma_model_thirteen_forest_plot_employment_compensation_outcomes <- almp_nma_model_thirteen_forest_plot_data_employment_compensation_outcomes |>
  ggplot(
    aes(
      x = effect,
      y = reorder(outcome, as.numeric(outcome), decreasing = TRUE)
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
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_forest_plot_labels_employment_compensation_outcomes,
      is.numeric,
      round,
      3
    ),
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
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months on Employment Compensation outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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

# export plot
ggsave(
  plot = almp_nma_model_thirteen_forest_plot_employment_compensation_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_forest_plot_employment_compensation_outcomes.png",
  height = 5,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 4. Visualise Employment Duration outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_thirteen_forest_plot_data_employment_duration_outcomes <- almp_nma_model_thirteen_component_draws |>
  filter(
    outcome_domain == "Employment Duration",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

almp_nma_model_thirteen_forest_plot_labels_employment_duration_outcomes <- almp_nma_model_thirteen_component_summary |>
  filter(
    outcome_domain == "Employment Duration",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

# create forest plot
almp_nma_model_thirteen_forest_plot_employment_duration_outcomes <- almp_nma_model_thirteen_forest_plot_data_employment_duration_outcomes |>
  ggplot(
    aes(
      x = effect,
      y = reorder(outcome, as.numeric(outcome), decreasing = TRUE)
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
        !outcome %in%
          c(
            "Period Unemployed"
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
    alpha = 0.7,
    point_interval = "median_qi"
  ) +
  stat_halfeye(
    data = . %>%
      filter(
        outcome %in%
          c(
            "Period Unemployed"
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
    alpha = 0.7,
    point_interval = "median_qi"
  ) +
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_forest_plot_labels_employment_duration_outcomes,
      is.numeric,
      round,
      3
    ),
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
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months on Employment Duration outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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

# export plot
ggsave(
  plot = almp_nma_model_thirteen_forest_plot_employment_duration_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_forest_plot_employment_duration_outcomes.png",
  height = 4,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 5. Visualise Education and Skills outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_thirteen_forest_plot_data_education_skills_outcomes <- almp_nma_model_thirteen_component_draws |>
  filter(
    outcome_domain == "Education and Skills",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes",
    outcome %in%
      c(
        "Secondary School (ISCED 3) Completion",
        "Bachelors Degree (ISCED 6) Participation"
      )
  ) |>
  mutate(
    #outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        #"Apprenticeship Participation",
        #"Occupational Licence Obtained",
        #"Secondary School (ISCED 3) Participation",
        "Secondary School (ISCED 3) Completion",
        #"Post-Secondary Non-Tertiary (ISCED 4) Participation",
        #"Post-Secondary Non-Tertiary (ISCED 4) Completion",
        #"Short-Cycle Tertiary (ISCED 5) Participation",
        #"Short-Cycle Tertiary (ISCED 5) Completion",
        "Bachelors Degree (ISCED 6) Participation" #,
        #"Bachelors Degree (ISCED 6) Completion"
      ),
      ordered = TRUE
    )
  ) |>
  rename(
    effect = theta
  )

almp_nma_model_thirteen_forest_plot_labels_education_skills_outcomes <- almp_nma_model_thirteen_component_summary |>
  filter(
    outcome_domain == "Education and Skills",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes",
    outcome %in%
      c(
        "Secondary School (ISCED 3) Completion",
        "Bachelors Degree (ISCED 6) Participation"
      )
  ) |>
  mutate(
    #outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        #"Apprenticeship Participation",
        #"Occupational Licence Obtained",
        #"Secondary School (ISCED 3) Participation",
        "Secondary School (ISCED 3) Completion",
        #"Post-Secondary Non-Tertiary (ISCED 4) Participation",
        #"Post-Secondary Non-Tertiary (ISCED 4) Completion",
        #"Short-Cycle Tertiary (ISCED 5) Participation",
        #"Short-Cycle Tertiary (ISCED 5) Completion",
        "Bachelors Degree (ISCED 6) Participation" #,
        #"Bachelors Degree (ISCED 6) Completion"
      ),
      ordered = TRUE
    )
  ) |>
  rename(
    effect = theta
  )

# create forest plot
almp_nma_model_thirteen_forest_plot_education_skills_outcomes <- almp_nma_model_thirteen_forest_plot_data_education_skills_outcomes |>
  ggplot(
    aes(
      x = effect,
      y = reorder(outcome, as.numeric(outcome), decreasing = TRUE)
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
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_forest_plot_labels_education_skills_outcomes,
      is.numeric,
      round,
      3
    ),
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
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months on Education and Skills outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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


# export plot
ggsave(
  plot = almp_nma_model_thirteen_forest_plot_education_skills_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_forest_plot_education_skills_outcomes.png",
  height = 5,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 6. Visualise Hours Worked outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_thirteen_forest_plot_data_hours_worked_outcomes <- almp_nma_model_thirteen_component_draws |>
  filter(
    outcome_domain == "Hours Worked",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

almp_nma_model_thirteen_forest_plot_labels_hours_worked_outcomes <- almp_nma_model_thirteen_component_summary |>
  filter(
    outcome_domain == "Hours Worked",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

# create forest plot
almp_nma_model_thirteen_forest_plot_hours_worked_outcomes <- almp_nma_model_thirteen_forest_plot_data_hours_worked_outcomes |>
  ggplot(
    aes(
      x = effect,
      y = outcome
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
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_forest_plot_labels_hours_worked_outcomes,
      is.numeric,
      round,
      3
    ),
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
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months on Hours Worked outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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


# export plot
ggsave(
  plot = almp_nma_model_thirteen_forest_plot_hours_worked_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_forest_plot_hours_worked_outcomes.png",
  height = 4,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 7. Visualise Labour Market Transitions outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_thirteen_forest_plot_data_labour_market_transitions <- almp_nma_model_thirteen_component_draws |>
  filter(
    outcome_domain == "Labour Market Transitions",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

almp_nma_model_thirteen_forest_plot_labels_labour_market_transitions_outcomes <- almp_nma_model_thirteen_component_summary |>
  filter(
    outcome_domain == "Labour Market Transitions",
    !component == "Other Active Components",
    posterior_different_prior_flag == "Yes"
  ) |>
  rename(
    effect = theta
  )

# create forest plot
almp_nma_model_thirteen_forest_plot_labour_market_transitions <- almp_nma_model_thirteen_forest_plot_data_labour_market_transitions |>
  ggplot(
    aes(
      x = effect,
      y = outcome
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
  # Add summary text labels
  geom_text(
    data = mutate_if(
      almp_nma_model_thirteen_forest_plot_labels_labour_market_transitions_outcomes,
      is.numeric,
      round,
      3
    ),
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
      "negative_outcome" = "#9e9b9bff"
    ),
    name = "Outcome Direction",
    labels = c(
      "positive_outcome" = "Favours Intervention",
      "negative_outcome" = "Favours Services as Usual"
    )
  ) +
  # specify labels
  labs(
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months on Labour Market Transitions from a\nBayesian CNMA for Employment and Education outcomes",
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


# export plot
ggsave(
  plot = almp_nma_model_thirteen_forest_plot_labour_market_transitions,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_forest_plot_labour_market_transitions.png",
  height = 6,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 9. Visualise study-level heterogeneity x study design and component
#-------------------------------------------------------------------------------

# create labels for plotting
almp_nma_model_thirteen_tau_component_design_summary <- almp_nma_model_thirteen_tau_component_design_summary |>
  rename(
    median = tau,
    lower = .lower,
    upper = .upper
  ) |>
  mutate(
    # create label
    facet_label = sprintf(
      "paste('%s', '\n', tau==%.3f, ' (95%% CrI [', %.3f, ', ', %.3f, '])')",
      as.character(component),
      median,
      lower,
      upper
    ),
    facet_label = forcats::fct_inorder(facet_label)
  )

# merge plot data label to the draws
almp_nma_model_thirteen_tau_component_study_design_plot_data <- almp_nma_model_thirteen_tau_component_design_draws |>
  left_join(
    almp_nma_model_thirteen_tau_component_design_summary |>
      select(
        design,
        component,
        facet_label
      ),
    by = c("component", "design")
  )

# plot tau distribution: each panel shows the posterior for study-level heterogeneity (τ) by study design and component
almp_nma_model_thirteen_tau_study_component_study_design_distribution_plot <- almp_nma_model_thirteen_tau_component_study_design_plot_data |>
  ggplot(
    aes(
      x = tau,
      fill = component,
      group = component
    )
  ) +
  stat_halfeye(
    .width = 0.95,
    colour = "#2d3239ff",
    point_interval = median_qi,
    slab_alpha = 0.5
  ) +
  geom_vline(
    data = almp_nma_model_thirteen_tau_component_design_summary,
    aes(xintercept = median),
    colour = "#2d3239ff",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  facet_grid(
    design ~ component #,
    #labeller = str_wrap(component, width = 25)
  ) +
  scale_fill_viridis_d(
    name = "design",
    option = "C"
  ) +
  lims(x = c(0, .75)) +
  labs(
    x = expression(tau),
    y = "Posterior density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = almp_nma_model_thirteen_tau_study_component_study_design_distribution_plot,
  filename = "./visualisation/output/prototype_models/almp_nma_model_thirteen_tau_component_study_design_distribution_plot.png",
  height = 7,
  width = 30,
  limitsize = FALSE,
  device = "png",
  type = "cairo-png"
)
