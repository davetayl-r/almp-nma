#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 19/09/2025                                                                           #
# Purpose: Visualise NMA model #8                                                            #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)
library(ggdist)
library(scales)
library(ggh4x)

# load data
almp_nma_model_eight_component_draws_location <- "./visualisation/inputs/prototype_models/almp_nma_model_eight_component_draws.RDS"
almp_nma_model_eight_component_draws <- readRDS(
  almp_nma_model_eight_component_draws_location
)

almp_nma_model_eight_component_summary_location <- "./visualisation/inputs/prototype_models/almp_nma_model_eight_component_summary.RDS"
almp_nma_model_eight_component_summary <- readRDS(
  almp_nma_model_eight_component_summary_location
)

#-------------------------------------------------------------------------------
# 1. Visualise Labour Force Status outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_eight_forest_plot_data_labour_market_outcomes <- almp_nma_model_eight_component_draws |>
  filter(
    outcome_domain == "Labour Force Status",
    !component == "Other Active Components"
  ) |>
  mutate(
    outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        "Currently Employed",
        "Recent Employment",
        "Employed Since Baseline",
        "Currently Not in the Labour Force",
        "Currently Unemployed",
        "Currently NEET",
        "Currently Self-Employed"
      ),
      ordered = TRUE
    )
  )

almp_nma_model_eight_forest_plot_labels_labour_market_outcomes <- almp_nma_model_eight_component_summary |>
  filter(
    outcome_domain == "Labour Force Status",
    !component == "Other Active Components"
  ) |>
  mutate(
    outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        "Currently Employed",
        "Recent Employment",
        "Employed Since Baseline",
        "Currently Not in the Labour Force",
        "Currently Unemployed",
        "Currently NEET",
        "Currently Self-Employed"
      ),
      ordered = TRUE
    )
  )

# create forest plot
almp_nma_model_eight_forest_plot_labour_market_outcomes <- almp_nma_model_eight_forest_plot_data_labour_market_outcomes |>
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
      almp_nma_model_eight_forest_plot_labels_labour_market_outcomes,
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
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months (± 6 months) on Labour Force Status outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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
  plot = almp_nma_model_eight_forest_plot_labour_market_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_eight_forest_plot_labour_market_outcomes.png",
  height = 10,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 2. Visualise Employment compensation outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_eight_forest_plot_data_employment_compensation_outcomes <- almp_nma_model_eight_component_draws |>
  filter(
    outcome_domain == "Employment Compensation",
    !component == "Other Active Components"
  )

almp_nma_model_eight_forest_plot_labels_employment_compensation_outcomes <- almp_nma_model_eight_component_summary |>
  filter(
    outcome_domain == "Employment Compensation",
    !component == "Other Active Components"
  )

# create forest plot
almp_nma_model_eight_forest_plot_employment_compensation_outcomes <- almp_nma_model_eight_forest_plot_data_employment_compensation_outcomes |>
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
      almp_nma_model_eight_forest_plot_labels_employment_compensation_outcomes,
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
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months (± 6 months) on Employment Compensation outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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
  plot = almp_nma_model_eight_forest_plot_employment_compensation_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_eight_forest_plot_employment_compensation_outcomes.png",
  height = 6,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 3. Visualise Employment Duration outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_eight_forest_plot_data_employment_duration_outcomes <- almp_nma_model_eight_component_draws |>
  filter(
    outcome_domain == "Employment Duration",
    !component == "Other Active Components"
  )

almp_nma_model_eight_forest_plot_labels_employment_duration_outcomes <- almp_nma_model_eight_component_summary |>
  filter(
    outcome_domain == "Employment Duration",
    !component == "Other Active Components"
  )

# create forest plot
almp_nma_model_eight_forest_plot_employment_duration_outcomes <- almp_nma_model_eight_forest_plot_data_employment_duration_outcomes |>
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
      almp_nma_model_eight_forest_plot_labels_employment_duration_outcomes,
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
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months (± 6 months) on Employment Duration outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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
  plot = almp_nma_model_eight_forest_plot_employment_duration_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_eight_forest_plot_employment_duration_outcomes.png",
  height = 5,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 4. Visualise Education and Skills outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_eight_forest_plot_data_education_skills_outcomes <- almp_nma_model_eight_component_draws |>
  filter(
    outcome_domain == "Education and Skills",
    !component == "Other Active Components"
  ) |>
  mutate(
    outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Participation",
        "Secondary School (ISCED 3) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion"
      ),
      ordered = TRUE
    )
  )

almp_nma_model_eight_forest_plot_labels_education_skills_outcomes <- almp_nma_model_eight_component_summary |>
  filter(
    outcome_domain == "Education and Skills",
    !component == "Other Active Components"
  ) |>
  mutate(
    outcome = fct_drop(outcome),
    outcome = factor(
      outcome,
      levels = c(
        "Apprenticeship Participation",
        "Occupational Licence Obtained",
        "Secondary School (ISCED 3) Participation",
        "Secondary School (ISCED 3) Completion",
        "Post-Secondary Non-Tertiary (ISCED 4) Participation",
        "Post-Secondary Non-Tertiary (ISCED 4) Completion",
        "Short-Cycle Tertiary (ISCED 5) Participation",
        "Short-Cycle Tertiary (ISCED 5) Completion",
        "Bachelors Degree (ISCED 6) Participation",
        "Bachelors Degree (ISCED 6) Completion"
      ),
      ordered = TRUE
    )
  )

# create forest plot
almp_nma_model_eight_forest_plot_education_skills_outcomes <- almp_nma_model_eight_forest_plot_data_education_skills_outcomes |>
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
      almp_nma_model_eight_forest_plot_labels_education_skills_outcomes,
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
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months (± 6 months) on Labour Force Status outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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
  plot = almp_nma_model_eight_forest_plot_education_skills_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_eight_forest_plot_education_skills_outcomes.png",
  height = 12,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 5. Visualise Hours Worked outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_eight_forest_plot_data_hours_worked_outcomes <- almp_nma_model_eight_component_draws |>
  filter(
    outcome_domain == "Hours Worked",
    !component == "Other Active Components"
  )

almp_nma_model_eight_forest_plot_labels_hours_worked_outcomes <- almp_nma_model_eight_component_summary |>
  filter(
    outcome_domain == "Hours Worked",
    !component == "Other Active Components"
  )

# create forest plot
almp_nma_model_eight_forest_plot_hours_worked_outcomes <- almp_nma_model_eight_forest_plot_data_hours_worked_outcomes |>
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
      almp_nma_model_eight_forest_plot_labels_hours_worked_outcomes,
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
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months (± 6 months) on Hours Worked outcomes from a\nBayesian CNMA for Employment and Education outcomes",
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
  plot = almp_nma_model_eight_forest_plot_hours_worked_outcomes,
  filename = "./visualisation/output/prototype_models/almp_nma_model_eight_forest_plot_hours_worked_outcomes.png",
  height = 4,
  width = 18,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 6. Visualise Labour Market Transitions outcomes
#-------------------------------------------------------------------------------

# subset data
almp_nma_model_eight_forest_plot_data_labour_market_transitions <- almp_nma_model_eight_component_draws |>
  filter(
    outcome_domain == "Labour Market Transitions",
    !component == "Other Active Components"
  )

almp_nma_model_eight_forest_plot_labels_labour_market_transitions_outcomes <- almp_nma_model_eight_component_summary |>
  filter(
    outcome_domain == "Labour Market Transitions",
    !component == "Other Active Components"
  )

# create forest plot
almp_nma_model_eight_forest_plot_labour_market_transitions <- almp_nma_model_eight_forest_plot_data_labour_market_transitions |>
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
      almp_nma_model_eight_forest_plot_labels_labour_market_transitions_outcomes,
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
    title = "Component-level effects of Active Labour Market Programs for young people in high-income countries at 24 months (± 6 months) on Labour Market Transitions from a\nBayesian CNMA for Employment and Education outcomes",
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
  plot = almp_nma_model_eight_forest_plot_labour_market_transitions,
  filename = "./visualisation/output/prototype_models/almp_nma_model_eight_forest_plot_labour_market_transitions.png",
  height = 4,
  width = 18,
  device = "png",
  type = "cairo-png"
)
