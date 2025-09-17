#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 17/09/2025                                                                           #
# Purpose: visualise age and sex distributions                                               #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)

# read visualisation data
almp_nma_summary_visualisation_data_location <- "./visualisation/inputs/almp_nma_summary_visualisation_data.RDS"
almp_nma_summary_visualisation_data <- readRDS(
  almp_nma_summary_visualisation_data_location
)

#-------------------------------------------------------------------------------
# 1. Visualise proportion female
#-------------------------------------------------------------------------------

# prepare plot data
almp_nma_sex_distribution_plot_data <- almp_nma_summary_visualisation_data |>
  select(
    study_id,
    proportion_female_treatment,
    prop_female_centred
  ) |>
  rename(
    proportion_female = proportion_female_treatment
  ) |>
  distinct() |>
  pivot_longer(
    cols = c(proportion_female, prop_female_centred),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = dplyr::recode(
      measure,
      proportion_female = "Proportion female",
      prop_female_centred = "Proportion female (centred)"
    )
  )

# calculate mean of raw proportion
mean_prop_female <- almp_nma_sex_distribution_plot_data |>
  filter(
    measure == "Proportion female"
  ) |>
  summarise(
    mean = mean(value)
  ) |>
  as.numeric()

# declare reference lines (mean for raw, 0 for centred)
reference_lines_sex <- tibble::tibble(
  measure = c(
    "Proportion female",
    "Proportion female (centred)"
  ),
  x = c(mean_prop_female, 0)
)

# plot distribution female
almp_nma_sex_distribution_plot <- almp_nma_sex_distribution_plot_data |>
  ggplot(
    aes(
      x = value,
      fill = measure
    )
  ) +
  geom_histogram(
    bins = 25,
    alpha = 0.9
  ) +
  geom_vline(
    data = reference_lines_sex,
    aes(xintercept = x),
    linetype = "dashed"
  ) +
  facet_wrap(
    ~measure,
    scales = "free_x"
  ) +
  labs(
    caption = sprintf(
      "Dashed vertical line: mean of proportion female = %.3f",
      m_prop
    ),
    x = NULL,
    y = "Count"
  ) +
  scale_fill_manual(
    values = c(
      "#BFB800",
      "#69C2C9"
    )
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.2, "lines"),
    plot.background = element_rect(
      colour = "#FFFFFF"
    ),
    legend.title = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(
      face = "bold",
      hjust = 0
    )
  )

# export plot
ggsave(
  plot = almp_nma_sex_distribution_plot,
  filename = "./visualisation/output/almp_nma_sex_distribution_plot.png",
  height = 6,
  width = 8,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 1. Visualise average age at commencement
#-------------------------------------------------------------------------------

# prepare plot data
almp_nma_age_distribution_plot_data <- almp_nma_summary_visualisation_data |>
  select(
    study_id,
    study_age_mean,
    study_age_mean_centred,
    study_age_mean_centred_18
  ) |>
  distinct() |>
  pivot_longer(
    cols = starts_with("study_age_mean"),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = dplyr::recode(
      measure,
      study_age_mean = "Mean age at baseline",
      study_age_mean_centred = "Mean age at baseline (centred)",
      study_age_mean_centred_18 = "Mean age at baseline (centred on 18)"
    )
  )

# calculate mean of raw proportion
mean_age_baseline <- almp_nma_age_distribution_plot_data |>
  filter(
    measure == "Mean age at baseline"
  ) |>
  summarise(
    mean = mean(value)
  ) |>
  as.numeric()

# declare reference lines (mean for raw, 0 for centred)
reference_lines_age <- tibble::tibble(
  measure = c(
    "Mean age at baseline",
    "Mean age at baseline (centred)",
    "Mean age at baseline (centred on 18)"
  ),
  x = c(mean_age_baseline, 0, 0)
)

# plot distribution female
almp_nma_age_distribution_plot <- almp_nma_age_distribution_plot_data |>
  ggplot(
    aes(
      x = value,
      fill = measure
    )
  ) +
  geom_histogram(
    bins = 25,
    alpha = 0.9
  ) +
  geom_vline(
    data = reference_lines_age,
    aes(xintercept = x),
    linetype = "dashed"
  ) +
  facet_wrap(
    ~measure,
    scales = "free_x"
  ) +
  labs(
    caption = sprintf(
      "Dashed vertical line: mean of proportion female = %.3f",
      m_prop
    ),
    x = NULL,
    y = "Count"
  ) +
  scale_fill_manual(
    values = c(
      "#BFB800",
      "#69C2C9",
      "#7D2248"
    )
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.2, "lines"),
    plot.background = element_rect(
      colour = "#FFFFFF"
    ),
    legend.title = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(
      face = "bold",
      hjust = 0
    )
  )

# export plot
ggsave(
  plot = almp_nma_age_distribution_plot,
  filename = "./visualisation/output/almp_nma_age_distribution_plot.png",
  height = 6,
  width = 8,
  device = "png",
  type = "cairo-png"
)
