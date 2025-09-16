#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 16/09/2025                                                                           #
# Purpose: visualise distribution of outcome timing                                          #
#============================================================================================#

# load required packages
library(tidyverse)
library(forcats)
library(ggplot2)
library(ggridges)

# read data
almp_nma_additive_model_data_location <- "./analysis/inputs/almp_nma_additive_model_data.RDS"
almp_nma_additive_model_data <- readRDS(almp_nma_additive_model_data_location)

#--------------------------------------------------------------------------------
# 1. Prepare plot data
#--------------------------------------------------------------------------------

outcome_timing_distribution_plot_data <- almp_nma_additive_model_data |>
  select(
    study,
    outcome_domain,
    outcome_timing
  ) |>
  filter(
    !is.na(outcome_timing)
  ) |>
  distinct(
    study,
    outcome_domain,
    outcome_timing
  ) |>
  mutate(
    outcome_timing = round(outcome_timing)
  )

#--------------------------------------------------------------------------------
# 2. Plot histogram
#--------------------------------------------------------------------------------

outcome_timing_distribution_histogram <- outcome_timing_distribution_plot_data |>
  ggplot(
    aes(x = outcome_timing)
  ) +
  geom_histogram(
    binwidth = 6,
    boundary = 0,
    closed = "right",
    colour = "white",
    linewidth = 0.2,
    fill = "#7D2248"
  ) +
  facet_wrap(
    ~outcome_domain,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_x_continuous(
    "Months after baseline",
    breaks = seq(0, 240, 12), #minor_breaks = seq(0, 240, 6),
    limits = c(0, 60)
  ) +
  labs(
    caption = "Note: Timing is censored at 60 months",
    y = "Number of observations"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    strip.text = element_text(
      face = "bold",
      hjust = 0.5,
      size = 10
    ),
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = outcome_timing_distribution_histogram,
  filename = "./visualisation/output/almp_nma_outcome_timing_distribution_histogram.png",
  height = 7,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#--------------------------------------------------------------------------------
# 3. Densities
#--------------------------------------------------------------------------------

outcome_timing_distribution_density_plot <- outcome_timing_distribution_plot_data |>
  ggplot(
    aes(
      x = outcome_timing,
      y = fct_reorder(outcome_domain, outcome_timing, .fun = median)
    )
  ) +
  geom_density_ridges(
    scale = 1.1,
    rel_min_height = 0.01,
    jittered_points = FALSE,
    fill = "#d3a8baff",
    colour = "#7D2248",
    linewidth = 0.3
  ) +
  scale_x_continuous(
    "Months after baseline",
    breaks = seq(0, 240, 12),
    limits = c(0, 60)
  ) +
  labs(
    y = "",
    caption = "Note: Timing is censored at 60 months"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    strip.text = element_text(
      face = "bold",
      hjust = 0.5,
      size = 10
    ),
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = outcome_timing_distribution_density_plot,
  filename = "./visualisation/output/almp_nma_outcome_timing_distribution_densities.png",
  height = 7,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#--------------------------------------------------------------------------------
# 4. Empirical Cumulative Distribution Function (ECDF)
#--------------------------------------------------------------------------------

outcome_timing_distribution_ecdf_plot <- outcome_timing_distribution_plot_data |>
  ggplot(
    aes(
      x = outcome_timing
    )
  ) +
  stat_ecdf(
    geom = "step",
    linewidth = 0.6,
    colour = "#7D2248"
  ) +
  facet_wrap(
    ~outcome_domain,
    ncol = 2
  ) +
  scale_x_continuous(
    "Months after baseline",
    breaks = seq(0, 240, 12),
    limits = c(0, 60)
  ) +
  labs(
    y = "Cumulative share of observations",
    caption = "Note: Timing is censored at 60 months"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    strip.text = element_text(
      face = "bold",
      hjust = 0.5,
      size = 10
    ),
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = outcome_timing_distribution_ecdf_plot,
  filename = "./visualisation/output/almp_nma_outcome_timing_distribution_ecdf.png",
  height = 7,
  width = 7,
  device = "png",
  type = "cairo-png"
)

#--------------------------------------------------------------------------------
# 5. Outcome window
#--------------------------------------------------------------------------------

# specify targets by outcome domain
outcome_domain_targets <- tribble(
  ~outcome_domain,
  ~target,
  ~window,
  "Labour Force Status",
  #12,
  #3,
  24,
  6,
  "Education and Skills",
  24,
  6,
  "Employment Duration",
  #12,
  #3,
  24,
  6,
  "Employment compensation",
  #12,
  #3,
  24,
  6,
  "Hours Worked",
  #12,
  #3,
  24,
  6,
  "Total Income",
  #12,
  #3,
  24,
  6,
  "Labour Market Transitions",
  #12,
  #3,
  24,
  6,
)

# bring targets onto the data
outcome_candidates <- outcome_timing_distribution_plot_data |>
  left_join(
    outcome_domain_targets,
    by = "outcome_domain"
  ) |>
  # if a domain isn't in outcome_domain_targets, fall back to a global rule (24±6)
  mutate(
    target = if_else(is.na(target), 24, target),
    window = if_else(is.na(window), 6, window),
    dist = abs(outcome_timing - target),
    within = dist <= window
  )

# # choose one time per study × domain using the rule:
# prefer within-window; among those, pick min dist; break ties by earlier time.
outcome_selected <- outcome_candidates |>
  group_by(study, outcome_domain) |>
  arrange(desc(within), dist, outcome_timing) |>
  slice(1) |>
  ungroup() |>
  mutate(outside_window = !within)

# plot timing window plot
outcome_distribution_window_plot <- ggplot() +
  # window band per facet
  geom_rect(
    data = outcome_domain_targets,
    aes(
      xmin = pmax(target - window, 0),
      xmax = target + window,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "#91b49dff",
    colour = NA
  ) +
  # anchor line
  geom_vline(
    data = outcome_domain_targets,
    aes(xintercept = target),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.7
  ) +
  # reference lines (optional)
  geom_vline(
    xintercept = ref_lines,
    linetype = "dotted",
    linewidth = 0.25,
    alpha = 0.4
  ) +
  # all observed timings (light)
  geom_point(
    data = outcome_candidates,
    aes(
      x = outcome_timing,
      y = fct_reorder(study, outcome_timing, .fun = median)
    ),
    size = 0.9,
    alpha = 0.35,
    colour = "grey35"
  ) +
  # highlight the outcome_selected observation (ringed point)
  geom_point(
    data = outcome_selected,
    aes(
      x = outcome_timing,
      y = fct_reorder(study, outcome_timing, .fun = median),
      fill = outside_window
    ),
    shape = 21,
    size = 2.0,
    stroke = 0.6,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c(`TRUE` = "white", `FALSE` = "grey20"),
    labels = c(`FALSE` = "Within window", `TRUE` = "Outside window"),
    name = NULL,
    guide = "legend"
  ) +
  facet_wrap(
    ~outcome_domain,
    ncol = 4
    #  , scales = "free_y"
  ) +
  scale_x_continuous(
    "Months after baseline",
    breaks = seq(0, 60, 12),
    limits = c(0, 60),
    expand = expansion(mult = c(0, .02))
  ) +
  labs(
    y = NULL,
    caption = "Dashed line = anchor; Green band = anchor window"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(
      face = "bold",
      hjust = 0.5,
      size = 10
    ),
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = outcome_distribution_window_plot,
  filename = "./visualisation/output/almp_nma_outcome_timing_window.png",
  height = 12,
  width = 10,
  device = "png",
  type = "cairo-png"
)
