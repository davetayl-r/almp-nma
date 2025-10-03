#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 25/09/2025                                                                           #
# Purpose: Visualise outcome matrix for additive NMA model                                   #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)
library(scales)

# load data
almp_nma_additive_model_component_summary_location <- "./visualisation/inputs/almp_nma_additive_model_component_summary.RDS"
almp_nma_additive_model_component_summary <- readRDS(
  almp_nma_additive_model_component_summary_location
)

#-------------------------------------------------------------------------------
# 1. Summary outcome matrix
#-------------------------------------------------------------------------------

almp_nma_additive_model_component_outcome_matrix <- almp_nma_additive_model_component_summary |>
  filter(
    !component == "Other Active Components"
  ) |>
  na.omit() |>
  ggplot(
    aes(
      x = component,
      y = outcome,
      fill = posterior_different_prior_flag
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  facet_grid(
    outcome_domain ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_manual(
    values = c("Yes" = "#008744", "No" = "#D62d20"),
    name = "Posterior different from prior"
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.placement = "outside"
  )

# export plot
ggsave(
  plot = almp_nma_additive_model_component_outcome_matrix,
  filename = "./visualisation/output/almp_nma_additive_model_component_outcome_matrix.png",
  height = 9,
  width = 15,
  device = "png",
  type = "cairo-png"
)
