#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 17/09/2025                                                                           #
# Purpose: visualise location                                                                #
#============================================================================================#

# load required packages
library(tidyverse)
library(sf)
library(ggplot2)

# read visualisation data
almp_nma_summary_visualisation_data_location <- "./visualisation/inputs/almp_nma_summary_visualisation_data.RDS"
almp_nma_summary_visualisation_data <- readRDS(
  almp_nma_summary_visualisation_data_location
)

#-------------------------------------------------------------------------------
# 1. Visualise distribution of locations
#-------------------------------------------------------------------------------

# prepare plot data
almp_nma_location_distribution_plot_data <- almp_nma_summary_visualisation_data |>
  select(
    study_id,
    location
  ) |>
  distinct() |>
  pivot_longer(
    cols = location,
    names_to = "measure",
    values_to = "country"
  ) |>
  select(
    -measure
  ) |>
  group_by(
    country
  ) |>
  tally(name = "count_studies") %>% # counts per location
  ungroup() %>%
  mutate(
    proportion_country = count_studies / sum(count_studies)
  ) |>
  arrange(
    desc(count_studies)
  )

total_studies <- almp_nma_location_distribution_plot_data |>
  summarise(
    total = sum(count_studies)
  ) |>
  as.numeric()

# plot distribution location
almp_nma_location_distribution_plot <- almp_nma_location_distribution_plot_data |>
  ggplot(
    aes(
      x = reorder(country, -proportion_country),
      y = proportion_country
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = "#7D2248"
  ) +
  geom_text(
    aes(label = count_studies),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "",
    y = "Per cent of total studies",
    caption = paste0("Total studies:", total_studies)
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
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
  plot = almp_nma_location_distribution_plot,
  filename = "./visualisation/output/almp_nma_location_distribution_plot.png",
  height = 6,
  width = 8,
  device = "png",
  type = "cairo-png"
)
