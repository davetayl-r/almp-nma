#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 17/09/2025                                                                           #
# Purpose: visualise study design type                                                       #
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
# 1. Visualise distribution of study designs
#-------------------------------------------------------------------------------

# prepare plot data
almp_nma_study_design_distribution_plot_data <- almp_nma_summary_visualisation_data |>
  select(
    study_id,
    study_design_type,
    study_design_detail
  ) |>
  distinct() |>
  group_by(
    study_design_type,
    study_design_detail
  ) |>
  tally(name = "count_studies") %>% # counts per location
  ungroup() |>
  mutate(
    proportion_design_detail = count_studies / sum(count_studies)
  ) |>
  arrange(
    desc(count_studies)
  ) |>
  # rename study design type
  mutate(
    study_design_detail = recode(
      study_design_detail,
      "RCT" = "Randomised controlled trial",
      "PSM" = "Propensity score matching",
      "DID" = "Difference-in-differences",
      "FE Reg" = "Fixed-effects regression",
      "IV" = "Instrumental variables",
      "RDD" = "Regression discontinuity design",
      "IPTW" = "Inverse probability of treatment weighting",
      "CEM" = "Coarsened exact matching",
      "Matching" = "Matching",
      "Natural experiment" = "Natural experiment",
      "Regression-adjustment" = "Regression adjustment",
      .default = study_design_detail
    )
  )

total_studies_design <- almp_nma_study_design_distribution_plot_data |>
  group_by(
    study_design_type
  ) |>
  summarise(
    total = sum(count_studies)
  ) |>
  pivot_wider(
    names_from = study_design_type,
    values_from = total
  )

total_studies <- almp_nma_study_design_distribution_plot_data |>
  summarise(
    total = sum(count_studies)
  ) |>
  as.numeric()

# plot distribution of study type
almp_nma_study_type_distribution_plot <- almp_nma_study_design_distribution_plot_data |>
  # recode labels for plot
  mutate(
    study_design_type = case_when(
      study_design_type == "Design-based identification" ~
        paste(
          "Design-based identification\n(n=",
          total_studies_design$`Design-based identification`,
          ")",
          sep = ""
        ),
      study_design_type == "Randomised design" ~
        paste(
          "Randomised design\n(n=",
          total_studies_design$`Randomised design`,
          ")",
          sep = ""
        ),
      study_design_type == "Selection on observables" ~
        paste(
          "Selection on observables\n(n=",
          total_studies_design$`Selection on observables`,
          ")",
          sep = ""
        )
    ),
    study_design_type = factor(
      study_design_type,
      levels = c(
        "Randomised design\n(n=72)",
        "Selection on observables\n(n=71)",
        "Design-based identification\n(n=15)"
      ),
      ordered = TRUE
    )
  ) |>
  ggplot(
    aes(
      x = reorder(study_design_detail, -proportion_design_detail),
      y = proportion_design_detail,
      fill = study_design_type
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(label = count_studies),
    vjust = -0.5,
    size = 3
  ) +
  facet_grid(
    . ~ study_design_type,
    scales = "free_x",
    space = "free_x"
  ) +
  scale_fill_manual(
    values = c(
      "#69C2C9",
      "#7D2248",
      "#BFB800"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "",
    y = "Per cent of total studies",
    caption = paste0("Total studies:", total_studies)
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 45, unit = "pt"),
    axis.text.x = element_text(hjust = 1, angle = 30),
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
      hjust = 0.5
    ),
    strip.clip = "off"
  )

almp_nma_study_type_distribution_plot

# export plot
ggsave(
  plot = almp_nma_study_type_distribution_plot,
  filename = "./visualisation/output/almp_nma_study_type_distribution_plot.png",
  height = 6,
  width = 8,
  device = "png",
  type = "cairo-png"
)
