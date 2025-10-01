#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 26/09/2025                                                                           #
# Purpose: Visualise additive NMA model heterogeneity                                        #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)
library(ggdist)
library(scales)

# load custom functions
source("./visualisation/code/visualisation_functions.R")

#-------------------------------------------------------------------------------
# 1. Load and clean data
#-------------------------------------------------------------------------------

almp_nma_additive_model_tau_component_study_design_draws_location <- "./visualisation/inputs/almp_nma_additive_model_tau_component_design_draws.RDS"
almp_nma_additive_model_tau_component_study_design_draws <- readRDS(
  almp_nma_additive_model_tau_component_study_design_draws_location
)

almp_nma_additive_model_tau_component_study_design_summary_location <- "./visualisation/inputs/almp_nma_additive_model_tau_component_design_summary.RDS"
almp_nma_additive_model_tau_component_study_design_summary <- readRDS(
  almp_nma_additive_model_tau_component_study_design_summary_location
)

almp_nma_additive_model_tau_study_design_summary_location <- "./visualisation/inputs/almp_nma_additive_model_tau_design_summary.RDS"
almp_nma_additive_model_tau_study_design_summary <- readRDS(
  almp_nma_additive_model_tau_study_design_summary_location
)

#-------------------------------------------------------------------------------
# 2. Visualise study-level heterogeneity x study design
#-------------------------------------------------------------------------------

# create labels for plotting
almp_nma_additive_model_tau_study_design_summary <- almp_nma_additive_model_tau_study_design_summary |>
  ungroup() |>
  rename(
    median = tau,
    lower = .lower,
    upper = .upper
  ) |>
  mutate(
    design = factor(
      design,
      levels = c(
        "Randomised design",
        "Design-based identification",
        "Selection on observables"
      ),
      ordered = TRUE
    ),
    facet_label = sprintf(
      "paste('%s', '\n', tau==%.3f, ' (95%% CrI [', %.3f, ', ', %.3f, '])')",
      as.character(design),
      median,
      lower,
      upper
    ),
    facet_label = factor(
      facet_label,
      levels = unique(facet_label[order(design)])
    )
  )

# merge plot data label to the draws
almp_nma_additive_model_tau_study_design_plot_data <- almp_nma_additive_model_tau_component_study_design_draws |>
  left_join(
    almp_nma_additive_model_tau_study_design_summary |>
      select(
        design,
        facet_label
      ),
    by = "design"
  )

# plot tau distribution: each panel shows the posterior for study-level heterogeneity by study design
almp_nma_additive_model_tau_study_design_distribution_plot <- almp_nma_additive_model_tau_study_design_plot_data |>
  ungroup() |>
  select(
    -component
  ) |>
  ggplot(
    aes(
      x = tau,
      fill = design
    )
  ) +
  stat_halfeye(
    .width = 0.95,
    colour = "#2d3239ff",
    point_interval = median_qi,
    slab_alpha = 0.7
  ) +
  geom_vline(
    data = almp_nma_additive_model_tau_study_design_summary,
    aes(xintercept = median),
    inherit.aes = FALSE,
    colour = "#2d3239ff",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  facet_wrap(
    ~facet_label,
    ncol = 1,
    labeller = label_parsed
  ) +
  scale_fill_manual(
    values = c("#8B4B6B", "#6FAADB", "#95C47C")
  ) +
  coord_cartesian(
    xlim = c(0, 1),
    clip = "off"
  ) +
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
  plot = almp_nma_additive_model_tau_study_design_distribution_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_study_design_distribution_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 3. Visualise study-level heterogeneity x component + study design
#-------------------------------------------------------------------------------

# Basic Skills Training

almp_nma_additive_model_tau_basic_skills_training_study_design_plot <- create_tau_distribution_plot(
  component_name = "Basic Skills Training",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_basic_skills_training_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_basic_skills_training_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Behavioural Skills Training

almp_nma_additive_model_tau_behavioural_skills_training_study_design_plot <- create_tau_distribution_plot(
  component_name = "Behavioural Skills Training",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_behavioural_skills_training_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_behavioural_skills_training_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Employment Coaching

almp_nma_additive_model_tau_employment_coaching_study_design_plot <- create_tau_distribution_plot(
  component_name = "Employment Coaching",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_employment_coaching_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_employment_coaching_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Employment Counselling

almp_nma_additive_model_tau_employment_counselling_study_design_plot <- create_tau_distribution_plot(
  component_name = "Employment Counselling",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_employment_counselling_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_employment_counselling_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Financial Assistance

almp_nma_additive_model_tau_financial_assistance_study_design_plot <- create_tau_distribution_plot(
  component_name = "Financial Assistance",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_financial_assistance_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_financial_assistance_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Job Search Assistance

almp_nma_additive_model_tau_job_search_assistance_study_design_plot <- create_tau_distribution_plot(
  component_name = "Job Search Assistance",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_job_search_assistance_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_job_search_assistance_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Job Search Preparation

almp_nma_additive_model_tau_job_search_preparation_study_design_plot <- create_tau_distribution_plot(
  component_name = "Job Search Preparation",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_job_search_preparation_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_job_search_preparation_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Paid Temporary Work Experience

almp_nma_additive_model_tau_paid_temporary_work_experience_study_design_plot <- create_tau_distribution_plot(
  component_name = "Paid Temporary Work Experience",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_paid_temporary_work_experience_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_paid_temporary_work_experience_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Public Works

almp_nma_additive_model_tau_public_works_study_design_plot <- create_tau_distribution_plot(
  component_name = "Public Works",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_public_works_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_public_works_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Self-Employment Support

almp_nma_additive_model_tau_self_employment_support_study_design_plot <- create_tau_distribution_plot(
  component_name = "Self-Employment Support",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_self_employment_support_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_self_employment_support_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Soft Skills Training

almp_nma_additive_model_tau_soft_skills_training_study_design_plot <- create_tau_distribution_plot(
  component_name = "Soft Skills Training",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_soft_skills_training_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_soft_skills_training_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Technical Skills Training (Off-the-Job)

almp_nma_additive_model_tau_technical_skills_training_off_the_job_study_design_plot <- create_tau_distribution_plot(
  component_name = "Technical Skills Training (Off-the-Job)",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_technical_skills_training_off_the_job_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_technical_skills_training_off_the_job_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Technical Skills Training (On-the-Job)

almp_nma_additive_model_tau_technical_skills_training_on_the_job_study_design_plot <- create_tau_distribution_plot(
  component_name = "Technical Skills Training (On-the-Job)",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_technical_skills_training_on_the_job_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_technical_skills_training_on_the_job_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Unpaid Temporary Work Experience

almp_nma_additive_model_tau_unpaid_temporary_work_experience_study_design_plot <- create_tau_distribution_plot(
  component_name = "Unpaid Temporary Work Experience",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_unpaid_temporary_work_experience_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_unpaid_temporary_work_experience_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)

# Wage Subsidies

almp_nma_additive_model_tau_wage_subsidies_study_design_plot <- create_tau_distribution_plot(
  component_name = "Wage Subsidies",
  summary_data = almp_nma_additive_model_tau_component_study_design_summary,
  plot_data = almp_nma_additive_model_tau_component_study_design_draws
)

# export plot
ggsave(
  plot = almp_nma_additive_model_tau_wage_subsidies_study_design_plot,
  filename = "./visualisation/output/heterogeneity/almp_nma_additive_model_tau_wage_subsidies_study_design_plot.png",
  height = 3.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)
