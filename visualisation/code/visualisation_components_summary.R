#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: visualise distribution of components                                              #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)

# load data
almp_nma_components_data_location <- "./visualisation/inputs/almp_nma_analysis_data_intervention_components.RDS"
almp_nma_components_data <- readRDS(almp_nma_components_data_location)

#-------------------------------------------------------------------------------
# 1. Create plot showing distribtution of intervention components as extracted
#-------------------------------------------------------------------------------

almp_nma_components_plot_data <- almp_nma_components_data |>
  # drop study that didn't make it into the final model
  filter(
    !study_id == "brunetti2017workplacetrainingprograms"
  ) |>
  pivot_longer(
    -study_id,
    names_to = "component",
    values_to = "count"
  ) |>
  mutate(
    group = case_when(
      str_detect(component, "int_") ~ "Intervention",
      str_detect(component, "com_") ~ "Comparison"
    ),
    group = factor(group, levels = c("Intervention", "Comparison")),
    component = str_remove_all(component, "int_"),
    component = str_remove_all(component, "com_"),
    component = case_when(
      component == "basic_skills_training" ~ "Basic Skills Training",
      component == "soft_skills_training" ~ "Soft Skills Training",
      component == "behavioural_skills_training" ~
        "Behavioural Skills Training",
      component == "job_specific_technical_skills_off_job_training" ~
        "Job Specific Technical Skills (Off-Job Training)",
      component == "business_skills_training" ~ "Business Skills Training",
      component == "business_advisory_and_mentoring" ~
        "Business Advisory and Mentoring",
      component == "financial_and_start_up_support" ~
        "Financial and Start-Up Support",
      component == "job_search_preparation" ~ "Job Search Preparation",
      component == "job_search_assistance" ~ "Job Search Assistance",
      component == "employment_counselling" ~ "Employment Counselling",
      component == "employment_coaching" ~ "Employment Coaching",
      component == "financial_assistance" ~ "Financial Assistance",
      component == "job_specific_technical_skills_on_job_training" ~
        "Job Specific Technical Skills (On-Job Training)",
      component == "paid_temporary_work_experience" ~
        "Paid Temporary Work Experience",
      component == "unpaid_temporary_work_experience" ~
        "Unpaid Temporary Work Experience",
      component == "wage_subsidies" ~ "Wage Subsidies",
      component == "public_works" ~ "Public Works",
      component == "other_active_component_nec" ~ "Other Active Component",
      component == "services_as_usual" ~ "Services as Usual",
      TRUE ~ component
    ),
    component = factor(
      component,
      levels = c(
        "Basic Skills Training",
        "Soft Skills Training",
        "Behavioural Skills Training",
        "Job Specific Technical Skills (Off-Job Training)",
        "Business Skills Training",
        "Business Advisory and Mentoring",
        "Financial and Start-Up Support",
        "Job Search Preparation",
        "Job Search Assistance",
        "Employment Counselling",
        "Employment Coaching",
        "Financial Assistance",
        "Job Specific Technical Skills (On-Job Training)",
        "Paid Temporary Work Experience",
        "Unpaid Temporary Work Experience",
        "Wage Subsidies",
        "Public Works",
        "Other Active Component",
        "Services as Usual"
      ),
      ordered = TRUE
    ),
    cluster = case_when(
      component %in%
        c(
          "Basic Skills Training",
          "Soft Skills Training",
          "Behavioural Skills Training",
          "Job Specific Technical Skills (Off-Job Training)"
        ) ~
        "Skills Development",
      component %in%
        c(
          "Business Skills Training",
          "Business Advisory and Mentoring",
          "Financial and Start-Up Support"
        ) ~
        "Self-Employment Support",
      component %in%
        c(
          "Job Search Preparation",
          "Job Search Assistance",
          "Employment Counselling",
          "Employment Coaching",
          "Financial Assistance"
        ) ~
        "Employment Services",
      component %in%
        c(
          "Job Specific Technical Skills (On-Job Training)",
          "Paid Temporary Work Experience",
          "Unpaid Temporary Work Experience"
        ) ~
        "Employment Experience",
      component %in% c("Wage Subsidies", "Public Works") ~
        "Subsidised Employment",
      component == "Other Active Component" ~ "Other",
      TRUE ~ "Other"
    ),
    cluster = factor(
      cluster,
      levels = c(
        "Skills Development",
        "Self-Employment Support",
        "Employment Services",
        "Employment Experience",
        "Subsidised Employment",
        "Other"
      ),
      ordered = TRUE
    )
  ) |>
  group_by(
    component,
    group,
    cluster
  ) |>
  summarise(
    count = sum(count)
  )

almp_nma_components_plot <- almp_nma_components_plot_data |>
  ggplot(
    aes(
      x = component,
      y = count,
      group = group,
      fill = group
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(label = ifelse(count > 0, count, "")),
    vjust = -0.5,
    size = 3
  ) +
  facet_grid(
    group ~ cluster,
    scales = "free_x",
    space = "free_x",
    labeller = labeller(
      cluster = label_wrap_gen(width = 12),
      group = label_wrap_gen(width = 10)
    )
  ) +
  labs(
    y = "Studies containing each component",
    x = ""
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    values = c("Intervention" = "#69C2C9", "Comparison" = "#7D2248")
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    legend.position = "none",
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 25, unit = "pt"),
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
      hjust = 0.5,
      size = 10
    ),
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = almp_nma_components_plot,
  filename = "./visualisation/output/almp_nma_components_plot.png",
  height = 7,
  width = 8,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 2. Create plot showing distribtution of consolidated intervention components
#-------------------------------------------------------------------------------

almp_nma_components_consolidated_plot_data <- almp_nma_components_data |>
  mutate(
    int_self_employment_support = case_when(
      int_business_skills_training == 1 |
        int_business_advisory_and_mentoring == 1 |
        int_financial_and_start_up_support == 1 ~
        1,
      TRUE ~ 0
    ),
    com_self_employment_support = case_when(
      com_business_skills_training == 1 |
        com_business_advisory_and_mentoring == 1 |
        com_financial_and_start_up_support == 1 ~
        1,
      TRUE ~ 0
    )
  ) |>
  select(
    -int_business_skills_training,
    -int_business_advisory_and_mentoring,
    -int_financial_and_start_up_support,
    -com_business_skills_training,
    -com_business_advisory_and_mentoring,
    -com_financial_and_start_up_support
  ) |>
  pivot_longer(
    -study_id,
    names_to = "component",
    values_to = "count"
  ) |>
  mutate(
    group = case_when(
      str_detect(component, "int_") ~ "Intervention",
      str_detect(component, "com_") ~ "Comparison"
    ),
    group = factor(group, levels = c("Intervention", "Comparison")),
    component = str_remove_all(component, "int_"),
    component = str_remove_all(component, "com_"),
    component = case_when(
      component == "basic_skills_training" ~ "Basic Skills Training",
      component == "soft_skills_training" ~ "Soft Skills Training",
      component == "behavioural_skills_training" ~
        "Behavioural Skills Training",
      component == "job_specific_technical_skills_off_job_training" ~
        "Job Specific Technical Skills (Off-Job Training)",
      component == "self_employment_support" ~ "Self-Employment Support",
      component == "job_search_preparation" ~ "Job Search Preparation",
      component == "job_search_assistance" ~ "Job Search Assistance",
      component == "employment_counselling" ~ "Employment Counselling",
      component == "employment_coaching" ~ "Employment Coaching",
      component == "financial_assistance" ~ "Financial Assistance",
      component == "job_specific_technical_skills_on_job_training" ~
        "Job Specific Technical Skills (On-Job Training)",
      component == "paid_temporary_work_experience" ~
        "Paid Temporary Work Experience",
      component == "unpaid_temporary_work_experience" ~
        "Unpaid Temporary Work Experience",
      component == "wage_subsidies" ~ "Wage Subsidies",
      component == "public_works" ~ "Public Works",
      component == "other_active_component_nec" ~ "Other Active Component",
      component == "services_as_usual" ~ "Services as Usual",
      TRUE ~ component
    ),
    component = factor(
      component,
      levels = c(
        "Basic Skills Training",
        "Soft Skills Training",
        "Behavioural Skills Training",
        "Job Specific Technical Skills (Off-Job Training)",
        "Self-Employment Support",
        "Job Search Preparation",
        "Job Search Assistance",
        "Employment Counselling",
        "Employment Coaching",
        "Financial Assistance",
        "Job Specific Technical Skills (On-Job Training)",
        "Paid Temporary Work Experience",
        "Unpaid Temporary Work Experience",
        "Wage Subsidies",
        "Public Works",
        "Other Active Component",
        "Services as Usual"
      ),
      ordered = TRUE
    ),
    cluster = case_when(
      component %in%
        c(
          "Basic Skills Training",
          "Soft Skills Training",
          "Behavioural Skills Training",
          "Job Specific Technical Skills (Off-Job Training)"
        ) ~
        "Skills Development",
      component %in%
        c(
          "Self-Employment Support"
        ) ~
        "Self-Employment Support",
      component %in%
        c(
          "Job Search Preparation",
          "Job Search Assistance",
          "Employment Counselling",
          "Employment Coaching",
          "Financial Assistance"
        ) ~
        "Employment Services",
      component %in%
        c(
          "Job Specific Technical Skills (On-Job Training)",
          "Paid Temporary Work Experience",
          "Unpaid Temporary Work Experience"
        ) ~
        "Employment Experience",
      component %in% c("Wage Subsidies", "Public Works") ~
        "Subsidised Employment",
      component == "Other Active Component" ~ "Other",
      TRUE ~ "Other"
    ),
    cluster = factor(
      cluster,
      levels = c(
        "Skills Development",
        "Self-Employment Support",
        "Employment Services",
        "Employment Experience",
        "Subsidised Employment",
        "Other"
      ),
      ordered = TRUE
    )
  ) |>
  group_by(
    component,
    group,
    cluster
  ) |>
  summarise(
    count = sum(count)
  )

almp_nma_components_consolidated_plot <- almp_nma_components_consolidated_plot_data |>
  ggplot(
    aes(
      x = component,
      y = count,
      group = group,
      fill = group
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(label = ifelse(count > 0, count, "")),
    vjust = -0.5,
    size = 3
  ) +
  facet_grid(
    group ~ cluster,
    scales = "free_x",
    space = "free_x",
    labeller = labeller(
      cluster = label_wrap_gen(width = 12),
      group = label_wrap_gen(width = 10)
    )
  ) +
  labs(
    y = "Studies containing each component",
    x = ""
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    values = c("Intervention" = "#69C2C9", "Comparison" = "#7D2248")
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),
    legend.position = "none",
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 25, unit = "pt"),
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
      hjust = 0.5,
      size = 10
    ),
    strip.clip = "off"
  )

# export plot
ggsave(
  plot = almp_nma_components_consolidated_plot,
  filename = "./visualisation/output/almp_nma_components_consolidated_plot.png",
  height = 5.5,
  width = 8,
  device = "png",
  type = "cairo-png"
)
