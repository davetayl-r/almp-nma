#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 16/09/2025                                                                           #
# Purpose: visualise study quality results                                                   #
#============================================================================================#

# load required packages
library(tidyverse)
library(ggplot2)

# read study quality data
qa_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_randomised.rds"
qa_randomised_raw <- readRDS(qa_randomised_raw_location)

qa_non_randomised_raw_location <- "./data_cleaning/outputs/almp_nma_qa_non_randomised.rds"
qa_non_randomised_raw <- readRDS(qa_non_randomised_raw_location)

#-------------------------------------------------------------------------------
# 1. Prepare Randomised Quality Assessment data
#-------------------------------------------------------------------------------

qa_randomised_raw_plot_data <- qa_randomised_raw |>
  pivot_longer(
    cols = -study_id,
    names_to = "domain",
    values_to = "response"
  ) |>
  mutate(
    domain = case_when(
      domain == "question_1" ~
        "Was true randomisation used for assignment of participants to treatment groups?",
      domain == "question_2" ~ "Was allocation to treatment groups concealed?",
      domain == "question_3" ~ "Were treatment groups similar at the baseline",
      domain == "question_4" ~
        "Were participants blind to treatment assignment?",
      domain == "question_5" ~
        "Were those delivering the treatment blind to treatment assignment",
      domain == "question_6" ~
        "Were treatment groups treated identically other than the intervention of interest?",
      domain == "question_7" ~
        "Were outcome assessors blind to treatment assignment?",
      domain == "question_8" ~
        "Were outcomes measured in the same way for treatment groups?",
      domain == "question_9" ~ "Were outcomes measured in a realiable way?",
      domain == "question_10" ~
        "Was follow up complete and if not, were differences between groups in terms of their follow up adequately described and analysed?",
      domain == "question_11" ~
        "Were participants analysed in the groups to which they are randomised?",
      domain == "question_12" ~ "Was appropriate statistical analyses used?",
      domain == "question_13" ~
        "Was the trial design appropriate and any deviations from the standard RCT design accounted for in the conduct and analysis of the trial?"
    ),
    domain = factor(
      domain,
      levels = c(
        "Was true randomisation used for assignment of participants to treatment groups?",
        "Was allocation to treatment groups concealed?",
        "Were treatment groups similar at the baseline",
        "Were participants blind to treatment assignment?",
        "Were those delivering the treatment blind to treatment assignment",
        "Were treatment groups treated identically other than the intervention of interest?",
        "Were outcome assessors blind to treatment assignment?",
        "Were outcomes measured in the same way for treatment groups?",
        "Were outcomes measured in a realiable way?",
        "Was follow up complete and if not, were differences between groups in terms of their follow up adequately described and analysed?",
        "Were participants analysed in the groups to which they are randomised?",
        "Was appropriate statistical analyses used?",
        "Was the trial design appropriate and any deviations from the standard RCT design accounted for in the conduct and analysis of the trial?"
      ),
      ordered = TRUE
    ),
    response = as.character(response),
    response = factor(
      response,
      levels = c(
        "Yes",
        "Unclear",
        "No"
      ),
      ordered = TRUE
    )
  )

#-------------------------------------------------------------------------------
# 2. Visualise raw Randomised Quality Assessment results
#-------------------------------------------------------------------------------

qa_randomised_raw_summary <- qa_randomised_raw_plot_data |>
  group_by(domain, response) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(domain) |>
  mutate(
    total = sum(count),
    percent = count / total # * 100
  ) |>
  ungroup()

qa_randomised_raw_plot <- qa_randomised_raw_summary |>
  ggplot(aes(x = response, y = percent, fill = response)) +
  geom_col() +
  facet_wrap(
    ~domain,
    labeller = labeller(
      domain = label_wrap_gen(width = 24)
    ),
    ncol = 4
  ) +
  scale_fill_manual(
    values = c(
      "Yes" = "#008744",
      "Unclear" = "#FFA700",
      "No" = "#D62d20"
    )
  ) +
  labs(
    title = "",
    x = "Response",
    y = "Per cent"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "",
    x = "",
    y = "Per cent"
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
      hjust = 0,
      size = 8
    )
  )

# export plot
ggsave(
  plot = qa_randomised_raw_plot,
  filename = "./visualisation/output/almp_nma_qa_randomised_raw_plot.png",
  height = 10,
  width = 8,
  device = "png",
  type = "cairo-png"
)

#-------------------------------------------------------------------------------
# 3. Prepare Randomised Quality Assessment data
#-------------------------------------------------------------------------------

qa_non_randomised_raw_plot_data <- qa_non_randomised_raw |>
  pivot_longer(
    cols = -study_id,
    names_to = "domain",
    values_to = "response"
  ) |>
  mutate(
    domain = case_when(
      domain == "question_1" ~
        "Is it clear in the study what is the \"cause\" and what is the \"effect\" (i.e there is no confusion about which variable comes first)?",
      domain == "question_2" ~ "Is there a control group?",
      domain == "question_3" ~
        "Were participants included in any comparisons similar?",
      domain == "question_4" ~
        "Were the participants included in any comparisons recieving similar treatment/care, other than the exposure or intervention of interest?",
      domain == "question_5" ~
        "Were there multiple measurements of the outcome, both pre and post the intervention/exposure?",
      domain == "question_6" ~
        "Were the outcomes of participants included in any comparisons measured in the same way?",
      domain == "question_7" ~ "Were the outcomes measured in a reliable way?",
      domain == "question_8" ~
        "Was follow-up complete and if not, were differences between groups in terms of their follow up adequately described and analysed?",
      domain == "question_9" ~ "Was appropriate statistical analyses used?"
    ),
    domain = factor(
      domain,
      levels = c(
        "Is it clear in the study what is the \"cause\" and what is the \"effect\" (i.e there is no confusion about which variable comes first)?",
        "Is there a control group?",
        "Were participants included in any comparisons similar?",
        "Were the participants included in any comparisons recieving similar treatment/care, other than the exposure or intervention of interest?",
        "Were there multiple measurements of the outcome, both pre and post the intervention/exposure?",
        "Were the outcomes of participants included in any comparisons measured in the same way?",
        "Were the outcomes measured in a reliable way?",
        "Was follow-up complete and if not, were differences between groups in terms of their follow up adequately described and analysed?",
        "Was appropriate statistical analyses used?"
      ),
      ordered = TRUE
    ),
    response = as.character(response),
    response = factor(
      response,
      levels = c(
        "Yes",
        "Unsure",
        "No"
      ),
      ordered = TRUE
    )
  )

#-------------------------------------------------------------------------------
# 4. Visualise raw Non-randomised Quality Assessment results
#-------------------------------------------------------------------------------

qa_non_randomised_raw_summary <- qa_non_randomised_raw_plot_data |>
  group_by(domain, response) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(domain) |>
  mutate(
    total = sum(count),
    percent = count / total # * 100
  ) |>
  ungroup()

qa_non_randomised_raw_plot <- qa_non_randomised_raw_summary |>
  ggplot(aes(x = response, y = percent, fill = response)) +
  geom_col() +
  facet_wrap(
    ~domain,
    labeller = labeller(
      domain = label_wrap_gen(width = 24)
    ),
    ncol = 4
  ) +
  scale_fill_manual(
    values = c(
      "Yes" = "#008744",
      "Unclear" = "#FFA700",
      "No" = "#D62d20"
    )
  ) +
  labs(
    title = "",
    x = "Response",
    y = "Per cent"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "",
    x = "",
    y = "Per cent"
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
      hjust = 0,
      size = 8
    )
  )

# export plot
ggsave(
  plot = qa_non_randomised_raw_plot,
  filename = "./visualisation/output/almp_nma_qa_non_randomised_raw_plot.png",
  height = 10,
  width = 8,
  device = "png",
  type = "cairo-png"
)
