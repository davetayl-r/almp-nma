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
# 1. Prepare age at commencement data
#-------------------------------------------------------------------------------

almp_nma_sex_distribution_plot_data <- almp_nma_summary_visualisation_data |>
  select(
    study_id,
    proportion_female_treatment,
    prop_female_centred
  ) |>
  distinct()


# select randomised domains
select(
  study_id,
  starts_with("qa_randomised")
) |>
  # drop na values (non-randomised studies)
  filter(if_all(starts_with("qa_"), ~ !is.na(.x))) |>
  # convert to long data for plotting
  pivot_longer(
    cols = -study_id,
    names_to = "domain",
    values_to = "response"
  ) |>
  mutate(
    domain = case_when(
      domain == "qa_randomised_q1" ~
        "Was true randomisation used for assignment of participants to treatment groups?",
      domain == "qa_randomised_q2" ~
        "Was allocation to treatment groups concealed?",
      domain == "qa_randomised_q3" ~
        "Were treatment groups similar at the baseline",
      domain == "qa_randomised_q4" ~
        "Were participants blind to treatment assignment?",
      domain == "qa_randomised_q5" ~
        "Were those delivering the treatment blind to treatment assignment",
      domain == "qa_randomised_q6" ~
        "Were treatment groups treated identically other than the intervention of interest?",
      domain == "qa_randomised_q7" ~
        "Were outcome assessors blind to treatment assignment?",
      domain == "qa_randomised_q8" ~
        "Were outcomes measured in the same way for treatment groups?",
      domain == "qa_randomised_q9" ~
        "Were outcomes measured in a realiable way?",
      domain == "qa_randomised_q10" ~
        "Was follow up complete and if not, were differences between groups in terms of their follow up adequately described and analysed?",
      domain == "qa_randomised_q11" ~
        "Were participants analysed in the groups to which they are randomised?",
      domain == "qa_randomised_q12" ~
        "Was appropriate statistical analyses used?",
      domain == "qa_randomised_q13" ~
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
      ordered = TRUE,
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

qa_non_randomised_raw_plot_data <- qa_raw |>
  # select non-randomised domains
  select(
    study_id,
    starts_with("qa_non_randomised")
  ) |>
  # drop na values (randomised domains)
  filter(if_all(starts_with("qa_"), ~ !is.na(.x))) |>
  # convert to long format for plotting
  pivot_longer(
    cols = -study_id,
    names_to = "domain",
    values_to = "response"
  ) |>
  mutate(
    domain = case_when(
      domain == "qa_non_randomised_q1" ~
        "Is it clear in the study what is the \"cause\" and what is the \"effect\" (i.e there is no confusion about which variable comes first)?",
      domain == "qa_non_randomised_q2" ~ "Is there a control group?",
      domain == "qa_non_randomised_q3" ~
        "Were participants included in any comparisons similar?",
      domain == "qa_non_randomised_q4" ~
        "Were the participants included in any comparisons recieving similar treatment/care, other than the exposure or intervention of interest?",
      domain == "qa_non_randomised_q5" ~
        "Were there multiple measurements of the outcome, both pre and post the intervention/exposure?",
      domain == "qa_non_randomised_q6" ~
        "Were the outcomes of participants included in any comparisons measured in the same way?",
      domain == "qa_non_randomised_q7" ~
        "Were the outcomes measured in a reliable way?",
      domain == "qa_non_randomised_q8" ~
        "Was follow-up complete and if not, were differences between groups in terms of their follow up adequately described and analysed?",
      domain == "qa_non_randomised_q9" ~
        "Was appropriate statistical analyses used?"
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
        "Unclear",
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

#-------------------------------------------------------------------------------
# 5. Summarise quality assessment
#-------------------------------------------------------------------------------

qa_summary_plot_data <- qa_raw |>
  select(
    study_design_type,
    low_study_quality
  ) |>
  mutate(
    total = n()
  ) |>
  group_by(study_design_type) |>
  summarise(
    n_studies = n(),
    n_low_quality = sum(low_study_quality, na.rm = TRUE),
    prop_low_quality = mean(low_study_quality, na.rm = TRUE),
    total = first(total) # carries forward the overall total
  ) |>
  mutate(
    prop_not_low_quality = 1 - prop_low_quality
  ) |>
  select(
    study_design_type,
    prop_low_quality,
    prop_not_low_quality
  ) |>
  pivot_longer(
    -study_design_type,
    names_to = "quality",
    values_to = "percent"
  ) |>
  mutate(
    quality = case_when(
      quality == "prop_low_quality" ~ "Low",
      quality == "prop_not_low_quality" ~ "Not-low"
    ),
    quality = factor(
      quality,
      levels = c(
        "Not-low",
        "Low"
      ),
      ordered = TRUE
    ),
    study_design_type = factor(
      study_design_type,
      levels = c(
        "Randomised design",
        "Selection on observables",
        "Design-based identification"
      )
    )
  )

qa_summary_plot <- qa_summary_plot_data |>
  ggplot(aes(x = quality, y = percent, fill = quality)) +
  geom_col() +
  geom_text(
    aes(
      label = round((percent * 100), 1)
    ),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(
    ~study_design_type,
    labeller = labeller(
      domain = label_wrap_gen(width = 24)
    ),
    ncol = 3
  ) +
  scale_fill_manual(
    values = c(
      "Not-low" = "#2b2b2b",
      "Low" = "#D62d20"
    )
  ) +
  labs(
    x = "Summary quality assessment",
    y = "Per cent"
  ) +
  scale_y_continuous(labels = scales::percent) +
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
      size = 9
    )
  )

# export plot
ggsave(
  plot = qa_summary_plot,
  filename = "./visualisation/output/almp_nma_qa_summary_plot.png",
  height = 5,
  width = 8,
  device = "png",
  type = "cairo-png"
)
