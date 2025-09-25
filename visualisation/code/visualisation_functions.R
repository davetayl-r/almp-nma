#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 24/09/2025                                                                           #
# Purpose: Helper functions for NMA visualisation                                            #
#============================================================================================#

#-------------------------------------------------------------------------------
# 1. Order factor variables after filtering
#-------------------------------------------------------------------------------

maintain_factor_order <- function(x) {
  original_levels <- levels(x)
  remaining_values <- unique(as.character(x))
  new_order <- original_levels[original_levels %in% remaining_values]
  factor(x, levels = new_order, ordered = is.ordered(x))
}

#-------------------------------------------------------------------------------
# 2. Produce outcome x component forest plot
#-------------------------------------------------------------------------------

create_forest_plot <- function(
  component_name,
  outcome_domain_name
) {
  # Subset summary data
  summary_data <- almp_nma_model_thirteen_component_summary_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = theta
    ) |>
    mutate(
      effect = as.numeric(effect),
      .lower = as.numeric(.lower),
      .upper = as.numeric(.upper),
      outcome = maintain_factor_order(outcome)
    )

  # Subset plot data
  plot_data <- almp_nma_model_thirteen_component_draws_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = theta
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Create main subgroup forest plot
  main_plot <- plot_data |>
    ggplot(
      aes(
        x = effect,
        y = fct_rev(outcome)
      )
    ) +
    # Zero reference line
    geom_vline(
      xintercept = 0,
      linewidth = 0.25,
      linetype = "dashed",
      alpha = 0.75,
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
      alpha = 0.8,
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
      alpha = 0.8,
      point_interval = "median_qi"
    ) +
    # Add summary text labels
    geom_text(
      data = mutate_if(
        summary_data,
        is.numeric,
        round,
        2
      ),
      aes(
        label = str_glue(
          "{sprintf('%.2f', effect)} [{sprintf('%.2f', .lower)}, {sprintf('%.2f', .upper)}]"
        ),
        x = effect
      ),
      hjust = "centre",
      nudge_y = -0.1,
      size = 3,
      color = "black"
    ) +
    # wrap y-axis labels
    scale_y_discrete(
      labels = label_wrap(20)
    ) +
    scale_x_continuous(
      limits = c(-1.1, 1.1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1")
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
    # hide colour from legend
    guides(
      fill = guide_legend(
        override.aes = list(
          colour = NA
        )
      )
    ) +
    coord_cartesian(
      expand = FALSE,
      clip = "off"
    ) +
    # specify labels
    labs(
      subtitle = "",
      x = "Posterior distributions with 95% credible intervals (Hedges' g)",
      y = "",
      caption = "Values report median effect size [95% Credible Interval]"
    ) +
    # set theme
    theme(
      plot.background = element_rect(fill = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(
        size = 10,
        colour = "#000000",
        margin = margin(t = 10, r = 0, b = 0, l = 0)
      ),
      axis.text.y = element_text(
        size = 10,
        colour = "#000000"
      ),
      axis.text.x = element_text(
        size = 9,
        colour = "#323030ff",
        vjust = -0.75
      ),
      strip.text = element_text(
        size = 10,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      strip.clip = "off",
      strip.text.y = element_text(angle = 0, hjust = 0),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      plot.subtitle = element_blank(),
      plot.title = element_blank(),
      panel.border = element_blank(),
      plot.caption = element_text(
        size = 10,
        margin = margin(t = 5)
      ),
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(t = 2, r = 15, b = 2, l = 2, unit = "pt")
    )

  return(main_plot)
}

#-------------------------------------------------------------------------------
# 3. Produce outcome x component (subgroup x sex) forest plot
#-------------------------------------------------------------------------------

create_subgroup_sex_forest_plot <- function(
  component_name,
  outcome_domain_name
) {
  # Subset summary data
  summary_data <- almp_nma_model_thirteen_study_level_subgroup_sex_summary_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = theta
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Subset summary contrast data
  contrast_summary_data <- almp_nma_model_thirteen_differential_treatment_effect_sex_summary_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = contrast_1_minus_0
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Subset plot data
  plot_data <- almp_nma_model_thirteen_study_level_subgroup_sex_draws_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = theta
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Subset contrast plot data
  contrast_plot_data <- almp_nma_model_thirteen_differential_treatment_effect_sex_draws_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = contrast_1_minus_0
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Create main subgroup forest plot
  main_plot <- plot_data |>
    ggplot(
      aes(
        x = effect,
        y = fct_rev(outcome)
      )
    ) +
    # Zero reference line
    geom_vline(
      xintercept = 0,
      linewidth = 0.25,
      linetype = "dashed",
      alpha = 0.75,
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
      alpha = 0.8,
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
      alpha = 0.8,
      point_interval = "median_qi"
    ) +
    # Add summary text labels
    geom_text(
      data = mutate_if(
        summary_data,
        is.numeric,
        round,
        2
      ),
      aes(
        label = str_glue(
          "{sprintf('%.2f', effect)} [{sprintf('%.2f', .lower)}, {sprintf('%.2f', .upper)}]"
        ),
        x = effect
      ),
      hjust = "centre",
      nudge_y = -0.1,
      size = 3,
      color = "black"
    ) +
    # wrap y-axis labels
    scale_y_discrete(
      labels = label_wrap(20)
    ) +
    scale_x_continuous(
      limits = c(-1.1, 1.1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1")
    ) +
    # wrap facets
    facet_grid(
      . ~ subgroup,
      labeller = label_wrap_gen(width = 15),
      scales = "free_y",
      space = "free_y"
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
    # hide colour from legend
    guides(
      fill = guide_legend(
        override.aes = list(
          colour = NA
        )
      )
    ) +
    coord_cartesian(
      expand = FALSE,
      clip = "off"
    ) +
    # specify labels
    labs(
      subtitle = "",
      x = "Posterior distributions with 95% credible intervals (Hedges' g)",
      y = ""
    ) +
    # set theme
    theme(
      plot.background = element_rect(fill = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(
        size = 10,
        colour = "#000000",
        margin = margin(t = 10, r = 0, b = 0, l = 0)
      ),
      axis.text.y = element_text(
        size = 10,
        colour = "#000000"
      ),
      axis.text.x = element_text(
        size = 9,
        colour = "#323030ff",
        vjust = -0.75
      ),
      strip.text = element_text(
        size = 10,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_blank(),
      strip.clip = "off",
      strip.text.y = element_text(angle = 0, hjust = 0),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      plot.subtitle = element_blank(),
      plot.title = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(t = 2, r = 15, b = 2, l = 2, unit = "pt")
    )

  # Create contrast forest plot
  contrast_plot <- contrast_plot_data |>
    ggplot(
      aes(
        x = effect,
        y = fct_rev(outcome)
      )
    ) +
    # Zero reference line
    geom_vline(
      xintercept = 0,
      linewidth = 0.25,
      linetype = "dashed",
      alpha = 0.75,
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
          "better_male",
          "better_female"
        ))
      ),
      .width = c(0.95),
      colour = "#000000",
      alpha = 0.8,
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
          "better_male",
          "better_female"
        ))
      ),
      .width = c(0.95),
      colour = "#000000",
      alpha = 0.8,
      point_interval = "median_qi"
    ) +
    # Add summary text labels
    geom_text(
      data = mutate_if(
        contrast_summary_data,
        is.numeric,
        round,
        2
      ),
      aes(
        label = str_glue(
          "{sprintf('%.2f', effect)} [{sprintf('%.2f', .lower)}, {sprintf('%.2f', .upper)}]"
        ),
        x = effect
      ),
      hjust = "centre",
      nudge_y = -0.1,
      size = 3,
      color = "black"
    ) +
    # wrap facets
    facet_wrap(
      . ~ "Contrast (Females - Males)"
    ) +
    # wrap y-axis labels
    scale_y_discrete(
      labels = label_wrap(20)
    ) +
    scale_x_continuous(
      limits = c(-0.6, 0.6),
      breaks = c(-0.5, 0, 0.5),
      labels = c("-1", "0", "1")
    ) +
    # specify colour scheme
    scale_fill_manual(
      values = c(
        "better_female" = "#d9a637",
        "better_male" = "#542437"
      ),
      name = "Outcome Direction",
      labels = c(
        "better_female" = "Favours Females",
        "better_male" = "Favours Males"
      )
    ) +
    # hide colour from legend
    guides(
      fill = guide_legend(
        override.aes = list(
          colour = NA
        )
      )
    ) +
    coord_cartesian(
      expand = FALSE,
      clip = "off"
    ) +
    # specify labels
    labs(
      subtitle = "",
      x = "Posterior distributions with 95% credible intervals (Hedges' g)",
      y = ""
    ) +
    # set theme
    theme(
      plot.background = element_rect(fill = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(
        size = 10,
        colour = "#000000",
        margin = margin(t = 10, r = 0, b = 0, l = 0)
      ),
      axis.text.x = element_text(
        size = 9,
        colour = "#323030ff",
        vjust = -0.75
      ),
      strip.text = element_text(
        size = 10,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_blank(),
      strip.clip = "off",
      strip.text.y = element_text(angle = 0, hjust = 0),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      plot.subtitle = element_blank(),
      plot.title = element_blank(),
      axis.text.y = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.margin = margin(t = 2, r = 15, b = 2, l = 2, unit = "pt"),
      axis.ticks = element_blank()
    )

  # Combine the plots
  combined_plot <- main_plot +
    contrast_plot +
    # aligns axis titles
    plot_layout(
      axis_titles = "collect",
      widths = c(2, 1)
    ) +
    plot_annotation(
      caption = "Values report median effect size [95% Credible Interval]",
      theme = theme(
        plot.caption = element_text(
          size = 10,
          margin = margin(t = 5)
        )
      )
    )

  return(combined_plot)
}

#-------------------------------------------------------------------------------
# 4. Produce outcome x component (subgroup x age) forest plot
#-------------------------------------------------------------------------------

create_subgroup_age_forest_plot <- function(
  component_name,
  outcome_domain_name
) {
  # Subset summary data
  summary_data <- almp_nma_model_thirteen_study_level_subgroup_age_summary_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = theta
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Subset plot data
  plot_data <- almp_nma_model_thirteen_study_level_subgroup_age_draws_filtered |>
    filter(
      component == component_name,
      outcome_domain == outcome_domain_name
    ) |>
    rename(
      effect = theta
    ) |>
    mutate(
      outcome = maintain_factor_order(outcome)
    )

  # Create main subgroup forest plot
  main_plot <- plot_data |>
    ggplot(
      aes(
        x = effect,
        y = fct_rev(outcome)
      )
    ) +
    # Zero reference line
    geom_vline(
      xintercept = 0,
      linewidth = 0.25,
      linetype = "dashed",
      alpha = 0.75,
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
      alpha = 0.8,
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
      alpha = 0.8,
      point_interval = "median_qi"
    ) +
    # Add summary text labels
    geom_text(
      data = mutate_if(
        summary_data,
        is.numeric,
        round,
        2
      ),
      aes(
        label = str_glue(
          "{sprintf('%.2f', effect)} [{sprintf('%.2f', .lower)}, {sprintf('%.2f', .upper)}]"
        ),
        x = effect
      ),
      hjust = "centre",
      nudge_y = -0.1,
      size = 3,
      color = "black"
    ) +
    # wrap y-axis labels
    scale_y_discrete(
      labels = label_wrap(20)
    ) +
    scale_x_continuous(
      limits = c(-1.1, 1.1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1")
    ) +
    # wrap facets
    facet_grid(
      . ~ age_group,
      labeller = label_wrap_gen(width = 15),
      scales = "free_y",
      space = "free_y"
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
    # hide colour from legend
    guides(
      fill = guide_legend(
        override.aes = list(
          colour = NA
        )
      )
    ) +
    coord_cartesian(
      expand = FALSE,
      clip = "off"
    ) +
    # specify labels
    labs(
      subtitle = "",
      x = "Posterior distributions with 95% credible intervals (Hedges' g)",
      y = "",
      caption = "Values report median effect size [95% Credible Interval]"
    ) +
    # set theme
    theme(
      plot.background = element_rect(fill = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(
        size = 10,
        colour = "#000000",
        margin = margin(t = 10, r = 0, b = 0, l = 0)
      ),
      axis.text.y = element_text(
        size = 10,
        colour = "#000000"
      ),
      axis.text.x = element_text(
        size = 9,
        colour = "#323030ff",
        vjust = -0.75
      ),
      strip.text = element_text(
        size = 10,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      strip.clip = "off",
      strip.text.y = element_text(angle = 0, hjust = 0),
      strip.placement = "outside",
      axis.title.y = element_blank(),
      plot.subtitle = element_blank(),
      plot.title = element_blank(),
      panel.border = element_blank(),
      plot.caption = element_text(
        size = 10,
        margin = margin(t = 5)
      ),
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(t = 2, r = 15, b = 2, l = 2, unit = "pt")
    )

  return(main_plot)
}
