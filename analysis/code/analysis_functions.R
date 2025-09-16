#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: Helper functions for the NMA                                                      #
#============================================================================================#

#-------------------------------------------------------------------------------
# 1. Create component indicator variables
#-------------------------------------------------------------------------------

create_component_matrix <- function(data, sep.comps = "+") {
  # extract unique interventions
  interventions <- unique(
    c(data$intervention, data$comparison)
  )

  # Split interventions into components
  intervention_components <- map(
    interventions,
    ~ str_split(.x, fixed(sep.comps))[[1]]
  )
  names(intervention_components) <- interventions

  # Get all unique components
  all_components <- unique(unlist(intervention_components))

  # Create component matrix (C matrix)
  component_matrix <- matrix(
    0,
    nrow = length(interventions),
    ncol = length(all_components)
  )
  rownames(component_matrix) <- interventions
  colnames(component_matrix) <- all_components

  for (i in seq_along(interventions)) {
    components <- intervention_components[[interventions[i]]]
    component_matrix[i, components] <- 1
  }

  return(component_matrix)
}

#-------------------------------------------------------------------------------
# 2. Identify closest outcome measurement to anchor point
#-------------------------------------------------------------------------------

select_outcome_timepoint <- function(
  data,
  # numeric months
  time_col = outcome_timing,
  # e.g., "Labour Force Status"
  domain_col = outcome_domain,
  # study ID
  study_col = study,
  # tbl with columns: domain, target, window
  anchors_tbl = NULL,
  # fallback if domain not in anchors_tbl
  default_target = default_target,
  default_window = default_window,
  # round months
  round_time = TRUE
) {
  time_col <- enquo(time_col)
  domain_col <- enquo(domain_col)
  study_col <- enquo(study_col)

  # Normalise anchors table (optional)
  if (is.null(anchors_tbl)) {
    anchors_tbl <- tibble(
      !!as_name(domain_col) := character(),
      target = numeric(),
      window = numeric()
    )
  } else {
    # Ensure it has matching domain column name
    anchors_tbl <- anchors_tbl |>
      rename(!!as_name(domain_col) := {{ domain_col }})
  }

  data |>
    mutate(
      .time = !!time_col,
      .time = if (round_time) round(.time) else .time
    ) |>
    left_join(anchors_tbl, by = as_name(domain_col)) |>
    mutate(
      target = if_else(is.na(target), default_target, target),
      window = if_else(is.na(window), default_window, window),
      distance_months = abs(.time - target),
      within_anchor_window = distance_months <= window
    ) |>
    group_by(!!study_col, !!domain_col) |>
    arrange(
      desc(within_anchor_window),
      distance_months,
      .time,
      .by_group = TRUE
    ) |>
    mutate(
      selected_primary_timepoint = if_else(
        row_number() == 1 & !is.na(.time),
        1L,
        0L
      ),
      selected_outside_window = selected_primary_timepoint == 1L &
        !within_anchor_window
    ) |>
    ungroup() |>
    rename(
      anchor_target_months = target,
      anchor_window_months = window
    ) |>
    # keep original time column intact; write back the rounded time if desired
    mutate(!!as_name(quo_name(time_col)) := .time) |>
    select(-.time)
}
