#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: Helper functions for the NMA                                                      #
#============================================================================================#

library(rlang)

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
  df,
  targets,
  study_var = "study",
  domain_var = "outcome_domain",
  time_var = "outcome_timing",
  default_target = default_target,
  default_window = default_window
) {
  study_sym <- sym(study_var)
  domain_sym <- sym(domain_var)
  time_sym <- sym(time_var)

  # attach targets and compute window/ distance
  cand <- df %>%
    left_join(targets, by = setNames("outcome_domain", domain_var)) %>%
    mutate(
      target = if_else(is.na(target), default_target, target),
      window = if_else(is.na(window), default_window, window),
      dist = abs(!!time_sym - target),
      within_anchor_window = (!!time_sym >= target - window) &
        (!!time_sym <= target + window)
    )

  # choose one timepoint per study × domain
  chosen_time <- cand %>%
    group_by(!!study_sym, !!domain_sym) %>%
    arrange(desc(within_anchor_window), dist, !!time_sym, .by_group = TRUE) %>%
    summarise(
      selected_timepoint = first(!!time_sym),
      selected_outside_window_group = !first(within_anchor_window),
      .groups = "drop"
    )

  # propagate the chosen timepoint back to all outcomes at that time
  out <- cand %>%
    left_join(chosen_time, by = c(study_var, domain_var)) %>%
    mutate(
      selected_primary_timepoint = (!!time_sym == selected_timepoint),
      # mark outside/inside window only for the selected rows; NA otherwise
      selected_outside_window = if_else(
        selected_primary_timepoint,
        selected_outside_window_group,
        NA
      ),
      # (optional) keep an integer flag like in your screenshot
      selected_primary_timepoint = as.integer(selected_primary_timepoint)
    ) %>%
    select(-dist, -selected_outside_window_group)

  out
}
