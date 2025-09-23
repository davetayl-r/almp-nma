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

#-------------------------------------------------------------------------------
# 3. Make a list of prior() objects for any coefficients matching a regex
#-------------------------------------------------------------------------------

make_coef_priors <- function(gp, pattern, sd) {
  coefs <- gp |>
    dplyr::filter(.data$class == "b", grepl(pattern, .data$coef)) |>
    dplyr::distinct(.data$coef) |>
    dplyr::pull(.data$coef)

  if (length(coefs) == 0) {
    return(list())
  }

  pri_str <- sprintf("normal(0, %g)", sd) # e.g., "normal(0, 0.12)"
  lapply(as.character(coefs), function(cf) {
    set_prior(pri_str, class = "b", coef = cf)
  })
}

#-------------------------------------------------------------------------------
# 4. Make a list of prior() objects for any sd coefficients matching a regex
#-------------------------------------------------------------------------------

make_sd_priors <- function(
  gp,
  coef_pattern = ".*", # regex on coef
  group_pattern = NULL, # optional regex on group
  dist = "student_t(3, 0, %s)", # or "normal(0, %s)"
  scale = 0.25
) {
  stopifnot(is.data.frame(gp), all(c("class", "group", "coef") %in% names(gp)))

  # start from SD rows only
  rows <- dplyr::filter(gp, .data$class == "sd")

  # optionally filter by group regex
  if (!is.null(group_pattern)) {
    rows <- dplyr::filter(rows, grepl(group_pattern, .data$group))
  }

  # filter by coef regex
  rows <- dplyr::filter(rows, grepl(coef_pattern, .data$coef))

  if (nrow(rows) == 0L) {
    return(list())
  }

  pri_str <- sprintf(dist, scale)

  # one prior per (group, coef)
  purrr::pmap(rows[, c("group", "coef")], function(group, coef) {
    set_prior(pri_str, class = "sd", group = group, coef = coef)
  })
}

#-------------------------------------------------------------------------------
# 5. Check for Kullback-Leibler divergence for N(post_mean, post_sd^2)
#-------------------------------------------------------------------------------

kullback_leibler_divergence_normal <- function(
  post_mean,
  post_sd,
  prior_sd = 0.40
) {
  0.5 *
    ((post_sd^2 + post_mean^2) / prior_sd^2 - 1 + 2 * log(prior_sd / post_sd))
}

#-------------------------------------------------------------------------------
# 6. Check for prior–posterior overlap (PPO) by numeric integration
#-------------------------------------------------------------------------------

prior_posterior_overlap <- function(
  post_mean,
  post_sd,
  prior_sd = 0.40
) {
  lo <- min(post_mean - 6 * post_sd, -6 * prior_sd)
  hi <- max(post_mean + 6 * post_sd, 6 * prior_sd)
  x <- seq(lo, hi, length.out = 4001)
  dx <- x[2] - x[1]
  prior_dens <- dnorm(x, mean = 0, sd = prior_sd)
  post_dens <- dnorm(x, mean = post_mean, sd = post_sd)
  sum(pmin(prior_dens, post_dens)) * dx
}

# ---------------------------------------------------------
# 7. Get draw-level differences for one component
# ---------------------------------------------------------
get_comp_outcome_draws <- function(comp, n_draws = NULL) {
  new_data_0 <- grid_base
  new_data_1 <- grid_base
  new_data_1[[comp]] <- 1L

  # posterior_linpred gives a matrix: draws x rows(newdata)
  matrix_0 <- if (is.null(n_draws)) {
    brms::posterior_linpred(
      almp_nma_model_ten,
      newdata = new_data_0,
      re_formula = re_outcome_only
    )
  } else {
    brms::posterior_linpred(
      almp_nma_model_ten,
      newdata = new_data_0,
      re_formula = re_outcome_only,
      ndraws = n_draws
    )
  }
  matrix_1 <- if (is.null(n_draws)) {
    brms::posterior_linpred(
      almp_nma_model_ten,
      newdata = new_data_1,
      re_formula = re_outcome_only
    )
  } else {
    brms::posterior_linpred(
      almp_nma_model_ten,
      newdata = new_data_1,
      re_formula = re_outcome_only,
      ndraws = n_draws
    )
  }

  # difference matrix to get draws x outcomes
  difference_matrix <- matrix_1 - matrix_0
  colnames(difference_matrix) <- as.character(new_data_0$outcome)

  # tidy data
  as_tibble(difference_matrix) |>
    mutate(.draw = row_number()) |>
    pivot_longer(
      cols = -.draw,
      names_to = "outcome",
      values_to = "estimate"
    ) |>
    mutate(
      component = comp,
      outcome = factor(outcome, levels = outcome_levels),
      .before = 1
    )
}
