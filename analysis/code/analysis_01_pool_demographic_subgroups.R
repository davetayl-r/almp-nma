# pool subgroups within stefanik2024supportingrightworkplace using inverse-variance weighting
stefanik2024supportingrightworkplace_pooled <- stefanik2024supportingrightworkplace_export |>
  # extract the base study ID by removing the letter suffix
  mutate(base_study_id = str_remove(study_id, "_[a-z]$")) |>
  # calculate weights for each effect size
  mutate(
    weight_d = 1 / d_var,
    weight_g = 1 / g_var
  ) |>
  # group by study and outcome characteristics
  group_by(base_study_id, outcome_domain, outcome, outcome_timing) |>
  # calculate pooled estimates using inverse-variance weighting
  summarise(
    # pooled effect size = sum(effect * weight) / sum(weight)
    d = sum(d * weight_d) / sum(weight_d),
    g = sum(g * weight_g) / sum(weight_g),
    # pooled standard error = sqrt(1 / sum(weight))
    d_se = sqrt(1 / sum(weight_d)),
    g_se = sqrt(1 / sum(weight_g)),
    # pooled variance = 1 / sum(weight)
    d_var = 1 / sum(weight_d),
    g_var = 1 / sum(weight_g),
    # retain other information
    outcome_source = first(outcome_source),
    favourable_direction = first(favourable_direction),
    estimand = first(estimand),
    intention_to_treat = first(intention_to_treat),
    conditional = first(conditional),
    .groups = "drop"
  ) |>
  # clean up for export
  rename(study_id = base_study_id) |>
  select(
    study_id,
    outcome_domain,
    outcome,
    outcome_source,
    favourable_direction,
    outcome_timing,
    estimand,
    intention_to_treat,
    conditional,
    d,
    d_se,
    d_var,
    g,
    g_se,
    g_var
  )
