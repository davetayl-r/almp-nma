#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 16/09/2025                                                                           #
# Purpose: visualise network map                                                             #
#============================================================================================#

# load required packages
library(tidyverse)
library(igraph)
library(ggplot2)
library(ggrepel)
library(ggpp)

# read data
hpsb_meta_analysis_data_location <- "./data/hpsb_meta_04_effect_size_intervention_components_study_level_data.RDS"
hpsb_meta_analysis_data <- readRDS(hpsb_meta_analysis_data_location)

# set seed
set.seed(345)

# prepare data for plot
hpsb_meta_network_data <- hpsb_meta_analysis_data |>
  # merge cbt groups
  mutate(
    treatment = str_replace_all(
      treatment,
      "adventure_therapy",
      "AT"
    ),
    treatment = str_replace_all(
      treatment,
      "cognitive_behaviour_therapy_group",
      "CBT(G)"
    ),
    comparison = str_replace_all(
      comparison,
      "cognitive_behaviour_therapy_group",
      "CBT(G)"
    ),
    treatment = str_replace_all(
      treatment,
      "cognitive_behaviour_therapy_individual",
      "CBT(I)"
    ),
    comparison = str_replace_all(
      comparison,
      "cognitive_behaviour_therapy_individual",
      "CBT(I)"
    ),
    treatment = str_replace_all(
      treatment,
      "exercise",
      "EX"
    ),
    treatment = str_replace_all(
      treatment,
      "functional_family_therapy",
      "FFT"
    ),
    treatment = str_replace_all(
      treatment,
      "family_therapy",
      "FT"
    ),
    comparison = str_replace_all(
      comparison,
      "family_therapy",
      "FT"
    ),
    treatment = str_replace_all(
      treatment,
      "group_therapy",
      "GT"
    ),
    comparison = str_replace_all(
      comparison,
      "group_therapy",
      "GT"
    ),
    treatment = str_replace_all(
      treatment,
      "individual_therapy",
      "IT"
    ),
    comparison = str_replace_all(
      comparison,
      "individual_therapy",
      "IT"
    ),
    treatment = str_replace_all(
      treatment,
      "other",
      "OTH"
    ),
    comparison = str_replace_all(
      comparison,
      "other",
      "OTH"
    ),
    treatment = str_replace_all(
      treatment,
      "mode_deactivation_therapy",
      "MST"
    ),
    treatment = str_replace_all(
      treatment,
      "multisystemtic_therapy",
      "MST"
    ),
    comparison = str_replace_all(
      comparison,
      "play_therapy",
      "PT"
    ),
    treatment = str_replace_all(
      treatment,
      "relapse_prevention",
      "RP"
    ),
    comparison = str_replace_all(
      comparison,
      "relapse_prevention",
      "RP"
    ),
    treatment = str_replace_all(
      treatment,
      "sex_education",
      "SE"
    ),
    comparison = str_replace_all(
      comparison,
      "sex_education",
      "SE"
    ),
    treatment = str_replace_all(
      treatment,
      "social_skills_training",
      "SST"
    ),
    comparison = str_replace_all(
      comparison,
      "social_skills_training",
      "SST"
    ),
    comparison = str_replace_all(
      comparison,
      "tau",
      "TAU"
    ),
    treatment = str_replace_all(
      treatment,
      "\\+",
      " + "
    ),
    treatment = str_wrap(treatment, 30),
    comparison = str_wrap(comparison, 30),
    study_design = case_when(
      study_design_type == "Randomised study design" ~ "Randomised",
      .default = "Non-randomised"
    )
  ) |>
  select(
    study_id,
    treatment,
    comparison,
    study_design,
    outcome_domain,
    outcome_construct
  ) |>
  distinct()

# ============================================================================
# STEP 1: Create nodes (unique treatments + comparisons)
# ============================================================================

treatment_nodes <- hpsb_meta_network_data |>
  select(intervention = treatment) |>
  distinct()

comparison_nodes <- hpsb_meta_network_data |>
  select(intervention = comparison) |>
  distinct()

# Combine all unique interventions
all_nodes <- bind_rows(treatment_nodes, comparison_nodes) |>
  distinct() |>
  mutate(node_id = row_number())

# ============================================================================
# STEP 2: Identify the most common comparator
# ============================================================================

# Count how often each intervention appears as a comparison
comparison_frequency <- hpsb_meta_network_data |>
  count(comparison, name = "comparison_count") |>
  arrange(desc(comparison_count))

# Also count treatment frequency for completeness
treatment_frequency <- hpsb_meta_network_data |>
  count(treatment, name = "treatment_count") |>
  arrange(desc(treatment_count))

# Combined frequency (total appearances)
total_frequency <- hpsb_meta_network_data |>
  select(intervention = treatment) |>
  bind_rows(hpsb_meta_network_data |> select(intervention = comparison)) |>
  count(intervention, name = "total_appearances") |>
  arrange(desc(total_appearances))

# Identify the reference intervention (most common comparator)
reference_intervention <- comparison_frequency$comparison[1]

# ============================================================================
# STEP 3: Create edges (direct comparisons between interventions)
# ============================================================================

# Create edge list from study comparisons
edges_raw <- hpsb_meta_network_data |>
  select(study_id, study_design, treatment, comparison, outcome_domain)

# Create both directions for undirected network (optional - depends on your preference)
edges_bidirectional <- bind_rows(
  edges_raw,
  edges_raw |>
    select(
      study_id,
      study_design,
      outcome_domain,
      treatment = comparison,
      comparison = treatment
    )
)

# Count studies for each comparison AND outcome domain
edges <- edges_bidirectional |>
  group_by(treatment, comparison, outcome_domain) |>
  summarise(
    n_studies = n_distinct(study_id),
    study_design_types = paste(unique(study_design), collapse = ", "),
    has_rct = any(study_design == "Randomised"),
    has_qed = any(study_design == "Non-randomised"),
    .groups = "drop"
  ) |>
  filter(treatment != comparison) |> # Remove self-loops
  # Remove duplicate edges (keep only one direction for undirected graph)
  rowwise() |>
  mutate(
    edge_id = paste(
      sort(c(treatment, comparison)),
      outcome_domain,
      collapse = " -- "
    )
  ) |>
  ungroup() |>
  distinct(edge_id, .keep_all = TRUE) |>
  select(-edge_id)

# ============================================================================
# STEP 4: Add node metadata (study design info, frequency, etc.)
# ============================================================================

# For each intervention, count unique studies (not total appearances)
node_metadata <- hpsb_meta_network_data |>
  select(intervention = treatment, study_design, study_id) |>
  bind_rows(
    hpsb_meta_network_data |>
      select(intervention = comparison, study_design, study_id)
  ) |>
  group_by(intervention, study_design) |>
  summarise(study_count = n_distinct(study_id), .groups = "drop") |>
  group_by(intervention) |>
  summarise(
    total_unique_studies = sum(study_count),
    primary_design = study_design[which.max(study_count)],
    design_mix = case_when(
      n_distinct(study_design) > 1 ~ "Mixed",
      TRUE ~ primary_design
    ),
    .groups = "drop"
  )

# Add frequency information
node_metadata <- node_metadata |>
  left_join(total_frequency, by = "intervention") |>
  mutate(
    total_unique_studies = replace_na(total_unique_studies, 1),
    total_appearances = replace_na(total_appearances, 1),
    primary_design = replace_na(primary_design, "Unknown"),
    design_mix = replace_na(design_mix, "Unknown"),
    is_reference = intervention == reference_intervention
  )

# Combine with node list
nodes_with_metadata <- all_nodes |>
  left_join(node_metadata, by = "intervention") |>
  mutate(
    total_unique_studies = replace_na(total_unique_studies, 1),
    total_appearances = replace_na(total_appearances, 1),
    primary_design = replace_na(primary_design, "Unknown"),
    design_mix = replace_na(design_mix, "Unknown"),
    is_reference = replace_na(is_reference, FALSE)
  )

# ============================================================================
# STEP 5: Create igraph object
# ============================================================================

hpsb_meta_igraph_object <- graph_from_data_frame(
  d = edges |>
    select(
      from = treatment,
      to = comparison,
      weight = n_studies
    ),
  vertices = nodes_with_metadata |>
    select(
      name = intervention,
      everything()
    ),
  directed = FALSE
)

# ============================================================================
# STEP 6: Calculate layout with special positioning for reference
# ============================================================================

# Start with a standard layout
layout_coords_initial <- layout_with_fr(hpsb_meta_igraph_object)

# process the layout to centre the reference category
ref_index <- which(V(hpsb_meta_igraph_object)$name == reference_intervention)
layout_coords <- layout_coords_initial
layout_coords[ref_index, ] <- c(0, 0)

# ============================================================================
# STEP 7: Add coordinates to nodes
# ============================================================================

nodes_with_coords <- nodes_with_metadata |>
  mutate(
    x = layout_coords[, 1],
    y = layout_coords[, 2]
  )

# ============================================================================
# STEP 8: Prepare edges for ggplot
# ============================================================================

edges_for_plot <- edges |>
  left_join(
    nodes_with_coords |>
      select(
        intervention,
        x,
        y
      ),
    by = c(
      "treatment" = "intervention"
    )
  ) |>
  rename(
    x1 = x,
    y1 = y
  ) |>
  left_join(
    nodes_with_coords |>
      select(
        intervention,
        x,
        y
      ),
    by = c(
      "comparison" = "intervention"
    )
  ) |>
  rename(
    x2 = x,
    y2 = y
  ) |>
  # Add edge type based on study design
  mutate(
    edge_type = case_when(
      has_rct & has_qed ~ "Mixed",
      has_rct ~ "RCT",
      has_qed ~ "QED",
      TRUE ~ "Unknown"
    )
  ) |>
  # Add stacking offset for multiple outcome domains
  group_by(
    treatment,
    comparison
  ) |>
  mutate(
    n_outcomes = n(),
    outcome_rank = row_number(),
    offset_distance = 0.01, # Adjust this value to control spacing
    offset = (outcome_rank - (n_outcomes + 1) / 2) * offset_distance,
    # Calculate perpendicular offset
    edge_length = sqrt((x2 - x1)^2 + (y2 - y1)^2),
    perp_x = -(y2 - y1) / edge_length * offset,
    perp_y = (x2 - x1) / edge_length * offset,
    # Apply offset to coordinates
    x1_adj = x1 + perp_x,
    y1_adj = y1 + perp_y,
    x2_adj = x2 + perp_x,
    y2_adj = y2 + perp_y
  ) |>
  ungroup()

# ============================================================================
# STEP 9: Create the ggplot
# ============================================================================

network_map <- ggplot() +
  # Add edges first (so they appear behind nodes) - use adjusted coordinates
  geom_segment(
    data = edges_for_plot,
    aes(
      x = x1_adj,
      y = y1_adj,
      xend = x2_adj,
      yend = y2_adj,
      color = outcome_domain
    ), # Color by outcome domain
    linewidth = 1,
    alpha = 0.7
  ) +
  # Add nodes - use shape for study design
  geom_point(
    data = nodes_with_coords,
    aes(
      x = x,
      y = y,
      size = total_unique_studies,
      shape = design_mix
    ), # Shape by study design instead of reference
    stroke = 1.5,
    alpha = 0.8,
    color = "black" # Black color for all nodes
  ) +
  # Add labels with repelling
  geom_text_repel(
    data = nodes_with_coords,
    aes(
      x = x,
      y = y,
      label = intervention
    ),
    size = 3,
    #max.overlaps = 20,
    #bg.color = "white",
    #bg.r = 0.1,
    #force = 2,
    #force_pull = 0.5,
    position = position_nudge_center(
      x = 0.25,
      y = 0.25,
      center_x = 0.5,
      center_y = 0.5,
      direction = "radial"
    )
  ) +
  # Customize edge color scale
  scale_color_discrete(name = "Outcome Domain") +
  # Customize node shape scale for study design
  scale_shape_manual(
    name = "Study Design Type",
    values = c(
      "Randomised" = 16, # Circle
      "Non-randomised" = 17, # Triangle
      "Mixed" = 15 # Square
    )
  ) +
  # Customize size scales
  scale_size_continuous(
    name = "Number of Studies",
    range = c(2, 8),
    guide = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = str_wrap(
      "Key — AT: Adventure Therapy; CBT(G): Cognitive Behaviour Therapy (Group); CBT(I): Cognitive Behaviour Therapy (Individual); EX: Exercise; FFT: Functional Family Therapy; FT: Family Therapy; GT: Group Therapy; IT: Individual Therapy; OTH: Other; MST: Multisystemic Therapy; RP: Relapse Prevention; SE: Sex Education; SST: Social Skills Training; TAU: Treatment as Usual",
      width = 220
    ),
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 1, size = 10)
  )

network_map
