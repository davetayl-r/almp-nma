#----------------------------------------------------------------------------================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 16/09/2025                                                                           #
# Purpose: visualise network map                                                             #
#----------------------------------------------------------------------------================#

# load required packages
library(tidyverse)
library(igraph)
library(ggplot2)
library(graphlayouts)
#library(ggpp)

# read data
almp_nma_network_raw_data_location <- "./visualisation/inputs/almp_nma_summary_visualisation_data.RDS"
almp_nma_network_raw_data <- readRDS(almp_nma_network_raw_data_location)

# set seed
set.seed(268)

# prepare data for plot
almp_nma_network_data <- almp_nma_network_raw_data |>
  # drop study that didn't make it into the final model
  filter(
    !study_id == "brunetti2017workplacetrainingprograms"
  ) |>
  select(
    study_id,
    intervention,
    comparison,
    outcome_domain
  ) |>
  distinct()

# ----------------------------------------------------------------------------
# 1: Create nodes (unique interventions + comparisons)
# ----------------------------------------------------------------------------

intervention_nodes <- almp_nma_network_data |>
  select(intervention = intervention) |>
  distinct()

comparison_nodes <- almp_nma_network_data |>
  select(intervention = comparison) |>
  distinct()

# Combine all unique interventions
all_nodes <- bind_rows(intervention_nodes, comparison_nodes) |>
  distinct() |>
  mutate(node_id = row_number())

# ----------------------------------------------------------------------------
# 2: Identify the most common comparator
# ----------------------------------------------------------------------------

# Count how often each intervention appears as a comparison
comparison_frequency <- almp_nma_network_data |>
  count(comparison, name = "comparison_count") |>
  arrange(desc(comparison_count))

# Also count intervention frequency for completeness
intervention_frequency <- almp_nma_network_data |>
  count(intervention, name = "intervention_count") |>
  arrange(desc(intervention_count))

# Combined frequency (total appearances)
total_frequency <- almp_nma_network_data |>
  select(intervention = intervention) |>
  bind_rows(almp_nma_network_data |> select(intervention = comparison)) |>
  count(intervention, name = "total_appearances") |>
  arrange(desc(total_appearances))

# Identify the reference intervention (most common comparator)
reference_intervention <- comparison_frequency$comparison[1]

# ----------------------------------------------------------------------------
# 3: Create edges (direct comparisons between interventions)
# ----------------------------------------------------------------------------

# Create edge list from study comparisons
edges_raw <- almp_nma_network_data |>
  select(study_id, intervention, comparison, outcome_domain)

# Create both directions for undirected network (optional - depends on your preference)
edges_bidirectional <- bind_rows(
  edges_raw,
  edges_raw |>
    select(
      study_id,
      outcome_domain,
      intervention = comparison,
      comparison = intervention
    )
)

# Count studies for each comparison AND outcome domain
edges <- edges_bidirectional |>
  group_by(intervention, comparison, outcome_domain) |>
  summarise(
    n_studies = n_distinct(study_id),
    .groups = "drop"
  ) |>
  filter(intervention != comparison) |> # Remove self-loops
  # Remove duplicate edges (keep only one direction for undirected graph)
  rowwise() |>
  mutate(
    edge_id = paste(
      sort(c(intervention, comparison)),
      outcome_domain,
      collapse = " -- "
    )
  ) |>
  ungroup() |>
  distinct(edge_id, .keep_all = TRUE) |>
  select(-edge_id)

# ----------------------------------------------------------------------------
# 4: Add node metadata
# ----------------------------------------------------------------------------

node_metadata <- almp_nma_network_data |>
  # collect study_id for all interventions
  select(intervention = intervention, study_id) |>
  bind_rows(
    almp_nma_network_data |>
      select(intervention = comparison, study_id)
  ) |>
  group_by(intervention) |>
  summarise(
    total_unique_studies = n_distinct(study_id),
    .groups = "drop"
  )

# Add frequency information (total appearances across rows, if you still want it)
node_metadata <- node_metadata |>
  left_join(total_frequency, by = "intervention") |>
  mutate(
    total_appearances = replace_na(total_appearances, 0),
    is_reference = intervention == reference_intervention
  )

# Merge into node list
nodes_with_metadata <- all_nodes |>
  left_join(node_metadata, by = "intervention") |>
  mutate(
    total_unique_studies = replace_na(total_unique_studies, 0),
    #total_appearances = replace_na(total_appearances, 0),
    is_reference = replace_na(is_reference, FALSE)
  )

# ----------------------------------------------------------------------------
# 5: Create igraph object
# ----------------------------------------------------------------------------

almp_nma_network_igraph_object <- graph_from_data_frame(
  d = edges |>
    select(
      from = intervention,
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

# ----------------------------------------------------------------------------
# STEP 6: Calculate layout with special positioning for reference
# ----------------------------------------------------------------------------

coords <- layout_with_graphopt(
  almp_nma_network_igraph_object,
  niter = 10000,
  charge = 0.06, # ↑ repulsion
  spring.length = 0.25 # ↑ spacing near hub
)

# ----------------------------------------------------------------------------
# STEP 7: Add coordinates to nodes
# ----------------------------------------------------------------------------

nodes_with_coords <- nodes_with_metadata |>
  mutate(
    x = coords[, 1],
    y = coords[, 2]
  )

# ----------------------------------------------------------------------------
# STEP 8: Prepare edges for ggplot
# ----------------------------------------------------------------------------

edges_for_plot <- edges |>
  left_join(
    nodes_with_coords |>
      select(
        intervention,
        x,
        y
      ),
    by = c(
      "intervention" = "intervention"
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
  # Add stacking offset for multiple outcome domains
  group_by(
    intervention,
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

# ----------------------------------------------------------------------------
# STEP 9: Create the ggplot
# ----------------------------------------------------------------------------

almp_nma_network_map <- ggplot() +
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
    linewidth = 1.5,
    alpha = 0.7
  ) +
  geom_point(
    data = nodes_with_coords,
    aes(
      x = x,
      y = y,
      size = total_unique_studies
    ),
    #stroke = 1.5,
    #alpha = 0.8,
    color = "black"
  ) +
  # Customize edge color scale
  scale_color_viridis_d(
    name = "Outcome Domain",
    option = "C"
  ) +
  # Customize size scales
  scale_size_continuous(
    name = "Number of Studies",
    range = c(2, 12),
    trans = "sqrt", # or "log10"
    breaks = c(1, 5, 10, 20, 120)
  ) +
  labs(
    title = "",
    subtitle = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    plot.background = element_rect(
      fill = "#FFFFFF"
    ),
    panel.background = element_rect(
      fill = "#FFFFFF"
    ),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
  )

# export plot
ggsave(
  plot = almp_nma_network_map,
  filename = "./visualisation/output/almp_nma_network_map.png",
  height = 8,
  width = 10,
  device = "png",
  type = "cairo-png"
)
