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
library(ggrepel)
library(ggpp)

# read data
almp_nma_network_raw_data_location <- "./visualisation/inputs/almp_nma_network_map_data.RDS"
almp_nma_network_raw_data <- readRDS(almp_nma_network_raw_data_location)

# set seed
set.seed(345)

# prepare data for plot
almp_nma_network_data <- almp_nma_network_raw_data |>
  select(
    study_id,
    intervention,
    comparison,
    outcome_domain
  ) |>
  # rename groups into readable keys
  mutate(
    intervention = str_replace_all(
      intervention,
      "basic_skills_training",
      "BST"
    ),
    intervention = str_replace_all(
      intervention,
      "soft_skills_training",
      "SST"
    ),
    intervention = str_replace_all(
      intervention,
      "behavioural_skills_training",
      "BeST"
    ),
    intervention = str_replace_all(
      intervention,
      "job_specific_technical_skills_off_job_training",
      "Off-JT"
    ),
    intervention = str_replace_all(
      intervention,
      "self_employment_support",
      "SES"
    ),
    intervention = str_replace_all(
      intervention,
      "job_search_preparation",
      "JSP"
    ),
    intervention = str_replace_all(
      intervention,
      "job_search_assistance",
      "JSA"
    ),
    intervention = str_replace_all(
      intervention,
      "employment_counselling",
      "Emp-counsel"
    ),
    intervention = str_replace_all(
      intervention,
      "employment_coaching",
      "Emp-coach"
    ),
    intervention = str_replace_all(
      intervention,
      "financial_assistance",
      "FIN"
    ),
    intervention = str_replace_all(
      intervention,
      "job_specific_technical_skills_on_job_training",
      "On-JT"
    ),
    intervention = str_replace_all(
      intervention,
      "paid_temporary_work_experience",
      "Paid-WE"
    ),
    intervention = str_replace_all(
      intervention,
      "unpaid_temporary_work_experience",
      "Unpaid-WE"
    ),
    intervention = str_replace_all(
      intervention,
      "wage_subsidies",
      "WS"
    ),
    intervention = str_replace_all(
      intervention,
      "public_works",
      "PW"
    ),
    intervention = str_replace_all(
      intervention,
      "other_active_component_nec",
      "Oth"
    ),
    comparison = str_replace_all(
      comparison,
      "basic_skills_training",
      "BST"
    ),
    comparison = str_replace_all(
      comparison,
      "soft_skills_training",
      "SST"
    ),
    comparison = str_replace_all(
      comparison,
      "behavioural_skills_training",
      "BeST"
    ),
    comparison = str_replace_all(
      comparison,
      "job_specific_technical_skills_off_job_training",
      "Off-JT"
    ),
    comparison = str_replace_all(
      comparison,
      "self_employment_support",
      "SES"
    ),
    comparison = str_replace_all(
      comparison,
      "job_search_preparation",
      "JSP"
    ),
    comparison = str_replace_all(
      comparison,
      "job_search_assistance",
      "JSA"
    ),
    comparison = str_replace_all(
      comparison,
      "employment_counselling",
      "Emp-counsel"
    ),
    comparison = str_replace_all(
      comparison,
      "employment_coaching",
      "Emp-coach"
    ),
    comparison = str_replace_all(
      comparison,
      "financial_assistance",
      "FIN"
    ),
    comparison = str_replace_all(
      comparison,
      "job_specific_technical_skills_on_job_training",
      "On-JT"
    ),
    comparison = str_replace_all(
      comparison,
      "paid_temporary_work_experience",
      "Paid-WE"
    ),
    comparison = str_replace_all(
      comparison,
      "unpaid_temporary_work_experience",
      "Unpaid-WE"
    ),
    comparison = str_replace_all(
      comparison,
      "wage_subsidies",
      "WS"
    ),
    comparison = str_replace_all(
      comparison,
      "public_works",
      "PW"
    ),
    comparison = str_replace_all(
      comparison,
      "other_active_component_nec",
      "Oth"
    ),
    comparison = str_replace_all(
      comparison,
      "services_as_usual",
      "SAU"
    ),
    intervention = str_replace_all(
      intervention,
      "\\+",
      " + "
    ),
    comparison = str_replace_all(
      comparison,
      "\\+",
      " + "
    ),
    intervention = str_wrap(intervention, 30),
    comparison = str_wrap(comparison, 30)
  ) |>
  select(
    study_id,
    intervention,
    comparison,
    outcome_domain
  ) |>
  distinct()

# ----------------------------------------------------------------------------
# STEP 1: Create nodes (unique interventions + comparisons)
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
# STEP 2: Identify the most common comparator
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
# STEP 3: Create edges (direct comparisons between interventions)
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
# STEP 4: Add node metadata (study design info, frequency, etc.)
# ----------------------------------------------------------------------------

# For each intervention, count unique studies (not total appearances)
node_metadata <- almp_nma_network_data |>
  select(intervention = intervention, study_id) |>
  bind_rows(
    almp_nma_network_data |>
      select(intervention = comparison, study_id)
  ) |>
  group_by(intervention) |>
  summarise(study_count = n_distinct(study_id), .groups = "drop") |>
  group_by(intervention) |>
  summarise(
    total_unique_studies = sum(study_count),
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

# ----------------------------------------------------------------------------
# STEP 5: Create igraph object
# ----------------------------------------------------------------------------

hpsb_meta_igraph_object <- graph_from_data_frame(
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

# Start with a standard layout
layout_coords_initial <- layout_with_fr(hpsb_meta_igraph_object)

# process the layout to centre the reference category
ref_index <- which(V(hpsb_meta_igraph_object)$name == reference_intervention)
layout_coords <- layout_coords_initial
layout_coords[ref_index, ] <- c(0, 0)

# ----------------------------------------------------------------------------
# STEP 7: Add coordinates to nodes
# ----------------------------------------------------------------------------

nodes_with_coords <- nodes_with_metadata |>
  mutate(
    x = layout_coords[, 1],
    y = layout_coords[, 2]
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
      "Key — AT: Adventure Therapy; CBT(G): Cognitive Behaviour Therapy (Group); CBT(I): Cognitive Behaviour Therapy (Individual); EX: Exercise; FFT: Functional Family Therapy; FT: Family Therapy; GT: Group Therapy; IT: Individual Therapy; OTH: Other; MST: Multisystemic Therapy; RP: Relapse Prevention; SE: Sex Education; SST: Social Skills Training; TAU: intervention as Usual",
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

#basic_skills_training == BST
#soft_skills_training = SST
#behavioural_skills_training == BeST
#job_specific_technical_skills_off_job_training == Off-JT
#self_employment_support = SES
#job_search_preparation = JSP
#job_search_assistance = JSA
#employment_counselling = Emp-counsel
#employment_coaching =  Emp-coach
#financial_assistance = FIN
#job_specific_technical_skills_on_job_training
##unpaid_temporary_work_experience = unpaid-WE
#wage_subsidies = WS
#public_works = PW
#other_active_component_nec = Oth
#services_as_usual = SAU
