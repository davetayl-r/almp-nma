#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 15/09/2025                                                                           #
# Purpose: Helper functions for the NMA                                                      #
#============================================================================================#

# declare function to create component indicator variables
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
