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
