#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 30/09/2025                                                                           #
# Purpose: visualise PRISMA flowchart                                                        #
#============================================================================================#

# load required packages
library(tidyverse)
library(readxl)
library(PRISMA2020)

# load data
raw_prisma_data_location <- "./visualisation/inputs/almp_nma_prisma_data.csv"
raw_prisma_data <- read.csv(raw_prisma_data_location)

# create prisma flowchart
prisma_data <- PRISMA_data(raw_prisma_data)

prisma_figure <- PRISMA_flowdiagram(
  data = prisma_data,
  interactive = FALSE,
  previous = FALSE,
  other = TRUE,
  fontsize = 12,
  font = "Helvetica",
  title_colour = "Goldenrod1",
  greybox_colour = "Gainsboro",
  main_colour = "Black",
  arrow_colour = "Black",
  arrow_head = "normal",
  arrow_tail = "none",
  side_boxes = TRUE
)
prisma_figure

# export flowchart
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./visualisation/output/almp_nma_prisma_flowchart.png",
  filetype = "png",
  overwrite = TRUE
)
