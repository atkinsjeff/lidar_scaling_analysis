# This script takes the NEON TOS plot metadata and finds the bounding box in UTM for the tower forestry 40 x 40 mplots
# located in the tower airshed.

library(tidyverse)

# neon gis data
gis <- read.csv("./data/neon_plot_data.csv")

# finding the max and min/
gis %>%
    group_by(siteID) %>%
    filter(plotType == "tower") %>%
    summarize(xmin = round(min(northing), -2),
              xmax = round(max(northing), -2),
              ymin = round(min(easting), -2),
              ymax = round(max(easting), -2)) %>%
    data.frame() -> neon.bounds


neon.bounds$x <- round(((neon.bounds$xmax + neon.bounds$xmin) / 2), -3)
neon.bounds$y <- round(((neon.bounds$ymax + neon.bounds$ymin) / 2), -3)