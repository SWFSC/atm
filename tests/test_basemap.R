library(atm)
library(tidyverse)

# Load test data
load(here::here("data/basemap.Rdata"))

# Create map
atm::get_basemap(nav, states, countries, landmarks, bathy, crs = 3310)

# Wrangle data -----------------------------------
# library(rnaturalearth)
# library(sf)
# library(tidyverse)
#
# nav <- read.csv("inst/extdata/nav.csv") %>%
#   st_as_sf(coords = c("long","lat"), crs = 4326)
#
# #
# landmarks <- read.csv("inst/extdata/landmarks.csv") %>% filter(group == "city") %>% project_df(to = 3310)
#
# # Get state data
# states <- ne_states(country = 'United States of America', returnclass = 'sf') %>%
#   filter(name == "California")
#
# # Get countries
# countries <- ne_countries(scale = "large", returnclass = "sf") %>%
#   filter(subregion %in% c("Northern America"))
#
# # Read bathy contours shapefile
# bathy <- st_read("inst/extdata/bathy_contours.shp") %>%
#   st_transform(4326) %>%
#   rename(Depth = Contour)

# save(nav, landmarks, states, countries, bathy, file = "inst/extdata/basemap.Rdata")
# save(nav, landmarks, states, countries, bathy, file = "data/basemap.Rdata")
