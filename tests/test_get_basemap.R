# Load test data
load(here::here("data/basemap_test.Rdata"))

# Create map
get_basemap(nav, states, countries, landmarks, bathy, crs = 3310)

