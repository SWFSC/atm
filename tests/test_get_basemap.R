load(here::here("data/basemap_test.Rdata"))

get_basemap(nav, states, countries, landmarks, bathy, crs = 3310)

# ggplot2::ggsave(filename = here::here("tests/get_map_test.png"),
#        height = 10, width = 5)
