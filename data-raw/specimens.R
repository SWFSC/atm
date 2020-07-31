library(tidyverse)

# Load raw specimen data
anchovy <- readRDS("inst/extdata/final.anchovy.rds") %>%
  mutate(scientificName = "Engraulis mordax")

herring <- readRDS("inst/extdata/final.herring.rds") %>%
  mutate(scientificName = "Clupea pallasii")

jackmack <- readRDS("inst/extdata/final.jackmac.rds") %>%
  mutate(scientificName = "Trachurus symmetricus")

pacmack <- readRDS("inst/extdata/final.pacmac.rds") %>%
  mutate(scientificName = "Scomber japonicus")

sardine <- readRDS("inst/extdata/final.sardine.rds") %>%
  mutate(scientificName = "Sardinops sagax")

# Format combined data
specimens <- anchovy %>%
  bind_rows(herring) %>%
  bind_rows(jackmack) %>%
  bind_rows(pacmack) %>%
  bind_rows(sardine) %>%
  select(scientificName, season, sex, weightg, totalLength_mm, standardLength_mm, forkLength_mm) %>%
  pivot_longer(cols = c(totalLength_mm, standardLength_mm, forkLength_mm),
               names_to = "length_type",
               values_to = "measurement") %>%
  filter(!is.na(weightg)) %>%
  ungroup()

# Save to RDS
saveRDS(specimens, file = "data/specimens.rds")
