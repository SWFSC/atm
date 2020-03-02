library(tidyverse)

anchovy <- readRDS("data/final.anchovy.rds") %>%
  mutate(scientificName = "Engraulis mordax")

herring <- readRDS("data/final.herring.rds") %>%
  mutate(scientificName = "Clupea pallasii")

jackmack <- readRDS("data/final.jackmac.rds") %>%
  mutate(scientificName = "Trachurus symmetricus")

pacmack <- readRDS("data/final.pacmac.rds") %>%
  mutate(scientificName = "Scomber japonicus")

sardine <- readRDS("data/final.sardine.rds") %>%
  mutate(scientificName = "Sardinops sagax")

cps.data <- anchovy %>%
  bind_rows(herring) %>%
  bind_rows(jackmack) %>%
  bind_rows(pacmack) %>%
  bind_rows(sardine) %>%
  select(cruise, ship, haul, collection, scientificName, standardLength_mm, forkLength_mm,
         totalLength_mm, weightg, sex, year, season)

saveRDS(cps.data, file = "data/cps_specimen_data.rds")

cps.plot <- cps.data %>%
  select(scientificName, season, sex, weightg, totalLength_mm, standardLength_mm, forkLength_mm) %>%
  pivot_longer(cols = c(totalLength_mm, standardLength_mm, forkLength_mm),
               names_to = "length_type",
               values_to = "measurement") %>%
  filter(!is.na(weightg)) %>%
  ungroup()


# ggplot(cps.plot, aes(measurement, weightg, colour = length_type)) +
#   geom_point(alpha = 0.2) +
#   facet_grid(rows = vars(length_type), cols = vars(scientificName),
#              scales = "free") +
#   theme_bw()

ggplot(filter(cps.plot, !is.na(measurement)),
              aes(measurement, weightg, colour = length_type)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~scientificName,
             scales = "free") +
  theme_bw()

# Create data frame with predicted weights
# Get max TL for plotting L/W models
L.max <- data.frame(scientificName = c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                                       "Scomber japonicus","Trachurus symmetricus"),
                    max.TL = c(30, 20, 40, 60, 60))

# Data frame for storing results
lw.df <- tibble()

# Generate length/weight curves
for (i in unique(L.max$scientificName)) {
  # Create a length vector for each species
  totalLength_mm <- seq(0, L.max$max.TL[L.max$scientificName == i]) * 10
  forkLength_mm <- atm::convert_length(i, totalLength_mm, "TL", "FL")
  standardLength_mm <- atm::convert_length(i, totalLength_mm, "TL", "SL")

  # Calculate weights from lengths
  weightg <- atm::estimate_weight(i, totalLength_mm, season = tolower("spring"))

  # Combine results
  lw.df <- bind_rows(lw.df, as_tibble(
        data.frame(scientificName = i, weightg, totalLength_mm,
                                       standardLength_mm, forkLength_mm)))
}

# Pivot lw.df
lw.df <- lw.df %>%
  pivot_longer(cols = c(totalLength_mm, standardLength_mm, forkLength_mm),
               names_to = "length_type", values_to = "measurement")

# Plot models on top of data; redo cps.plot
ggplot() +
  geom_line(data = lw.df, aes(measurement, weightg, colour = length_type),
            linetype = "dashed", size = 1) +
  geom_point(data = filter(cps.plot, !is.na(measurement)),
       aes(measurement, weightg, colour = length_type), alpha = 0.2) +
  facet_wrap(~scientificName,
             scales = "free") +
  theme_bw()
