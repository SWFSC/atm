library(tidyverse)

# Create a test data frame for spring
est.df <- dplyr::bind_rows(
  data.frame(scientificName = rep(c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                                    "Scomber japonicus","Trachurus symmetricus"), 2),
             totalLength_mm = 20,
             weightg = 1,
             model.type = c(rep("OLS", 5), rep("GLM", 5)),
             season = "spring"),
  data.frame(scientificName = rep(c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                                    "Scomber japonicus","Trachurus symmetricus"), 2),
             totalLength_mm = 20,
             weightg = 1,
             model.type = c(rep("OLS", 5), rep("GLM", 5)),
             season = "summer")) %>%
  mutate(
    weight.est = atm::estimate_weight(scientificName, totalLength_mm, model.type, season),
    length.est = atm::estimate_length(scientificName, weight.est, model.type, season)
  )

# Test for differences
all.equal(est.df$totalLength_mm, est.df$length.est)

# Create a test data frame for spring
conv.df <- data.frame(
  scientificName = c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                     "Scomber japonicus","Trachurus symmetricus")) %>%
  mutate(L.in = 20) %>%
  mutate(
    # From TL
    TL2SL = atm::convert_length(scientificName, L.in, "TL", "SL"),
    TL2FL = atm::convert_length(scientificName, L.in, "TL", "FL"),
    # From SL
    SL2TL = atm::convert_length(scientificName, L.in, "SL", "TL"),
    SL2FL = atm::convert_length(scientificName, L.in, "SL", "FL"),
    # From FL
    FL2TL = atm::convert_length(scientificName, L.in, "FL", "TL"),
    FL2SL = atm::convert_length(scientificName, L.in, "FL", "SL")
  )

# Print results
conv.df
