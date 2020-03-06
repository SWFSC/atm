# Create a test data frame for spring
spring.df <- data.frame(scientificName = rep(c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                                         "Scomber japonicus","Trachurus symmetricus", "Atherinops"), 2),
                      totalLength_mm = 20,
                      weightg = 1,
                      model.type = c(rep("OLS", 6), rep("GLM", 6)),
                      season = "spring")

# Create a test data frame for summer
summer.df <- data.frame(scientificName = rep(c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                                               "Scomber japonicus","Trachurus symmetricus", "Atherinops"), 2),
                        totalLength_mm = 20,
                        weightg = 1,
                        model.type = c(rep("OLS", 6), rep("GLM", 6)),
                        season = "summer")
# # #
# # # Test values
# i = "Clupea pallasii"
# j = "OLS"
# k = "spring"
#
# df.in <- spring.df
# df <- dplyr::filter(df.in, scientificName == i, model.type == j)

# Function tests
# Estimate weight-Spring
spring.df$weight.est <- estimate_weight(spring.df$scientificName, spring.df$totalLength_mm, spring.df$model.type, spring.df$season)

spring.df
# Estimate weight-Summer
summer.df$weight.est <- estimate_weight(summer.df$scientificName, summer.df$totalLength_mm, summer.df$model.type, summer.df$season)

summer.df

# Estimate length-Spring
spring.df$tl.est <- estimate_length(spring.df$scientificName, spring.df$weightg, spring.df$model.type, spring.df$season)

spring.df

# Estimate length-Summer
summer.df$tl.est <- estimate_length(summer.df$scientificName, summer.df$weightg, summer.df$model.type, summer.df$season)

summer.df


# Create a test data frame for spring
df <- data.frame(scientificName = c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
              "Scomber japonicus","Trachurus symmetricus", "Atherinops"))

# From TL
df$TL2SL <- convert_length(df$scientificName, 20, "TL", "SL")
df$TL2FL <- convert_length(df$scientificName, 20, "TL", "FL")
# From SL
df$SL2TL <- convert_length(df$scientificName, 20, "SL", "TL")
df$SL2FL <- convert_length(df$scientificName, 20, "SL", "FL")

# From FL
df$FL2TL <- convert_length(df$scientificName, 20, "FL", "TL")
df$FL2SL <- convert_length(df$scientificName, 20, "FL", "SL")

df
