# Test neighbor_quantifier function for calculating spatial statistics around points
# Juan Zwolinski and Kevin Stierhoff
# 27 March 2025

devtools::install_github("SWFSC/atm")

library(atm)

# Simulated data, 3 trawls, 2 transects
trawl <- data.frame(long = c(-125, -125.5, -124.3),
                    lat  = c(  35,   36,     35.5))

sA    <- data.frame(long = rep(seq(-126, -124, l = 100), 2),
                    lat = c(rep(35.3, 100), rep(36, 100)) ,
                    nasc = exp(rnorm(200)))

# Small radius, no samples in two of the trawls
test.sp <- neighbor_quantifier(small.file = trawl, large.file = sA, variable ="nasc", method = "sp",
                               radius = 10, save.fig = TRUE)
test.sp

test.sf <- neighbor_quantifier(small.file = trawl, large.file = sA, variable ="nasc", method = "sf",
                               radius = 10, save.fig = TRUE)
test.sf

# Increased range, 18, 41, and 32 samples
test.sp <- neighbor_quantifier(small.file = trawl, large.file = sA, variable ="nasc", method = "sp",
                               radius = 20, save.fig = TRUE)
test.sp

test.sf <- neighbor_quantifier(small.file = trawl, large.file = sA, variable ="nasc", method = "sf",
                               radius = 20, save.fig = TRUE)
test.sf

# Increased range even more, 34, 51, 38 samples
test.sp <- neighbor_quantifier(small.file = trawl, large.file = sA, variable ="nasc", method = "sp",
                               radius = 30, save.fig = TRUE)
test.sp

test.sf <- neighbor_quantifier(small.file = trawl, large.file = sA, variable ="nasc", method = "sf",
                               radius = 30, save.fig = TRUE)
test.sf
