# A function for calculating spatial statistics around points
# Juan Zwolinski and Kevin Stierhoff
# 12 March 2025

library(tidyverse)
library(sf)
library(sp)
library(mapview)

# Simulated data, 3 trawls, 2 transects
trawl <- data.frame(long = c(-125, -125.5, -124.3),
                    lat  = c(  35,   36,     35.5))

sA    <- data.frame(long = rep(seq(-126, -124, l = 100), 2),
                    lat = c(rep(35.3, 100), rep(36, 100)) ,
                    nasc = exp(rnorm(200)))

# Final function, contains both sp and sf methods
neighbor_quantifier <- function(small.file, large.file, variable = NULL,
                                method = "sp", crs = 4326, radius = 20, length = 100,
                                plot.fig = TRUE, save.fig = TRUE, fig.name = NULL){
  # small.file is the file to average to
  # large.file is the file that provides the quantitative information of the required quantitative "variable"
  # Both small.file and large.file must have long and lat
  # method is either "sp" (cartesian space) or "sf" (cartesian or geodetic space)
  # crs is the coordinate reference system; 4326 for lat/long data, e.g., from a GPS
  # radius is in nautical miles
  # length is the length of half circle, in meters?

  # Rename columns in large file
  names(large.file)[match(variable, names(large.file) )] <- "variable"

  # Initialize vectors for storing results
  n <- mean <- max <- min <- sd <- numeric(0)

  if (method == "sp") {
    for(i in 1:nrow(small.file)){
      # Create temporary file
      small.file.temp <- small.file[i, ]

      # Create nodes for polygon used by {sp}
      polygon.x <- c(small.file.temp$long + seq(-radius, radius, l = length) / 60 / cos(small.file.temp$lat*pi / 180),
                     small.file.temp$long + rev(seq(-radius, radius, l = length) / 60 / cos(small.file.temp$lat*pi / 180)))

      polygon.y <- c(small.file.temp$lat + sqrt(radius^2 - seq(-radius, radius, l = length)^2) / 60,
                     small.file.temp$lat - sqrt(radius^2  - rev(seq(-radius, radius, l = length))^2) / 60)

      # Put polygon coordinates into a data frame
      polygon.i <- data.frame(polygon = i, X = polygon.x, Y = polygon.y)

      if (exists("polygon.i.xy")) {
        polygon.i.xy <- dplyr::bind_rows(polygon.i.xy, polygon.i)
      } else {
        polygon.i.xy <- polygon.i
      }

      if (exists("large.file.intersects")) {
        # Get intervals that intersect polygons, for plotting
        large.file.intersects <- large.file.intersects %>%
          bind_rows(large.file[
            sp::point.in.polygon(large.file$long, large.file$lat,
                                 polygon.x, polygon.y) == 1, ])
      } else {
        large.file.intersects <- large.file[
          sp::point.in.polygon(large.file$long, large.file$lat,
                               polygon.x, polygon.y) == 1, ]
      }

      # Compute number of intervals within each polygon
      n <- c(n, sum(sp::point.in.polygon(large.file$long[!is.na(large.file$variable)],
                                         large.file$lat[!is.na(large.file$variable)],
                                         polygon.x, polygon.y) == 1))

      # Compute mean values within each polygon
      mean <- c(mean,
                mean(large.file$variable[
                  sp::point.in.polygon(large.file$long, large.file$lat,
                                       polygon.x, polygon.y) == 1], na.rm = TRUE))

      # Compute min values within each polygon
      min  <- c(min,
                min(large.file$variable[
                  sp::point.in.polygon(large.file$long, large.file$lat,
                                       polygon.x, polygon.y) == 1], na.rm = TRUE))

      # Compute max values within each polygon
      max  <- c(max,
                max(large.file$variable[
                  sp::point.in.polygon(large.file$long, large.file$lat,
                                       polygon.x, polygon.y) == 1], na.rm = TRUE))

      # Compute standard deviation values within each polygon
      sd   <- c(sd,
                sd(large.file$variable[
                  sp::point.in.polygon(large.file$long, large.file$lat,
                                       polygon.x, polygon.y) == 1], na.rm = TRUE))
    }

    if (plot.fig) {
      # Add polygon to plot
      var.plot <- ggplot2::ggplot() +
        ggplot2::geom_point(data = large.file, aes(long, lat, size = variable/max(lat)*10),
                            shape = 21, show.legend = FALSE) +
        ggplot2::geom_point(data = polygon.i.xy, aes(X, Y, group = polygon), shape = 21, fill = NA) +
        ggplot2::geom_point(data = large.file.intersects,
                            aes(long, lat, size = variable/max(lat)*10),
                            colour = "green", show.legend = FALSE) +
        ggplot2::geom_point(data = small.file, aes(long, lat), shape = 21, fill = "red", size = 3) +
        ggplot2::theme_bw()

      if (save.fig) {
        if (is.null(fig.name)) {
          ggplot2::ggsave(var.plot, filename = "variable-plot-sp.png")
        } else {
          ggplot2::ggsave(var.plot, filename = fig.name)
        }
      }
    }
  } else if (method == "sf") {
    # Convert files to sf objects
    small.file.sf <- sf::st_as_sf(small.file, coords = c("long", "lat"), crs = crs) %>%
      dplyr::mutate(
        long = as.data.frame(sf::st_coordinates(.))$X,
        lat = as.data.frame(sf::st_coordinates(.))$Y)

    large.file.sf <- sf::st_as_sf(large.file, coords = c("long", "lat"), crs = crs) %>%
      mutate(
        long = as.data.frame(sf::st_coordinates(.))$X,
        lat = as.data.frame(sf::st_coordinates(.))$Y)

    # Rename columns in large file
    names(large.file.sf)[match(variable, names(large.file.sf) )] <- "variable"

    for(i in 1:nrow(small.file.sf)){
      # Create circular polygon using a spatial buffer
      polygon.i <- small.file.sf[i, ] %>%
        sf::st_buffer(radius*1852)

      if (exists("polygon.i.sf")) {
        polygon.i.sf <- dplyr::bind_rows(polygon.i.sf, polygon.i)
      } else {
        polygon.i.sf <- polygon.i
      }

      # Get number of intersecting points
      n <- c(n, length(st_intersection(large.file.sf, polygon.i)$variable))

      # Compute mean values within each polygon
      mean <- c(mean, mean(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))

      # Compute min values within each polygon
      min  <- c(min, min(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))

      # Compute max values within each polygon
      max  <- c(max, max(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))

      # Compute standard deviation values within each polygon
      sd   <- c(sd, sd(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))
    }

    # Get intervals that intersect polygons, for plotting
    large.file.intersects <- st_intersection(large.file.sf, polygon.i.sf)

    if (plot.fig) {
      # Add polygon to plot
      var.plot <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = large.file.sf, aes(size = variable/max(lat)*10),
                         shape = 21, show.legend = FALSE) +
        ggplot2::geom_sf(data = large.file.intersects,
                         aes(size = variable/max(long)*10),
                         colour = "green", show.legend = FALSE) +
        ggplot2::geom_sf(data = small.file.sf, shape = 21, fill = "red", size = 3) +
        ggplot2::geom_sf(data = polygon.i.sf, fill = NA, linetype = "dashed") +
        ggplot2::coord_sf() +
        ggplot2::theme_bw()

      if (save.fig) {
        if (is.null(fig.name)) {
          ggplot2::ggsave(var.plot, filename = "variable-plot-sf.png")
        } else {
          ggplot2::ggsave(var.plot, filename = fig.name)
        }
      }
    }

  }
  # Return results
  return(data.frame(n, mean, min, max, sd))
}

# Clear graphics window
graphics.off()

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

# Unused functions ---------------------------------------------------------
# # Revised function (uses sf)
# neighbor_quantifier_sf <- function(small.file, large.file, variable = NULL, radius = 20, plot.data = TRUE, length = 100, crs = 4326) {
#   # small.file is the file to average to
#   # large.file is the file that provides the quantitative information of the required quantitative "variable"
#   # Both small.file and large.file must have long and lat
#   # radius is in nautical miles
#   # length is the number of nodes on the polygon around small.file locations
#
#   # Require the {sf} package
#   require(sf)
#   require(tidyverse)
#
#   small.file.sf <- st_as_sf(small.file, coords = c("long","lat"), crs = crs) %>%
#     mutate(
#       long = as.data.frame(st_coordinates(.))$X,
#       lat = as.data.frame(st_coordinates(.))$Y)
#
#   large.file.sf <- st_as_sf(large.file, coords = c("long","lat"), crs = crs) %>%
#     mutate(
#       long = as.data.frame(st_coordinates(.))$X,
#       lat = as.data.frame(st_coordinates(.))$Y)
#
#   # Rename variables
#   names(large.file)[match(variable, names(large.file) )] <- "variable"
#   names(large.file.sf)[match(variable, names(large.file.sf) )] <- "variable"
#
#   # Initialize variables
#   n <- mean <- max <- min <- sd <- numeric(0)
#
#   # For each point to compute statistics
#   for(i in 1:nrow(small.file)){
#     print(i)
#
#     # Create temporary file
#     small.file.temp <- small.file[i, ] # temporary file
#
#     # Create circular polygons using math
#     polygon.x <- c(small.file.temp$long + seq(-radius, radius, l =length)/60/cos(small.file.temp$lat*pi/180),
#                    small.file.temp$long + rev(seq(-radius, radius, l =length)/60/cos(small.file.temp$lat*pi/180)))
#     polygon.y <- c(small.file.temp$lat + sqrt(radius^2 - seq(-radius, radius, l =length)^2)/60,
#                    small.file.temp$lat - sqrt(radius^2  - rev(seq(-radius, radius, l =length))^2)/60)
#
#     # Create circular polygon using a mathematical method
#     polygon.xy <- data.frame(polygon.x, polygon.y) %>%
#       st_as_sf(coords = c("polygon.x","polygon.y"), crs = 4326) %>%
#       summarise(do_union = F) %>%
#       st_cast("POLYGON") %>%
#       st_make_valid()
#
#     # Create circular polygon using a spatial buffer
#     polygon.i <- small.file.sf[i, ] %>% st_buffer(radius*1852)
#
#     # Combine polygon objects
#     if (exists("polygon.xy.sf")) {
#       polygon.xy.sf <- bind_rows(polygon.xy, polygon.xy.sf)
#     } else {
#       polygon.xy.sf <- polygon.xy
#     }
#
#     if (exists("polygon.i.sf")) {
#       polygon.i.sf <- bind_rows(polygon.i.sf, polygon.i)
#     } else {
#       polygon.i.sf <- polygon.i
#     }
#
#     # Get number of intersecting points
#     n <- c(n, length(st_intersection(large.file.sf, polygon.i)$variable))
#
#     # Compute mean
#     mean <- c(mean, mean(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))
#
#     # Compute min
#     min  <- c(min, min(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))
#
#     # Compute max
#     max  <- c(max, max(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))
#
#     # Compute standard deviation
#     sd   <- c(sd, sd(st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))
#   }
#
#   if (plot.data) {
#     # Add polygon to plot
#     var.plot <- ggplot() +
#       geom_sf(data = large.file.sf, aes(size = variable/max(lat)*10),
#               shape = 21, show.legend = FALSE) +
#       geom_sf(data = small.file.sf, shape = 21, fill = "red", size = 3) +
#       geom_sf(data = polygon.i.sf, fill = NA, linetype = "dotted") +
#       geom_sf(data = polygon.xy.sf, colour = "red", fill = NA, linetype = "dashed") +
#       coord_sf() +
#       theme_bw()
#
#     ggsave(var.plot, filename = "variable-plot-sf.png")
#   }
#
#   # Return results
#   return(data.frame(n, mean, min, max, sd))
# }
