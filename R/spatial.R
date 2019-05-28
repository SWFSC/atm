#' Calculate distance to a feature
#'
#' @param x1 Origin longitude.
#' @param y1 Origin latitude.
#' @param x2 Destination longitude.
#' @param y2 Destination latitude.
#' @return A vector containing the distance from origin to destination.
#' @import dplyr
#' @examples
#' calc_dist(long1, lat1, long2, lat2)
#' @export
calc_dist <- function(x1, y1, x2, y2) {
  if (length(x2) == 1) {
    meany <- mean(c(y1, y2))
    diffx <- abs(x1 - x2) * cos(meany * pi / 180) * 60
    diffy <- abs(y1 - y2) * 60
    dist  <- sqrt(diffy^2 + diffx^2)
    return(dist)
  }

  if (length(x2) > 1) {
    meany <- apply(cbind(y1, y2), FUN = mean, MAR = 1)
    diffx <- abs(x1 - x2) * cos(meany * pi / 180) * 60
    diffy <- abs(y1 - y2) * 60
    dist  <- sqrt(diffy^2 + diffx^2)
    return(dist)
  }
}

#' Extract acoustic intervals for nearshore extrapolation
#'
#' @param nasc.df A data frame containing total backscatter from all CPS
#'   (\code{cps.nasc}), acoustic proportions (\code{prop.spp}), and
#'   weight-specific backscattering cross sections (\code{sigmawg.spp}) in a
#'   given stratum
#' @param feature.df A data frame containing the latitude and longitude of the
#'   feature used to reduce \code{nasc.df}
#' @param lat.radius A vector used to restrict the features being compared.
#' @return A data frame containing backscatter equidistinant from a feature.
#' @examples
#' estimate_inshore(nasc.df, feature.df, lat.radius = 0.1)
#' @export
estimate_inshore <- function(nasc.df, feature.df, lat.radius = 0.1) {
  # Create data frame for results
  return.df <- data.frame()

  # Select the inshore and offshore points
  nasc.df$inshore <- 0
  for (i in sort(unique(nasc.df$transect))) {
    nasc.df$inshore[nasc.df$transect == i & nasc.df$long == max(nasc.df$long[nasc.df$transect == i])] <- 1
  }

  nasc.df$offshore <- 0
  for (i in sort(unique(nasc.df$transect))) {
    nasc.df$offshore[ nasc.df$transect == i & nasc.df$long == min(nasc.df$long[nasc.df$transect == i])] <- 1
  }

  # Calculate coastal distance
  nasc.df$coastal.distance <- NA
  nearest.point.long       <- numeric(0)
  nearest.point.lat        <- numeric(0)

  for (i in sort(unique(nasc.df$transect))) {
    # print(paste("Transect ", i))
    nearest.point.long.vector <- feature.df$long[
      feature.df$long >  (nasc.df$long[nasc.df$transect == i & nasc.df$inshore == 1] - 0.25) &
        feature.df$long <  (nasc.df$long[nasc.df$transect == i & nasc.df$inshore == 1] + 0.5) &
        feature.df$lat  <  (nasc.df$lat[nasc.df$transect  == i & nasc.df$inshore == 1] + lat.radius) &
        feature.df$lat  >  (nasc.df$lat[nasc.df$transect  == i & nasc.df$inshore == 1] - lat.radius) &
        !is.na(feature.df$long)]

    nearest.point.lat.vector <- feature.df$lat[
      feature.df$long >  (nasc.df$long[nasc.df$transect == i & nasc.df$inshore == 1] - 0.25) &
        feature.df$long <  (nasc.df$long[nasc.df$transect == i & nasc.df$inshore == 1] + 0.5) &
        feature.df$lat  <  (nasc.df$lat[nasc.df$transect  == i & nasc.df$inshore == 1] + lat.radius) &
        feature.df$lat  >  (nasc.df$lat[nasc.df$transect  == i & nasc.df$inshore == 1] - lat.radius) &
        !is.na(feature.df$long)]

    dist.vector <- calc_dist(nasc.df$long[nasc.df$transect == i & nasc.df$inshore == 1],
                             nasc.df$lat[ nasc.df$transect == i & nasc.df$inshore == 1],
                             nearest.point.long.vector,
                             nearest.point.lat.vector)

    nasc.df$coastal.distance[nasc.df$transect == i & nasc.df$inshore == 1] <- min(dist.vector)

    nearest.point.long <- c(nearest.point.long, nearest.point.long.vector[dist.vector == min(dist.vector)])
    nearest.point.lat  <- c(nearest.point.lat,   nearest.point.lat.vector[dist.vector == min(dist.vector)])

    nasc.df$coastal.distance[nasc.df$transect == i & nasc.df$inshore == 0] <-
      calc_dist(nasc.df$long[nasc.df$transect == i & nasc.df$inshore == 1],
                nasc.df$lat[ nasc.df$transect == i & nasc.df$inshore == 1],
                nasc.df$long[ nasc.df$transect == i & nasc.df$inshore == 0],
                nasc.df$lat[ nasc.df$transect == i & nasc.df$inshore == 0]) +
      nasc.df$coastal.distance[nasc.df$transect == i & nasc.df$inshore == 1]

    return.df <- rbind(return.df,
                       nasc.df[nasc.df$transect == i, ][nasc.df$coastal.distance[nasc.df$transect == i] <= 2 *
                                                          nasc.df$coastal.distance[nasc.df$transect == i & nasc.df$inshore == 1], ])

  }
  # Acoustic data with equivalent distance to the coast, for each transect
  return(return.df)
}

#' Draw convex hull polygon around a collection of points
#'
#' @param df A data frame containing the latitude (\code{lat}) and longitude
#' (\code{long}) of point features.
#' @return A data frame with convex hull vertices.
#' @examples
#' find_hull(df)
#' @export
find_hull <- function(df) df[chull(df$long, df$lat), ]

#' Convert simple feature to data frame with a different coordinate reference
#' system (CRS).
#'
#' @param sf A simple feature object.
#' @param crs The new coordinate reference system (CRS).
#' @return A data frame with projected coordinates \code{X} and \code{Y}.
#' @examples
#' project_sf(sf_obj, crs = 4326)
#' @export
project_sf <- function(sf, crs) {
  # Transform simple feature
  sf <- sf %>%
    sf::st_transform(crs = crs)

  # Get coordinates in projected x/y
  sf.xy <- as.data.frame(st_coordinates(sf))

  # Combine and return data frame
  df <- sf %>%
    dplyr::bind_cols(sf.xy) %>%
    sf::st_set_geometry(NULL)

  return(df)
}

#' Project a data frame with geographic coordinates to a different coordinate
#' reference system (CRS).
#'
#' @param df A data frame with geographic coordinates (latitude and longitude,
#'   in decimal degrees) and a WGS84 CRS (4326).
#' @param crs.proj The new projected coordinate reference system.
#' @param lat The column containing latitude values (decimal degrees).
#' @param long The column containing longitude values (decimal degrees).
#' @return A data frame with a different CRS.
#' @examples
#' project_df(df, lat, long, crs.proj = 3310)
#' @export
project_df <- function(df, lat, long, crs.geog = 4326, crs.proj) {
  # Convert data frame to sf and transform
  df <- sf::st_as_sf(df, coords = c(long,lat), crs = crs.geog) %>%
    sf::st_transform(crs = crs.proj)

  # Get coordinates in projected x/y
  df.xy <- as.data.frame(sf::st_coordinates(df))

  # Combine and return data frame
  df <- df %>%
    dplyr::bind_cols(df.xy) %>%
    sf::st_set_geometry(NULL)

  return(df)
}

