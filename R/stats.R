#' Calculate bootstrap estimates of species biomass
#'
#' @param nasc.df A data frame containing total backscatter from all CPS
#'   (\code{cps.nasc}), acoustic proportions (\code{prop.spp}), and
#'   weight-specific backscattering cross sections (\code{sigmawg.spp}) in a
#'   given stratum
#' @param clf.df A data frame containing cluster length frequencies for a given
#'   \code{species} (\code{cps.nasc}), acoustic proportions (\code{prop.spp}),
#'   and weight-specific backscattering cross sections (\code{sigmawg.spp}) in a
#'   given stratum
#' @param stratum.num A vector containing stratum number.
#' @param stratum.area A vector containing stratum area (in square meters).
#' @param species A vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @param do.lf Return a length frequency (\code{TRUE/FALSE})
#' @param boot.number A vector defining the number of bootstrap samples.
#' @return A data frame containing estimates of biomass and abundance or a list
#'   of biomass, abundance, and length frequency.
#' @examples
#' estimate_bootstrap(nasc.df, clf.df, stratum.num, stratum.area,
#' "Engraulis mordax", do.lf = TRUE, boot.num = 1000)
#' @export
estimate_bootstrap <- function(nasc.df, clf.df,
                               stratum.num, stratum.area,
                               species = NULL, do.lf = FALSE,
                               boot.number = 0){

  # Filter df to keep only data within the defined stratum
  nasc.df  <- filter(nasc.df, stratum.num == stratum)

  # Extract length columns and bins from clf file
  L.cols  <- grep("L\\d", names(clf.df))
  L.vec   <- sort(as.numeric(stringr::str_extract(names(clf.df[L.cols]), "\\d{1,2}")))

  # Define species-specific parameters

  if (species == "Clupea pallasii") {
    # Create a length-based weight vector
    # FL converted to TL from Palance
    # TL converted to W from sardine
    weight.vec <- 4.446313e-06*((-0.323 + L.vec*10*1.110)/10)^3.197

    nasc.df <- nasc.df %>%
      dplyr::select(transect, cluster, cps.nasc, prop.her, sigmawg.her, sigmaindiv.her, meanwg.her) %>%
      dplyr::rename(prop       = prop.her,
             sigmawg    = sigmawg.her,
             sigmaindiv = sigmaindiv.her,
             meanwg     = meanwg.her)

  } else if (species == "Etrumeus acuminatus") {
    # Create a length-based weight vector
    # FL converted to TL from Palance
    # TL converted to W from sardine
    weight.vec <- 4.446313e-06*((-0.323 + L.vec*10*1.110)/10)^3.197

    nasc.df <- nasc.df %>%
      dplyr::select(transect, cluster, cps.nasc, prop.rher, sigmawg.rher, sigmaindiv.rher, meanwg.rher) %>%
      dplyr::rename(prop       = prop.rher,
                    sigmawg    = sigmawg.rher,
                    sigmaindiv = sigmaindiv.rher,
                    meanwg     = meanwg.rher)

  } else if (species == "Engraulis mordax") {
    # Create a length-based weight vector
    weight.vec <-  exp(-12.964)*((2.056 + L.vec*10*1.1646)/10)^3.387

    # # Using GLM L/W from Palance, with SL converted to TL
    # weight.vec <- 2.8732e-06*((5.1 + L.vec*10*1.137)/10)^3.167

    # Extract proportions and sigmas for desired species
    nasc.df <- nasc.df %>%
      dplyr::select(transect, cluster, cps.nasc, prop.anch, sigmawg.anch, sigmaindiv.anch, meanwg.anch) %>%
      dplyr::rename(prop       = prop.anch,
             sigmawg    = sigmawg.anch,
             sigmaindiv = sigmaindiv.anch,
             meanwg     = meanwg.anch)

  } else if (species == "Sardinops sagax") {
    # Create a length-based weight vector
    weight.vec <- 4.446313e-06*((3.574 + L.vec*10*1.149)/10)^3.197

    # # Using GLM L/W from Palance, with SL converted to TL
    # weight.vec <- 4.551e-06*((0.724 + L.vec*10*1.157)/10)^3.12

    # Extract proportions and sigmas for desired species
    nasc.df <- nasc.df %>%
      dplyr::select(transect, cluster, cps.nasc, prop.sar, sigmawg.sar, sigmaindiv.sar, meanwg.sar) %>%
      dplyr::rename(prop       = prop.sar,
             sigmawg    = sigmawg.sar,
             sigmaindiv = sigmaindiv.sar,
             meanwg     = meanwg.sar)

  } else if (species == "Scomber japonicus") {
    # Create a length-based weight vector
    weight.vec <- 7.998343e-06*((7.295 + L.vec*10*1.078)/10)^3.01

    # # Using GLM L/W from Palance, with FL converted to TL
    # weight.vec <- 3.5503e-06*((-4.114 + L.vec*10*1.115)/10)^3.165

    # Extract proportions and sigmas for desired species
    nasc.df <- nasc.df %>%
      dplyr::select(transect, cluster, cps.nasc, prop.mack, sigmawg.mack, sigmaindiv.mack, meanwg.mack) %>%
      dplyr::rename(prop       = prop.mack,
             sigmawg    = sigmawg.mack,
             sigmaindiv = sigmaindiv.mack,
             meanwg     = meanwg.mack)

  } else if (species == "Trachurus symmetricus") {
    # Create a length-based weight vector
    weight.vec <- 7.998343e-06*((7.295 + L.vec*10*1.078)/10)^3.01

    # # Using GLM L/W from Palance, with FL converted to TL
    # weight.vec <- 5.9361e-06*((0.896 + L.vec*10*1.100)/10)^3.069

    # Extract proportions and sigmas for desired species
    nasc.df <- nasc.df %>%
      dplyr::select(transect, cluster, cps.nasc, prop.jack, sigmawg.jack, sigmaindiv.jack, meanwg.jack) %>%
      dplyr::rename(prop       = prop.jack,
             sigmawg    = sigmawg.jack,
             sigmaindiv = sigmaindiv.jack,
             meanwg     = meanwg.jack)
  }

  # Calculate bootstrap estimate for bootstrap num == 0
  if (boot.number == 0) {
    biomass   <- mean(nasc.df$cps.nasc*nasc.df$prop/(4*pi*nasc.df$sigmawg))    * stratum.area/1852/1852/1e+06
    abundance <- mean(nasc.df$cps.nasc*nasc.df$prop/(4*pi*nasc.df$sigmaindiv)) * stratum.area/1852/1852
    biomass3  <- mean(nasc.df$cps.nasc*nasc.df$prop/(4*pi*nasc.df$sigmaindiv)  * nasc.df$meanwg) * stratum.area/1852/1852/1e+06

    if (do.lf == T) {
      abundance.vector <- as.vector(
        unlist(apply(clf.df[!is.na(match(clf.df$cluster, unlist(lapply(split(nasc.df$cluster, nasc.df$cluster), FUN = unique)))), L.cols] *
                       matrix(rep(unlist(lapply(split((nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmaindiv)), nasc.df$cluster), FUN = sum)) /
                                    sum(unlist(lapply(split((nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmaindiv)), nasc.df$cluster), FUN = sum))),
                                  rep(length(L.vec), length(unlist(lapply(split(nasc.df$cluster, nasc.df$cluster), FUN = unique))))),
                              ncol = length(L.vec), byrow = T), FUN = sum, MARGIN = 2) * abundance))

      biomass2   <- sum(abundance.vector*weight.vec/1e+06)
      abundance2 <- sum(abundance.vector)
    }
  }
  # Calculate bootstrap estimate for bootstrap num > 0
  if (boot.number > 0) {
    biomass   <- mean(nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmawg))    * stratum.area/1852/1852/1e+06
    abundance <- mean(nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmaindiv)) * stratum.area/1852/1852
    biomass3  <- mean(nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmaindiv)  * nasc.df$meanwg) * stratum.area/1852/1852/1e+06

    if (do.lf == T) {
      abundance.matrix <- as.vector(
        unlist(apply(clf.df[!is.na(match(clf.df$cluster, unlist(lapply(split(nasc.df$cluster, nasc.df$cluster), FUN = unique)))), L.cols] *
                       matrix(rep(unlist(lapply(split((nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmaindiv)), nasc.df$cluster), FUN = sum)) /
                                    sum(unlist(lapply(split((nasc.df$cps.nasc * nasc.df$prop/(4*pi*nasc.df$sigmaindiv)), nasc.df$cluster), FUN = sum))),
                                  rep(length(L.vec),length(unlist(lapply(split(nasc.df$cluster, nasc.df$cluster), FUN = unique))))),
                              ncol = length(L.vec), byrow = T),FUN = sum, MARGIN = 2) * abundance))

      biomass2 <- sum(abundance.matrix * weight.vec/1e+06)

    }

    for (i in 1:boot.number) {
      # This function works by resampling the transects with probabilities proportional to the transect length
      resampled.df <- numeric(0)

      # Select transects
      resampled.transects <- sample(nasc.df$transect, dplyr::n_distinct(nasc.df$transect), replace = T)

      for (j in resampled.transects) {
        resampled.df <- rbind(resampled.df, nasc.df[nasc.df$transect == j, ])
      }

      # Add bootstrap estimates to point estimates
      biomass   <- c(biomass,   mean(resampled.df$cps.nasc*resampled.df$prop/(4*pi*resampled.df$sigmawg))    * stratum.area/1852/1852/1e+06)
      abundance <- c(abundance, mean(resampled.df$cps.nasc*resampled.df$prop/(4*pi*resampled.df$sigmaindiv)) * stratum.area/1852/1852)
      biomass3  <- c(biomass3,  mean(resampled.df$cps.nasc*resampled.df$prop/(4*pi*resampled.df$sigmaindiv)  * resampled.df$meanwg)*stratum.area/1852/1852/1e+06)

      if (do.lf == T) {
        abundance.matrix <- rbind(abundance.matrix, as.vector(
          unlist(apply(clf.df[!is.na(match(clf.df$cluster, unlist(lapply(split(resampled.df$cluster, resampled.df$cluster), FUN = unique)))),L.cols] *
                         matrix(rep(unlist(lapply(split((resampled.df$cps.nasc * resampled.df$prop/(4*pi*resampled.df$sigmaindiv)), resampled.df$cluster), FUN = sum)) /
                                      sum(unlist(lapply(split((resampled.df$cps.nasc * resampled.df$prop/(4*pi*resampled.df$sigmaindiv)), resampled.df$cluster), FUN = sum))),
                                    rep(length(L.vec),length(unlist(lapply(split(resampled.df$cluster, resampled.df$cluster), FUN = unique))))),
                                ncol = length(L.vec), byrow = T), FUN = sum, MARGIN = 2)*abundance[i + 1])))

        biomass2 <- c(biomass2,(sum(apply(clf.df[!is.na(match(clf.df$cluster, unlist(lapply(split(resampled.df$cluster, resampled.df$cluster), FUN = unique)))), L.cols] *
                                            matrix(rep(unlist(lapply(split((resampled.df$cps.nasc * resampled.df$prop/(4*pi*resampled.df$sigmaindiv)), resampled.df$cluster), FUN = sum)) /
                                                         sum(unlist(lapply(split((resampled.df$cps.nasc * resampled.df$prop/(4*pi*resampled.df$sigmaindiv)), resampled.df$cluster), FUN = sum))),
                                                       rep(length(L.vec),length(unlist(lapply(split(resampled.df$cluster, resampled.df$cluster), FUN = unique))))),
                                                   ncol = length(L.vec), byrow = T), FUN = sum, MARGIN = 2) * abundance[i + 1] * weight.vec/1e+06)))
      }
    }
  }
  if (boot.number == 0 & do.lf == T) {
    return.data.frame <- (data.frame(biomass = biomass, abundance = abundance, biomass2 = biomass2,
                                     biomass3 = biomass3, abundance2 = abundance2))
    return(list(data.frame = return.data.frame, abundance.vector = abundance.vector))
  }
  if (boot.number == 0 & do.lf == F) {
    return.data.frame <- (data.frame(biomass = biomass, abundance = abundance, biomass3 = biomass3))
    return(list(data.frame = return.data.frame))
  }
  if (boot.number > 0 & do.lf == T) {
    return.data.frame <- (data.frame(biomass = biomass, abundance = abundance, biomass2 = biomass2,  biomass3 = biomass3))
    return(list(data.frame = return.data.frame, abundance.matrix = abundance.matrix))
  }
  if (boot.number > 0 & do.lf == F) {
    return(data.frame(biomass = biomass, abundance = abundance))
  }
}

#' Calculate point estimate of species biomass
#'
#' @param df A data frame containing total backscatter from all CPS
#'   (\code{cps.nasc}), acoustic proportions (\code{prop.spp}), and
#'   weight-specific backscattering cross sections (\code{sigmawg.spp}) in a
#'   given stratum
#' @param stratum.area A vector containing stratum areas (in square meters).
#' @param species A vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @return A data frame with the stratum areas and point estimates of total
#'   biomass.
#' @examples
#' estimate_point(df, stratum.area, "Engraulis mordax")
#' @export
estimate_point <- function(df, stratum.area, species){
  # Initialize biomass varible
  biomass.total <- double()

  # Calculate biomass for each polygon
  for (j in sort(unique(stratum.area$stratum))) {
    # Subset data frame for points in polygon
    df.sub <- dplyr::filter(df, stratum == j)

    # Select spp. proportion and sigmawg based on species
    if (species == "Clupea pallasii") {
      prop    <- df.sub$prop.her
      sigmawg <- df.sub$sigmawg.her
    } else if (species == "Engraulis mordax") {
      prop    <- df.sub$prop.anch
      sigmawg <- df.sub$sigmawg.anch
    } else if (species == "Sardinops sagax") {
      prop    <- df.sub$prop.sar
      sigmawg <- df.sub$sigmawg.sar
    } else if (species == "Scomber japonicus") {
      prop    <- df.sub$prop.mack
      sigmawg <- df.sub$sigmawg.mack
    } else if (species == "Trachurus symmetricus") {
      prop    <- df.sub$prop.jack
      sigmawg <- df.sub$sigmawg.jack
    } else if (species == "Etrumeus acuminatus") {
      prop    <- df.sub$prop.rher
      sigmawg <- df.sub$sigmawg.rher
    }
    # Calculate total polygon biomass, in tons
    biomass.total <- c(biomass.total,
                       (mean((df.sub$cps.nasc*prop/(4*pi*sigmawg)))*10^3 *
                          stratum.area$area[stratum.area$stratum == j])/1852/1852/1e+06)
  }
  # Convert biomass to tons
  data.frame(stratum.area, biomass.total)
}

#' Calculate statistics around points
#'
#' @param small.file A data frame containing points around which statistics are computed; must contain 'lat' and 'long'.
#' @param large.file A data frame containing points upon which statistics are computed; must contain 'lat' and 'long'.
#' @param variable Column name upon which to compute statistics
#' @param method Either 'sp' to compute spatial statistics in Cartesian space, using the {sp} package, or 'sf' to compute
#' statistics in geographic space using the {sf} package.
#' @param crs Coordinate reference system for using the 'sf' method (default is 4326 for WGS84 projection)
#' @param radius Length of radius (in nautical miles) around points in `small.file` used to compute spatial polygons
#' @param length Length of vector used to create polygon nodes
#' @param plot.fig Plot results (T/F)
#' @param save.fig Save figure (T/F)
#' @param fig.name Name of figure.
#' @return A data frame with the number (n), mean, and standard deviation of the `variable` within each polygon.
#' @export

neighbor_quantifier <- function(small.file, large.file, variable = NULL,
                                method = "sp", crs = 4326, radius = 20, length = 100,
                                plot.fig = TRUE, save.fig = TRUE, fig.name = NULL){

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
          dplyr::bind_rows(large.file[
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
        ggplot2::geom_point(data = large.file, ggplot2::aes(long, lat, size = variable/max(lat)*10),
                            shape = 21, show.legend = FALSE) +
        ggplot2::geom_point(data = polygon.i.xy, ggplot2::aes(X, Y, group = polygon), shape = 21, fill = NA) +
        ggplot2::geom_point(data = large.file.intersects,
                            ggplot2::aes(long, lat, size = variable/max(lat)*10),
                            colour = "green", show.legend = FALSE) +
        ggplot2::geom_point(data = small.file, ggplot2::aes(long, lat), shape = 21, fill = "red", size = 3) +
        ggplot2::coord_map() +
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
      dplyr::mutate(
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
      n <- c(n, length(sf::st_intersection(large.file.sf, polygon.i)$variable))

      # Compute mean values within each polygon
      mean <- c(mean, mean(sf::st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))

      # Compute min values within each polygon
      min  <- c(min, min(sf::st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))

      # Compute max values within each polygon
      max  <- c(max, max(sf::st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))

      # Compute standard deviation values within each polygon
      sd   <- c(sd, sd(sf::st_intersection(large.file.sf, polygon.i)$variable, na.rm = TRUE))
    }

    # Get intervals that intersect polygons, for plotting
    large.file.intersects <- sf::st_intersection(large.file.sf, polygon.i.sf)

    if (plot.fig) {
      # Add polygon to plot
      var.plot <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = large.file.sf, ggplot2::aes(size = variable/max(lat)*10),
                         shape = 21, show.legend = FALSE) +
        ggplot2::geom_sf(data = large.file.intersects,
                         ggplot2::aes(size = variable/max(lat)*10),
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
