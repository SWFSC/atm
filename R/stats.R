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
    }
    # Calculate total polygon biomass, in tons
    biomass.total <- c(biomass.total,
                       (mean((df.sub$cps.nasc*prop/(4*pi*sigmawg)))*10^3 *
                          stratum.area$area[stratum.area$stratum == j])/1852/1852/1e+06)
  }
  # Convert biomass to tons
  data.frame(stratum.area, biomass.total)
}
