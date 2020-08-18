#' Estimate target strength of CPS from total length
#'
#' @param species A value or vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @param TL A value or vector containing total length measurements.
#' @param units Length measurement units (e.g., "mm" or "cm"). If mm, lengths are converted to cm internally but returned in cm.
#' @return A data frame with species name, total length, target strength, backscattering coefficients, and
#'   weight estimates.
#' @examples
#' estimate_ts("Sardinops sagax", TL, units = "mm")
#' @export
estimate_ts <- function(species, TL, units = "mm") {
  # Create data frame from inputs
  df.in <- data.frame(species, TL) %>%
    dplyr::mutate(id = seq_along(species))

  # Convert to cm, if necessary
  if (units == "mm") {
    df.in$TL <- df.in$TL/10
  }

  # Create data frame for results
  df.out <- data.frame()

  # Estimate target strength for each species
  for (i in unique(df.in$species)) {
    # Filter by species i
    df <- dplyr::filter(df.in, species == i)

    # If sardine
    if (i == "Sardinops sagax")    {
      df$TS.wg        <- -14.9*log10(df$TL) - 13.21
      df$sigma.wg     <- 10^(df$TS.wg/10)
      # dB individual^-1^
      df$TS.ind       <- 17.07*log10(df$TL) - 66.73
      df$sigma.ind    <- 10^(df$TS.ind/10)
      # Barange df to weight
      df$estimated.wg <- 4.446313e-06*(df$TL)^3.197
      # AST L/W
      df$true.wg      <- exp(-10.997)*(df$TL)^2.757
    }
    # If anchovy (must be verified)
    if (i == "Engraulis mordax")    {
      df$TS.wg        <- -13.87*log10(df$TL) - 11.797
      df$sigma.wg     <- 10^(df$TS.wg/10)
      # dB individual^-1^ (b~20~)
      df$TS.ind       <- 20*log10(df$TL) - 68.09875
      df$sigma.ind    <- 10^(df$TS.ind/10)
      # AST L/W; updated summer 2016
      df$estimated.wg <- exp(-12.964)*(df$TL)^3.387
      df$true.wg      <- exp(-12.964)*(df$TL)^3.387
    }
    # If Pacific mackerel
    if (i == "Scomber japonicus") {
      df$TS.wg        <- -15.44*log10(df$TL) - 7.75
      df$sigma.wg     <- 10^(df$TS.wg/10)
      # dB individual^-1^
      df$TS.ind       <- 14.66*log10(df$TL) - 58.72
      df$sigma.ind    <- 10^(df$TS.ind/10)
      df$estimated.wg <- 7.998343e-06*(df$TL)^3.01
      df$true.wg      <- 7.998343e-06*(df$TL)^3.01
    }
    # If Jack mackerel
    if (i == "Trachurus symmetricus") {
      df$TS.wg        <- -15.44*log10(df$TL) - 7.75
      df$sigma.wg     <- 10^(df$TS.wg/10)
      # dB individual^-1^
      df$TS.ind       <- 14.66*log10(df$TL) - 58.72
      df$sigma.ind    <- 10^(df$TS.ind/10)
      df$estimated.wg <- 7.998343e-06*(df$TL)^3.01
      df$true.wg      <- 7.998343e-06*(df$TL)^3.01
    }
    # If Pacific herring
    if (i == "Clupea pallasii")    {
      # Depth-compensated target strength
      df$TS.wg        <- -11.97*log10(df$TL) - 11.58561
      df$sigma.wg     <- 10^(df$TS.wg/10)
      # Depth-compensated target strength
      df$TS.ind       <- 20*log10(df$TL) - 65.10561
      df$sigma.ind    <- 10^(df$TS.ind/10)
      # Barange L/W
      df$estimated.wg <- 4.446313e-06*(df$TL)^3.197
      # AST L/W
      df$true.wg      <- exp(-10.997)*(df$TL)^2.757
    }
    df.out <- dplyr::bind_rows(df.out, df)
  }

  # Convert to cm, if necessary
  if (units == "mm") {
    df.out$TL <- df.out$TL*10
  }

  dplyr::arrange(df.out, id)
}
