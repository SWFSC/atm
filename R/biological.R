#' Estimate CPS weight
#' @description Calculate the estimated weight of CPS species from a known length measurement, using the mass-length relationship
#' models presented in Palance et al. 2019.
#' @param scientificName A vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @param totalLength_mm A vector containing total length (TL), in millimeters.
#' @param model.type The model used to estimate weight, either `OLS` (Ordinary Least Squares) or `GLM` (Generalized Linear Model).
#' @param season The season in which the specimen was collected, either `spring` or `summer`.
#' @return Specimen weight, in grams.
#' @export
#'
#' @examples
#' estimate_weight("Sardinops sagax", 300, model.type = "GLM", "summer")
estimate_weight <- function(scientificName, totalLength_mm, model.type = "GLM", season) {
  # Create a data frame from the input vectors
  df.in <- data.frame(scientificName, totalLength_mm, model.type, season) %>%
    dplyr::mutate(id = seq_along(scientificName))

  # Create data frame for results
  df.out <- data.frame()

  for (i in unique(df.in$scientificName)) {
    for (j in unique(df.in$model.type)) {
      # Subset dataframe by name and model type
      df <- dplyr::filter(df.in, scientificName == i, model.type == j)

      # Set seasonal correction
      i_s <- ifelse(df$season == "spring", 0, 1)

      # Compute weight
      if (i == "Clupea pallasii") {
        if (j == "GLM") {
          df$weightg <- exp(-13.140)*df$totalLength_mm^3.253
        } else if (j == "OLS") {
          df$weightg <- exp(-13.156 + 0.044)*df$totalLength_mm^3.256
        }
      } else if (i == "Engraulis mordax") {
        if (j == "GLM") {
          df$weightg <- exp(-12.847 + (i_s*0.087))*df$totalLength_mm^3.167
        } else if (j == "OLS") {
          df$weightg <- exp(-13.043 + 0.071 + (i_s*0.086))*df$totalLength_mm^3.206
        }
      } else if (i == "Sardinops sagax") {
        if (j == "GLM") {
          df$weightg <- exp(-12.475 + (i_s*0.174))*df$totalLength_mm^3.121
        } else if (j == "OLS") {
          df$weightg <- exp(-12.555 + 0.052 + (i_s*0.172))*df$totalLength_mm^3.135
        }
      } else if (i == "Scomber japonicus") {
        if (j == "GLM") {
          df$weightg <- exp(-12.631 + (i_s*0.083))*df$totalLength_mm^3.165
        } else if (j == "OLS") {
          df$weightg <- exp(-12.650 + 0.045 + (i_s*0.082))*df$totalLength_mm^3.168
        }
      } else if (i == "Trachurus symmetricus") {
        if (j == "GLM") {
          df$weightg <- exp(-12.108 + (i_s*0.074))*df$totalLength_mm^3.069
        } else if (j == "OLS") {
          df$weightg <- exp(-12.149 + 0.044 + (i_s*0.072))*df$totalLength_mm^3.076
        }
      } else if (i == "Etrumeus acuminatus") {
        # For round herring, model fit to 2024 data by J. Zwolinski
        if (j == "GLM") {
          df$weightg <- exp(-13.36408)*df$totalLength_mm^3.327691
        } else if (j == "OLS") {
          df$weightg <- exp(-13.36408)*df$totalLength_mm^3.327691
        }
      } else if (i == "Merluccius productus") {
        # From Alvarez-Trasvina et al 2022 Acta Ichthyologia et Piscatoria
        if (j == "GLM") {
          df$weightg <- 3e-06*df$totalLength_mm^3.11
        } else if (j == "OLS") {
          df$weightg <- 3e-06*df$totalLength_mm^3.11
        }
      } else {
        df$weightg <- NA_real_
      }
      # Combine results
      df.out <- dplyr::bind_rows(df.out, df)
    }
  }
  # Return weight estimate
  dplyr::arrange(df.out, id) %>% dplyr::pull(weightg)
}

#' Estimate CPS length
#'
#' @description Calculate the estimated length of CPS species from a known weight measurement, using the mass-length relationship
#' models presented in Palance et al. 2019.
#' @param scientificName A vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @param weightg A vector containing specimen weight, in grams.
#' @param model.type The model used to estimate weight, either `OLS` (Ordinary Least Squares) or `GLM` (Generalized Linear Model).
#' @param season The season in which the specimen was collected, either `spring` or `summer`.
#' @return Specimen total length (TL), in millimeters.
#' @export
#' @examples
#' estimate_length("Sardinops sagax", 5, model.type = "GLM", "summer")
estimate_length <- function(scientificName, weightg, model.type = "GLM", season) {
  # Create a data frame from the input vectors
  df.in <- data.frame(scientificName, weightg, model.type, season) %>%
    dplyr::mutate(id = seq_along(scientificName))

  # Create data frame for results
  df.out <- data.frame()

  for (i in unique(df.in$scientificName)) {
    for (j in unique(df.in$model.type)) {
      # Subset dataframe by name and model type
      df <- dplyr::filter(df.in, scientificName == i, model.type == j)

      # Set seasonal correction
      i_s <- ifelse(df$season == "spring", 0, 1)

      if (i == "Clupea pallasii") {
        if (j == "GLM") {
          df$totalLength_mm <- (df$weightg/exp(-13.140))^(1/3.253)
        } else if (j == "OLS") {
          df$totalLength_mm <- (df$weightg/exp(-13.156+0.044))^(1/3.256)
        }
      } else if (i == "Engraulis mordax") {
        if (j == "GLM") {
          df$totalLength_mm <- (df$weightg/exp(-12.847 + (i_s*0.087)))^(1/3.167)
        } else if (j == "OLS") {
          df$totalLength_mm <- (df$weightg/exp(-13.043 + 0.071 + (i_s*0.086)))^(1/3.206)
        }
      } else if (i == "Sardinops sagax") {
        if (j == "GLM") {
          df$totalLength_mm <- (df$weightg/exp(-12.475 + (i_s*0.174)))^(1/3.121)
        } else if (j == "OLS") {
          df$totalLength_mm <- (df$weightg/exp(-12.555 + 0.052 + (i_s*0.172)))^(1/3.135)
        }
      } else if (i == "Scomber japonicus") {
        if (j == "GLM") {
          df$totalLength_mm <- (df$weightg/exp(-12.631 + (i_s*0.083)))^(1/3.165)
        } else if (j == "OLS") {
          df$totalLength_mm <- (df$weightg/exp(-12.650 + 0.045 + (i_s*0.082)))^(1/3.168)
        }
      } else if (i == "Trachurus symmetricus") {
        if (j == "GLM") {
          df$totalLength_mm <- (df$weightg/exp(-12.108 + (i_s*0.074)))^(1/3.069)
        } else if (j == "OLS") {
          df$totalLength_mm <- (df$weightg/exp(-12.149 + 0.044 + (i_s*0.072)))^(1/3.076)
        }
      } else if (i == "Etrumeus acuminatus") {
        # For round herring, use P. herring
        if (j == "GLM") {
          df$totalLength_mm <- (df$weightg/exp(-13.140))^(1/3.253)
        } else if (j == "OLS") {
          df$totalLength_mm <- (df$weightg/exp(-13.156+0.044))^(1/3.256)
        }
      } else {
        df$totalLength_mm <- NA_real_
      }
      # Combine results
      df.out <- dplyr::bind_rows(df.out, df)
    }
  }
  # Return weight estimate
  dplyr::arrange(df.out, id) %>% dplyr::pull(totalLength_mm)
}

#' Convert CPS length
#'
#' @description Converts between various length measures (total length (TL), standard length (SL), and fork length (FL)) for CPS species,
#' using the major-axis regression models presented in Palance et al. 2019. Not all conversions are possible. In such cases, `NA` is returned.
#' @param scientificName A vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @param L.in The length measurement to be converted (unitless) .
#' @param from The measurement type from which length is converted.
#' @param to The measurement type to which length is converted.
#' @return The converted length (unitless).
#' @export
#' @examples
#' convert_length("Sardinops sagax", L.in = 20, from = "SL", to = "TL")
convert_length <- function(scientificName, L.in, from, to) {
  # Create a data frame from the input vectors
  df.in <- data.frame(scientificName, L.in, from, to) %>%
    dplyr::mutate(id = seq_along(scientificName))

  # Create data frame for results
  df.out <- data.frame()

  for (i in unique(df.in$scientificName)) {
    for (j in unique(df.in$from)) {
      for (k in unique(df.in$to)) {
        # Subset dataframe by name and model type
        df <- dplyr::filter(df.in, scientificName == i, from == j, to == k)

        if (i == "Clupea pallasii") {
          # Convert from TL
          if (j == "TL") {
            if (k == "SL") {
              df$L.out <- (df$L.in + 1.607)/1.200
            } else if (k == "FL") {
              df$L.out <- (df$L.in + 0.323)/1.110
            } else {
              df$L.out <- NA_real_
            }
            # Convert from SL
          } else if (j == "SL") {
            if (k == "TL") {
              df$L.out <- 1.200*df$L.in - 1.607
            } else {
              df$L.out <- NA_real_
            }
            # Convert from FL
          } else if (j == "FL") {
            if (k == "TL") {
              df$L.out <- 1.110*df$L.in - 0.323
            } else if (k == "SL") {
              df$L.out <- NA_real_
            }
          }
        } else if (i == "Engraulis mordax") {
          # Convert from TL
          if (j == "TL") {
            if (k == "SL") {
              df$L.out <- (df$L.in - 5.100)/1.137
            } else if (k == "FL") {
              df$L.out <- (df$L.in - 1.870)/1.081
            } else {
              df$L.out <- NA_real_
            }
            # Convert from SL
          } else if (j == "SL") {
            if (k == "TL") {
              df$L.out <- 1.137*df$L.in + 5.100
            } else if (k == "FL") {
              df$L.out <- (df$L.in + 5.736)/0.965
            } else {
              df$L.out <- NA_real_
            }
            # Convert from FL
          } else if (j == "FL") {
            if (k == "TL") {
              df$L.out <- 1.081*df$L.in + 1.870
            } else if (k == "SL") {
              df$L.out <- 0.965*df$L.in - 5.736
            } else {
              df$L.out <- NA_real_
            }
          }
        } else if (i == "Sardinops sagax") {
          # Convert from TL
          if (j == "TL") {
            if (k == "SL") {
              df$L.out <- (df$L.in - 0.724)/1.157
            } else if (k == "FL") {
              df$L.out <- (df$L.in + 5.036)/1.134
            } else {
              df$L.out <- NA_real_
            }
            # Convert from SL
          } else if (j == "SL") {
            if (k == "TL") {
              df$L.out <- 1.157*df$L.in + 0.724
            } else if (k == "FL") {
              df$L.out <- (df$L.in + 5.243)/0.980
            } else {
              df$L.out <- NA_real_
            }
            # Convert from FL
          } else if (j == "FL") {
            if (k == "TL") {
              df$L.out <- 1.134*df$L.in - 5.036
            } else if (k == "SL") {
              df$L.out <- 0.980*df$L.in - 5.243
            } else {
              df$L.out <- NA_real_
            }
          }
        } else if (i == "Scomber japonicus") {
          # Convert from TL
          if (j == "TL") {
            if (k == "FL") {
              df$L.out <- (df$L.in + 4.114)/1.115
            } else {
              df$L.out <- NA_real_
            }
            # Convert from SL
          } else if (j == "SL") {
            df$L.out <- NA_real_
            # Convert from FL
          } else if (j == "FL") {
            if (k == "TL") {
              df$L.out <- 1.115*df$L.in - 4.114
            } else {
              df$L.out <- NA_real_
            }
          }
        } else if (i == "Trachurus symmetricus") {
          # Convert from TL
          if (j == "TL") {
            if (k == "FL") {
              df$L.out <- (df$L.in - 0.896)/1.110
            } else {
              df$L.out <- NA_real_
            }
            # Convert from SL
          } else if (j == "SL") {
            df$L.out <- NA_real_
            # Convert from FL
          } else if (j == "FL") {
            if (k == "TL") {
              df$L.out <- 1.110*df$L.in + 0.896
            } else {
              df$L.out <- NA_real_
            }
          }
        } else if (i == "Etrumeus acuminatus") {
          # For round herring, use P. herring
          # Convert from TL
          if (j == "TL") {
            if (k == "SL") {
              df$L.out <- (df$L.in + 1.607)/1.200
            } else if (k == "FL") {
              df$L.out <- (df$L.in + 0.323)/1.110
            } else {
              df$L.out <- NA_real_
            }
            # Convert from SL
          } else if (j == "SL") {
            if (k == "TL") {
              df$L.out <- 1.200*df$L.in - 1.607
            } else {
              df$L.out <- NA_real_
            }
            # Convert from FL
          } else if (j == "FL") {
            if (k == "TL") {
              df$L.out <- 1.110*df$L.in - 0.323
            } else if (k == "SL") {
              df$L.out <- NA_real_
            }
          }
        } else {
          df$L.out <- NA_real_
        }
        # Combine results
        df.out <- dplyr::bind_rows(df.out, df)
      }
    }
  }
  # Return weight estimate
  dplyr::arrange(df.out, id) %>% dplyr::pull(L.out)
}
