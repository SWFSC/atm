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
#' estimate_weight("Sardinops sagax", 300, model.type = "OLS", "summer")
estimate_weight <- function(scientificName, totalLength_mm, model.type = "OLS", season) {
  # Set seasonal correction
  i_s <- ifelse (season == "spring", 0, 1)

  if (scientificName == "Clupea pallasii") {
    if (model.type == "GLM") {
      weightg = exp(-13.140)*totalLength_mm^3.253
    } else if (model.type == "OLS") {
      weightg = exp(-13.156+0.044)*totalLength_mm^3.256
    }
  } else if (scientificName == "Engraulis mordax") {
    if (model.type == "GLM") {
      weightg = exp(-12.847 + (i_s*0.087))*totalLength_mm^3.167
    } else if (model.type == "OLS") {
      weightg = exp(-13.043 + 0.071 + (i_s*0.086))*totalLength_mm^3.206
    }
  } else if (scientificName == "Sardinops sagax") {
    if (model.type == "GLM") {
      weightg = exp(-12.475 + (i_s*0.174))*totalLength_mm^3.121
    } else if (model.type == "OLS") {
      weightg = exp(-12.555 + 0.052 + (i_s*0.172))*totalLength_mm^3.135
    }
  } else if (scientificName == "Scomber japonicus") {
    if (model.type == "GLM") {
      weightg = exp(-12.631 + (i_s*0.083))*totalLength_mm^3.165
    } else if (model.type == "OLS") {
      weightg = exp(-12.650 + 0.045 + (i_s*0.082))*totalLength_mm^3.168
    }
  } else if (scientificName == "Trachurus symmetricus") {
    if (model.type == "GLM") {
      weightg = exp(-12.108 + (i_s*0.074))*totalLength_mm^3.069
    } else if (model.type == "OLS") {
      weightg = exp(-12.149 + 0.044 + (i_s*0.072))*totalLength_mm^3.076
    }
  }

  # Return weight estimate
  return(weightg)
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
#' estimate_length("Sardinops sagax", 5, model.type = "OLS", "summer")
estimate_length <- function(scientificName, weightg, model.type = "OLS", season) {
  # Set seasonal correction
  i_s <- ifelse (season == "spring", 0, 1)

  if (scientificName == "Clupea pallasii") {
    if (model.type == "GLM") {
      totalLength_mm = (weightg/exp(-13.140))^(1/3.253)
    } else if (model.type == "OLS") {
      totalLength_mm = (weightg/exp(-13.156+0.044))^(1/3.256)
    }
  } else if (scientificName == "Engraulis mordax") {
    if (model.type == "GLM") {
      totalLength_mm = (weightg/exp(-12.847 + (i_s*0.087)))^(1/3.167)
    } else if (model.type == "OLS") {
      totalLength_mm = (weightg/exp(-13.043 + 0.071 + (i_s*0.086)))^(1/3.206)
    }
  } else if (scientificName == "Sardinops sagax") {
    if (model.type == "GLM") {
      totalLength_mm = (weightg/exp(-12.475 + (i_s*0.174)))^(1/3.121)
    } else if (model.type == "OLS") {
      totalLength_mm = (weightg/exp(-12.555 + 0.052 + (i_s*0.172)))^(1/3.135)
    }
  } else if (scientificName == "Scomber japonicus") {
    if (model.type == "GLM") {
      totalLength_mm = (weightg/exp(-12.631 + (i_s*0.083)))^(1/3.165)
    } else if (model.type == "OLS") {
      totalLength_mm = (weightg/exp(-12.650 + 0.045 + (i_s*0.082)))^(1/3.168)
    }
  } else if (scientificName == "Trachurus symmetricus") {
    if (model.type == "GLM") {
      totalLength_mm = (weightg/exp(-12.108 + (i_s*0.074)))^(1/3.069)
    } else if (model.type == "OLS") {
      totalLength_mm = (weightg/exp(-12.149 + 0.044 + (i_s*0.072)))^(1/3.076)
    }
  }

  # Return weight estimate
  return(totalLength_mm)
}

#' Convert CPS length
#'
#' @description Converts between various length measures (total length (TL), standard length (SL), and fork length (FL)) for CPS species,
#' using the major-axis regression models presented in Palance et al. 2019.
#' @param scientificName A vector containing the species' scientific name (Clupea
#'   pallasii, Engraulis mordax, Sardinops sagax, Scomber japonicus, or
#'   Trachurus symmetricus).
#' @param L.in The length measurement (unitless) to be converted.
#' @param from The measurement type to be converted from.
#' @param to The measurement type to be converted to.
#' @return The converted length.
#' @export
#' @examples
#' convert_length("Sardinops sagax", 20, model.type = "OLS", "summer")
convert_length <- function(scientificName, L.in, from, to) {
  if (scientificName == "Clupea pallasii") {
    # Convert from TL
    if (from == "TL") {
      if (to == "SL") {
        L.out = (L.in + 1.607)/1.200
      } else if (to == "FL") {
        L.out = (L.in + 0.323)/1.110
      } else {
        L.out = NA
      }
      # Convert from SL
    } else if (from == "SL") {
      if (to == "TL") {
        L.out = 1.200*L.in - 1.607
      } else {
        L.out = NA
      }
      # Convert from FL
    } else if (from == "FL") {
      if (to == "TL") {
        L.out = 1.110*L.in - 0.323
      } else if (to == "SL") {
        L.out = NA
      }
    }
  } else if (scientificName == "Engraulis mordax") {
    # Convert from TL
    if (from == "TL") {
      if (to == "SL") {
        L.out = (L.in - 5.100)/1.137
      } else if (to == "FL") {
        L.out = (L.in - 1.870)/1.081
      } else {
        L.out = NA
      }
      # Convert from SL
    } else if (from == "SL") {
      if (to == "TL") {
        L.out = 1.137*L.in + 5.100
      } else if (to == "FL") {
        L.out = (L.in + 5.736)/0.965
      } else {
        L.out = NA
      }
      # Convert from FL
    } else if (from == "FL") {
      if (to == "TL") {
        L.out = 1.081*L.in + 1.870
      } else if (to == "SL") {
        L.out = 0.965*L.in - 5.736
      } else {
        L.out = NA
      }
    }
  } else if (scientificName == "Sardinops sagax") {
    # Convert from TL
    if (from == "TL") {
      if (to == "SL") {
        L.out = (L.in - 0.724)/1.157
      } else if (to == "FL") {
        L.out = (L.in + 5.036)/1.134
      } else {
        L.out = NA
      }
      # Convert from SL
    } else if (from == "SL") {
      if (to == "TL") {
        L.out = 1.157*L.in + 0.724
      } else if (to == "FL") {
        L.out = (L.in + 5.243)/0.980
      } else {
        L.out = NA
      }
      # Convert from FL
    } else if (from == "FL") {
      if (to == "TL") {
        L.out = 1.134*L.in - 5.036
      } else if (to == "SL") {
        L.out = 0.980*L.in - 5.243
      } else {
        L.out = NA
      }
    }
  } else if (scientificName == "Scomber japonicus") {
    # Convert from TL
    if (from == "TL") {
      if (to == "FL") {
        L.out = (L.in + 4.114)/1.115
      } else {
        L.out = NA
      }
      # Convert from SL
    } else if (from == "SL") {
      L.out = NA
      # Convert from FL
    } else if (from == "FL") {
      if (to == "TL") {
        L.out = 1.115*L.in - 4.114
      } else {
        L.out = NA
      }
    }
  } else if (scientificName == "Trachurus symmetricus") {
    # Convert from TL
    if (from == "TL") {
      if (to == "FL") {
        L.out = (L.in - 0.896)/1.110
      } else {
        L.out = NA
      }
      # Convert from SL
    } else if (from == "SL") {
      L.out = NA
      # Convert from FL
    } else if (from == "FL") {
      if (to == "TL") {
        L.out = 1.110*L.in + 0.896
      } else {
        L.out = NA
      }
    }
  }
  # Return converted length
  return(L.out)
}


