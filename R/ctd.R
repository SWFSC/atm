#' Extract header information from CTD and UCTD casts.
#'
#' @param header.filename Name of header file.
#' @param type Cast type (CTD or UCTD).
#' @return A data frame containing header information.
#' @export
extract_ctd_header <- function(header.filename, type) {
  if (type == "UCTD") {
    # Process UCTD header file -----------------------------------------------------
    # Read header text
    header.txt <- readLines(header.filename)

    # Extract cast date as dttm
    cast.date  <- lubridate::dmy_hms(
      stringr::str_extract(
        unlist(
          stringr::str_extract_all(header.txt,
                                   pattern = '\\*Cast[\\s\\S]*\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}')),
        '\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}'))

    # if (length(cast.date) == 0) {
    #   cast.date  <- lubridate::dmy_hms(
    #     stringr::str_extract(
    #       unlist(stringr::str_extract_all(header.txt,
    #                                     pattern = '\\*Cast[\\s\\S]*stop')),
    #       '\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}'))
    # }

    # Extract probe serial number
    sn <- as.numeric(stringr::str_extract(
      unlist(stringr::str_extract_all(header.txt,
                                      pattern = '\\*SerialNumber=\\d{8}'))[1],"\\d{8}"))

    # Extract cast name from file name
    cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
      stringr::str_replace(".asc", "")

  } else if (type == "CTD") {
    # Process CTD header file -----------------------------------------------------
    # Read header text
    header.txt <- readLines(header.filename)

    # Extract cast date as dttm
    cast.date  <- lubridate::mdy_hms(
      stringr::str_extract(
        unlist(stringr::str_extract_all(header.txt, pattern = '\\* NMEA UTC \\(Time\\).*')),
        "\\w{3}\\s\\d{2}\\s\\d{4}\\s*\\d{2}:\\d{2}:\\d{2}"))

    if (length(cast.date) == 0) {
      cast.date  <- lubridate::mdy_hms(
        stringr::str_extract(
          unlist(stringr::str_extract_all(
            header.txt, pattern = '\\*Cast[\\s\\S]*\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}')),
          "\\d{2}\\s\\w{3}\\s\\d{4}\\s\\d{2}:\\d{2}:\\d{2}"))
    }
    # Set serial number to NA
    sn <- NA

    # Extract cast name from file name
    cast <- tail(stringr::str_split(header.filename, "/")[[1]], n = 1) %>%
      stringr::str_replace(".hdr", "")
  }

  # Combine data frames for export
  data.frame(cast = cast, path = header.filename, cast.date = cast.date, sn = sn)
}

#' Extract cast data from CTD and UCTD casts.
#'
#' @param cast.filename Name of cast file.
#' @param type Cast type (CTD or UCTD).
#' @return A data frame containing cast data.
#' @export
extract_ctd_cast <- function(cast.filename, type) {
  # Extract cast name from file path
  cast <- tail(stringr::str_split(cast.filename, "/")[[1]], n = 1) %>%
    stringr::str_replace("_processed.asc", "")

  if (type == "UCTD") {
    # Process UCTD cast -------------------------------------------------------
    # Read cast data and rename columns
    read.table(cast.filename, header = TRUE) %>%
      dplyr::rename(scan = Scan, C = C0S.m, 'T' = Tnc90C, P = PrM,
                    Z = DepSM, S = Sal00, Sv = SvCM, avgsVCM = AvgsvCM,
                    Dens = Density00, Flag = Flag) %>%
      dplyr::mutate(
        scan = scan - 1,
        s    = scan/16,       # Calculate time (s) from scan (scan rate is 16 Hz)
        dt   = c(0, diff(s)), # Calculate time interval
        dZ   = c(1, diff(Z)), # Calculate change in depth (dZ, m)
        dZt  = as.numeric(forecast::ma(dZ/dt, order = 5)),
        dZt  = na_if(dZt, Inf),
        Z    = -Z,
        cast = cast,
        path = cast.filename)

  } else if (type == "CTD") {
    # Process CTD cast --------------------------------------------------------
    # Read cast data and rename columns
    read.table(cast.filename, header = TRUE) %>%
      dplyr::rename(P = PrDM, 'T' = T090C, C = C0S.m, Z = DepSM, S = Sal00,
                    Sv = SvCM, avgsvCM = AvgsvCM, Dens = Density00, Flag = Flag) %>%
      dplyr::mutate(
        Z    = -Z, # Make depth negative
        cast = cast,
        path = cast.filename)
  }
}
