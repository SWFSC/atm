#' Extract vertically integrated backscatter from CSV files
#'
#' @param filename Name or path to CSV file exported from Echoview.
#' @param Sv.max Maximum Sv value for cell filtering.
#'
#' @return A data frame containing vertically integrated backscatter data.
#' @export
extract_csv <- function(filename, Sv.max = NULL) {
  # Read CSV file
  tmp <- data.table::fread(filename, sep = ",")

  # Extract transect name from the file name
  # New method using regex, to better handle variations in file names
  transect <- stringr::str_extract(filename, pattern = "_\\d{3}[\\w\\d\\W]([\\d]{1})?") %>%
    # Replace leading underscore
    stringr::str_replace("_", "") %>%
    # Replace trailing special characters
    stringr::str_replace("[^a-zA-Z0-9]$", "")

  # Are data from CPS?
  is.cps <- ifelse(stringr::str_detect(filename, "CPS"), TRUE, FALSE)

  if (!is.null(Sv.max)) {
    # Remove cells where Sv_max > Sv.max
    tmp <- dplyr::filter(tmp, Sv_max < Sv.max)
  }

  # Create Sv_max variable if missing
  if (!"Sv_max" %in% colnames(tmp)) {
    tmp$Sv_max <- NA_real_
  }

  # Summarize NASC by interval
  if ("cps.NASC" %in% colnames(tmp)) {
    tmp %>%
      dplyr::group_by(Interval) %>%
      dplyr::summarise(
        long     = Lon_M[1],
        lat      = Lat_M[1],
        date     = Date_M[1],
        time     = as.character(Time_M[1]),
        dist_m   = Dist_M[1],
        Sv_max   = max(Sv_max),
        NASC.5   = sum(NASC[Layer_depth_max <=   5]),
        NASC.10  = sum(NASC[Layer_depth_max <=  10]),
        NASC.15  = sum(NASC[Layer_depth_max <=  15]),
        NASC.20  = sum(NASC[Layer_depth_max <=  20]),
        NASC.25  = sum(NASC[Layer_depth_max <=  25]),
        NASC.30  = sum(NASC[Layer_depth_max <=  30]),
        NASC.35  = sum(NASC[Layer_depth_max <=  35]),
        NASC.40  = sum(NASC[Layer_depth_max <=  40]),
        NASC.45  = sum(NASC[Layer_depth_max <=  45]),
        NASC.50  = sum(NASC[Layer_depth_max <=  50]),
        NASC.55  = sum(NASC[Layer_depth_max <=  55]),
        NASC.60  = sum(NASC[Layer_depth_max <=  60]),
        NASC.65  = sum(NASC[Layer_depth_max <=  65]),
        NASC.70  = sum(NASC[Layer_depth_max <=  70]),
        NASC.75  = sum(NASC[Layer_depth_max <=  75]),
        NASC.80  = sum(NASC[Layer_depth_max <=  80]),
        NASC.85  = sum(NASC[Layer_depth_max <=  85]),
        NASC.90  = sum(NASC[Layer_depth_max <=  90]),
        NASC.95  = sum(NASC[Layer_depth_max <=  95]),
        NASC.100 = sum(NASC[Layer_depth_max <= 100]),
        NASC.150 = sum(NASC[Layer_depth_max <= 150]),
        NASC.250 = sum(NASC[Layer_depth_max <= 250]),
        NASC.350 = sum(NASC[Layer_depth_max <= 350]),
        NASC     = NASC.250,
        cps.nasc = sum(cps.NASC),
        depth    = max(Layer_depth_max) + 3,
        CPS      = ifelse(is.cps, 1, 0),
        filename = filename,
        transect = transect,
        type     = ifelse(is.cps, "CPS", "Krill"),
        datetime = lubridate::ymd_hms(paste(date, time)))
  } else if ("cps.nasc" %in% colnames(tmp)) {
    tmp %>%
      dplyr::group_by(Interval) %>%
      dplyr::summarise(
        long     = Lon_M[1],
        lat      = Lat_M[1],
        date     = Date_M[1],
        time     = as.character(Time_M[1]),
        dist_m   = Dist_M[1],
        Sv_max   = max(Sv_max),
        NASC.5   = sum(NASC[Layer_depth_max <=   5]),
        NASC.10  = sum(NASC[Layer_depth_max <=  10]),
        NASC.15  = sum(NASC[Layer_depth_max <=  15]),
        NASC.20  = sum(NASC[Layer_depth_max <=  20]),
        NASC.25  = sum(NASC[Layer_depth_max <=  25]),
        NASC.30  = sum(NASC[Layer_depth_max <=  30]),
        NASC.35  = sum(NASC[Layer_depth_max <=  35]),
        NASC.40  = sum(NASC[Layer_depth_max <=  40]),
        NASC.45  = sum(NASC[Layer_depth_max <=  45]),
        NASC.50  = sum(NASC[Layer_depth_max <=  50]),
        NASC.55  = sum(NASC[Layer_depth_max <=  55]),
        NASC.60  = sum(NASC[Layer_depth_max <=  60]),
        NASC.65  = sum(NASC[Layer_depth_max <=  65]),
        NASC.70  = sum(NASC[Layer_depth_max <=  70]),
        NASC.75  = sum(NASC[Layer_depth_max <=  75]),
        NASC.80  = sum(NASC[Layer_depth_max <=  80]),
        NASC.85  = sum(NASC[Layer_depth_max <=  85]),
        NASC.90  = sum(NASC[Layer_depth_max <=  90]),
        NASC.95  = sum(NASC[Layer_depth_max <=  95]),
        NASC.100 = sum(NASC[Layer_depth_max <= 100]),
        NASC.150 = sum(NASC[Layer_depth_max <= 150]),
        NASC.250 = sum(NASC[Layer_depth_max <= 250]),
        NASC.350 = sum(NASC[Layer_depth_max <= 350]),
        NASC     = NASC.250,
        cps.nasc = sum(cps.nasc),
        depth    = max(Layer_depth_max) + 3,
        CPS      = ifelse(is.cps, 1, 0),
        filename = filename,
        transect = transect,
        type     = ifelse(is.cps, "CPS", "Krill"),
        datetime = lubridate::ymd_hms(paste(date, time)))
  } else {
    tmp %>%
      dplyr::group_by(Interval) %>%
      dplyr::summarise(
        long     = Lon_M[1],
        lat      = Lat_M[1],
        date     = Date_M[1],
        time     = as.character(Time_M[1]),
        dist_m   = Dist_M[1],
        Sv_max   = max(Sv_max),
        NASC.5   = sum(NASC[Layer_depth_max <=   5]),
        NASC.10  = sum(NASC[Layer_depth_max <=  10]),
        NASC.15  = sum(NASC[Layer_depth_max <=  15]),
        NASC.20  = sum(NASC[Layer_depth_max <=  20]),
        NASC.25  = sum(NASC[Layer_depth_max <=  25]),
        NASC.30  = sum(NASC[Layer_depth_max <=  30]),
        NASC.35  = sum(NASC[Layer_depth_max <=  35]),
        NASC.40  = sum(NASC[Layer_depth_max <=  40]),
        NASC.45  = sum(NASC[Layer_depth_max <=  45]),
        NASC.50  = sum(NASC[Layer_depth_max <=  50]),
        NASC.55  = sum(NASC[Layer_depth_max <=  55]),
        NASC.60  = sum(NASC[Layer_depth_max <=  60]),
        NASC.65  = sum(NASC[Layer_depth_max <=  65]),
        NASC.70  = sum(NASC[Layer_depth_max <=  70]),
        NASC.75  = sum(NASC[Layer_depth_max <=  75]),
        NASC.80  = sum(NASC[Layer_depth_max <=  80]),
        NASC.85  = sum(NASC[Layer_depth_max <=  85]),
        NASC.90  = sum(NASC[Layer_depth_max <=  90]),
        NASC.95  = sum(NASC[Layer_depth_max <=  95]),
        NASC.100 = sum(NASC[Layer_depth_max <= 100]),
        NASC.150 = sum(NASC[Layer_depth_max <= 150]),
        NASC.250 = sum(NASC[Layer_depth_max <= 250]),
        NASC.350 = sum(NASC[Layer_depth_max <= 350]),
        NASC     = NASC.250,
        depth    = max(Layer_depth_max) + 3,
        CPS      = ifelse(is.cps, 1, 0),
        filename = filename,
        transect = transect,
        type     = ifelse(is.cps, "CPS", "Krill"),
        datetime = lubridate::ymd_hms(paste(date, time)))
  }
}

#' Format ggplot2 axis labels to fancy scientific.
#'
#' @param l A simple feature object.
#' @return A text string as an expression.
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point() + scale_y_continuous(labels = fancy_sci)
#' @export
fancy_sci <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # change formatting of 0
  l <- gsub("0e\\+00","0",l)
  # return this as an expression
  parse(text = l)
}

#' Identify NaN values in a data frame.
#'
#' @param df A data frame containing NaN values.
#' @return The indices of NaN elements in a data frame.
#' @examples
#' is.nan.df(df)
#' @export
is.nan.df <- function(df)
  do.call(cbind, lapply(df, is.nan))

#' Extract nodes from XML file.
#'
#' @param doc An XML object.
#' @param target A regular expression.
#' @return A text vector.
#' @seealso See \url{https://rpubs.com/hrbrmstr/xml2power} for original
#'   functions; used by \code{\link{xtrct_df}}
#' @import xml2
#' @export
xtrct <- function(doc, target) {
  xml2::xml_find_all(doc, target) %>%
    xml2::xml_text() %>%
    trimws() }

#' Extract child nodes and values from node list.
#'
#' @param doc An XML object.
#' @param top A named node.
#' @return A data frame of named child vectors.
#' @seealso See \url{https://rpubs.com/hrbrmstr/xml2power} for original
#'   functions, and related helper function \code{\link{xtrct}}
#' @export
xtrct_df <- function(doc, top) {
  xml2::xml_find_first(doc, sprintf(".//%s", top)) %>%
    xml2::xml_children() %>%
    xml2::xml_name() %>%
    purrr::map(~{
      xtrct(doc, sprintf(".//%s/%s", top, .x)) %>%
        list() %>%
        purrr::set_names(.x)
    }) %>%
    purrr::flatten_df() %>%
    readr::type_convert()
}

#' Create gps.csv files used by Echoview.
#'
#' @param lat Latitude in decimal degrees.
#' @param long Longitude in decimal degrees.
#' @param date_time Date/time field in ISO 8601 date format (e.g., %Y-%m-%d %H:%M:%S).
#' @return A data frame containing  \code{Date}, \code{Time}, \code{latitude}, and \code{longitude}.
#' @export
extract_gps <- function(df, lat = df$lat, long = df$long, date_time = df$datetime) {
  df <- df %>%
    mutate(GPS_date = format(datetime, format = "%F"),
           GPS_time = format(datetime, format = "%T")) %>%
    select(GPS_date, GPS_time, latitude = lat, longitude = long)
  return(df)
}

#' Convert latitude or longitude from SCS format to decimal degrees
#'
#' @param x Latitude or longitude in SCS format.
#' @return Latitude or longitude in decimal degrees.
#' @export
scs2dd <- function (x) {
  if (length(grep("N", x)) > 0) {
    # Remove all non-numeric or decimal characters
    x <- gsub("[^0-9.]", "", x)
    # Parse the remaining characters to extract the latitude
    y <- as.numeric(substr(x, 1, 2)) + signif(as.numeric(substr(x, 3, 9))/60, digits = 6)
  }
  else {
    # Remove all non-numeric or decimal characters
    x <- gsub("[^0-9.]", "", x)
    # Parse the remaining characters to extract the longitude
    y <- -(as.numeric(substr(x, 1, 3)) + signif(as.numeric(substr(x, 4, 10))/60, digits = 6))
  }
  return(y)
}

#' Convert date and time from SCS format to POSIXct
#'
#' @param date Date in SCS format.
#' @param time Time in SCS format.
#' @return A date/time object in POSIXct format.
#' @export
scs2posix <- function(date, time) {
  x <- as.POSIXct(paste(date, time), tz = "GMT", format = "%m/%d/%Y %H:%M:%S")
  return(x)
}
