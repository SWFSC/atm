#' Extract vertically integrated backscatter from CSV files
#'
#' @param filename Name or path to CSV file exported from Echoview.
#'
#' @return A data frame containing vertically integrated backscatter data.
#' @export
extract_csv <- function(filename) {
  # Read CSV file
  tmp <- data.table::fread(filename, sep = ",")

  # Extract transect name from the file name
  transect <- tail(unlist(stringr::str_split(filename, "/")), n = 1) %>%
    stringr::str_split("_") %>%
    purrr::pluck(1, 2)

  # Are data from CPS?
  is.cps <- ifelse(stringr::str_detect(filename, "CPS"), TRUE, FALSE)

  # Create variable cps.nasc if not already present
  if (is.cps) {
    if (!"cps.nasc" %in% colnames(tmp)) {
      tmp$cps.nasc <- NA_real_
    }
  }

  # Summarize NASC by interval
  tmp %>%
    dplyr::group_by(Interval) %>%
    dplyr::summarise(
      long     = Lon_M[1],
      lat      = Lat_M[1],
      date     = Date_M[1],
      time     = as.character(Time_M[1]),
      dist_m   = Dist_M[1],
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
      cps.nasc = cps.nasc[1],
      depth    = max(Layer_depth_max) + 3,
      CPS      = ifelse(is.cps, 1, 0),
      filename = filename,
      transect = transect,
      type     = ifelse(is.cps, "CPS", "Krill"),
      datetime = lubridate::ymd_hms(paste(date, time)))
}

#' Format ggplot2 axis labels to fancy scientific.
#'
#' @param l A simple feature object.
#' @return A text string as an expression.
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point() + scale_y_continuous(labels = fancy_scientific)
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
