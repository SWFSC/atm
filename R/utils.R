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
#' @import xml2
#' @import purrr
#' @import readr
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
