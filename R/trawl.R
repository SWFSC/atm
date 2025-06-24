#' Extract measurements from Simrad TV80 measurement file
#'
#' @param file The file path, optimally derived using fs::dir_ls().
#' @param col.names `TRUE`, if column names are present. A charactor vector containing column names if not.
#' @param skip.rows Number of rows to skip. 0 if col.name = `TRUE`, 1 if col.names is a character vector.
#' @return A data frame with data from the PX sensors, Trawl Eye sensor, and trawl vessel.
#' @examples
#' extract_tv80("C:/data/TV80/2023/12/11/20231211_142842_measurements.csv")
#' @export

extract_tv80 <- function(file, col.names = TRUE, skip.rows = 0) {
  # Convert file to path to extract file name
  file.name <- fs::path_file(fs::as_fs_path(file))

  # Read and format CSV file data
  df <- readr::read_delim(file, delim = ";", name_repair = "minimal", lazy = FALSE,
                          col_names = col.names, skip = skip.rows) %>%
    # Remove duplicated columns
    dplyr::select(which(!duplicated(names(.)))) %>%
    # Format vessel latitude and longitude to decimal degree format
    dplyr::mutate(VES_Latitude = as.numeric(stringr::str_sub(VES_Latitude,1,2)) +
                    as.numeric(stringr::str_sub(VES_Latitude,4,11))/60,
                  VES_Longitude = as.numeric(stringr::str_sub(VES_Longitude,1,3)) +
                    as.numeric(stringr::str_sub(VES_Longitude,5,12))/60) %>%
    # Convert all character columns to numeric
    dplyr::mutate_if(is.character, as.numeric) %>%
    # Create date/time variable and add filename
    dplyr::mutate(datetime = lubridate::ymd_hms(DateTime),
                  file = file.name)

  return(df)
}
