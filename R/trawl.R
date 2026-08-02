#' Extract measurements from Simrad TV80 measurement file
#'
#' @param file The file path, optimally derived using fs::dir_ls().
#' @return A data frame with data from the PX sensors, Trawl Eye sensor, and trawl vessel.
#' @examples
#' extract_tv80("C:/data/TV80/2023/12/11/20231211_142842_measurements.csv")
#' @export

extract_tv80 <- function(file) {
# extract_tv80 <- function(file, col.names = TRUE, skip.rows = 0) {
  # Convert file to path to extract file name
  file.name <- fs::path_file(fs::as_fs_path(file))

  # # 1. Peek at the first line to evaluate if a header exists
  # first_line <- try(read_lines(file, n_max = 1), silent = TRUE)
  #
  # # Check if the file was empty or couldn't be opened
  # if (inherits(first_line, "try-error") || length(first_line) == 0) {
  #   message("Skipping ", file, ": File is empty or unreadable.")
  #   # next
  # }
  #
  # # 2. Heuristic: Check if the first row contains numeric data
  # # (Assuming headers are text-only. Adjust the delimiter in strsplit if using TSV)
  # first_row_values <- strsplit(first_line, ",")[[1]]
  # has_numbers <- any(grepl("[0-9]", first_row_values))
  #
  # if (has_numbers) {
  #   message("Skipping ", file, ": Detected data in the first row (no header).")
  #   # next
  # }

  # Read and format CSV file data
  # 3. If it looks like a header, try reading the file
  df <- tryCatch({
  readr::read_delim(file, delim = ";", name_repair = "minimal", lazy = FALSE) %>%
    # Remove duplicated columns
    dplyr::select(which(!duplicated(names(.)))) %>%
    # Format vessel latitude and longitude to decimal degree format
    dplyr::mutate(VES_Latitude = as.numeric(stringr::str_sub(VES_Latitude,1,2)) +
                    as.numeric(stringr::str_sub(VES_Latitude,4,11))/60,
                  VES_Longitude = as.numeric(stringr::str_sub(VES_Longitude,1,3)) +
                    as.numeric(stringr::str_sub(VES_Longitude,5,12))/60) %>%
    # Convert all character columns to numeric
    dplyr::mutate_if(is.character, as.numeric) %>%
    # Create date/time variable and add file name
    dplyr::mutate(datetime = lubridate::ymd_hms(DateTime),
                  file = file.name)
  }, error = function(e) {
    # If read_csv throws a hard error, catch it and skip
    message("Skipping ", file, ": Failed to parse - ", e$message)
    return(NULL)
  })

  # 4. If successful, append it to the data frame
  if (!is.null(df)) {
    message("Successfully read ", file)
  }

  # Return data frame
  return(df)
}
