#' Clone estimATM repository
#'
#' @param repo_url URL for estimATM repository on GitHub.
#' @param path Path to desired project location.
#' @param overwrite Overwrite existing directory.
#'
clone_estimATM <- function(repo_url = "https://github.com/kstierhoff/estimATM", path, overwrite = FALSE) {
  if (exists(path)) {
    if (overwrite) {
      # Clone repository
      git2r::clone(repo_url, path)

    } else {
      # Halt
      print("Error, not overwriting existing directory")
    }
  } else {
    # Create new directory
    dir.create(path)

    # Clone repository
    git2r::clone(repo_url, path)
  }

  # Create required directories
  fs::dir_create(paste(path, c("Data", "Figs","Output")))
}
