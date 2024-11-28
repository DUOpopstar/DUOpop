#' @title Load and install packages if necessary
#'
#' @description Specified packages are loaded and installed if necessary.
#'
#' @param ... the packages you want to install
#'
#' @return
#' loaded packages
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_packages("tidyverse")
#' load_packages("tidyverse", "dplyr")
#' }
#' @import
#' purrr
#' tibble
#'
#' @importFrom
#' utils
#' capture.output
#' install.packages
#' installed.packages
#'

load_packages <- function(...) {

  # Capture the package names as strings
  pkg <- as.character(match.call(expand.dots = TRUE))[-1]

  # Check for missing packages
  new_pkg <- check_installed_packages(pkg)

  # Install missing packages
  install_missing_packages(new_pkg)

  # Load packages
  loaded <- load_packages_helper(pkg)

  # Separate successfully loaded and failed packages
  successful <- pkg[loaded]
  failed <- pkg[!loaded]

  # Provide feedback on any packages that failed to load
  if (length(failed) > 0) {
    warning("Some packages could not be loaded: ", paste(failed, collapse = ", "))
  }

  # Return a list of loaded and failed packages
  return(list(loaded = successful, failed = failed))
}
