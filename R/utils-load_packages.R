# =====================================================
# Helper Function: check_installed_packages ----
# =====================================================
#' @title check installed packages
#' @param pkg A character vector of package names.
#' @return A character vector of packages that are not installed.
#' @keywords internal
check_installed_packages <- function(pkg) {
  installed <- rownames(installed.packages())
  new_pkg <- pkg[!(pkg %in% installed)]
  return(new_pkg)
}

# =====================================================
# Helper Function: install_missing_packages ----
# =====================================================
#' @title check missing packages
#' @param new_pkg A character vector of packages that are not installed.
#' @return Packages specified in new_pkg are installed.
#' @keywords internal
install_missing_packages <- function(new_pkg) {
  if (length(new_pkg)) {
    install.packages(new_pkg, repos = "http://cran.rstudio.com")
  }
}

# =====================================================
# Helper Function: load_packages_helper ----
# =====================================================
#' @title load packages
#' @param pkg A character vector of package names.
#' @return A logical vector indicating whether each package was successfully loaded.
#' @keywords internal
load_packages_helper <- function(pkg) {
  loaded <- sapply(pkg, function(p) {
    success <- require(p, character.only = TRUE)
    return(success)
  })
  return(loaded)
}
