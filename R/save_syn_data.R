#' Save synthetic data
#'
#' This function saves a synthetic data object in a CSV and Rds file.
#'
#' @param syn_object A synthetic data object, resulting from the use of the Synthpop package.
#' @param path A specified path where the results will be saved.
#' @param file_name A name for the CSV and Rds file.
#' @param overwrite Should an already existing file with a similar file_name and
#' path be overwritten? (default = FALSE)
#'
#' @return A CSV and Rds file from the synthetic data
#' @export
#'
#' @examples
#' syn_object <- list(syn = data.frame(a = 1:3, b = 4:6))
#' # specify a temp directory
#' path <- gsub("\\", "/", tempdir(), fixed = TRUE)
#' if (!grepl("/$", path)) {path <- paste0(path, "/")}
#' save_syn_data(syn_object = syn_object, path = path,
#' file_name = "syndata_v01", overwrite = FALSE)
#'
#' @import
#' utils
#' purrr
#' tibble
save_syn_data <- function(syn_object, path, file_name, overwrite = FALSE){

  #check the synthetic object
  validate_syn_object(syn_object, path, file_name, overwrite)

  #save the data as CSV and RDS
  save_as_csv(syn_object$syn, path, file_name, overwrite)
  save_as_rds(syn_object, path, file_name, overwrite)

  #notify user when the data is saved successfully
  notify_save_success(file_name, path)
  }
