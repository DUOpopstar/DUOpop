# Helper functions save_syn_data()

#' Check if the syn_object is not empty
#'
#' @param syn_object A synthetic data object, resulting from the use of the Synthpop package.
#' @return A warning if the syn_object is empty.
#' @keywords internal
check_not_empty <- function(syn_object){
  if(purrr::is_empty(syn_object) == TRUE) {
    warning("The syn_object you want to save is empty.")
  }
}

#' Check if the syn_object contains $syn
#'
#' @param syn_object A synthetic data object, resulting from the use of the Synthpop package.
#' @return An error if the syn_object does not contain $syn.
#' @keywords internal
check_syn <- function(syn_object){
  if(!"syn" %in% names(syn_object)) {
    stop("The data input is incorrect. Save_syn_data() only works with a synthetic
         data object, including $syn, resulting from the use of the Synthpop package.")
  }
}

#' Check if the file doesn't already exist in the specified path
#'
#' @param path A specified path where the results will be saved.
#' @param file_name A name for the CSV and Rds file.
#' @param overwrite Should an already existing file with a similar file_name and
#' path be overwritten? (default = FALSE)
#' @return An error if the file_name already exists in the specified path.
#' @keywords internal
check_file_exists <- function(path, file_name, overwrite){
  if(file.exists(file = paste0(path, file_name, ".csv")) && !overwrite){
    stop("The CSV file already exists in the current directory!
         In case you want to overwrite, add overwrite = TRUE to save_syn_data()")
  }
  else if(file.exists(file = paste0(path, file_name, ".Rds")) && !overwrite){
    stop("The Rds file already exists in the current directory!
         In case you want to overwrite, add overwrite = TRUE to save_syn_data()")
  }
}

#' Validate the synthetic object
#'
#' @param syn_object A synthetic data object, resulting from the use of the Synthpop package.
#' @param path A specified path where the results will be saved.
#' @param file_name A name for the CSV and Rds file.
#' @param overwrite Should an already existing file with a similar file_name and
#' path be overwritten? (default = FALSE)
#' @return Warning if the syn_object is empty, and errors if the syn_object does not
#' contain $syn and if the specified file_name already exists in the specified path.
#' @keywords internal
validate_syn_object <- function(syn_object, path, file_name, overwrite) {
  check_not_empty(syn_object)
  check_syn(syn_object)
  check_file_exists(path, file_name, overwrite)
}

#' Save data as CSV
#'
#' @param data A synthetic data object including $syn, resulting from the use of the Synthpop package.
#' @param path A specified path where the results will be saved.
#' @param file_name A name for the CSV file.
#' @param overwrite Should an already existing file with a similar file_name and
#' path be overwritten? (default = FALSE)
#' @return A CSV file from the synthetic data.
#' @keywords internal
save_as_csv <- function(data, path, file_name, overwrite) {
  file_path <- paste0(path, file_name, ".csv")
  if (!file.exists(file_path) || overwrite == TRUE) {
    write.csv2(x = data, file = file_path, row.names = FALSE)
  }
}

#' Save data as Rds
#'
#' @param object A synthetic data object including $syn, resulting from the use of the Synthpop package.
#' @param path A specified path where the results will be saved.
#' @param file_name A name for the Rds file.
#' @param overwrite Should an already existing file with a similar file_name and
#' path be overwritten? (default = FALSE)
#' @return A Rds file from the synthetic data.
#' @keywords internal
save_as_rds <- function(object, path, file_name, overwrite) {
  file_path <- paste0(path, file_name, ".Rds")
  if (!file.exists(file_path) || overwrite == TRUE) {
    saveRDS(object = object, file = file_path)
  }
}


#' Notify the user about successful save
#'
#' @param file_name A name for the CSV and Rds file.
#' @param path A specified path where the results will be saved.
#' @return A notification that the data is saved successfully, including the specified
#' file_name and path.
#' @keywords internal
notify_save_success <- function(file_name, path) {
  message <- paste0("The data is saved successfully as ", file_name, ".csv and ", file_name, ".Rds in ", path)
  print(message)
}
