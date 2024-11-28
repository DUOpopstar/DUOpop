### Helper functions for save_session_info()

#' Check if specified path exists
#' @param path The path where the log file information will be stored.
#' @return An error if the specified path does not exist.
#' @keywords internal
validate_path <- function(path) {
  if (!dir.exists(path)) {
    stop("The specified path does not exist: ", path)
  }
}

#' Generate a file name with a timestamp
#' @param file_name Specifying the file name of the text file where the session info will be stored.
#' The timestamp will be added to the file name.
#' @param path The path where the log file information will be stored.
#' @return A path including file name with a timestamp.
#' @keywords internal
create_log_file_path <- function(file_name, path) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  modified_file_name <- paste0(file_name, "_", timestamp, ".txt")
  file_path <- file.path(path, modified_file_name)
  return(file_path)
}

#' Capture session information with a timestamp
#' @return Session information.
#' @keywords internal
capture_session_info <- function() {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  session_info <- c(paste("Timestamp:", timestamp), capture.output(sessionInfo()))
  return(session_info)
}
