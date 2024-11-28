#' Save Session Information to a Log File with Timestamp
#'
#' Captures and saves the current R session information, including loaded
#' packages and their versions, to a specified log file. This is useful for
#' documenting R environment settings for reproducibility in data analysis.
#' The function adds a timestamp to the log file name and at the beginning of the log file content.
#'
#' @param file_name Specifying the file name of the text file where the session info will be stored.
#' The timestamp will be added to the file name.
#' @param path The path where the log file information will be stored.
#' @return The path to the log file where session information was stored, including the timestamp in the file name.
#' @import
#' readr
#' utils
#' @export
#' @examples
#' path <- temp_dir <- gsub("\\", "/", tempdir(), fixed = TRUE)
#' if (!grepl("/$", temp_dir)) {temp_dir <- paste0(temp_dir, "/")}
#' save_session_info(file_name = "session_info_log", path = path)
save_session_info <- function(file_name, path) {

  # Validate path
  validate_path(path)

  # Generate the path including file name with timestamp
  log_file_path <- create_log_file_path(file_name, path)

  # Capture session info with a timestamp
  session_info <- capture_session_info()

  # Write session info to the log file
  readr::write_lines(x = session_info, file = log_file_path)

  return(paste("Session information successfully saved to:", log_file_path))
}
