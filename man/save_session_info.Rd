% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_session_info.R
\name{save_session_info}
\alias{save_session_info}
\title{Save Session Information to a Log File with Timestamp}
\usage{
save_session_info(file_name, path)
}
\arguments{
\item{file_name}{Specifying the file name of the text file where the session info will be stored.
The timestamp will be added to the file name.}

\item{path}{The path where the log file information will be stored.}
}
\value{
The path to the log file where session information was stored, including the timestamp in the file name.
}
\description{
Captures and saves the current R session information, including loaded
packages and their versions, to a specified log file. This is useful for
documenting R environment settings for reproducibility in data analysis.
The function adds a timestamp to the log file name and at the beginning of the log file content.
}
\examples{
path <- temp_dir <- gsub("\\\\", "/", tempdir(), fixed = TRUE)
if (!grepl("/$", temp_dir)) {temp_dir <- paste0(temp_dir, "/")}
save_session_info(file_name = "session_info_log", path = path)
}
