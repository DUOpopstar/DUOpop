% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_syn_data.R
\name{save_syn_data}
\alias{save_syn_data}
\title{Save synthetic data}
\usage{
save_syn_data(syn_object, path, file_name, overwrite = FALSE)
}
\arguments{
\item{syn_object}{A synthetic data object, resulting from the use of the Synthpop package.}

\item{path}{A specified path where the results will be saved.}

\item{file_name}{A name for the CSV and Rds file.}

\item{overwrite}{Should an already existing file with a similar file_name and
path be overwritten? (default = FALSE)}
}
\value{
A CSV and Rds file from the synthetic data
}
\description{
This function saves a synthetic data object in a CSV and Rds file.
}
\examples{
syn_object <- list(syn = data.frame(a = 1:3, b = 4:6))
# specify a temp directory
path <- gsub("\\\\", "/", tempdir(), fixed = TRUE)
if (!grepl("/$", path)) {path <- paste0(path, "/")}
save_syn_data(syn_object = syn_object, path = path,
file_name = "syndata_v01", overwrite = FALSE)

}
