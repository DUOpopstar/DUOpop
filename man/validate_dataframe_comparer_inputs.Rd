% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-dataframe_comparer.R
\name{validate_dataframe_comparer_inputs}
\alias{validate_dataframe_comparer_inputs}
\title{Check if Inputs are Data Frames}
\usage{
validate_dataframe_comparer_inputs(real_data, syn_data = NULL)
}
\arguments{
\item{real_data}{The main data frame that must be checked.}

\item{syn_data}{An optional second data frame to check. If NULL, it is ignored.}
}
\description{
This helper function checks whether the provided inputs are data frames
and stops execution with an error message if they are not.
}
\keyword{internal}
