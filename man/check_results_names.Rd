% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_results_names}
\alias{check_results_names}
\title{Helper function to throw informative error when column names are duplicated,
in particular, when the same parameter is in the user-provided method
results output and also in vary_across.}
\usage{
check_results_names(names, method_name)
}
\arguments{
\item{names}{Vector of column names to check for uniqueness}
}
\value{
Throws an error if duplicate names are found. Returns the original
names otherwise.
}
\description{
Helper function to throw informative error when column names are duplicated,
in particular, when the same parameter is in the user-provided method
results output and also in vary_across.
}
\keyword{internal}
