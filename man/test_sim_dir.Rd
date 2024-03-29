% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run-tests.R
\name{test_sim_dir}
\alias{test_sim_dir}
\title{Test simChef Ingredients}
\usage{
test_sim_dir()
}
\description{
\code{test_sim_dir()} wraps around
\code{\link[testthat:test_dir]{testthat::test_dir()}} to run all dgp-, method-, evaluator-,
and visualizer-related tests.
}
\details{
This function only works if the simulation study is set up as an R
project and if it is run when this R project is active. Additionally, tests
must be stored in sub-directories whose names match those suggesting in the
"Setting Up Your Simulation" vignette. These details are conveniently taken
care of when a simulation study is set up using \code{\link[=create_sim]{create_sim()}}.
}
\examples{
\dontrun{
# run dgp-, method-, evaluator-, and visualizer-related tests
test_sim_dir()}

}
