% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluator-lib-utils.R
\name{add_na_counts}
\alias{add_na_counts}
\title{Developer function to add number of NAs to evaluator results}
\usage{
add_na_counts(out, data, value_col, na_rm, ...)
}
\arguments{
\item{out}{Evaluator results tibble to append number of NA results to.}

\item{data}{Data used to compute number of NAs.}

\item{value_col}{Character string, specifying the column used to compute
the number of NAs.}

\item{na_rm}{A \code{logical} value indicating whether \code{NA} values
should be stripped before the computation proceeds.}

\item{...}{Additional name-value pairs to pass to dplyr::mutate() to append
columns.}
}
\value{
Tibble with additional rows containing the new metric "num_na" and
its corresponding ".estimate"
}
\description{
A helper function to append rows with number of NAs (per group,
if applicable) to evaluator results tibble.
}
\examples{
# generate example fit_results data with NA responses
fit_results <- tibble::tibble(
  .rep = rep(1:2, times = 2),
  .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
  .method_name = c("Method"),
  # true response
  y = lapply(1:4, FUN = function(x) c(rnorm(100 - x), rep(NA, x))),
  # predicted response
  predictions = lapply(1:4, FUN = function(x) rnorm(100))
)

# evaluate root mean squared error and number of NA responses for each row in
# fit_results
rmse_na_fun <- function(data, truth_col, estimate_col, na_rm = TRUE) {
  out <- tibble::tibble(
    .metric = "rmse",
    .estimate = yardstick::rmse_vec(
      data[[truth_col]], data[[estimate_col]], na_rm = na_rm
    )
  ) \%>\%
    add_na_counts(data = data, value_col = truth_col, na_rm = na_rm)
  return(out)
}
eval_results <- eval_constructor(
  fit_results = fit_results,
  fun = rmse_na_fun,
  truth_col = "y",
  estimate_col = "predictions",
  na_rm = TRUE
) \%>\%
  tidyr::unnest(.eval_result)

}
