% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signals.R
\name{abort}
\alias{abort}
\title{Signals an error with default subclass "simChef_error".}
\usage{
abort(message = NULL, class = "simChef_error", call = rlang::caller_env(), ...)
}
\arguments{
\item{message}{Message to include with the condition.}

\item{class}{Subclass passed to the appropriate \code{rlang} signal function.}

\item{call}{The function environment that is most relevant from the user's
perspective. Default is \code{rlang::caller_env()}, which gives the environment
attached to the function that called this signal function.}

\item{...}{Additional arguments to pass to the appropriate \code{rlang} signal
function.}
}
\description{
Signals an error with default subclass "simChef_error".
}
\keyword{internal}
