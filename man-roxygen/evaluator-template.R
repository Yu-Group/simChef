#' @section Public fields:
#'
#' \describe{
#' \item{\code{name}}{The name of the \code{Evaluator}.}
#' \item{\code{eval_fun}}{The evaluation function.}
#' \item{\code{eval_params}}{(Named) list of parameters to input into the evaluation function.}
#' \item{\code{rmd_options}}{List of options to control the aesthetics of the
#'   displayed \code{Evaluator}'s results table in the knitted R Markdown
#'   report. See [pretty_DT()] for possible options.}
#' \item{\code{}}{If \code{TRUE} (default), show \code{Evaluator}'s results as
#'   a table in the R Markdown report; if \code{FALSE}, hide output in the
#'   R Markdown report.}
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#' \item{\code{Evaluator$new()}} See \link{create_evaluator}.
#' }
