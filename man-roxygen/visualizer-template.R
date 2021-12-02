#' @section Public fields:
#'
#' \describe{
#' \item{\code{name}}{The name of the \code{Visualizer}.}
#' \item{\code{viz_fun}}{The visualizer function.}
#' \item{\code{viz_params}}{(Named) list of parameters to input into the visualizer function.}
#' \item{\code{rmd_options}}{List of options to control the aesthetics of the
#'   displayed \code{Visualizer}'s visualizations in the knitted R Markdown
#'   report. Currently, possible options are "height" and "width" (in inches).}
#' \item{\code{rmd_show}}{If \code{TRUE} (default), show the resulting visualization
#'   in the R Markdown report; if \code{FALSE}, hide output in the
#'   R Markdown report.}
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#' \item{\code{Visualizer$new()}} See \link{create_visualizer}.
#' }
