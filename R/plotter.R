#' A plot of an Experiment's results and evaluation metrics
#'
#' @export
Plotter <- R6::R6Class(
  classname = 'Plotter',
  public = list(
    plot_fun = NULL,
    plot_params = NULL,
    initialize = function(plot_fun, ...) {
      self$plot_fun <- plot_fun
      self$plot_params <- list(...)
    },
    plot = function(results = NULL, eval_results = NULL, ...) {
      if (identical(self$plot_params, list())) {
        plot_out <- self$plot_fun(results = results,
                                  eval_results = eval_results)
      } else {
        plot_out <- do.call(self$plot_fun, 
                            c(list(results = results,
                                   eval_results = eval_results), 
                              self$plot_params))
      }
      return(plot_out)
    }
  )
)

#' @export
create_plot <- function(plot_fun, ...) {
  return(Plotter$new(plot_fun, ...))
}
