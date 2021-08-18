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
    plot = function(results = NULL, eval_results = NULL, vary_param = NULL,
                    ...) {
      args_list <- list(results = results, eval_results = eval_results,
                        vary_param = vary_param)
      if (!identical(self$plot_params, list())) {
        args_list <- c(args_list, self$plot_params)
      }
      plot_out <- R.utils::doCall(self$plot_fun, args = args_list)
      return(plot_out)
    }
  )
)

#' @export
create_plot <- function(plot_fun, ...) {
  return(Plotter$new(plot_fun, ...))
}
