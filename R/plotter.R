#' A plot of an Experiment's results and evaluation metrics
#'
#' @export
Plotter <- R6::R6Class(
  classname = 'Plotter',
  public = list(
    plot_fun = NULL,
    plot_params = NULL,
    rmd_options = list(height = 6, width = 10),
    initialize = function(plot_fun, rmd_options = list(), ...) {
      self$plot_fun <- plot_fun
      self$plot_params <- list(...)
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
    },
    plot = function(results = NULL, eval_results = NULL, vary_param = NULL,
                    ...) {
      args_list <- list(results = results, eval_results = eval_results,
                        vary_param = vary_param)
      if (!identical(self$plot_params, list())) {
        always_args_list <- self$plot_params
      } else {
        always_args_list <- NULL
      }
      plot_out <- R.utils::doCall(self$plot_fun,
                                  args = args_list, 
                                  alwaysArgs = always_args_list)
      return(plot_out)
    }
  )
)

#' @export
create_plot <- function(plot_fun, ...) {
  return(Plotter$new(plot_fun, ...))
}
