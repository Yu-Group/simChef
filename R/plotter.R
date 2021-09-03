#' \code{R6} class representing a plotter.
#'
#' @docType class
#'
#' @description A plotter (or plotting function) to **plot** the performance of 
#'   methods and/or its evaluation metrics from the \code{Experiment} run.
#'
#' @export
Plotter <- R6::R6Class(
  classname = 'Plotter',
  public = list(
    #' @field name The name of the \code{Plotter}.
    name = NULL,
    #' @field plot_fun The plotting function.
    plot_fun = NULL,
    #' @field plot_params (Named) list of parameters to input into the 
    #'   plotting function.
    plot_params = NULL,
    #' @field rmd_options List of options to control the aesthetics of the
    #'   \code{Plotter}'s displayed plot in the knitted R Markdown report. 
    #'   Currently, possible options are "height" and "width" (in inches).
    rmd_options = list(height = 6, width = 10),
    #' @field show If \code{TRUE} (default), show the resulting plot in the R 
    #'   Markdown report; if \code{FALSE}, hide output in the R Markdown report.
    show = TRUE,
    #' @description Create a new \code{Plotter}.
    #'
    #' @param plot_fun The plotting function.
    #' @param rmd_options List of options to control the aesthetics of the
    #'   \code{Plotter}'s displayed plot in the knitted R Markdown report. 
    #'   Currently, possible options are "height" and "width" (in inches).
    #' @param ... Arguments to pass into \code{plot_fun()}.
    #'
    #' @details When plotting or running the \code{Experiment} (see 
    #'   \code{Experiment$plot() or \code{Experiment$run()}}), the named
    #'   arguments \code{fit_results}, \code{eval_results}, and 
    #'   \code{vary_param} are automatically passed into the \code{Plotter}
    #'   function \code{plot_fun()} and serve as placeholders for the 
    #'   \code{Experiment$fit()} results, the \code{Experiment$evaluate()} 
    #'   results, and the name of the varying parameter, respectively. 
    #'   To plot the performance of a method(s) fit and/or its evaluation 
    #'   metrics then, the \code{Plotter} function \code{plot_fun()} should
    #'   take in the named arguments \code{fit_results} and/or 
    #'   \code{eval_results}. See \code{Experiment$fit()} or 
    #'   \code{fit_experiment()} for details on the format of \code{fit_results}.
    #'   See \code{Experiment$evaluate()} or \code{evaluate_experiment()} for
    #'   details on the format of \code{eval_results}. If the \code{Plotter}
    #'   is used for \code{Experiments} with varying parameters, 
    #'   \code{vary_param} should be used as a stand in for the name of this
    #'   varying parameter.
    #'
    #' @return A new \code{Plotter} object.
    initialize = function(plot_fun, name = NULL, rmd_options = list(), ...) {
      self$name <- NULL
      self$plot_fun <- plot_fun
      self$plot_params <- list(...)
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
    },
    #' @description Plot the performance of methods and/or their evaluation
    #'   metrics from the \code{Experiment} using the \code{Plotter} and the 
    #'   provided parameters.
    #' 
    #' @param fit_results A tibble, typically returned by the 
    #'   \code{Experiment$fit()} method.
    #' @param eval_results A list of tibbles, typically returned by the
    #'   \code{Experiment$evaluate()} method.
    #' @param vary_param Name of parameter/argument that was varied in the 
    #'   \code{Experiment}, i.e., \code{Experiment$get_vary_across()$param_name}.
    #'   Use \code{NULL} (default) if no \code{vary_across} component in 
    #'   \code{Experiment} run.
    #' @param ... Not used.
    #' 
    #' @return Result of \code{plot_fun()}, which should be a plot.
    plot = function(fit_results = NULL, eval_results = NULL,
                    vary_params = NULL, ...) {
      args_list <- list(fit_results = fit_results,
                        eval_results = eval_results,
                        vary_params = vary_params)
      if (!identical(self$plot_params, list())) {
        always_args_list <- self$plot_params
      } else {
        always_args_list <- NULL
      }
      plot_results <- R.utils::doCall(self$plot_fun,
                                      args = args_list,
                                      alwaysArgs = always_args_list)
      return(plot_results)
    }
  )
)

#' Create a new \code{Plotter}.
#'
#' @name create_plotter
#' 
#' @param plot_fun The plotting function.
#' @param ... Arguments to pass into \code{plot_fun()}.
#'
#' @return A new instance of \code{Plotter}.
#'
#' @export
create_plotter <- function(plot_fun, ...) {
  return(Plotter$new(plot_fun, ...))
}
