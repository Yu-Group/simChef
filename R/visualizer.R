#' \code{R6} class representing a visualizer.
#'
#' @docType class
#'
#' @description A visualizer (or visualization function) to **visualize** the performance of
#'   methods and/or its evaluation metrics from the \code{Experiment} run.
#'
#' @export
Visualizer <- R6::R6Class(
  classname = 'Visualizer',
  public = list(
    #' @field name The name of the \code{Visualizer}.
    name = NULL,
    #' @field visualizer_fun The visualization function.
    visualizer_fun = NULL,
    #' @field visualizer_params (Named) list of parameters to input into the
    #'   visualizer function.
    visualizer_params = NULL,
    #' @field rmd_options List of options to control the aesthetics of the
    #'   \code{Visualizer}'s visualizations in the knitted R Markdown report.
    #'   Currently, possible options are "height" and "width" (in inches).
    rmd_options = list(height = 6, width = 10),
    #' @field show If \code{TRUE} (default), show the resulting visualization in the R
    #'   Markdown report; if \code{FALSE}, hide output in the R Markdown report.
    show = TRUE,
    #' @description Create a new \code{Visualizer}.
    #'
    #' @param visualizer_fun The visualization function.
    #' @param name (Optional) The name of the \code{Visualizer}.
    #' @param rmd_options (Optional) List of options to control the aesthetics
    #'   of the \code{Visualizer}'s visualization in the knitted R Markdown
    #'   report. Currently, possible options are "height" and "width" (in
    #'   inches).
    #' @param ... Arguments to pass into \code{visualizer_fun()}.
    #'
    #' @details When visualizing or running the \code{Experiment} (see
    #'   \code{Experiment$visualize() or \code{Experiment$run()}}), the named
    #'   arguments \code{fit_results}, \code{eval_results}, and
    #'   \code{vary_params} are automatically passed into the \code{Visualizer}
    #'   function \code{visualizer_fun()} and serve as placeholders for the
    #'   \code{Experiment$fit()} results, the \code{Experiment$evaluate()}
    #'   results, and the name of the varying parameter, respectively.
    #'   To visualize the performance of a method(s) fit and/or its evaluation
    #'   metrics then, the \code{Visualizer} function \code{visualizer_fun()} should
    #'   take in the named arguments \code{fit_results} and/or
    #'   \code{eval_results}. See \code{Experiment$fit()} or
    #'   \code{fit_experiment()} for details on the format of \code{fit_results}.
    #'   See \code{Experiment$evaluate()} or \code{evaluate_experiment()} for
    #'   details on the format of \code{eval_results}. If the \code{Visualizer}
    #'   is used for \code{Experiments} with varying parameters,
    #'   \code{vary_params} should be used as a stand in for the name of this
    #'   varying parameter.
    #'
    #' @return A new \code{Visualizer} object.
    initialize = function(visualizer_fun, name = NULL, rmd_options = list(), ...) {
      self$name <- name
      self$visualizer_fun <- visualizer_fun
      self$visualizer_params <- list(...)
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
    },
    #' @description Visualize the performance of methods and/or their evaluation
    #'   metrics from the \code{Experiment} using the \code{Visualizer} and the
    #'   provided parameters.
    #'
    #' @param fit_results A tibble, typically returned by the
    #'   \code{Experiment$fit()} method.
    #' @param eval_results A list of tibbles, typically returned by the
    #'   \code{Experiment$evaluate()} method.
    #' @param vary_params Name of parameter/argument that was varied in the
    #'   \code{Experiment} (see \code{Experiment$get_vary_across()}).
    #'   Use \code{NULL} (default) if no \code{vary_across} component in
    #'   \code{Experiment} run.
    #' @param ... Not used.
    #'
    #' @return Result of \code{visualizer_fun()}.
    visualize = function(fit_results = NULL, eval_results = NULL,
                         vary_params = NULL, ...) {
      args_list <- list(fit_results = fit_results,
                        eval_results = eval_results,
                        vary_params = vary_params)
      if (!identical(self$visualizer_params, list())) {
        always_args_list <- self$visualizer_params
      } else {
        always_args_list <- NULL
      }
      visualize_results <- R.utils::doCall(self$visualizer_fun,
                                      args = args_list,
                                      alwaysArgs = always_args_list)
      return(visualize_results)
    }
  )
)

#' Create a new \code{Visualizer}.
#'
#' @name create_visualizer
#'
#' @param visualizer_fun The visualization function.
#' @param name (Optional) The name of the \code{Visualizer}.
#' @param rmd_options (Optional) List of options to control the aesthetics of
#'   the \code{Visualizer}'s visualization in the knitted R Markdown report.
#'   Currently, possible options are "height" and "width" (in inches).
#' @param ... Arguments to pass into \code{visualizer_fun()}.
#'
#' @return A new instance of \code{Visualizer}.
#'
#' @export
create_visualizer <- function(visualizer_fun, name = NULL, rmd_options = list(), ...) {
  return(Visualizer$new(visualizer_fun, name = name, rmd_options = rmd_options, ...))
}
