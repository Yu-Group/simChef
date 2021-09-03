#' \code{R6} class representing an evaluator.
#'
#' @docType class
#'
#' @description An evaluator (or evaluation function) to **evaluate** the
#'   performance of methods in the \code{Experiment}.
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',
  public = list(
    #' @field name The name of the \code{Evaluator}.
    name = NULL,
    #' @field eval_fun The evaluation function.
    eval_fun = NULL,
    #' @field eval_params (Named) list of parameters to input into the
    #'   evaluation function.
    eval_params = NULL,
    #' @field rmd_options List of options to control the aesthetics of the
    #'   displayed \code{Evaluator}'s results table in the knitted R Markdown
    #'   report. See [prettyDT()] for possible options.
    rmd_options = list(digits = 2, sigfig = FALSE,
                       options = list(scrollX = TRUE, scrollCollapse = TRUE)),
    #' @field show If \code{TRUE} (default), show \code{Evaluator}'s results as
    #'   a table in the R Markdown report; if \code{FALSE}, hide output in the
    #'   R Markdown report.
    show = TRUE,
    #' @description Create a new \code{Evaluator}.
    #'
    #' @param eval_fun The evaluation function.
    #' @param rmd_options List of options to control the aesthetics of the
    #'   displayed \code{Evaluator}'s results table in the knitted R Markdown
    #'   report. See [prettyDT()] for possible options.
    #' @param ... Arguments to pass into \code{eval_fun()}.
    #'
    #' @details When evaluating or running the \code{Experiment} (see
    #'   \code{Experiment$evaluate() or \code{Experiment$run()}}), the named
    #'   arguments \code{fit_results} and \code{vary_param} are automatically
    #'   passed into the \code{Evaluator} function \code{eval_fun()} and serve
    #'   as placeholders for the \code{Experiment$fit()} results (i.e., the
    #'   results from the method fits) and the name of the varying parameter,
    #'   respectively. To evaluate the performance of a method(s) fit then,
    #'   the \code{Evaluator} function \code{eval_fun()} should almost always
    #'   take in the named argument \code{fit_results}. See
    #'   \code{Experiment$fit()} or \code{fit_experiment()} for details on the
    #'   format of \code{fit_results}. If the \code{Evaluator}
    #'   is used for \code{Experiments} with varying parameters,
    #'   \code{vary_param} should be used as a stand in for the name of this
    #'   varying parameter.
    #'
    #' @return A new \code{Evaluator} object.
    initialize = function(eval_fun, name = NULL, rmd_options = list(), ...) {
      self$name <- name
      self$eval_fun <- eval_fun
      self$eval_params <- list(...)
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
    },
    #' @description Evaluate the performance of method(s) in the
    #'   \code{Experiment} using the \code{Evaluator} and its given parameters.
    #'
    #' @param fit_results A tibble, typically returned by the
    #'   \code{Experiment$fit()} method.
    #' @param vary_param Name of parameter/argument that was varied in the
    #'   \code{Experiment}, i.e., \code{Experiment$get_vary_across()$param_name}.
    #'   Use \code{NULL} (default) if no \code{vary_across} component in
    #'   \code{Experiment} run.
    #' @param ... Not used.
    #'
    #' @return Result of \code{eval_fun()}, coerced into a tibble.
    evaluate = function(fit_results, vary_param = NULL, ...) {
      args_list <- list(fit_results = fit_results,
                        vary_params = vary_params)
      if (!identical(self$eval_params, list())) {
        always_args_list <- self$eval_params
      } else {
        always_args_list <- NULL
      }
      eval_results <- R.utils::doCall(self$eval_fun,
                                      args = args_list,
                                      alwaysArgs = always_args_list)
      return(list_to_tibble(eval_results))
    }
  )
)

#' Create a new \code{Evaluator}.
#'
#' @name create_evaluator
#'
#' @param eval_fun The evaluation function.
#' @param ... Arguments to pass into \code{eval_fun()}.
#'
#' @return A new instance of \code{Evaluator}.
#'
#' @export
create_evaluator <- function(eval_fun, ...) {
  return(Evaluator$new(eval_fun, ...))
}
