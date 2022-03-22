#' \code{R6} class representing an evaluator.
#'
#' @docType class
#'
#' @description An evaluator (or evaluation function) to **evaluate** the
#'   performance of methods in the \code{Experiment}.
#'
#' @template evaluator-template
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',
  public = list(
    name = NULL,
    eval_fun = NULL,
    eval_params = NULL,
    rmd_options = list(digits = 2, sigfig = FALSE,
                       options = list(scrollX = TRUE, scrollCollapse = TRUE)),
    rmd_show = TRUE,
    initialize = function(.eval_fun, .name = NULL, .rmd_options = list(),
                          .rmd_show = TRUE, ...) {
      self$eval_fun <- .eval_fun
      self$name <- .name
      for (opt in names(.rmd_options)) {
        self$rmd_options[[opt]] <- .rmd_options[[opt]]
      }
      self$rmd_show <- .rmd_show
      self$eval_params <- rlang::list2(...)
    },
    # @description Evaluate the performance of method(s) in the
    #   \code{Experiment} using the \code{Evaluator} and its given parameters.
    #
    # @param fit_results A tibble, typically returned by the
    #   \code{Experiment$fit()} method.
    # @param vary_params Name of parameters/arguments that were varied in the
    #   \code{Experiment} (see \code{Experiment$get_vary_across()}).
    #   Use \code{NULL} (default) if no \code{vary_across} component in
    #   \code{Experiment} run.
    # @param ... Not used.
    #
    # @return Result of \code{eval_fun()}, coerced into a tibble.
    evaluate = function(fit_results, vary_params = NULL, ...) {
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
    },
    # @description Print an \code{Evaluator} in a nice format, showing the 
    #   \code{Evaluator}'s name, function, parameters, and R Markdown options.
    #
    # @return The original \code{Evaluator} object.
    print = function() {
      if (is.null(self$name)) {
        cat("Evaluator Name: NULL \n")
      } else {
        cat("Evaluator Name:", self$name, "\n")
      }
      cat("   Function: ")
      cat(str(self$eval_fun, give.attr = F))
      cat("   Parameters: ")
      cat(str(self$eval_params,
              indent.str = "     ", no.list = F))
      cat("   R Markdown Options: ")
      cat(str(self$rmd_options,
              indent.str = "     ", no.list = F))
      cat("   Show in R Markdown:", self$rmd_show)
      invisible(self)
    }
  )
)

#' Create a new \code{Evaluator}.
#'
#' @name create_evaluator
#'
#' @param .eval_fun The evaluation function.
#' @param .name (Optional) The name of the \code{Evaluator}. The argument must be
#'   specified by position or typed out in whole; no partial matching is allowed
#'   for this argument.
#' @param .rmd_options (Optional) List of options to control the aesthetics of
#'   the displayed \code{Evaluator}'s results table in the knitted R Markdown
#'   report. See [vthemes::pretty_DT()] for possible options. The argument must be
#'   specified by position or typed out in whole; no partial matching is allowed
#'   for this argument.
#' @param .rmd_show If \code{TRUE} (default), show \code{Evaluator}'s results as
#'   a table in the R Markdown report; if \code{FALSE}, hide output in the
#'   R Markdown report.
#' @param ... Arguments to pass into \code{.eval_fun()}.
#'
#' @details When evaluating or running the \code{Experiment} (see
#'   \code{evaluate_experiment()} or \code{run_experiment()}), the named
#'   arguments \code{fit_results} and \code{vary_params} are automatically
#'   passed into the \code{Evaluator} function \code{.eval_fun()} and serve
#'   as placeholders for the \code{fit_experiment()} results (i.e., the
#'   results from the method fits) and the name of the varying parameter,
#'   respectively. To evaluate the performance of a method(s) fit then,
#'   the \code{Evaluator} function \code{.eval_fun()} should almost always
#'   take in the named argument \code{fit_results}. See
#'   \code{Experiment$fit()} or \code{fit_experiment()} for details on the
#'   format of \code{fit_results}. If the \code{Evaluator}
#'   is used for \code{Experiments} with varying parameters,
#'   \code{vary_params} should be used as a stand in for the name of this
#'   varying parameter.
#'
#' @return A new \code{Evaluator} object.
#' 
#' @examples
#' # create an example Evaluator function
#' reject_prob_fun <- function(fit_results, vary_params = NULL, alpha = 0.05) {
#'   group_vars <- c(".dgp_name", ".method_name", vary_params)
#'   eval_out <- fit_results %>%
#'     dplyr::group_by(across({{group_vars}})) %>%
#'     dplyr::summarise(
#'       `X1 Reject Prob.` = mean(`X1 p-value` < alpha),
#'       `X2 Reject Prob.` = mean(`X2 p-value` < alpha)
#'     )
#'   return(eval_out)
#' }
#' 
#' # create Evaluator using the default arguments (i.e., alpha = 0.05)
#' reject_prob_eval <- create_evaluator(.eval_fun = reject_prob_fun, 
#'                                      .name = "Rejection Prob (alpha = 0.05)")
#' # create Evaluator using non-default arguments (here, alpha = 0.1)
#' reject_prob_eval2 <- create_evaluator(.eval_fun = reject_prob_fun,
#'                                       .name = "Rejection Prob (alpha = 0.1)",
#'                                       # additional named parameters to pass to reject_prob_fun(),
#'                                       alpha = 0.1)
#' 
#' # create Evaluator from a function in the built-in Evaluator library
#' pred_err_eval <- create_evaluator(.eval_fun = summarize_pred_err,
#'                                   .name = "Prediction Error", 
#'                                   # additional named parameters to pass to summarize_pred_err()
#'                                   truth_col = "y", estimate_col = "predictions")
#'                                   
#' # set rmd options for displaying Evaluator in Rmd report to show 3 decimal points
#' pred_err_eval <- create_evaluator(.eval_fun = summarize_pred_err,
#'                                   .name = "Prediction Error", 
#'                                   .rmd_options = list(digits = 3),
#'                                   # additional named parameters to pass to summarize_pred_err()
#'                                   truth_col = "y", estimate_col = "predictions")
#'
#' @export
create_evaluator <- function(.eval_fun, .name = NULL, .rmd_options = list(),
                             .rmd_show = TRUE, ...) {
  Evaluator$new(.eval_fun, .name, .rmd_options, .rmd_show, ...)
}
