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
    doc_options = list(digits = 2, sigfig = FALSE,
                       options = list(scrollX = TRUE, scrollCollapse = TRUE)),
    doc_show = TRUE,
    initialize = function(.eval_fun, .name = NULL, .doc_options = list(),
                          .doc_show = TRUE, ...) {
      self$eval_fun <- .eval_fun
      self$name <- .name
      for (opt in names(.doc_options)) {
        self$doc_options[[opt]] <- .doc_options[[opt]]
      }
      self$doc_show <- .doc_show
      self$eval_params <- rlang::list2(...)
    },
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
      cat(str(self$doc_options,
              indent.str = "     ", no.list = F))
      cat("   Show in R Markdown:", self$doc_show, "\n")
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
#' @param .doc_options (Optional) List of options to control the aesthetics of
#'   the displayed \code{Evaluator}'s results table in the knitted R Markdown
#'   report. See [vthemes::pretty_DT()] for possible options. The argument must be
#'   specified by position or typed out in whole; no partial matching is allowed
#'   for this argument.
#' @param .doc_show If \code{TRUE} (default), show \code{Evaluator}'s results as
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
#' # set doc options for displaying Evaluator in Rmd report to show 3 decimal points
#' pred_err_eval <- create_evaluator(.eval_fun = summarize_pred_err,
#'                                   .name = "Prediction Error",
#'                                   .doc_options = list(digits = 3),
#'                                   # additional named parameters to pass to summarize_pred_err()
#'                                   truth_col = "y", estimate_col = "predictions")
#'
#' @export
create_evaluator <- function(.eval_fun, .name = NULL, .doc_options = list(),
                             .doc_show = TRUE, ...) {
  Evaluator$new(.eval_fun, .name, .doc_options, .doc_show, ...)
}

#' Evaluate an \code{Evaluator}.
#'
#' @name evaluate_evaluator
#' @description Evaluate the performance of method(s) in the
#'   \code{Experiment} using the \code{Evaluator} and its given parameters.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param evaluator An \code{Evaluator} object.
#' @param ... Not used.
#'
#' @return Result of \code{Evaluator$eval_fun()}, coerced into a tibble.
#'
#' @inherit visualize_visualizer examples
#' @export
evaluate_evaluator <- function(evaluator, fit_results, vary_params = NULL,
                               ...) {
  evaluator$evaluate(fit_results, vary_params, ...)
}
