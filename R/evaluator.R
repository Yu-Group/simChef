# NOTE: R6 methods can't use the `@inheritParams` tag. If you update the
# `@param` tags below then be sure to manually replace the corresponding tags
# above `Evaluator$initialize()`.



#' Create a new `Evaluator`
#'
#' @name create_evaluator
#'
#' @description Create an [Evaluator] which can `evaluate()` the performance of
#'   methods in an [Experiment].
#'
#' @inherit Evaluator details
#'
#' @param .eval_fun The user-defined evaluation function.
#' @param .name (Optional) The name of the `Evaluator`, helpful for later
#'   identification. The argument must be specified by position or typed out in
#'   whole; no partial matching is allowed for this argument.
#' @param .doc_options (Optional) List of options to control the aesthetics of
#'   the displayed `Evaluator`'s results table in the knitted R Markdown report.
#'   See [vthemes::pretty_DT()] for possible options. The argument must be
#'   specified by position or typed out in whole; no partial matching is allowed
#'   for this argument.
#' @param .doc_show If `TRUE` (default), show `Evaluator`'s results as a table
#'   in the R Markdown report; if `FALSE`, hide output in the R Markdown report.
#' @param ... User-defined arguments to pass into `.eval_fun()`.
#'
#' @return A new [Evaluator] object.
#'
#' @examples
#' # create DGP
#' dgp_fun <- function(n, beta, rho, sigma) {
#'   cov_mat <- matrix(c(1, rho, rho, 1), byrow = T, nrow = 2, ncol = 2)
#'   X <- MASS::mvrnorm(n = n, mu = rep(0, 2), Sigma = cov_mat)
#'   y <- X %*% beta + rnorm(n, sd = sigma)
#'   return(list(X = X, y = y))
#' }
#' dgp <- create_dgp(.dgp_fun = dgp_fun,
#'                   .name = "Linear Gaussian DGP",
#'                   n = 50, beta = c(1, 0), rho = 0, sigma = 1)
#'
#' # create Method
#' lm_fun <- function(X, y, cols) {
#'   X <- X[, cols]
#'   lm_fit <- lm(y ~ X)
#'   pvals <- summary(lm_fit)$coefficients[-1, "Pr(>|t|)"] %>%
#'     setNames(paste(paste0("X", cols), "p-value"))
#'   return(pvals)
#' }
#' lm_method <- create_method(
#'   .method_fun = lm_fun,
#'   .name = "OLS",
#'   cols = c(1, 2)
#' )
#'
#' # create Experiment
#' experiment <- create_experiment() %>%
#'   add_dgp(dgp) %>%
#'   add_method(lm_method) %>%
#'   add_vary_across(.dgp = dgp, rho = seq(0.91, 0.99, 0.02))
#'
#' future::plan("multisession", workers = 10)
#' fit_results <- fit_experiment(experiment, n_reps=10)
#'
#' # create an example Evaluator function
#' reject_prob_fun <- function(fit_results, vary_params = NULL, alpha = 0.05) {
#'   fit_results[is.na(fit_results)] <- 1
#'   group_vars <- c(".dgp_name", ".method_name", vary_params)
#'   eval_out <- fit_results %>%
#'     dplyr::group_by(across({{group_vars}})) %>%
#'     dplyr::summarise(
#'       n_reps = dplyr::n(),
#'       `X1 Reject Prob.` = mean(`X1 p-value` < alpha),
#'       `X2 Reject Prob.` = mean(`X2 p-value` < alpha)
#'     )
#'   return(eval_out)
#' }
#'
#' reject_prob_eval <- create_evaluator(.eval_fun = reject_prob_fun,
#'                                      .name = "Rejection Prob (alpha = 0.05)")
#'
#' reject_prob_eval$evaluate(fit_results, vary_params = "rho")
#'
#' @export
create_evaluator <- function(.eval_fun, .name = NULL, .doc_options = list(),
                             .doc_show = TRUE, ...) {
  Evaluator$new(.eval_fun, .name, .doc_options, .doc_show, ...)
}

#' `R6` class representing an evaluator
#'
#' @name Evaluator
#'
#' @docType class
#'
#' @description `Evaluator` which can `evaluate()` the performance of
#'   methods in an [Experiment].
#'
#'   Generally speaking, users won't directly interact with the `Evaluator` R6
#'   class, but instead indirectly through [create_evaluator()] and the
#'   following `Experiment` helpers:
#'
#'   - [add_evaluator()]
#'   - [update_evaluator()]
#'   - [remove_evaluator()]
#'   - [get_evaluators()]
#'   - [evaluate_experiment()]
#'
#' @seealso [create_evaluator]
#'
#' @details When evaluating or running the `Experiment` (see
#'   [evaluate_experiment()] or [run_experiment()]), the named
#'   arguments `fit_results` and `vary_params` are automatically
#'   passed into the `Evaluator` function `.eval_fun()` and serve
#'   as placeholders for the `fit_experiment()` results (i.e., the
#'   results from the method fits) and the name of the varying parameter,
#'   respectively.
#'
#'   To evaluate the performance of a method(s) fit then,
#'   the `Evaluator` function `.eval_fun()` should almost always
#'   take in the named argument `fit_results`. See
#'   `Experiment$fit()` or [fit_experiment()] for details on the
#'   format of `fit_results`. If the `Evaluator`
#'   is used for `Experiments` with varying parameters,
#'   `vary_params` should be used as a stand in for the name of this
#'   varying parameter.
#'
#' @examples
#' # create DGP
#' dgp_fun <- function(n, beta, rho, sigma) {
#'   cov_mat <- matrix(c(1, rho, rho, 1), byrow = T, nrow = 2, ncol = 2)
#'   X <- MASS::mvrnorm(n = n, mu = rep(0, 2), Sigma = cov_mat)
#'   y <- X %*% beta + rnorm(n, sd = sigma)
#'   return(list(X = X, y = y))
#' }
#' dgp <- create_dgp(.dgp_fun = dgp_fun,
#'                   .name = "Linear Gaussian DGP",
#'                   n = 50, beta = c(1, 0), rho = 0, sigma = 1)
#'
#' # create Method
#' lm_fun <- function(X, y, cols) {
#'   X <- X[, cols]
#'   lm_fit <- lm(y ~ X)
#'   pvals <- summary(lm_fit)$coefficients[-1, "Pr(>|t|)"] %>%
#'     setNames(paste(paste0("X", cols), "p-value"))
#'   return(pvals)
#' }
#' lm_method <- create_method(
#'   .method_fun = lm_fun,
#'   .name = "OLS",
#'   cols = c(1, 2)
#' )
#'
#' # create Experiment
#' experiment <- create_experiment() %>%
#'   add_dgp(dgp) %>%
#'   add_method(lm_method) %>%
#'   add_vary_across(.dgp = dgp, rho = seq(0.91, 0.99, 0.02))
#'
#' future::plan("multisession", workers = 10)
#' fit_results <- fit_experiment(experiment, n_reps=10)
#'
#' # create an example Evaluator function
#' reject_prob_fun <- function(fit_results, vary_params = NULL, alpha = 0.05) {
#'   fit_results[is.na(fit_results)] <- 1
#'   group_vars <- c(".dgp_name", ".method_name", vary_params)
#'   eval_out <- fit_results %>%
#'     dplyr::group_by(across({{group_vars}})) %>%
#'     dplyr::summarise(
#'       n_reps = dplyr::n(),
#'       `X1 Reject Prob.` = mean(`X1 p-value` < alpha),
#'       `X2 Reject Prob.` = mean(`X2 p-value` < alpha)
#'     )
#'   return(eval_out)
#' }
#'
#' reject_prob_eval <- Evaluator$new(.eval_fun = reject_prob_fun,
#'                                   .name = "Rejection Prob (alpha = 0.05)")
#'
#' reject_prob_eval$evaluate(fit_results, vary_params = "rho")
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',

  public = list(

    #' @field name The name of the `Evaluator`.
    name = NULL,

    #' @field eval_fun The user-defined evaluation function.
    eval_fun = NULL,

    #' @field eval_params A (named) list of default parameters to input into
    #'   the evaluator function.
    eval_params = NULL,

    #' @field doc_options List of options to control the aesthetics of
    #'   the displayed `Evaluator`'s results table in the knitted R Markdown report.
    doc_options = list(digits = 2, sigfig = FALSE,
      options = list(scrollX = TRUE, scrollCollapse = TRUE)),

    #' @field doc_show Boolean indicating whether or not to show the
    #'   `Evaluator`'s results as a table in the R Markdown report.
    doc_show = TRUE,

    # NOTE: R6 methods can't use the `@inheritParams` tag. If you want to update
    # the `@param` tags below, do so in the `create_evaluator()` docs above and
    # then copy-paste the corresponding `@param` tags below.

    #' @description Initialize a new `Evaluator` object.
    #'
    #' @param .eval_fun The user-defined evaluation function.
    #' @param .name (Optional) The name of the `Evaluator`, helpful for later
    #'   identification. The argument must be specified by position or typed out in
    #'   whole; no partial matching is allowed for this argument.
    #' @param .doc_options (Optional) List of options to control the aesthetics of
    #'   the displayed `Evaluator`'s results table in the knitted R Markdown report.
    #'   See [vthemes::pretty_DT()] for possible options. The argument must be
    #'   specified by position or typed out in whole; no partial matching is allowed
    #'   for this argument.
    #' @param .doc_show If `TRUE` (default), show `Evaluator`'s results as a table
    #'   in the R Markdown report; if `FALSE`, hide output in the R Markdown report.
    #' @param ... User-defined arguments to pass into `.eval_fun()`.
    #'
    #' @return A new instance of `Evaluator`.
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

    #' @description Evaluate the performance of method(s) in the
    #'   `Experiment` using the `Evaluator` and its given parameters.
    #'
    #' @param fit_results A tibble, as returned by [fit_experiment()].
    #' @param vary_params A vector of `DGP` or `Method` parameter names that are
    #'   varied across in the `Experiment`.
    #' @param ... Not used.
    #'
    #' @return Result of `Evaluator$eval_fun()`, coerced into a tibble.
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

    #' @description Print an `Evaluator` in a nice format, showing the
    #'   `Evaluator`'s name, function, parameters, and R Markdown options.
    #'
    #' @return The original `Evaluator` object, invisibly.
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

