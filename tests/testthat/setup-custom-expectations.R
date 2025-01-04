#' Custom expectation which wraps calls to `Experiment$fit` or `Experiment$run`
#' to capture errors when using unimplemented values of `parallel_strategy`.
#'
#' @param fit_or_run_expr Call to `Experiment$fit()` or `Experiment$run()` or
#'   corresponding helpers, possibly including assignment to a variable via `<-`
#'   (see `expectations_when_valid`). If this call uses an unimplemented
#'   `parallel_strategy`, then it is expected to throw a `simChef_error`.
#' @param class_when_valid A class string that should be included among the
#'   classes of the output from evaluating `fit_or_run_expr` when the `parallel_strategy`
#'   used is valid.
#' @param expectations_when_valid Additional expectations to evaluate in the
#'   when the `parallel_strategy` used is valid (implemented) and includes the
#'   expected return class. If assignment to a variable is used in the expr
#'   `fit_or_run_expr`, then this variable will hold the captured result of the
#'   right-hand side call to `fit()` or `run()` when the expectations are run.
#'   If not, then the captured result can be found in a variable named ".x".
#'
#' @keywords internal
expect_error_if_invalid_strat <- function(fit_or_run_expr,
                                          class_when_valid = c("tbl",
                                                               "simChef_error"),
                                          expectations_when_valid) {

  class_when_valid <- match.arg(class_when_valid)
  current_env <- environment()
  parent_env <- parent.frame()

  # possible values of `parallel_strategy`
  strats <- list(
    "reps", "dgps", "methods", c("reps", "dgps"), c("reps", "methods"),
    c("dgps", "methods"), c("reps", "dgps", "methods")
  )

  valid_strats <- list(c("reps"))

  # quote expr and get it's expression
  quo <- rlang::enquo(fit_or_run_expr)
  expr <- rlang::quo_get_expr(quo)

  # get the call to Experiment$run or Experiment$fit
  expr_list <- as.list(expr)

  # get the call and determine the variable for expectations_when_valid

  call_expr <- expr_list[[1]]
  result_name <- ".x"

  if (identical(as.character(call_expr), "<-")) {
    call_expr <- expr_list[[3]]
    result_name <- as.character(expr_list[[2]])
  } else {
    call_expr <- expr
  }

  # get the parallel strategy
  strat <- rlang::call_args(call_expr)$parallel_strategy

  if (is.null(strat)) {
    strat <- "reps"
  } else {
    strat <- eval(strat, rlang::quo_get_env(quo))
  }

  # substitute value of strat in the call
  call_expr <- rlang::call_modify(call_expr,
                                  parallel_strategy = strat)

  # capture evaluation of expr `fit_or_run_expr`
  act <- tryCatch(
    quasi_label(quo),
    simChef_error = identity
  )

  if ("simChef_error" %in% class(act)) {
    # save errors
    act <- list(
      val = act
    )
  }

  class_val <- class(act$val)

  # values for printing
  act$lab <- rlang::as_label(call_expr)
  act$class <- rlang::expr_deparse(
    rlang::quo_get_expr(rlang::enquo(class_val))
  )
  act$strat <- rlang::expr_deparse(
    rlang::quo_get_expr(rlang::enquo(strat))
  )

  # test for success or failure

  strat_is_strat <- any(sapply(strats, identical, strat))
  strat_is_valid <- any(sapply(valid_strats, identical, strat))

  if (!strat_is_strat) {

    # given `parallel_strategy` isn't in the list of possible values

    msg <- sprintf(
      "`parallel_strategy` \"%s\" not in the list of possible values. Typo?",
      act$strat
    )
    fail(msg)
  }

  if (strat_is_valid) {

    # strat is valid

    if (!class_when_valid %in% class_val) {

      msg <- sprintf("`%s` return class was %s but expected %s.",
                     act$lab, act$class, class_when_valid)
      fail(msg)
    }

    ## # set or get variable with name `result_name` from the parent environment
    ## parent_val <- rlang::env_cache(env = parent_env,
    ##                                nm = result_name,
    ##                                default = act$val)

    ## if (!identical(parent_val, act$val)) {

    ##   # reassign the variable to the call result
    ##   assign(result_name, act$val, envir = parent_env)

    ##   # return the parent's value to what it was before when this function and
    ##   # expectations_when_valid are done
    ##   withr::defer(assign(result_name, parent_val),
    ##                envir = current_env,
    ##                priority = "last")
    ## } else {
    ##   # remove result name from the parent environment
    ##   withr::defer(rm(result_name),
    ##                envir = current_env,
    ##                priority = "last")
    ## }

    if (!missing(expectations_when_valid)) {

      result <- tryCatch(
        expr = {
          # create the result variable
          assign(result_name, act$val, envir = parent_env)

          # evaluate the additional expectations
          force(expectations_when_valid)
        },
        expectation_failure = identity
      )

      if ("expectation_failure" %in% class(result)) {
        fail(result$message)
        return(invisible(act$val))
      }
    }

    succeed()
    return(invisible(act$val))

  }

  if ("simChef_error" %in% class_val) {

    # create variable for the captured error in the parent environment
    assign(result_name, act$val, envir = parent_env)

    # unsupported parallel_strategy threw simChef_error, which is expected
    succeed()
    return(invisible(act$val))
  }

  # unexpectedly, no error thrown
  msg <- paste(
    sprintf(
      paste(
        "`parallel_strategy` \"%s\" is unsupported but didn't cause an error.",
        "Instead, return class was %s.",
        "Maybe update the list of valid strategies in `setup-experiment.R`?"
      ),
      act$strat, act$class
    )
  )
  fail(msg)

}

expect_partial_results_and_errors <- function(err) {

  expect_s3_class(err, "simChef_error")

  expect_false(is.null(err$partial_results))
  expect_false(is.null(err$errors))

  expect_true(all(
    c(".rep", ".dgp_name", ".method_name") %in% names(err$partial_results)
  ))

  expect_named(err$errors, c(".rep",
                             ".dgp",
                             ".dgp_name",
                             ".dgp_params",
                             ".method",
                             ".method_name",
                             ".method_params",
                             ".method_output",
                             ".err",
                             ".pid",
                             ".gc"))

  for (i in seq(nrow(err$errors))) {
    expect_true("simChef_error" %in% class(err$errors$.err[[i]]))

    expect_true("DGP" %in% class(err$errors$.dgp[[i]]))

    if (is.logical(err$errors$.method[[i]])) {
      # TODO: should simplify_tibble return entirely empty columns with
      # list(NULL) values, or possibly just remove them all together?
      expect_true(is.na(err$errors$.method[[i]]))

    } else {
      expect_true("Method" %in% class(err$errors$.method[[i]]))
    }
  }

}
