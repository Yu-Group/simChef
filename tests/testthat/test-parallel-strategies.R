test_that("Various parallel strategies in experiment work properly", {

  # TODO: test this separately
  # withr::local_options(list(future.globals.onReference = "error"))

  dgp_fun1 <- function(y = "") return(x = list(paste0("data1", y)))
  dgp_fun2 <- function() return(x = list("data2"))
  dgp_fun3 <- function() { stop("Uh oh!") }
  dgp1 <- create_dgp(dgp_fun1)
  dgp2 <- create_dgp(dgp_fun2)
  dgp3 <- create_dgp(dgp_fun3)

  method_fun1 <- function(x, y = "") return(list(result = paste0(x, "+method1", y)))
  method_fun2 <- function(x) return(list(result = paste0(x, "+method2")))
  method_fun3 <- function(x) { stop("Uh oh!") }
  method1 <- create_method(method_fun1)
  method2 <- create_method(method_fun2)
  method3 <- create_method(method_fun3)

  experiment <- create_experiment(
    dgp_list = list(dgp1, dgp2),
    method_list = list(method1, method2)
  )

  expected_results <- c("data1+method1", "data1+method2",
                        "data2+method1", "data2+method2")

  strategies <- list(c("reps"), c("dgps"), c("methods"),
                     c("reps", "dgps"), c("reps", "methods"),
                     c("dgps", "methods"), c("reps", "dgps", "methods"))

  for (strat in strategies) {

    expect_error_if_invalid_strat(
      results <- experiment$fit(
        n_reps = 2, parallel_strategy = strat, verbose = 0
      ),
      expectations_when_valid = {

        expect_named(results, c(".rep",
                                ".dgp_name",
                                ".method_name",
                                "result"))

        results_tally <- results %>%
          dplyr::group_by(result) %>%
          dplyr::tally()

        expect_true(length(results_tally$result) == 4)
        expect_true(all(expected_results %in% results_tally$result))
        expect_equal(results_tally$n, rep(2, 4))

      }
    )

    experiment$add_dgp(dgp3)

    expect_error_if_invalid_strat(
      err <- experiment$fit(
        n_reps = 2, parallel_strategy = strat, verbose = 0
      ),
      class_when_valid = "simChef_error",
      expectations_when_valid = {

        expect_partial_results_and_errors(err)
        expect_true(all(err$errors$.dgp_name == "dgp3"))
      }
    )

    experiment$remove_dgp("dgp3")
    experiment$add_method(method3)

    expect_error_if_invalid_strat(
      err <- experiment$fit(n_reps = 2, parallel_strategy = strat, verbose = 0),
      class_when_valid = "simChef_error",
      expectations_when_valid = {

        expect_partial_results_and_errors(err)
        expect_true(all(err$errors$.method_name == "method3"))
      }
    )

    experiment$remove_method("method3")

    # add vary_across
    experiment %>%
      add_vary_across(.dgp = dgp1, y = c("a", "b", "c")) %>%
      add_vary_across(.method = method1, y = c("a", "b", "c"))

    dgp_fun3 <- function(y = "") {
      if (y == "b") stop("Why b?!")
      return(x = list(paste0("data3", y)))
    }
    dgp3 <- create_dgp(dgp_fun3)

    method_fun3 <- function(x, y = "") {
      if (y == "c") stop("Why c?!")
      return(list(result = paste0(x, "+method3", y)))
    }
    method3 <- create_method(method_fun3)

    expected_results <- c("data1a+method1a", "data1a+method1b", "data1a+method1c",
                          "data1b+method1a", "data1b+method1b", "data1b+method1c",
                          "data1c+method1a", "data1c+method1b", "data1c+method1c",
                          "data1a+method2", "data1b+method2", "data1c+method2",
                          "data2+method1a", "data2+method1b", "data2+method1c",
                          "data2+method2")

    expect_error_if_invalid_strat(
      results <- experiment$fit(n_reps = 2,
                                parallel_strategy = strat,
                                verbose = 0),
      expectations_when_valid = {

        expect_named(results, c(".rep",
                                ".dgp_name",
                                ".method_name",
                                "y_dgp",
                                "y_method",
                                "result"))

        results_tally <- results %>%
          dplyr::group_by(result) %>%
          dplyr::tally()

        expect_true(length(results_tally$result) == 16)
        expect_true(all(expected_results %in% results_tally$result))
        expect_equal(results_tally$n, rep(2, 16))
      }
    )

    experiment$add_dgp(dgp3)

    experiment %>%
      add_vary_across(.dgp = dgp3, y = c("a", "b", "c"))

    expect_error_if_invalid_strat(
      err <- experiment$fit(n_reps = 2,
                            parallel_strategy = strat,
                            verbose = 0),
      class_when_valid = "simChef_error",
      expectations_when_valid = {

        expect_partial_results_and_errors(err)
        expect_true(all(err$errors$.dgp_name == "dgp3"))
      }
    )

    experiment$remove_dgp("dgp3")

    experiment$add_method(method3)

    experiment %>%
      add_vary_across(.method = method3, y = c("a", "b", "c"))

    expect_error_if_invalid_strat(
      err <- experiment$fit(n_reps = 2,
                            parallel_strategy = strat,
                            verbose = 0),
      class_when_valid = "simChef_error",
      expectations_when_valid = {

        expect_partial_results_and_errors(err)
        expect_true(all(err$errors$.method_name == "method3"))
      }
    )

    experiment$remove_method("method3")

    experiment %>%
      remove_vary_across(dgp = dgp1) %>%
      remove_vary_across(method = method1)

  } # for (strat in strategies) {


  dgp_fun1 <- function(y = "") return(x = list(paste0("data1", y)))
  dgp1 <- create_dgp(dgp_fun1)

  method_fun1 <- function(x) return(list(y = paste0(x, "+method1")))
  method1 <- create_method(method_fun1)

  experiment <- create_experiment(dgp_list = list(dgp1),
                                  method_list = list(method1)) %>%
    add_vary_across(.dgp = dgp1, y = c("a", "b", "c"))

  for (strat in strategies) {

    # return error if duplicate column names
    expect_error_if_invalid_strat(
      experiment$fit(n_reps = 2,
                     parallel_strategy = strat,
                     verbose = 0),
      class_when_valid = "simChef_error"
    )
  }

  dgp_fun <- function(x, y = NULL) list(x = x)
  dgp <- create_dgp(.dgp_fun = dgp_fun, x = 1:10)

  method_fun <- function(x, idx = 1) list(x_idx = x[idx])
  method <- create_method(.method_fun = method_fun)

  fit_results_fun <- function(fit_results) fit_results
  fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)
  vary_params_fun <- function(vary_params = NULL) vary_params
  vary_params_eval <- create_evaluator(.eval_fun = vary_params_fun)

  experiment <- create_experiment(name = "test-vary-across-dgp") %>%
    add_dgp(dgp, name = "DGP") %>%
    add_method(method, name = "Method") %>%
    add_evaluator(fit_results_eval, name = "Fit Results") %>%
    add_evaluator(vary_params_eval, name = "Vary Params")

  # test multi-type (dgp, method) vary across case
  experiment <- experiment %>%
    add_vary_across(.dgp = "DGP", x = list(1, 3:5)) %>%
    add_vary_across(.method = "Method", idx = list(1, 1:2))

  parallel_strategies <- list(
    "reps", "dgps", "methods", c("reps", "dgps"), c("reps", "methods"),
    c("dgps", "methods"), c("reps", "dgps", "methods")
  )

  for (strat in parallel_strategies) {

    expect_error_if_invalid_strat(
      fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0,
                                    parallel_strategy = strat),
      expectations_when_valid = {
        expect_equal(nrow(fit_results), 4)
        expect_true(all(fit_results$x_idx %in% list(1, c(1, NA), 3, 3:4)))
        expect_true(all(fit_results$idx %in% list(1, 1:2, 1, 1:2)))
        expect_true(all(fit_results$x %in% list(1, 1, 3:5, 3:5)))
      }
    )

  }

})
