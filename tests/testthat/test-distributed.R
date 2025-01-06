# sets up experiment and returns sequential and parallel results
# should be called from within a function (e.g. test_that())
fit_experiment_fixture <- function(parallel_plan) {

  # set this to TRUE to debug memory usage (this will cause the tests to fail
  # because a bunch of plan-specific info gets added to output results)
  withr::local_options(
    list(
      simChef.debug = FALSE,
      future.globals.maxSize = 850 * 1024^2
    )
  )

  dgp_fun <- function(n, p, error_prob = 0) {

    error <- sample(c(TRUE, FALSE), size = 1,
                    prob = c(error_prob, 1 - error_prob))

    if (error) {
      stop("uh oh!")
    }

    ## generate covariate matrix
    X <- matrix(rnorm(n = n * p), nrow = n)

    ## sample linear model coefficients
    betas <- sample(
      c(0, 0.5, 1), size = p,
      replace = TRUE, prob = c(0.5, 0.3, 0.2)
    )

    ## create a linear response y with additive gaussian noise
    y <- cbind(1, X) %*% c(1, betas) + rnorm(n)
    return(list(X = X, y = y, betas = betas))

  }

  dgp <- create_dgp(dgp_fun, n = 100, p = 3)

  method_fun <- function(X, y, ...) {

    fit <- lm(y ~ as.matrix(X))

    y_pred <- cbind(1, as.matrix(X)) %*% coef(fit)

    return(c(list(y_pred = y_pred), ...))
  }

  method <- create_method(.method_fun = method_fun)

  experiment <- create_experiment() |>
    add_dgp(dgp) |>
    add_method(method)

  # save the random seed before fitting experiment
  seed <- sample.int(.Machine$integer.max, 1L)

  # with_seed temporarily sets seed and then reset it back where it was
  sequential <- withr::with_seed(
    seed,
    with_plan(
      # sequential plan results are expected
      future::sequential,
      fit_experiment(experiment,
                     n_reps = 10,
                     verbose = 0),
      split = TRUE
    )
  )

  reps <- 1

  if (getOption("simChef.debug", FALSE)) {
    # check that repeated runs on the same experiment don't blow up memory usage
    reps <- 3
  }

  ## # debugging
  ## print(capture.output(parallel_plan))

  parallel <- with_plan(
    # non-sequential plan results
    parallel_plan,
    replicate(reps, {
      # use the same starting seed as before
      withr::local_seed(seed)
      fit_experiment(experiment,
                     n_reps = 10,
                     verbose = 0)
    }, simplify = FALSE)
  )

  bad_dgp <- create_dgp(dgp_fun, n = 100, p = 3, error_prob = 0.1)

  experiment |>
    remove_dgp("dgp1") |>
    add_dgp(bad_dgp)

  err <- with_plan(
    # non-sequential plan results w/ errors
    parallel_plan,
    replicate(reps, {
      # use the same starting seed as before, but increased n_reps
      withr::local_seed(seed)
      tryCatch(
        fit_experiment(experiment,
                       n_reps = 50,
                       verbose = 0),
        simChef_error = identity
      )
    }, simplify = FALSE)
  )

  return(list(parallel, sequential, err))

}

# ---- begin tests

test_that("fit_experiment works properly with future::multicore", {
  # multicore plan isn't supported on windows
  skip_on_os("windows")

  results <- fit_experiment_fixture(future::multicore)
  sequential_results <- results[[2]]

  for (parallel_results in results[[1]]) {
    expect_identical(parallel_results, sequential_results)
  }

  for (err in results[[3]]) {

    # NOTE: When errors are returned, they can add quite a bit to the simulation
    # environment. This could just be because 'simChef_error' objects include
    # workers' copies of the simulation loop closure (and it's environment).
    # This needs more testing.

    expect_partial_results_and_errors(err)

  }

})

test_that("fit_experiment works properly with future::multisession", {
  results <- fit_experiment_fixture(future::multisession)
  sequential_results <- results[[2]]

  for (parallel_results in results[[1]]) {
    expect_identical(parallel_results, sequential_results)
  }

  for (err in results[[3]]) {

    # NOTE: When errors are returned, they can add quite a bit to the simulation
    # environment. This could just be because 'simChef_error' objects include
    # workers' copies of the simulation loop closure (and it's environment). For
    # `future::multisession`, the increase in memory usage seems to be similar
    # to that of `future::multicore`.

    expect_partial_results_and_errors(err)

  }

})

test_that("fit_experiment works properly with future::cluster", {
  results <- fit_experiment_fixture(future::cluster)
  sequential_results <- results[[2]]

  for (parallel_results in results[[1]]) {
    expect_identical(parallel_results, sequential_results)
  }

  for (err in results[[3]]) {

    # NOTE: When errors are returned, they can add quite a bit to the simulation
    # environment. This could just be because 'simChef_error' objects include
    # workers' copies of the simulation loop closure (and it's environment). For
    # `future::cluster`, the increase in memory usage seems to be similar to
    # that of `future::multicore`.

    expect_partial_results_and_errors(err)

  }

})

test_that("fit_experiment works properly with future.callr::callr", {
  results <- fit_experiment_fixture(future.callr::callr)
  sequential_results <- results[[2]]

  for (parallel_results in results[[1]]) {
    expect_identical(parallel_results, sequential_results)
  }

  for (err in results[[3]]) {

    # NOTE: When errors are returned, they can add quite a bit to the simulation
    # environment. This could just be because 'simChef_error' objects include
    # workers' copies of the simulation loop closure (and it's environment). For
    # `future.callr::callr`, the increase in memory usage seems to be similar to
    # that of `future::multicore`.

    expect_partial_results_and_errors(err)

  }

})
