# ---- setup

n_cores <- future::availableCores(methods = "system")

# get the original plan so we can reset at the end
old_plan <- future::plan()

# defer will set the plan as it was when the test suite finishes or fails
withr::defer(future::plan(old_plan))

# sets up experiment and returns sequential and parallel results
# should be called from within a function (e.g. test_that())
fit_experiment_fixture <- function(plan,
                                   workers,
                                   env = parent.frame()) {

  dgp_fun <- function(n, p) {

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

  experiment <- create_experiment() %>%
    add_dgp(dgp) %>%
    add_method(method)

  # sequential plan results are expected
  future::plan(future::sequential)

  # save the random seed before fitting experiment
  seed <- sample.int(.Machine$integer.max, 1L)

  # with_seed temporarily sets seed and then reset it back where it was
  sequential <- withr::with_seed(
    seed,
    fit_experiment(experiment, n_reps = 10) # get sequential results
  )

  # set the parallel plan
  future::plan(plan, workers = workers)

  parallel <- withr::with_seed(
    seed, # use the same starting seed as before
    fit_experiment(experiment, n_reps = 10) # get parallel results
  )

  return(list(parallel, sequential))

}

# ---- begin tests

test_that("fit_experiment works properly with future::multicore", {

  # multicore plan isn't supported on windows
  skip_on_os("windows")

  results <- fit_experiment_fixture(future::multicore, n_cores)
  expect_identical(results[[1]], results[[2]])

})

test_that("fit_experiment works properly with future::multisession", {

  results <- fit_experiment_fixture(future::multisession, n_cores)
  expect_identical(results[[1]], results[[2]])

})

test_that("fit_experiment works properly with future::cluster", {

  results <- fit_experiment_fixture(future::cluster, n_cores)
  expect_identical(results[[1]], results[[2]])

})

skip("test-experiment.R not currently working with future::multisession")

# run test-experiment.R with multisession plan
future::plan(future::multisession)
source("test-experiment.R")
