withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {

  # put all tests inside of this block to avoid leaving temporary files laying
  # around when tests fail (for some reason withr::local_tempdir() isn't
  # working)

  test_that("Experiment checkpointing works as expected", {

    # setup temporary state
    local_plan(future::sequential, split = TRUE)
    withr::local_options(list(simChef.debug = FALSE))

    dgp_fun <- function(n=100, rho=0.5, noise_level=1) {
      X <- data.frame(.n = n, .rho = rho, .noise_level = noise_level)
      return(list(X = X))
    }

    counter <- 0

    method_fun <- function(X, param1=1, param2=2, vec=c(1,2,3)) {
      if (param1 == 2 && param2 == 3 && 2 %in% vec) {
        # simulate an error due to conditions external to the method_fun
        counter <<- counter + 1
        if (counter > 6) {
          counter <<- 0
          stop("Unexpected error!")
        }
      }
      return(X)
    }

    experiment <- create_experiment(
      name = "checkpoint-exper"
    ) %>%
      add_dgp(create_dgp(dgp_fun, n = 10)) %>%
      add_method(create_method(method_fun)) %>%
      add_vary_across(
        .method = "method1",
        param1 = c(1, 2),
        param2 = c(3, 4),
        vec = list(c(1, 3, 4), 2:7)
      )

    err <- expect_error(
      experiment$fit(n_reps = 10, checkpoint_n_reps = 5, verbose = 0)
    )

    expect_partial_results_and_errors(err)

    fit_results <- get_cached_results(experiment, "fit")

    distinct_results <- fit_results %>%
      dplyr::select(-.rep) %>%
      dplyr::distinct()

    expect_equal(max(as.numeric(fit_results$.rep)), 5)
    expect_equal(nrow(fit_results), 40)

    vary_results <- mapply(list, distinct_results$param1, distinct_results$param2,
                           distinct_results$vec, SIMPLIFY = FALSE)
    expected_vary_results <- purrr::cross(
      list(c(1,2), c(3,4), list(c(1,3,4), 2:7))
    )

    expect_equal(vary_results, expected_vary_results)

    fit_results <- experiment$fit(n_reps = 10, checkpoint_n_reps = 5, verbose=0)

    distinct_results2 <- fit_results %>%
      dplyr::select(-.rep) %>%
      dplyr::distinct()

    expect_equal(max(as.numeric(fit_results$.rep)), 10)
    expect_equal(nrow(fit_results), 80)
    expect_equal(distinct_results, distinct_results2)

  }) # test_that("Experiment checkpointing works as expected", {

}) # withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {
