#' Run Tests
#'
#' @description \code{run_tests()} runs the tests in the `tests/` directory.
#'   This function only works if the simulation study is set up as an R project
#'   and if it is run when this R project is active.
#'
#' @importFrom usethis proj_get
#'
#' @export
run_tests <- function() {

  # construct path to testthat.R
  sim_project_path <- usethis::proj_get()
  testthat_path <- paste(sim_project_path, "tests/testthat.R", sep = "/")

  # attempt to run tests
  if (file.exists(testthat_path)) {
    source(testthat_path)
  } else {
    stop(
      "Cannot find ", testthat_path, ". Is this simulation your active project?"
    )
  }

  invisible()
}


#' Test simChef Ingredients
#'
#' @description \code{test_sim_dir()} wraps around
#'   \code{\link[testthat]{test_dir}()} to run all dgp-, method-, evaluator-,
#'   and visualizer-related tests.
#'
#' @details This function only works if the simulation study is set up as an R
#'   project and if it is run when this R project is active. Additionally, tests
#'   must be stored in sub-directories whose names match those suggesting in the
#'   "Setting Up Your Simulation" vignette. These details are conveniently taken
#'   care of when a simulation study is set up using \code{\link{create_sim}()}.
#'
#' @importFrom usethis proj_get ui_line
#' @importFrom assertthat assert_that
#' @importFrom testthat test_dir
#'
#' @export
test_sim_dir <- function() {

  # set up paths
  path_to_tests <- paste0(usethis::proj_get(), "/tests/testthat")

  # make sure that the testthat directory exists
  assertthat::assert_that(
    dir.exists(path_to_tests),
     msg = paste0(
       "Cannot find ", path_to_tests,
       ". Is this simulation your active project?"
     )
  )

  # make sure that subdirectories exist
  assertthat::assert_that(
    all(dir.exists(paste0(
      path_to_tests,
      c("/dgp-tests", "/method-tests", "/eval-tests", "/viz-tests")
    ))),
    msg = paste0("No test subdirectories found; dgp-, method-, evaluator- and ",
                 "visualizer-related tests must be saved in",
                 "tests/testthat/dgp-tests, tests/testthat/metod-tests, ",
                 "tests/testthat/eval-tests and tests/testthat/viz-tests, ",
                 "respectively.")
  )

  # test dgp-related functions
  dgp_test_path <- paste0(path_to_tests, "/dgp-tests")
  if (length(list.files(dgp_test_path)) > 0) {
    cat("\n")
    usethis::ui_line("Running dgp-related tests:")
    testthat::test_dir(dgp_test_path)
  }

  # test method-related functions
  method_test_path <- paste0(path_to_tests, "/method-tests")
  if (length(list.files(method_test_path)) > 0) {
    cat("\n")
    usethis::ui_line("Running method-related tests:")
    testthat::test_dir(method_test_path)
  }

  # test evaluator-related functions
  eval_test_path <- paste0(path_to_tests, "/eval-tests")
  if (length(list.files(eval_test_path)) > 0) {
    cat("\n")
    usethis::ui_line("Running evaluator-related tests:")
    testthat::test_dir(eval_test_path)
  }

  # test visualizer-related functions
  viz_test_path <- paste0(path_to_tests, "/viz-tests")
  if (length(list.files(viz_test_path)) > 0) {
    cat("\n")
    usethis::ui_line("Running visualizer-related tests:")
    testthat::test_dir(viz_test_path)
  }

  invisible()
}
