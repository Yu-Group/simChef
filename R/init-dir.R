#' @title Create a simulation project
#'
#' @description `create_sim()` initializes a directory for your simulation
#'   study. It wraps around [usethis::create_project()], as well as
#'   [usethis::use_git()] and
#'   [renv::init()].
#'
#' @param path A `character` specifying the path for your simulation
#'   directory.
#' @param init_git A `logical` indicating whether to intialize your
#'   simulation directory as a git repository.
#' @param init_renv A `logical` stating whether to initialize `renv` with
#'   [renv::init()]. Defaults to `FALSE`.
#' @param tests A `logical` indicating whether to generate sub-directories
#'   for organizing unit tests. Defaults to `TRUE`.
#' @param hpc A `logical` indicating whether to create sub-directories for
#'   organizing files related to high-power computing environments. Defaults to
#'   `FALSE`.
#'
#' @examples
#' \dontrun{
#' # create template directory for simulation project
#' create_sim("path/to/sim")}
#'
#' @export
create_sim <- function(
  path,
  init_git = TRUE,
  init_renv = FALSE,
  tests = TRUE,
  hpc = FALSE
) {

  ## ensure arguments are appropriate
  assertthat::assert_that(is.character(path))
  assertthat::assert_that(is.logical(init_git))
  assertthat::assert_that(is.logical(init_renv))
  assertthat::assert_that(is.logical(tests))
  assertthat::assert_that(is.logical(hpc))

  ## don't overwrite existing project
  assertthat::assert_that(
    !dir.exists(paths = path),
    msg = "This directory already exists. Try another."
  )

  ## intialize a project
  usethis::create_project(
    path = path,
    open = FALSE
  )

  ## state the necessary directories
  dirs <- c(
    "R", "R/dgp", "R/method", "R/eval", "R/viz", "results"
  )

  ## add the optional directories
  if (tests) {
    dirs <- c(
      dirs, "tests", "tests/testthat", "tests/testthat/dgp-tests",
      "tests/testthat/method-tests", "tests/testthat/eval-tests",
      "tests/testthat/viz-tests"
    )
  }
  if (hpc) {
    dirs <- c(dirs, "scripts", "logs")
  }

  ## create the specified directories
  sapply(
    dirs,
    function(dir) {
      dir.create(path = paste(path, dir, sep = "/"), showWarnings = FALSE)
    }
  )

  ## state the necessary files
  files <- c("README.md", "R/meal.R")
  if (tests) {
    files <- c(files, "tests/testthat.R")
  }

  ## create the specified files
  file.create(paste(path, files, sep = "/"))

  ## write the header of R/meal.R
  meal_file <- file(paste(path, "R/meal.R", sep = "/"))
  writeLines(
    c(
      "# load required libraries",
      "library(simChef)",
      "# other libraries ...",
      "",
      "# get simulatin'!"
    ),
    con = meal_file
  )
  close(meal_file)

  ## write minimal template for README.md
  readme_file <- file(paste(path, "README.md", sep = "/"))
  writeLines(
    c(
      "# Simulation Study Title",
      "",
      "## Description",
      "",
      "What's this simulation study about?"
    ),
    con = readme_file
  )
  close(readme_file)

  ## write tests/testthat.R if necessary
  if (tests) {
    testthat_file <- file(paste(path, "tests/testthat.R", sep = "/"))
    writeLines(
      c(
        "library(simChef)",
        "library(testthat)",
        "# load required packages here",
        "",
        "load_all()",
        "test_sim_dir()"
      ),
      con = testthat_file
    )
    close(testthat_file)
  }

  ## activate the project
  if (rlang::is_interactive()) {
    usethis::proj_activate(path = path)

    ## initialize a git repository if asked
    if (init_git) usethis::use_git()

    ## intialize renv if desired
    if (init_renv) renv::init()
  }
}
