#' @title Create a simulation project
#'
#' @description `init_sim_project()` initializes a directory for your simulation
#'   study. It wraps around \code{usethis::create_project()}, as well as
#'   \code{usethis::use_git()} and \code{renv::init()}.
#'
#' @param path Character string, specifying the path for your simulation
#'   directory.
#' @param init_git Logical, indicating whether to intialize your
#'   simulation directory as a git repository. Defaults to `TRUE`.
#' @param init_renv Logical, stating whether to initialize `renv` with
#'   `init()` from the `renv` package. Defaults to `TRUE`.
#' @param init_tests Logical, indicating whether to generate sub-directories
#'   for organizing unit tests. Defaults to `TRUE`.
#' @param init_dirs Character vector, specifying additional directories to
#'   create in the simulation project. Defaults to `NULL`.
#'
#' @examples
#' \dontrun{
#' # create template directory for simulation project
#' init_sim_project("path/to/sim")}
#'
#' @export
init_sim_project <- function(
  path,
  init_git = TRUE,
  init_renv = TRUE,
  init_tests = TRUE,
  init_dirs = NULL
) {
  rlang::check_installed(
    "usethis",
    reason = "to create a simulation project with `init_sim_project()`"
  )

  ## ensure arguments are appropriate
  if (!is.character(path)) {
    stop("path must be a character string.")
  }
  if (!is.logical(init_git)) {
    stop("init_git must be a logical.")
  }
  if (!is.logical(init_renv)) {
    stop("init_renv must be a logical.")
  }
  if (!is.logical(init_tests)) {
    stop("init_tests must be a logical.")
  }

  ## don't overwrite existing project
  if (dir.exists(paths = path)) {
    stop("This directory already exists. Try another.")
  }

  ## intialize a project
  usethis::create_project(
    path = path,
    open = FALSE
  )

  ## state the necessary directories
  dirs <- c(
    "R",
    file.path("R", "dgp"),
    file.path("R", "method"),
    file.path("R", "eval"),
    file.path("R", "viz"),
    "meals",
    "results"
  )

  ## add the optional directories
  if (init_tests) {
    dirs <- c(
      dirs,
      "tests",
      file.path("tests", "testthat"),
      file.path("tests", "testthat", "dgp-tests"),
      file.path("tests", "testthat", "method-tests"),
      file.path("tests", "testthat", "eval-tests"),
      file.path("tests", "testthat", "viz-tests")
    )
  }

  # add additional directories
  if (!is.null(init_dirs)) {
    dirs <- c(dirs, init_dirs)
  }

  ## create the specified directories
  sapply(
    dirs,
    function(dir) {
      dir.create(path = file.path(path, dir), showWarnings = FALSE)
    }
  )

  ## state the necessary files
  files <- c("README.md", file.path("meals", "meal.R"))
  if (init_tests) {
    files <- c(files, file.path("tests", "testthat.R"))
  }

  ## create the specified files
  file.create(file.path(path, files))

  ## write the header of R/meal.R
  meal_file <- file(file.path(path, "meals", "meal.R"))
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
  readme_file <- file(file.path(path, "README.md"))
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
  if (init_tests) {
    testthat_file <- file(file.path(path, "tests", "testthat.R"))
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
    usethis::proj_set(path = path)

    ## initialize a git repository if asked
    if (init_git) usethis::use_git()

    ## intialize renv if desired
    if (init_renv) {
      rlang::check_installed(
        "renv",
        reason = "to initialize renv with `init_sim_project()`"
      )
      renv::init(path, bare = TRUE)
    }

    usethis::proj_activate(path = path)
  }
}
