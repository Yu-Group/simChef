#' This function is passed to expect_snapshot() and scrubs stochastic lines
#' (e.g., the "time taken:" lines in verbose Experiment output)
#'
#' @param lines a character vector of output line see
#'  \code{testthat::expect_snapshot}
#' @return the character vector lines, scrubbed of stochastic output
#'
transform_fun <- function(lines) {
  sapply(lines, function(line) {
    gsub("time taken: [[:alnum:]]*.?[[:alnum:]]* ", "time taken: _x_ ", line)
  })
}

#' This function is passed to expect_snapshot() and scrubs stochastic lines
#' (i.e,. the temp file path) from the verbose of render_docs()
#'
#' @param lines a character vector of output line see
#'  \code{testthat::expect_snapshot}
#' @return the character vector lines, scrubbed of stochastic output
#'
remove_tempdir <- function(lines) {
  sapply(lines, function(line) {
    gsub(
      "Rendered document can be found at .*$",
      "Rendered document can be found at _x_",
      line
    )
  })
}

set_plan <- function(plan, ...) {
  # get the original plan so we can reset at the end
  old_plan <- future::plan()
  future::plan(plan, ...)
  return(old_plan)
}

reset_plan <- function(plan) {
  future::plan(plan)
}

# `with_plan(new, code, ...)` will temporarily set the parallel plan by calling
# `future::plan(new, ...)` and reset it as it was previously when `code`
# finishes
## with_plan <- local({
##   withr::local_package("withr")
##   with_(set_plan, reset_plan)
## })
with_plan <- withr::with_(set_plan, reset_plan)

# `local_plan(new, ...)` will temporarily set the parallel plan by calling
# `future::plan(new, ...)` and reset it as it was previously when the function /
# frame in which `local_plan()` was called exits
local_plan <- withr::local_(set_plan, reset_plan)

# needed so that the `with_*` and `local_*` work even when `withr` is not on the
# search path
defer <- withr::defer
