# TODO: add covr
## covr: skip=all
.onLoad <- function(libname, pkgname) {

  op <- options()
  op.simChef <- list(
    simChef.debug = FALSE,
    simChef.plot_theme = "default"
    # TODO: simChef.verbosity
  )
  toset <- !(names(op.simChef) %in% names(op))
  if(any(toset)) options(op.simChef[toset])

  invisible()
}
