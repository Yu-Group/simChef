.package <- new.env()

# TODO: add covr
## covr: skip=all
.onLoad <- function(libname, pkgname) {
  .package[["future_worker_error"]] <- FALSE

  op <- options()
  op.simChef <- list(
    simChef.debug = FALSE
    # TODO: simChef.verbosity
  )
  toset <- !(names(op.simChef) %in% names(op))
  if(any(toset)) options(op.simChef[toset])

  invisible()
}
