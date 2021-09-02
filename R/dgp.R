#' A data-generating process
#'
#' @export
DGP <- R6::R6Class(
  classname = 'DGP',
  public = list(
    name = NULL,
    dgp_fun = NULL,
    dgp_params = NULL,
    initialize = function(dgp_fun, name = NULL, ...) {
      self$name <- name
      self$dgp_fun <- dgp_fun
      self$dgp_params <- list(...)
    },
    generate = function(...) {
      dgp_params <- self$dgp_params
      new_dgp_params <- list(...)
      if (length(new_dgp_params) > 0) {
        for (i in 1:length(new_dgp_params)) {
          dgp_params[[names(new_dgp_params)[i]]] <- new_dgp_params[[i]]
        }
      }
      
      if (identical(dgp_params, list())) {
        data_list <- self$dgp_fun()
      } else {
        data_list <- do.call(self$dgp_fun, dgp_params)
      }
      
      # check if data_list is a list; if not, coerce to list
      if (!inherits(data_list, "list")) {
        data_list <- list(data_list)
      }
      
      return(data_list)
    }
  )
)

#' @export
create_dgp <- function(dgp_fun, ...) {
  return(DGP$new(dgp_fun, ...))
}
