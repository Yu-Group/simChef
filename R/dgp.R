#' A data-generating process
#'
#' @export
DGP <- R6::R6Class(
  classname = 'DGP',
  public = list(
    n_obs = NULL,
    dgp_fun = NULL,
    dgp_params = NULL,
    initialize = function(dgp_fun, ...) {
      self$dgp_fun <- dgp_fun
      self$dgp_params <- list(...)
    },
    generate = function(...) {
      if (identical(self$dgp_params, list())) {
        data_list <- self$dgp_fun()
      } else {
        data_list <- do.call(self$dgp_fun, self$dgp_params)
      }
      
      # check if data_list is a list; if not, coerce to list
      if (!inherits(data_list, "list")) {
        data_list <- list(data_list)
      }
      
      # get number of observations
      if (!is.null(nrow(data_list[[1]]))) {
        self$n_obs <- nrow(data_list[[1]])
      } else {
        self$n_obs <- length(data_list[[1]])
      }
      
      return(data_list)
    }
  )
)

#' @export
create_dgp <- function(dgp_fun=NULL, ...) {
  return(DGP$new(dgp_fun, ...))
}
