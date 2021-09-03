#' \code{R6} class representing a data-generating process.
#'
#' @docType class
#'
#' @description A data-generating process which will be used in the 
#'   \code{Experiment} to **generate** data.
#'
#' @export
DGP <- R6::R6Class(
  classname = 'DGP',
  public = list(
    #' @field dgp_fun The data-generating process function.
    dgp_fun = NULL,
    #' @field dgp_params (Named) list of parameters to input into the 
    #'   data-generating process function.
    dgp_params = NULL,
    #' @description Create a new \code{DGP} (data-generating process).
    #'
    #' @param dgp_fun The data-generating process function.
    #' @param ... Arguments to pass into \code{dgp_fun()}.
    #'
    #' @return A new \code{DGP} object.
    initialize = function(dgp_fun, ...) {
      self$dgp_fun <- dgp_fun
      self$dgp_params <- list(...)
    },
    #' @description Generate data from a \code{DGP} with the provided \code{DGP}
    #'   parameters.
    #' 
    #' @param ... Arguments to pass into \code{dgp_fun()} that will overwrite 
    #'   the initialized \code{DGP} parameters. If no additional arguments are 
    #'   provided, data will be generated using \code{dgp_fun()} with the 
    #'   parameters that were set when \code{DGP$new()} was called.
    #' 
    #' @return Result of \code{dgp_fun()}.
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

#' Create a new \code{DGP} (data-generating process).
#'
#' @name create_dgp
#' 
#' @param dgp_fun The data-generating process function.
#' @param ... Arguments to pass into \code{dgp_fun()}.
#'
#' @return A new instance of \code{DGP}.
#'
#' @export
create_dgp <- function(dgp_fun=NULL, ...) {
  return(DGP$new(dgp_fun, ...))
}
