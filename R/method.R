#' A method to evaluate.
#'
#' @export
Method <- R6::R6Class(
  classname = 'Method',
  public = list(
    method_fun = NULL,
    method_params = NULL,
    initialize = function(method_fun, ...) {
      self$method_fun <- method_fun
      self$method_params <- list(...)
    },
    run = function(data_list, ...) {
      method_params <- self$method_params
      new_method_params <- list(...)
      if (length(new_method_params) > 0) {
        for (i in 1:length(new_method_params)) {
          method_params[[names(new_method_params)[i]]] <- new_method_params[[i]]
        }
      }
      
      if (identical(method_params, list())) {
        result_list <- do.call(self$method_fun, data_list)
      } else {
        result_list <- do.call(self$method_fun, c(data_list, method_params))
      }
      if (is.null(names(result_list))) {
        names(result_list) <- paste0("result", 1:length(result_list))
      }
      result_tib <- tibble::as_tibble(result_list)
      return(result_tib)
    }
  )
)

#' @export
create_method <- function(method_fun, ...) {
  return(Method$new(method_fun, ...))
}
