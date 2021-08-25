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
    fit = function(data_list, ...) {
      method_params <- self$method_params
      new_method_params <- list(...)
      if (length(new_method_params) > 0) {
        for (i in 1:length(new_method_params)) {
          method_params[[names(new_method_params)[i]]] <- new_method_params[[i]]
        }
      }
      
      if (identical(method_params, list())) {
        fit_results <- do.call(self$method_fun, data_list)
      } else {
        fit_results <- do.call(self$method_fun, c(data_list, method_params))
      }
      if (is.null(names(fit_results))) {
        names(fit_results) <- paste0("result", 1:length(fit_results))
      }
      return(tibble::as_tibble(fit_results))
    }
  )
)

#' @export
create_method <- function(method_fun, ...) {
  return(Method$new(method_fun, ...))
}
