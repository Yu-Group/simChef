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
      result_list <- do.call(self$method_fun, c(data_list, self$method_params))
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
