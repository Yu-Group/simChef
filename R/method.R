#' A method to evaluate.
#'
#' @export
Method <- R6::R6Class(
  classname = 'Method',
  public = list(
    method_fun = NULL,
    initialize = function(method_fun, ...) {
      self$method_fun <- method_fun
    },
    run = function(nested_data, ...) {
      result_list <- method_fun(nested_data$data_list, ...)
      if (is.null(names(result_list))) {
        names(result_list) <- paste0("result", 1:length(result_list))
      }
      result_tib <- tibble::as_tibble(result_list)
      result_tib$n_obs <- nested_data$n_obs[1]
      return(result_tib)
    }
  )
)

#' @export
create_method <- function(method_fun, ...) {
  return(Method$new(method_fun, ...))
}
