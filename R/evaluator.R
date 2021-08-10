#' An evaluator of an Experiment's results.
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',
  public = list(
    eval_fun = NULL,
    eval_params = NULL,
    initialize = function(eval_fun, ...) {
      self$eval_fun <- eval_fun
      self$eval_params <- list(...)
    },
    evaluate = function(results, ...) {
      if (identical(self$eval_params, list())) {
        eval_out <- self$eval_fun(results = results)
      } else {
        eval_out <- do.call(self$eval_fun, 
                            c(list(results = results), self$eval_params))
      }
      return(tibble::as_tibble(eval_out))
    }
  )
)

#' @export
create_evaluator <- function(eval_fun, ...) {
  return(Evaluator$new(eval_fun, ...))
}
