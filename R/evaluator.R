#' An evaluator of an Experiment's results.
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',
  public = list(
    eval_fun = NULL,
    initialize = function(eval_fun, ...) {
      self$eval_fun <- eval_fun
    },
    evaluate = function(nested_results, ...) {
      return(eval_fun(nested_results$result_list, ...))
    }
  )
)

#' @export
create_evaluator <- function(eval_fun, ...) {
  return(Evaluator$new(eval_fun, ...))
}
