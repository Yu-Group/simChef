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
    evaluate = function(results, vary_param = NULL, ...) {
      args_list <- list(results = results, vary_param = vary_param)
      if (!identical(self$eval_params, list())) {
        args_list <- c(args_list, self$eval_params)
      }
      eval_out <- R.utils::doCall(self$eval_fun, args = args_list)
      return(tibble::as_tibble(eval_out))
    }
  )
)

#' @export
create_evaluator <- function(eval_fun, ...) {
  return(Evaluator$new(eval_fun, ...))
}
