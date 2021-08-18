#' An evaluator of an Experiment's results.
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',
  public = list(
    eval_fun = NULL,
    eval_params = NULL,
    rmd_options = list(digits = 2, sigfig = FALSE,
                       options = list(scrollX = TRUE, scrollCollapse = TRUE)),
    initialize = function(eval_fun, rmd_options = list(), ...) {
      self$eval_fun <- eval_fun
      self$eval_params <- list(...)
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
    },
    evaluate = function(results, vary_param = NULL, ...) {
      args_list <- list(results = results, vary_param = vary_param)
      if (!identical(self$eval_params, list())) {
        always_args_list <- self$eval_params
      } else {
        always_args_list <- NULL
      }
      eval_out <- R.utils::doCall(self$eval_fun, 
                                  args = args_list,
                                  alwaysArgs = always_args_list)
      return(tibble::as_tibble(eval_out))
    }
  )
)

#' @export
create_evaluator <- function(eval_fun, ...) {
  return(Evaluator$new(eval_fun, ...))
}
