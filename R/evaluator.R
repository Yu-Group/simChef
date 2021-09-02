#' An evaluator of an Experiment's results.
#'
#' @export
Evaluator <- R6::R6Class(
  classname = 'Evaluator',
  public = list(
    name = NULL,
    eval_fun = NULL,
    eval_params = NULL,
    rmd_options = list(digits = 2, sigfig = FALSE,
                       options = list(scrollX = TRUE, scrollCollapse = TRUE)),
    initialize = function(eval_fun, name = NULL, rmd_options = list(), ...) {
      self$name <- name
      self$eval_fun <- eval_fun
      self$eval_params <- list(...)
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
    },
    evaluate = function(fit_results, vary_params = NULL, ...) {
      args_list <- list(fit_results = fit_results,
                        vary_params = vary_params)
      if (!identical(self$eval_params, list())) {
        always_args_list <- self$eval_params
      } else {
        always_args_list <- NULL
      }
      eval_results <- R.utils::doCall(self$eval_fun,
                                      args = args_list,
                                      alwaysArgs = always_args_list)
      return(list_to_tibble(eval_results))
    }
  )
)

#' @export
create_evaluator <- function(eval_fun, ...) {
  return(Evaluator$new(eval_fun, ...))
}
