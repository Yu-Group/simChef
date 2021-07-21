#' A computational experiment.
#'
#' @export
Experiment <- R6::R6Class(
  classname = 'Experiment',
  private = list(
    .add_obj = function(field_name, obj, obj_name, ...) {
      list_name <- paste0(field_name, "_list")
      if (is.null(obj_name)) {
        # give a default name like "dgp1"
        obj_name <- paste0(field_name, length(self[[list_name]]) + 1)
      }
      if (!is.null(self[[list_name]][[obj_name]])) {
        stop(
          sprintf("The name '%s' already exists in the %s list. ",
                  obj_name, field_name),
          sprintf("Use update_%s instead.", field_name),
          call. = FALSE
        )
      } else {
        self[[list_name]][[obj_name]] <- obj
      }
    },
    .update_obj = function(field_name, obj, obj_name, ...) {
      list_name <- paste0(field_name, "_list")
      if (!obj_name %in% names(self[[list_name]])) {
        stop(
          sprintf("The name '%s' isn't in the %s list. ",
                  obj_name, field_name),
          sprintf("Use add_%s instead.", field_name),
          call. = FALSE
        )
      }
      self[[list_name]][[obj_name]] <- NULL
      private$.add_obj(field_name, obj, obj_name)
    },
    .throw_empty_list_error = function(field_name, action_name="run") {
      stop(
        sprintf("No %s has been added yet. ", field_name),
        sprintf("Use add_%s before trying to %s the experiment.",
                field_name, action_name),
        call. = FALSE
      )
    },
    .make_evaluator_name = function(dgp_name = NULL, method_name = NULL) {
      if (!is.null(dgp_name) && !is.null(method_name)) {
        name <- paste0(dgp_name, "_", method_name)
      } else if (is.null(dgp_name) && is.null(method_name)) {
        name <- NULL
      } else {
        name <- paste0(dgp_name, method_name)
      }
      return(name)
    }
  ),
  public = list(
    n_reps = NULL,
    dgp_list = list(),
    method_list = list(),
    evaluator_list = list(),
    initialize = function(n_reps, dgp_list=list(), method_list=list(),
                          evaluator_list=list(), ...) {
      # TODO: check that n_reps has length 1 or is the same length as dgp_list
      self$n_reps <- n_reps
      self$dgp_list <- dgp_list
      self$method_list <- method_list
      self$evaluator_list <- evaluator_list
    },
    run = function(parallel_strategy = c("reps", "dgps", "methods", "dgps+methods"),
                   trial_run = FALSE, ...) {
      parallel_strategy <- match.arg(parallel_strategy)
      dgp_list <- self$dgp_list
      method_list <- self$method_list
      if (length(dgp_list) == 0) {
        private$.throw_empty_list_error("dgp")
      }
      if (length(method_list) == 0) {
        private$.throw_empty_list_error("method")
      }
      n_reps <- self$n_reps
      if (trial_run) {
        n_reps <- 1
      }
      if (parallel_strategy == "reps") {
        results <- purrr::map_dfr(dgp_list, function(dgp) {
          purrr::map_dfr(method_list, function(method) {
            replicates <- future.apply::future_replicate(n_reps, {
              datasets <- dgp$generate()
              return(method$run(datasets))
            }, simplify=FALSE)
            dplyr::bind_rows(replicates)
          }, .id = "method")
        }, .id = "dgp")
      } else {
        results <- tibble::tibble()
      }
      return(results)
    },
    evaluate = function(results, ...) {
      evaluator_list <- self$evaluator_list
      if (length(evaluator_list) == 0) {
        private$.throw_empty_list_error("evaluator", "evaluate")
      }
      eval_results <- purrr::map(evaluator_list, function(evaluator) {
        evaluator$evaluate(results)
      })
      return(eval_results)
    },
    add_dgp = function(dgp, name=NULL, ...) {
      private$.add_obj("dgp", dgp, name)
    },
    update_dgp = function(dgp, name, ...) {
      private$.update_obj("dgp", dgp, name)
    },
    add_method = function(method, name=NULL, ...) {
      private$.add_obj("method", method, name)
    },
    update_method = function(method, name, ...) {
      private$.update_obj("method", method, name)
    },
    add_evaluator = function(evaluator, dgp_name = NULL, method_name = NULL,
                             ...) {
      name <- private$.make_evaluator_name(dgp_name, method_name)
      private$.add_obj("evaluator", evaluator, name)
    },
    update_evaluator = function(evaluator, name = NULL, dgp_name = NULL, method_name = NULL,
                                ...) {
      if (is.null(name)) {
        name <- private$.make_evaluator_name(dgp_name, method_name)
      }
      if (is.null(name)) {
        # this will throw an error, which is what we want
        private$.update_obj("evaluator", evaluator)
      } else {
        private$.update_obj("evaluator", evaluator, name)
      }
    }
  )
)

#' @export
create_experiment <- function(n_reps, ...) {
  return(Experiment$new(n_reps, ...))
}

#' @export
run_experiment <- function(experiment, ...) {
  return(experiment$run(...))
}

#' @export
add_dgp <- function(experiment, dgp, name=NULL, ...) {
  experiment$add_dgp(dgp, ...)
  return(experiment)
}

#' @export
update_dgp <- function(experiment, dgp, name, ...) {
  experiment$update_dgp(dgp, name, ...)
  return(experiment)
}

#' @export
add_method <- function(experiment, method, name=NULL, ...) {
  experiment$add_method(method, name, ...)
  return(experiment)
}

#' @export
update_method <- function(experiment, method, name, ...) {
  experiment$update_method(method, name, ...)
  return(experiment)
}

#' @export
add_evaluator <- function(experiment, evaluator, dgp_name=NULL,
                          method_name=NULL, ...) {
  experiment$add_evaluator(evaluator, dgp_name, method_name, ...)
  return(experiment)
}

#' @export
update_evaluator <- function(experiment, evaluator, name=NULL, dgp_name=NULL,
                             method_name=NULL, ...) {
  experiment$update_evaluator(evaluator, name=NULL, dgp_name=NULL,
                              method_name=NULL, ...)
  return(experiment)
}
