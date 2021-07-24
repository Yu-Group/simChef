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
    },
    .save_results = function(results, save_dir = NULL, save_filename = NULL,
                             save_filename_null = "run_results.rds") {
      if (is.null(save_filename)) {
        save_filename <- save_filename_null
      }
      if (is.null(save_dir)) {
        if (is.null(self$name)) {
          save_dir <- file.path("results", "experiment")
        } else {
          save_dir <- file.path("results", self$name)
        }
      }
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = T)
      }
      saveRDS(results, file.path(save_dir, save_filename))
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      attr(results, "saved_to") <- file.path(save_dir, save_filename)
      return(results)
    }
  ),
  public = list(
    n_reps = NULL,
    name = NULL,
    dgp_list = list(),
    method_list = list(),
    evaluator_list = list(),
    saved_results = list(),
    initialize = function(n_reps, name = NULL,
                          dgp_list=list(), method_list=list(),
                          evaluator_list=list(), ...) {
      # TODO: check that n_reps has length 1 or is the same length as dgp_list
      self$n_reps <- n_reps
      self$name <- name
      self$dgp_list <- dgp_list
      self$method_list <- method_list
      self$evaluator_list <- evaluator_list
    },
    run = function(parallel_strategy = c("reps", "dgps", "methods", "dgps+methods"),
                   trial_run = FALSE, 
                   save = FALSE, save_dir = NULL, save_filename = NULL, ...) {
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
      
      if (save) {
        results <- private$.save_results(results = results, 
                                         save_dir = save_dir, 
                                         save_filename = save_filename)
        self$saved_results[[".base"]] <- list(run_results = attr(results,
                                                                 "saved_to"))
      }
      
      return(results)
    },
    run_across = function(dgp, method, param_name, param_values,
                          parallel_strategy = c("reps", "dgps", "methods",
                                                "dgps+methods"),
                          trial_run = FALSE,
                          save = FALSE, save_dir = NULL, save_filename = NULL,
                          ...) {
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
      # TODO: better error checking for dgp/method input or create new class
      # TODO: check for match with param_name
      if (missing(dgp) & missing(method)) {
        stop("Must specify either dgp or method.")
      } else if (!missing(dgp)) {
        if (inherits(dgp, "DGP")) {
          obj_name <- sapply(dgp_list, function(x) identical(x, dgp)) %>%
            which() %>%
            names()
        } else if (dgp %in% names(self$dgp_list)) {
          obj_name <- dgp
        } else {
          stop("dgp must either be a DGP object or the name of a dgp in the current experiment.")
        }
      } else if (!missing(method)) {
        if (inherits(method, "Method")) {
          obj_name <- sapply(method_list, function(x) identical(x, method)) %>%
            which() %>%
            names()
        } else if (method %in% names(self$method_list)) {
          obj_name <- method
        } else {
          stop("method must either be a Method object or the name of a method in the current experiment.")
        }
      }
      
      # TODO: add parallelization
      # TODO: tweak to work for varying multiple parameters simultaneously
      # Q: do we want to enable varying parameters across multiple dgps/methods?
      if (!missing(dgp)) {
        results <- future.apply::future_replicate(n_reps, {
          purrr::map_dfr(param_values, function(param_value) {
            input_param <- list(param = param_value) %>%
              setNames(param_name)
            datasets <- do.call(dgp_list[[obj_name]]$generate, input_param)
            purrr::map_dfr(method_list, function(method) {
              method$run(datasets)
            }, .id = "method")
          }, .id = param_name) %>%
            dplyr::mutate(dgp = obj_name) %>%
            dplyr::relocate(dgp, .before = method)
        }, simplify = FALSE) %>%
          dplyr::bind_rows(.id = "rep")
      } else if (!missing(method)) {
        results <- future.apply::future_replicate(n_reps, {
          purrr::map_dfr(dgp_list, function(dgp) {
            datasets <- dgp$generate()
            purrr::map_dfr(param_values, function(param_value) {
              input_param <- list(param = param_value) %>%
                setNames(param_name)
              do.call(method_list[[obj_name]]$run,
                      c(list(datasets), input_param))
            }, .id = param_name) %>%
              dplyr::mutate(method = obj_name)
          }, .id = "dgp") %>%
            dplyr::relocate(method, .after = dgp)
        }, simplify = FALSE) %>%
          dplyr::bind_rows(.id = "rep")
      }
      
      if (is.null(names(param_values))) {
        names(param_values) <- 1:length(param_values)
        results[[param_name]] <- param_values[results[[param_name]]]
        attr(results[[param_name]], "names") <- NULL 
      }
      
      # add attributes to keep track of the simulation call
      attr(results, "type") <- obj_name
      attr(results, "param_name") <- param_name
      attr(results, "param_values") <- param_values
      
      if (save) {
        save_filename_null <- paste0("varying_", obj_name, "_", param_name, 
                                     ".rds")
        results <- private$.save_results(
          results = results, 
          save_dir = save_dir, 
          save_filename = save_filename,
          save_filename_null = save_filename_null
        )
        if (!(obj_name %in% names(self$saved_results))) {
          self$saved_results[[obj_name]] <- list()
        }
        self$saved_results[[obj_name]][[param_name]] <- list(
          run_results = attr(results, "saved_to")
        )
      }
      
      return(results)
    },
    evaluate = function(results, 
                        save = FALSE, save_dir = NULL, save_filename = NULL, 
                        ...) {
      evaluator_list <- self$evaluator_list
      if (length(evaluator_list) == 0) {
        private$.throw_empty_list_error("evaluator", "evaluate")
      }
      eval_results <- purrr::map(evaluator_list, function(evaluator) {
        evaluator$evaluate(results)
      })
      
      if (save) {
        if (is.null(attr(results, "saved_to"))) {
          save_filename_null <- "eval_results.rds"
        } else {
          save_filename_null <- str_remove(attr(results, "saved_to"),
                                           ".rds$") %>%
            paste0("_eval.rds")
        }
        eval_results <- private$.save_results(
          results = eval_results, 
          save_dir = save_dir, 
          save_filename = save_filename,
          save_filename_null = save_filename_null
        )
        
        if (is.null(attr(results, "type"))) {
          if (!(".base" %in% names(self$saved_results))) {
            self$saved_results[[".base"]] <- list()
          }
          self$saved_results[[".base"]][["eval_results"]] <- attr(eval_results,
                                                                  "saved_to")
        } else {
          obj_name <- attr(results, "type")
          param_name <- attr(results, "param_name")
          if (!(obj_name %in% names(self$saved_results))) {
            self$saved_results[[obj_name]] <- list()
          }
          if (!(param_name %in% names(self$saved_results[[obj_name]]))) {
            self$saved_results[[obj_name]][[param_name]] <- list()
          }
          self$saved_results[[obj_name]][[param_name]][["eval_results"]] <- 
            attr(eval_results, "saved_to")
        }
      }
      
      return(eval_results)
    },
    create_doc_template = function(save_dir = NULL, ...) {
      if (is.null(save_dir)) {
        if (is.null(self$name)) {
          save_dir <- file.path("results", "experiment", "docs")
        } else {
          save_dir <- file.path("results", self$name, "docs")
        }
      }
      
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      
      if (!file.exists(file.path(save_dir, "objectives.md"))) {
        write.csv(NULL, file = file.path(save_dir, "objectives.md"), quote = F)
      }
      # TODO: add plotters or viz .md 
      fields <- c("dgp", "method", "evaluator")
      for (field in fields) {
        for (obj_name in names(self[[paste0(field, "_list")]])) {
          fname <- file.path(save_dir, paste0(obj_name, ".md"))
          if (!file.exists(fname)) {
            write.csv(NULL, file = fname, quote = F)
          }
        }
      }
      self$saved_results[[".docs"]] <- list(dir = file.path(save_dir, "docs"))
      return(invisible(self))
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
