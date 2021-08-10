#' A computational experiment.
#'
#' @export
Experiment <- R6::R6Class(
  classname = 'Experiment',
  private = list(
    .parent = NULL,
    .child_list = list(),
    .dgp_list = list(),
    .method_list = list(),
    .evaluator_list = list(),
    .plotter_list = list(),
    .add_obj = function(field_name, obj, obj_name, ...) {
      # TODO: check if obj is already in list by another name
      obj_list <- private$.get_obj_list(field_name, ...)
      if (is.null(obj_name)) {
        # give a default name like "dgp1"
        obj_name <- paste0(field_name, length(obj_list) + 1)
      }
      if (!is.null(obj_list[[obj_name]])) {
        stop(
          sprintf("The name '%s' already exists in the %s list. ",
                  obj_name, field_name),
          sprintf("Use update_%s instead.", field_name),
          call. = FALSE
        )
      } else {
        list_name <- paste0(".", field_name, "_list")
        private[[list_name]][[obj_name]] <- obj
      }
    },
    .update_obj = function(field_name, obj, obj_name, ...) {
      obj_list <- private$.get_obj_list(field_name, ...)
      if (!obj_name %in% names(obj_list)) {
        stop(
          sprintf("The name '%s' isn't in the %s list. ",
                  obj_name, field_name),
          sprintf("Use add_%s instead.", field_name),
          call. = FALSE
        )
      }
      list_name <- paste0(".", field_name, "_list")
      private[[list_name]][[obj_name]] <- obj
    },
    .throw_empty_list_error = function(field_name, action_name="run") {
      stop(
        sprintf("No %s has been added yet. ", field_name),
        sprintf("Use add_%s before trying to %s the experiment.",
                field_name, action_name),
        call. = FALSE
      )
    },
    .get_obj_list = function(field_name, getter_name=NULL) {
      list_name <- paste0(".", field_name, "_list")
      obj_list <- private[[list_name]]
      if (!is.null(private$.parent)) {
        if (is.null(getter_name)) {
          getter_name <- paste0("get_", field_name, "s")
        }
        ancestor_list <- private$.parent[[getter_name]]()
        # filter out ancestor objs that are present in this Experiment's list
        filter <- !sapply(names(ancestor_list), `%in%`, names(obj_list))
        obj_list <- c(ancestor_list[filter], obj_list)
      }
      return(obj_list)
    },
    .check_obj = function(obj, expected_class) {
      if (!inherits(obj, expected_class)) {
        err_msg <- sprintf("%s must be an instance of pcs.sim.pkg::%s",
                           as.character(substitute(obj)), expected_class)
        stop(err_msg, call.=FALSE)
      }
    },
    .check_obj_list = function(obj_list, expected_class) {
      lapply(obj_list, function(obj) {
        if (!inherits(obj, expected_class)) {
          stop(
            sprintf("Expected all objects in %s_list ",
                    tolower(expected_class)),
            sprintf("to be instances of %s, ", expected_class),
            sprintf("but found an object with the following class(es): %s",
                    paste0(class(obj), collapse=", ")),
            call. = FALSE
          )
        }
      })
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
        save_dir <- dirname(save_filename)
        save_filename <- basename(save_filename)
        if (identical(save_dir, ".")) {
          save_dir <- NULL
        }
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
      attr(results, "saved_to") <- file.path(save_dir, save_filename)
      return(results)
    }
  ),
  public = list(
    n_reps = NULL,
    name = NULL,
    saved_results = list(),
    initialize = function(n_reps, name = NULL,
                          dgp_list=list(), method_list=list(),
                          evaluator_list=list(), plotter_list=list(), 
                          parent=NULL, ...) {
      # TODO: check that n_reps has length 1 or is the same length as dgp_list
      private$.check_obj_list(dgp_list, "DGP")
      private$.check_obj_list(method_list, "Method")
      private$.check_obj_list(evaluator_list, "Evaluator")
      private$.check_obj_list(plotter_list, "Plotter")
      private$.dgp_list <- dgp_list
      private$.method_list <- method_list
      private$.evaluator_list <- evaluator_list
      private$.plotter_list <- plotter_list
      self$n_reps <- n_reps
      self$name <- name
      if (!is.null(parent)) {
        self$set_parent(parent)
      }
    },
    run = function(parallel_strategy = c("reps", "dgps", "methods", "dgps+methods"),
                   trial_run = FALSE,
                   save = FALSE, save_dir = NULL, save_filename = NULL, ...) {
      parallel_strategy <- match.arg(parallel_strategy)
      dgp_list <- private$.get_obj_list("dgp")
      method_list <- private$.get_obj_list("method")
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
        saveRDS(self, file.path(dirname(attr(results, "saved_to")),
                                "experiment.rds"))
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
      dgp_list <- private$.get_obj_list("dgp")
      method_list <- private$.get_obj_list("dgp")
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
        } else if (dgp %in% names(dgp_list)) {
          obj_name <- dgp
        } else {
          stop("dgp must either be a DGP object or the name of a dgp in the current experiment.")
        }
      } else if (!missing(method)) {
        if (inherits(method, "Method")) {
          obj_name <- sapply(method_list, function(x) identical(x, method)) %>%
            which() %>%
            names()
        } else if (method %in% names(method_list)) {
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
        saveRDS(self, file.path(dirname(attr(results, "saved_to")),
                                "experiment.rds"))
      }

      return(results)
    },
    evaluate = function(results,
                        save = FALSE, save_dir = NULL, save_filename = NULL,
                        ...) {
      evaluator_list <- private$.get_obj_list("evaluator")
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
        saveRDS(self, file.path(dirname(attr(results, "saved_to")),
                                "experiment.rds"))
      }

      return(eval_results)
    },
    plot = function(results = NULL, eval_results = NULL,
                    save = FALSE, save_dir = NULL, save_filename = NULL,
                    ...) {
      plotter_list <- private$.get_obj_list("plotter")
      if (length(plotter_list) == 0) {
        private$.throw_empty_list_error("plotter", "plot results from")
      }
      plot_results <- purrr::map(plotter_list, function(plotter) {
        plotter$plot(results, eval_results)
      })
      
      return(plot_results)
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
    has_parent = function() {
      return(!is.null(private$.parent))
    },
    get_parent = function() {
      return(private$.parent)
    },
    set_parent = function(parent) {
      if (!inherits(parent, "Experiment")) {
        stop("parent must be an instance of pcs.sim.pkg::Experiment",
             call.=FALSE)
      }
      private$.parent <- parent
      parent$add_child(self, self$name)
    },
    add_child = function(child, name=NULL, ...) {
      private$.check_obj(child, "Experiment")
      private$.add_obj("child", child, name, getter_name="get_children")
    },
    get_children = function() {
      return(private$.child_list)
    },
    add_dgp = function(dgp, name=NULL, ...) {
      private$.check_obj(dgp, "DGP")
      private$.add_obj("dgp", dgp, name)
    },
    update_dgp = function(dgp, name, ...) {
      private$.check_obj(dgp, "DGP")
      private$.update_obj("dgp", dgp, name)
    },
    get_dgps = function() {
      return(private$.get_obj_list("dgp"))
    },
    add_method = function(method, name=NULL, ...) {
      private$.check_obj(method, "Method")
      private$.add_obj("method", method, name)
    },
    update_method = function(method, name, ...) {
      private$.check_obj(method, "Method")
      private$.update_obj("method", method, name)
    },
    get_methods = function() {
      return(private$.get_obj_list("method"))
    },
    add_evaluator = function(evaluator, dgp_name = NULL, method_name = NULL,
                             ...) {
      private$.check_obj(evaluator, "Evaluator")
      name <- private$.make_evaluator_name(dgp_name, method_name)
      private$.add_obj("evaluator", evaluator, name)
    },
    update_evaluator = function(evaluator, name = NULL, dgp_name = NULL, method_name = NULL,
                                ...) {
      private$.check_obj(evaluator, "Evaluator")
      if (is.null(name)) {
        name <- private$.make_evaluator_name(dgp_name, method_name)
      }
      if (is.null(name)) {
        # this will throw an error, which is what we want
        private$.update_obj("evaluator", evaluator)
      } else {
        private$.update_obj("evaluator", evaluator, name)
      }
    },
    get_evaluators = function() {
      return(private$.get_obj_list("evaluator"))
    },
    add_plot = function(plotter, name=NULL, ...) {
      private$.check_obj(plotter, "Plotter")
      private$.add_obj("plotter", plotter, name)
    },
    update_plot = function(plotter, name, ...) {
      private$.check_obj(plotter, "Plotter")
      private$.update_obj("plotter", plotter, name)
    },
    get_plots = function() {
      return(private$.get_obj_list("plotter"))
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
  experiment$add_dgp(dgp, name, ...)
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

#' @export
add_plot <- function(experiment, plotter, name=NULL, ...) {
  experiment$add_plot(plotter, name, ...)
  return(experiment)
}

#' @export
update_plotter <- function(experiment, plotter, name, ...) {
  experiment$update_plot(plotter, name, ...)
  return(experiment)
}
