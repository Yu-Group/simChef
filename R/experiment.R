#' A computational experiment.
#'
#' @export
Experiment <- R6::R6Class(
  classname = 'Experiment',
  private = list(
    .save_dir = NULL,
    .parent = NULL,
    .child_list = list(),
    .dgp_list = list(),
    .method_list = list(),
    .evaluator_list = list(),
    .plotter_list = list(),
    .vary_across = list(),
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
    .throw_empty_list_error = function(field_name, action_name = "run") {
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
    .save_results = function(results, save_filename) {
      if (identical(private$.vary_across, list())) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- file.path(private$.save_dir, 
                              paste0(private$.vary_across$dgp,
                                     private$.vary_across$method),
                              paste("Varying", private$.vary_across$param_name))
      }
      save_file <- file.path(save_dir, save_filename)
      if (!dir.exists(dirname(save_file))) {
        dir.create(dirname(save_file), recursive = TRUE)
      }
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      saveRDS(results, save_file)
    }
  ),
  public = list(
    n_reps = NULL,
    name = NULL,
    initialize = function(n_reps, name = "experiment",
                          dgp_list = list(), method_list = list(),
                          evaluator_list = list(), plotter_list = list(), 
                          parent = NULL, save_dir = NULL, ...) {
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
      if (is.null(save_dir)) {
        if (is.null(parent)) {
          save_dir <- file.path("results", name)
        } else {
          save_dir <- file.path(parent$get_save_dir(), name)
        }
      }
      private$.save_dir <- R.utils::getAbsolutePath(save_dir)
    },
    run = function(parallel_strategy = c("reps", "dgps", "methods", "dgps+methods"),
                   trial_run = FALSE, save = FALSE, ...) {
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
      
      if (identical(private$.vary_across, list())) {
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
      } else {
        # TODO: add parallelization
        # TODO: tweak to work for varying multiple parameters simultaneously
        # Q: do we want to vary parameters across multiple dgps/methods?
        param_name <- private$.vary_across$param_name
        param_values <- private$.vary_across$param_values
        if (!is.null(private$.vary_across$dgp)) {
          obj_name <- private$.vary_across$dgp
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
        } else if (!is.null(private$.vary_across$method)) {
          obj_name <- private$.vary_across$method
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
      }
      
      if (save) {
        private$.save_results(results, save_filename = "run_results.rds")
      }
      return(results)
    },
    evaluate = function(results, save = FALSE, ...) {
      evaluator_list <- private$.get_obj_list("evaluator")
      if (length(evaluator_list) == 0) {
        private$.throw_empty_list_error("evaluator", "evaluate")
      }
      eval_results <- purrr::map(evaluator_list, function(evaluator) {
        evaluator$evaluate(results = results, 
                           vary_param = private$.vary_across$param_name)
      })

      if (save) {
        private$.save_results(eval_results, save_filename = "eval_results.rds")
      }
      
      return(eval_results)
    },
    plot = function(results = NULL, eval_results = NULL, save = FALSE, ...) {
      plotter_list <- private$.get_obj_list("plotter", "get_plots")
      if (length(plotter_list) == 0) {
        private$.throw_empty_list_error("plotter", "plot results from")
      }
      plot_results <- purrr::map(plotter_list, function(plotter) {
        plotter$plot(results = results, eval_results = eval_results,
                     vary_param = private$.vary_across$param_name)
      })
      
      if (save) {
        private$.save_results(plot_results, save_filename = "plot_results.rds")
      }
      
      return(plot_results)
    },
    create_doc_template = function(...) {
      save_dir <- private$.save_dir
      if (!dir.exists(file.path(save_dir, "docs"))) {
        dir.create(file.path(save_dir, "docs"), recursive = TRUE)
      }
      
      if (!file.exists(file.path(save_dir, "docs", "objectives.md"))) {
        fname <- file.path(save_dir, "docs", "objectives.md")
        write.csv(NULL, file = fname, quote = F)
      }
      
      descendants <- map(list.dirs(save_dir),
                         function(d) {
                           if (file.exists(file.path(d, "experiment.rds"))) {
                             return(readRDS(file.path(d, "experiment.rds")))
                           } else {
                             return(NULL)
                           }
                         })
      descendants[sapply(descendants, is.null)] <- NULL
      
      fields <- c("dgp", "method", "evaluator", "plot")
      for (field in fields) {
        obj_names <- purrr::map(descendants, 
                                ~names(.x[[paste0("get_", field, "s")]]())) %>%
          purrr::reduce(c) %>%
          unique()
        for (obj_name in obj_names) {
          fname <- file.path(save_dir, "docs", paste0(field, "s"),
                             paste0(obj_name, ".md"))
          if (!file.exists(fname)) {
            if (!dir.exists(dirname(fname))) {
              dir.create(dirname(fname), recursive = TRUE)
            }
            write.csv(NULL, file = fname, quote = F)
          }
        }
      }
      return(invisible(self))
    },
    create_rmd = function(...) {
      self$create_doc_template()
      input_fname <- system.file("rmd", "results.Rmd", package = packageName())
      output_fname <- file.path(private$.save_dir, paste0(self$name, ".html"))
      params_list <- list(sim_name = self$name, sim_path = private$.save_dir)
      rmarkdown::render(input = input_fname, 
                        params = params_list,
                        output_file = output_fname)
      output_fname <- str_replace_all(output_fname, " ", "\\\\ ")
      system(paste("open", output_fname))
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
    get_descendants = function(include_self=FALSE) {
      children <- self$get_children()
      if (include_self) {
        descendants <- list(self)
        names(descendants) <- self$name
        descendants <- c(descendants, children)
      } else {
        descendants <- children
      }
      for (child in children) {
        descendants <- c(descendants, child$get_descendants())
      }
      return(descendants)
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
    add_evaluator = function(evaluator, name=NULL, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.add_obj("evaluator", evaluator, name)
    },
    update_evaluator = function(evaluator, name, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.update_obj("evaluator", evaluator, name)
    },
    get_evaluators = function() {
      return(private$.get_obj_list("evaluator"))
    },
    add_plot = function(plotter, name=NULL, ...) {
      private$.check_obj(plotter, "Plotter")
      private$.add_obj("plotter", plotter, name, getter_name = "get_plots")
    },
    update_plot = function(plotter, name, ...) {
      private$.check_obj(plotter, "Plotter")
      private$.update_obj("plotter", plotter, name)
    },
    get_plots = function() {
      return(private$.get_obj_list("plotter", "get_plots"))
    },
    vary_across = function(dgp = NULL, method = NULL,
                           param_name, param_values) {
      
      dgp_list <- private$.get_obj_list("dgp")
      method_list <- private$.get_obj_list("method")
      
      # TODO: better error checking for dgp/method input or create new class
      # TODO: check for match with param_name
      if (is.null(dgp) & is.null(method)) {
        stop("Must specify either dgp or method.")
      } else if (!is.null(dgp)) {
        if (inherits(dgp, "DGP")) {
          obj_name <- sapply(dgp_list, function(x) identical(x, dgp)) %>%
            which() %>%
            names()
        } else if (dgp %in% names(dgp_list)) {
          obj_name <- dgp
        } else {
          stop("dgp must either be a DGP object or the name of a dgp in the current experiment.")
        }
        private$.vary_across$dgp <- obj_name
        private$.vary_across$method <- NULL
      } else if (!is.null(method)) {
        if (inherits(method, "Method")) {
          obj_name <- sapply(method_list, function(x) identical(x, method)) %>%
            which() %>%
            names()
        } else if (method %in% names(method_list)) {
          obj_name <- method
        } else {
          stop("method must either be a Method object or the name of a method in the current experiment.")
        }
        private$.vary_across$method <- obj_name
        private$.vary_across$dgp <- NULL
      }
      
      # add error checking: param_name should match something from dgp
      private$.vary_across$param_name <- param_name
      private$.vary_across$param_values <- param_values
    },
    get_vary_across = function() {
      return(private$.vary_across)
    },
    reset_vary_across = function() {
      private$.vary_across <- list()
    },
    get_save_dir = function() {
      return(private$.save_dir)
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
evaluate_experiment <- function(experiment, ...) {
  return(experiment$evaluate(...))
}

#' @export
plot_experiment <- function(experiment, ...) {
  return(experiment$plot(...))
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
add_evaluator <- function(experiment, evaluator, name=NULL, ...) {
  experiment$add_evaluator(evaluator, name, ...)
  return(experiment)
}

#' @export
update_evaluator <- function(experiment, evaluator, name, ...) {
  experiment$update_evaluator(evaluator, name, ...)
  return(experiment)
}

#' @export
add_plot <- function(experiment, plotter, name=NULL, ...) {
  experiment$add_plot(plotter, name, ...)
  return(experiment)
}

#' @export
update_plot <- function(experiment, plotter, name, ...) {
  experiment$update_plot(plotter, name, ...)
  return(experiment)
}

#' @export
vary_across <- function(experiment, dgp = NULL, method = NULL,
                        param_name, param_values) {
  experiment$vary_across(dgp = dgp, method = method, 
                         param_name = param_name, param_values = param_values)
  return(experiment)
}

#' @export
create_rmd <- function(experiment, experiment_dirname) {
  if (missing(experiment) & missing(experiment_dirname)) {
    stop("Must provide argument for one of experiment or experiment_dirname")
  }
  if (missing(experiment)) {
    experiment <- readRDS(file.path(experiment_dirname, "experiment.rds"))
  }
  experiment$create_doc_template()
  experiment$create_rmd()
}
