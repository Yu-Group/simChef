#' \code{R6} class representing a simulation experiment.
#'
#' @docType class
#'
#' @description A simulation experiment with any number of \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Plotters}.
#'
#' @details When run, an \code{Experiment} seamlessly combines \code{DGPs} and \code{Methods},
#'   computing results in parallel. Those results can then be evaluated using
#'   \code{Evaluators} and plotted using \code{Plotters}.
#'
#' @export
Experiment <- R6::R6Class(
  classname = 'Experiment',
  private = list(
    .save_dir = NULL,
    .dgp_list = list(),
    .method_list = list(),
    .evaluator_list = list(),
    .plotter_list = list(),
    .vary_across_list = list(
      dgp = list(),
      method = list()
    ),
    .future.globals = TRUE,
    .future.packages = NULL,
    .add_obj = function(field_name, obj, obj_name, ...) {
      # TODO: check if obj is already in list by another name
      obj_list <- private$.get_obj_list(field_name, ...)
      if (is.null(obj_name)) {
        if (is.null(obj$name) || nchar(obj$name) == 0) {
          # give a default name like "dgp1"
          obj_name <- paste0(field_name, length(obj_list) + 1)
        } else {
          obj_name <- obj$name
        }
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
    .remove_obj = function(field_name, obj_name, ...) {
      obj_list <- private$.get_obj_list(field_name, ...)
      if (is.null(obj_list[[obj_name]])) {
        stop(
          sprintf("Cannot remove '%s'. ", obj_name),
          sprintf("The name '%s' does not exist in the %s list. ",
                  obj_name, field_name),
          call. = FALSE
        )
      } else {
        list_name <- paste0(".", field_name, "_list")
        private[[list_name]][[obj_name]] <- NULL
      }
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
      return(obj_list)
    },
    .check_obj = function(obj, expected_class) {
      if (!inherits(obj, expected_class)) {
        err_msg <- sprintf("%s must be an instance of pcs.sim.pkg::%s",
                           as.character(substitute(obj)), expected_class)
        stop(err_msg, call.=FALSE)
      }
    },
    .add_obj_list = function(obj_list, expected_class) {
      if (length(obj_list) > 0) {
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
        obj_list_names <- names(obj_list)
        if (is.null(obj_list_names)) {
          obj_list_names <- paste0(tolower(expected_class), 1:length(obj_list))
        } else {
          empty_names <- sapply(obj_list_names, function(obj_name) {
            nchar(obj_name) == 0
          })
          obj_list_names[empty_names] <- paste0(
            tolower(expected_class), 1:sum(empty_names)
          )
        }
        names(obj_list) <- obj_list_names
        private[[paste0(".", tolower(expected_class), "_list")]] <- obj_list
      }
    },
    .check_vary_across = function(dgp, method, ...) {
      dots_list <- list(...)
      if (missing(dgp) && missing(method)) {
        stop("Must specify either dgp or method.")
      } else if ((!missing(dgp)) + (!missing(method)) != 1) {
        stop("Must specify one of dgp or method, but not both")
      } else if (!missing(dgp)) {
        obj <- dgp
        field_name <- "dgp"
        class_name <- "DGP"
      } else if (!missing(method)) {
        obj <- method
        field_name <- "method"
        class_name <- "Method"
      }
      obj_list <- private$.get_obj_list(field_name)
      if (inherits(obj, class_name)) {
        obj_name <- sapply(obj_list,
                           function(x) check_equal(x, obj)) %>%
          which() %>%
          names()
      } else if (obj %in% names(obj_list)) {
        obj_name <- obj
      } else {
        stop(
          sprintf(
            "%s must either be a %s object or the name of a %s in the current Experiment.",
            field_name, class_name, class_name
          ),
          call. = FALSE
        )
      }
      obj_fun_args <- formalArgs(obj_list[[obj_name]][[paste0(field_name, "_fun")]])
      dots_list_valid_names <- names(dots_list) %in% obj_fun_args
      if (!all(dots_list_valid_names) && (!("..." %in% obj_fun_args))) {
        invalid_names <- names(dots_list)[!dots_list_valid_names]
        invalid_names <- paste0(invalid_names, collapse=", ")
        stop(
          sprintf("%s: not valid argument(s) to %s's %s_fun",
                  invalid_names, obj_name, field_name),
          call. = FALSE
        )
      }

      return(list(dots_list = dots_list,
                  field_name = field_name,
                  obj_name = obj_name))
    },
    .is_vary_across = function() {
      identical(private$.vary_across_list, list(dgp = list(), method = list()))
    },
    .get_vary_params = function() {
      param_names <- purrr::map(private$.vary_across_list,
                                ~purrr::map(.x, names) %>%
                                  purrr::reduce(c)) %>%
        purrr::reduce(c)
      return(param_names)
    },
    .combine_vary_params = function(field_name = c("dgp", "method")) {
      field_name <- match.arg(field_name)
      obj_list <- private$.get_obj_list(field_name)
      obj_names <- names(obj_list)
      param_list <- purrr::map(obj_names, function(obj_name) {
        obj_params <- private$.vary_across_list[[field_name]][[obj_name]]
        if (is.null(obj_params)) {
          obj_params <- list()
        }
        obj_params <- c(obj_name, obj_params)
        names(obj_params)[1] <- paste0(field_name, "_name")
        param_grid <- expand.grid(obj_params)
        return(apply(param_grid, 1, as.list, simplify=FALSE))
      }) %>% purrr::reduce(c)
      return(param_list)
    },
    .save_results = function(results, results_type = c("fit", "eval", "plot"),
                             verbose = 1) {
      results_type <- match.arg(results_type)
      if (verbose >= 1) {
        message(sprintf("Saving %s results...", results_type))
        start_time <- Sys.time()
      }
      if (!private$.is_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        obj_names <- purrr::map(private$.vary_across_list, names) %>%
          purrr::reduce(c) %>%
          paste(collapse = "-")
        param_names <- private$.get_vary_params() %>%
          paste(collapse = "-")
        save_dir <- file.path(private$.save_dir, obj_names,
                              paste("Varying", param_names))
      }
      save_file <- file.path(save_dir, paste0(results_type, "_results.rds"))
      if (!dir.exists(dirname(save_file))) {
        dir.create(dirname(save_file), recursive = TRUE)
      }
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      saveRDS(results, save_file)
      if (verbose >= 1) {
        message(sprintf("%s results saved | time taken: %f seconds",
                        R.utils::capitalize(results_type),
                        difftime(Sys.time(), start_time, units = "secs")))
        message("==============================")
      }
    },
    .get_cached_results = function(results_type = c("fit", "eval", "plot"),
                                   verbose = 1) {
      results_type <- match.arg(results_type)
      if (verbose >= 1) {
        message(sprintf("Reading in cached %s results...", results_type))
      }
      if (!private$.is_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        obj_names <- purrr::map(private$.vary_across_list, names) %>%
          purrr::reduce(c) %>%
          paste(collapse = "-")
        param_names <- param_names <- private$.get_vary_params() %>%
          paste(collapse = "-")
        save_dir <- file.path(private$.save_dir, obj_names,
                              paste("Varying", param_names))
      }
      save_file <- file.path(save_dir, paste0(results_type, "_results.rds"))
      if (file.exists(save_file)) {
        if (verbose >= 1) {
          message("==============================")
        }
        return(readRDS(save_file))
      } else {
        if (verbose >= 1) {
          message(sprintf("Cannot find cached %s results.", results_type))
        }
        return(NULL)
      }
    },
    deep_clone = function(name, value) {
      if (is.list(value) && length(value) > 0 && inherits(value[[1]], "R6")) {
        lapply(value, function(v) v$clone(deep = TRUE))
      } else {
        value
      }
    }
  ),
  public = list(
    #' @field name The name of the \code{Experiment}.
    name = NULL,
    #' @description Create a new \code{Experiment}.
    #'
    #' @param name The name of the \code{Experiment}.
    #' @param dgp_list An optional list of \code{DGP} objects.
    #' @param method_list An optional list of \code{Method} objects.
    #' @param evaluator_list An optional list of \code{Evaluator} objects.
    #' @param plotter_list An option list of \code{Plotter} objects.
    #' @param future.globals Character vector of names in the global environment
    #'   to pass to parallel workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for a specific run of the
    #'   experiment, use the same argument in \code{Experiment$run}.
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for a specific run of the
    #'   experiment, use the same argument in \code{Experiment$run}.
    #' @Param clone_from An optional \code{Experiment} object to use as a base
    #'   for this one.
    #' @param save_dir An optional directory in which to save the experiment's
    #'   results. If \code{NULL}, results are saved at
    #'   ./results/\{Experiment$name\}
    #' @param ... Not used.
    #'
    #' @return A new \code{Experiment} object.
    initialize = function(name = "experiment",
                          dgp_list = list(), method_list = list(),
                          evaluator_list = list(), plotter_list = list(),
                          future.globals = TRUE, future.packages = NULL,
                          clone_from = NULL, save_dir = NULL, ...) {
      if (!is.null(clone_from)) {
        private$.check_obj(clone_from, "Experiment")
        clone <- clone_from$clone(deep = TRUE)
        dgp_list <- c(clone$get_dgps(), dgp_list)
        method_list <- c(clone$get_methods(), method_list)
        evaluator_list <- c(clone$get_evaluators(), evaluator_list)
        plotter_list <- c(clone$get_plotters(), plotter_list)
        if (is.null(save_dir)) {
          save_dir <- clone$get_save_dir()
        }
      }
      private$.add_obj_list(dgp_list, "DGP")
      private$.add_obj_list(method_list, "Method")
      private$.add_obj_list(evaluator_list, "Evaluator")
      private$.add_obj_list(plotter_list, "Plotter")
      private$.future.globals <- future.globals
      private$.future.packages <- future.packages
      self$name <- name
      if (is.null(save_dir)) {
        save_dir <- file.path("results", name)
      }
      private$.save_dir <- R.utils::getAbsolutePath(save_dir)
    },
    #' @description Run the entire simulation experiment pipeline (fitting,
    #'   evaluating, and plotting)
    #'
    #' @param n_reps The number of replicates of the \code{Experiment} for this
    #'   run.
    #' @param parallel_strategy A vector with some combination of "reps",
    #'   "dgps", or "methods". Determines how computation will be distributed
    #'   across available resources.
    #' @param future.globals Character vector of names in the global environment
    #'   to pass to parallel workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for all runs of the
    #'   experiment, use the same argument during initialization.
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for all runs of the
    #'   experiment, use the same argument during initialization.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'   results. If cached results cannot be found, run experiment anyways.
    #'   Can also be a vector with some combination of "fit",
    #'   "eval", or "plot" to use their respective cached results.
    #' @param save If \code{TRUE}, save results to disk. Can also be a vector
    #'   with some combination of "fit", "eval", or "plot" to save to disk.
    #' @param verbose Level of verboseness. Default is 1, which prints out
    #'   messages after major checkpoints in the experiment. If 0, no messages
    #'   are printed.
    #' @param ... Not used.
    #'
    #' @return A list of results from the simulation experiment
    #' \describe{
    #' \item{fit_results}{A tibble containing results from the \code{fit}
    #'   method. In addition to results columns, has columns named 'rep', 'dgp',
    #'   'method', and the \code{vary_across} parameter name if applicable.}
    #' \item{eval_results}{A list of tibbles containing results from the
    #'   \code{evaluate} method, which evaluates each \code{Evaluator} in
    #'   the \code{Experiment}. Length of list is equivalent to the number of
    #'   \code{Evaluators}.}
    #' \item{plot_results}{A list of tibbles containing results from the
    #'   \code{plot} method, which plots each \code{Plotter} in
    #'   the \code{Experiment}. Length of list is equivalent to the number of
    #'   \code{Plotters}.}
    #' }
    run = function(n_reps = 1, parallel_strategy = c("reps", "dgps", "methods"),
                   future.globals = NULL, future.packages = NULL,
                   use_cached = FALSE, save = FALSE, verbose = 1, ...) {
      if (!is.logical(use_cached)) {
        use_cached <- c("fit", "eval", "plot") %in% use_cached
      } else {
        if (length(use_cached) > 1) {
          warning("The input use_cached is a logical vector of length > 1. ",
                  "Only the first element of use_cached is used.")
        }
        use_cached <- rep(use_cached[1], 3)
      }
      if (!is.logical(save)) {
        save <- c("fit", "eval", "plot") %in% save
      } else {
        if (length(save) > 1) {
          warning("The input save is a logical vector of length > 1. ",
                  "Only the first element of save is used.")
        }
        save <- rep(save[1], 3)
      }

      fit_results <- self$fit(n_reps, parallel_strategy = parallel_strategy,
                              future.globals = future.globals,
                              future.packages = future.packages,
                              use_cached = use_cached[1], save = save[1],
                              verbose = verbose)
      eval_results <- self$evaluate(fit_results = fit_results,
                                    use_cached = use_cached[2], save = save[2],
                                    verbose = verbose)
      plot_results <- self$plot(fit_results = fit_results,
                                eval_results = eval_results,
                                use_cached = use_cached[3], save = save[3],
                                verbose = verbose)
      return(list(fit_results = fit_results,
                  eval_results = eval_results,
                  plot_results = plot_results))
    },
    #' @description Generate data from each of the \code{DGPs} in the
    #'   \code{Experiment}.
    #'
    #' @param n_reps The number of datasets to generate per \code{DGP}.
    #' @param ... Not used.
    #'
    #' @return A list of length equal to the number of \code{DGPs} in the
    #'   \code{Experiment}. Each element in the list is a list of \code{n_reps}
    #'   datasets generated by the given \code{DGP}.
    generate_data = function(n_reps = 1, ...) {
      # TODO: generate data that was used in run() or fit() (e.g., w/ same seed)
      dgp_list <- private$.get_obj_list("dgp")
      if (length(dgp_list) == 0) {
        private$.throw_empty_list_error("dgp", "generate data from")
      }
      if (is.null(private$.vary_across$dgp)) {
        dgp_results <- purrr::map(dgp_list, function(dgp) {
          replicates <- replicate(n_reps, {
            return(dgp$generate())
          }, simplify = FALSE)
        })
      } else {
        param_name <- private$.vary_across$param_name
        param_values <- private$.vary_across$param_values
        obj_name <- private$.vary_across$dgp
        dgp_results <- purrr::map(dgp_list[obj_name], function(dgp) {
          purrr::map(param_values, function(param_value) {
            input_param <- list(param = param_value) %>%
              setNames(param_name)
            replicates <- replicate(n_reps, {
              return(do.call(dgp$generate, input_param))
            }, simplify = FALSE)
          })
        })
        if (is.null(names(param_values))) {
          if (!is.list(param_values)) {
            names(dgp_results[[1]]) <- paste0(param_name, param_values)
          }
        }
      }
      return(dgp_results)
    },
    #' @description Fit all \code{Methods} in the \code{Experiment} and return
    #'   results.
    #'
    #' @param n_reps The number of replicates to run.
    #' @param parallel_strategy A vector with some combination of "reps",
    #'   "dgps", or "methods". Determines how computation will be distributed
    #'   across available resources.
    #' @param future.globals Character vector of names in the global environment
    #'   to pass to parallel workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for all runs of the
    #'   experiment, use the same argument during initialization.
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for all runs of the
    #'   experiment, use the same argument during initialization.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'   results. If cached results cannot be found, fit experiment anyways.
    #' @param save If \code{TRUE}, save results to disk.
    #' @param verbose Level of verboseness. Default is 1, which prints out
    #'   messages after major checkpoints in the experiment. If 0, no messages
    #'   are printed.
    #' @param ... Not used.
    #'
    #' @return A tibble containing the results from fitting all \code{Methods}
    #'   across all \code{DGPs} for \code{n_reps} repetitions. In addition to
    #'   results columns, has columns named 'rep', 'dgp', 'method', and the
    #'   \code{vary_across} parameter name if applicable.
    fit = function(n_reps = 1, parallel_strategy = c("reps", "dgps", "methods"),
                   future.globals = NULL, future.packages = NULL,
                   use_cached = FALSE, save = FALSE, verbose = 1, ...) {
      if (use_cached) {
        results <- private$.get_cached_results("fit", verbose = verbose)
        if (!is.null(results)) {
          return(results)
        }
      }

      if (verbose >= 1) {
        message(sprintf("Fitting %s...", self$name))
        start_time <- Sys.time()
      }
      parallel_strategy <- unique(parallel_strategy)
      if (length(parallel_strategy) == 0) {
        parallel_strategy <- "reps"
      } else {
        parallel_strategy <- sapply(
          parallel_strategy, match.arg, choices=c("reps", "dgps", "methods")
        )
      }
      strategy_string <- NULL
      if ("reps" %in% parallel_strategy) {
        strategy_string <- "reps"
      }
      if ("dgps" %in% parallel_strategy) {
        strategy_string <- paste0(c(strategy_string, "dgps"), collapse="+")
      }
      if ("methods" %in% parallel_strategy) {
        strategy_string <- paste0(c(strategy_string, "methods"), collapse="+")
      }
      parallel_strategy <- strategy_string
      dgp_list <- private$.get_obj_list("dgp")
      method_list <- private$.get_obj_list("method")
      if (length(dgp_list) == 0) {
        private$.throw_empty_list_error("dgp", "generate data from")
      }
      if (length(method_list) == 0) {
        private$.throw_empty_list_error("method", "fit methods in")
      }

      if (is.null(future.packages)) {
        future.packages <- private$.future.packages
      }
      if (is.null(future.globals)) {
        future.globals <- private$.future.globals
      }

      dgp_params_list <- private$.combine_vary_params("dgp")
      method_params_list <- private$.combine_vary_params("method")

      fit_results <- switch(
        parallel_strategy,
        "reps" = {
          results <- future.apply::future_replicate(n_reps, {
            purrr::map_dfr(dgp_params_list, function(dgp_params) {
              dgp_name <- dgp_params$dgp_name
              dgp_params$dgp_name <- NULL
              data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
              purrr::map_dfr(method_params_list, function(method_params) {
                method_name <- method_params$method_name
                param_df <- list_to_tibble_row(
                  c(dgp_name = dgp_name, dgp_params, method_params)
                )
                method_params$method_name <- NULL
                method_params$data_list <- data_list
                result <- do.call(method_list[[method_name]]$fit, method_params)
                return(result %>% tibble::add_column(param_df, .before=1))
              })
            })
          }, simplify=FALSE,
          future.globals = future.globals,
          future.packages = future.packages)
          dplyr::bind_rows(results, .id = "rep")
        },
        "dgps" = {
          results <- future.apply::future_lapply(
            dgp_params_list, function(dgp_params) {
              reps <- replicate(n_reps, {
                dgp_name <- dgp_params$dgp_name
                dgp_params$dgp_name <- NULL
                data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
                purrr::map_dfr(method_params_list, function(method_params) {
                  method_name <- method_params$method_name
                  param_df <- list_to_tibble_row(
                    c(dgp_name = dgp_name, dgp_params, method_params)
                  )
                  method_params$method_name <- NULL
                  method_params$data_list <- data_list
                  result <- do.call(method_list[[method_name]]$fit, method_params)
                  return(result %>% tibble::add_column(param_df, .before=1))
                })
              }, simplify=FALSE)
              dplyr::bind_rows(reps, .id = "rep")
            }, future.seed = TRUE,
            future.globals = future.globals,
            future.packages = future.packages)
          dplyr::bind_rows(results)
        },
        "methods" = {
          results <- future.apply::future_lapply(
            method_params_list, function(method_params) {
              reps <- replicate(n_reps, {
                purrr::map_dfr(dgp_params_list, function(dgp_params) {
                  dgp_name <- dgp_params$dgp_name
                  dgp_params$dgp_name <- NULL
                  data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
                  method_name <- method_params$method_name
                  param_df <- list_to_tibble_row(
                    c(dgp_name = dgp_name, dgp_params, method_params)
                  )
                  method_params$method_name <- NULL
                  method_params$data_list <- data_list
                  result <- do.call(method_list[[method_name]]$fit, method_params)
                  return(result %>% tibble::add_column(param_df, .before=1))
                })
              }, simplify=FALSE)
              dplyr::bind_rows(reps, .id = "rep")
            }, future.seed = TRUE,
            future.globals = future.globals,
            future.packages = future.packages)
          dplyr::bind_rows(results)
        },
        "reps+dgps" = {
          n_dgps <- length(dgp_params_list)
          dgp_params_list <- rep(dgp_params_list, times = n_reps)
          results <- future.apply::future_lapply(
            dgp_params_list,
            function(dgp_params) {
              dgp_name <- dgp_params$dgp_name
              dgp_params$dgp_name <- NULL
              data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
              purrr::map_dfr(method_params_list, function(method_params) {
                method_name <- method_params$method_name
                param_df <- list_to_tibble_row(
                  c(dgp_name = dgp_name, dgp_params, method_params)
                )
                method_params$method_name <- NULL
                method_params$data_list <- data_list
                result <- do.call(method_list[[method_name]]$fit, method_params)
                return(result %>% tibble::add_column(param_df, .before=1))
              })
            }, future.seed = TRUE,
            future.globals = future.globals,
            future.packages = future.packages
          )
          results <- dplyr::bind_rows(results)
          results$rep <- rep(1:n_reps, each = n_dgps*length(method_params_list))
          return(results)
        },
        "reps+methods" = {
          n_methods <- length(method_params_list)
          method_params_list <- rep(method_params_list, times = n_reps)
          results <- future.apply::future_lapply(
            method_params_list,
            function(method_params) {
              purrr::map_dfr(dgp_params_list, function(dgp_params) {
                dgp_name <- dgp_params$dgp_name
                dgp_params$dgp_name <- NULL
                data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
                method_name <- method_params$method_name
                param_df <- list_to_tibble_row(
                  c(dgp_name = dgp_name, dgp_params, method_params)
                )
                method_params$method_name <- NULL
                method_params$data_list <- data_list
                result <- do.call(method_list[[method_name]]$fit, method_params)
                return(result %>% tibble::add_column(param_df, .before=1))
              })
            },
            future.seed = TRUE,
            future.globals = future.globals,
            future.packages = future.packages
          )
          results <- dplyr::bind_rows(results)
          results$rep <- rep(
            1:n_reps, each = length(dgp_params_list)*n_methods
          )
          return(results)
        },
        "dgps+methods" = {
          mapply_args <- purrr::cross2(
            dgp_params_list, method_params_list
          )
          # shuffle the inputs to avoid bad load balancing
          mapply_args <- mapply_args[sample(1:length(mapply_args))]
          dgp_mapply_args <- lapply(mapply_args, `[[`, 1)
          method_mapply_args <- lapply(mapply_args, `[[`, 2)
          results <- future.apply::future_mapply(
            function(dgp_params, method_params) {
              reps <- replicate(n_reps, {
                dgp_name <- dgp_params$dgp_name
                dgp_params$dgp_name <- NULL
                data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
                method_name <- method_params$method_name
                param_df <- list_to_tibble_row(
                  c(dgp_name = dgp_name, dgp_params, method_params)
                )
                method_params$method_name <- NULL
                method_params$data_list <- data_list
                result <- do.call(method_list[[method_name]]$fit, method_params)
                return(result %>% tibble::add_column(param_df, .before=1))
              }, simplify=FALSE)
              dplyr::bind_rows(reps, .id = "rep")
            },
            dgp_mapply_args, method_mapply_args,
            future.seed = TRUE, SIMPLIFY = FALSE,
            future.globals = future.globals,
            future.packages = future.packages
          )
          dplyr::bind_rows(results)
        },
        "reps+dgps+methods" = {
          mapply_args <- purrr::cross3(
            1:n_reps, dgp_params_list, method_params_list
          )
          # shuffle the inputs to avoid bad load balancing
          mapply_args <- mapply_args[sample(1:length(mapply_args))]
          reps <- sapply(mapply_args, `[[`, 1)
          dgp_mapply_args <- lapply(mapply_args, `[[`, 2)
          method_mapply_args <- lapply(mapply_args, `[[`, 3)
          results <- dplyr::bind_rows(
            future.apply::future_mapply(
              function(dgp_params, method_params) {
                dgp_name <- dgp_params$dgp_name
                dgp_params$dgp_name <- NULL
                data_list <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
                method_name <- method_params$method_name
                param_df <- list_to_tibble_row(
                  c(dgp_name = dgp_name, dgp_params, method_params)
                )
                method_params$method_name <- NULL
                method_params$data_list <- data_list
                result <- do.call(method_list[[method_name]]$fit, method_params)
                return(result %>% tibble::add_column(param_df, .before=1))
              },
              dgp_mapply_args, method_mapply_args,
              future.seed = TRUE, SIMPLIFY = FALSE,
              future.globals = future.globals,
              future.packages = future.packages
            )
          )
          results$rep <- reps
          return(results)
        }
      )

      fit_results <- simplify_tibble(fit_results)

      if (verbose >= 1) {
        message(sprintf("Fitting completed | time taken: %f minutes",
                        difftime(Sys.time(), start_time, units = "mins")))
        if (!save) {
          message("==============================")
        }
      }

      if (save) {
        private$.save_results(fit_results, "fit", verbose)
      }
      return(fit_results)
    },
    #' @description Evaluate the performance of method fits across all
    #'   \code{Evaluators} in the \code{Experiment} and return evaluation
    #'   results.
    #'
    #' @param fit_results A tibble, as returned by the \code{fit} method.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'  evaluation results. If cached results cannot be found, evaluate
    #'  experiment anyways.
    #' @param save If \code{TRUE}, save evaluation results to disk.
    #' @param verbose Level of verboseness. Default is 1, which prints out
    #'   messages after major checkpoints in the experiment. If 0, no messages
    #'   are printed.
    #' @param ... Not used.
    #'
    #' @return A list of evaluation result tibbles, one for each
    #'   \code{Evaluator}.
    evaluate = function(fit_results, use_cached = FALSE, save = FALSE,
                        verbose = 1, ...) {
      if (use_cached) {
        results <- private$.get_cached_results("eval", verbose = verbose)
        if (!is.null(results)) {
          return(results)
        }
      }
      if (verbose >= 1) {
        message(sprintf("Evaluating %s...", self$name))
        start_time <- Sys.time()
      }
      evaluator_list <- private$.get_obj_list("evaluator")
      if (length(evaluator_list) == 0) {
        private$.throw_empty_list_error("evaluator", "evaluate")
      }
      eval_results <- purrr::map(evaluator_list, function(evaluator) {
        evaluator$evaluate(fit_results = fit_results,
                           vary_params = private$.get_vary_params())
      })

      if (verbose >= 1) {
        message(sprintf("Evaluation completed | time taken: %f minutes",
                        difftime(Sys.time(), start_time, units = "mins")))
        if (!save) {
          message("==============================")
        }
      }

      if (save) {
        private$.save_results(eval_results, "eval", verbose)
      }

      return(eval_results)
    },
    #' @description Plot the performance of methods and/or its evaluation
    #'   metrics using all \code{Plotters} in the \code{Experiment} and return
    #'   plot results.
    #'
    #' @param fit_results A tibble, as returned by the \code{fit} method.
    #' @param eval_results A list of result tibbles, as returned by the
    #'   \code{evaluate} method.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'   results. If cached results cannot be found, plot experiment anyways.
    #' @param save If \code{TRUE}, save plots to disk.
    #' @param verbose Level of verboseness. Default is 1, which prints out
    #'   messages after major checkpoints int the experiment. If 0, no messages
    #'   are printed.
    #' @param ... Not used.
    #'
    #' @return A list of plots, one for each \code{Plotter}.
    plot = function(fit_results = NULL, eval_results = NULL,
                    use_cached = FALSE, save = FALSE, verbose = 1, ...) {
      if (use_cached) {
        results <- private$.get_cached_results("plot", verbose = verbose)
        if (!is.null(results)) {
          return(results)
        }
      }
      if (verbose >= 1) {
        message(sprintf("Plotting %s...", self$name))
        start_time <- Sys.time()
      }
      plotter_list <- private$.get_obj_list("plotter")
      if (length(plotter_list) == 0) {
        private$.throw_empty_list_error("plotter", "plot results from")
      }
      plot_results <- purrr::map(plotter_list, function(plotter) {
        plotter$plot(fit_results = fit_results,
                     eval_results = eval_results,
                     vary_params = private$.get_vary_params())
      })

      if (verbose >= 1) {
        message(sprintf("Plotting completed | time taken: %f minutes",
                        difftime(Sys.time(), start_time, units = "mins")))
        if (!save) {
          message("==============================")
        }
      }

      if (save) {
        private$.save_results(plot_results, "plot", verbose)
      }

      return(plot_results)
    },
    #' @description Create documentation template (a series of .md files) to
    #'   fill out for the R Markdown results report. The documentation files can
    #'   be found in the \code{Experiment}'s results directory under docs/.
    #'
    #' @param save_dir An optional directory in which to find the experiment's
    #'   saved results.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object.
    create_doc_template = function(save_dir = self$get_save_dir(), ...) {
      if (!dir.exists(file.path(save_dir, "docs"))) {
        dir.create(file.path(save_dir, "docs"), recursive = TRUE)
      }

      if (!file.exists(file.path(save_dir, "docs", "objectives.md"))) {
        fname <- file.path(save_dir, "docs", "objectives.md")
        write.csv(NULL, file = fname, quote = F)
      }

      descendants <- purrr::map(
        list.dirs(save_dir), function(d) {
          if (file.exists(file.path(d, "experiment.rds"))) {
            return(readRDS(file.path(d, "experiment.rds")))
          } else {
            return(NULL)
          }
        }
      )
      descendants[sapply(descendants, is.null)] <- NULL

      fields <- c("dgp", "method", "evaluator", "plotter")
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
      invisible(self)
    },
    #' @description Knits an R Markdown file summarizing the results of an
    #'   \code{Experiment}. Outputs an R Markdown-generated html file, saved in
    #'   the \code{Experiment}'s root results directory.
    #'
    #' @param open If \code{TRUE}, open the R Markdown-generated html file in a
    #'   web browser.
    #' @param verbose Level of verboseness (0, 1, 2) when knitting R Markdown.
    #'   Default is 2.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object.
    create_rmd = function(open = TRUE, verbose = 2, ...) {
      self$create_doc_template()
      input_fname <- system.file("rmd", "results.Rmd", package = packageName())
      output_fname <- file.path(private$.save_dir, paste0(self$name, ".html"))
      params_list <- list(sim_name = self$name, sim_path = private$.save_dir,
                          verbose = verbose)
      rmarkdown::render(input = input_fname,
                        params = params_list,
                        output_file = output_fname,
                        quiet = TRUE)
      output_fname <- stringr::str_replace_all(output_fname, " ", "\\\\ ")
      if (open) {
        system(paste("open", output_fname))
      }
      invisible(self)
    },
    #' @description Add a new \code{DGP} to the \code{Experiment}.
    #'
    #' @param dgp A \code{DGP} object.
    #' @param name Name of \code{DGP}.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{DGP} added.
    add_dgp = function(dgp, name=NULL, ...) {
      private$.check_obj(dgp, "DGP")
      private$.add_obj("dgp", dgp, name)
      invisible(self)
    },
    #' @description Update an existing \code{DGP} in the \code{Experiment}.
    #'
    #' @param dgp A \code{DGP} object.
    #' @param name Name of existing \code{DGP} to update.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the updated \code{DGP}.
    update_dgp = function(dgp, name, ...) {
      private$.check_obj(dgp, "DGP")
      private$.update_obj("dgp", dgp, name)
      invisible(self)
    },
    #' @description Remove a \code{DGP} from the \code{Experiment}.
    #'
    #' @param name Name of \code{DGP} to remove.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{DGP} removed.
    remove_dgp = function(name, ...) {
      private$.remove_obj("dgp", name)
      invisible(self)
    },
    #' @description Get all \code{DGPs} in the \code{Experiment}.
    #'
    #' @return A list of \code{DGP} objects in the \code{Experiment}.
    get_dgps = function() {
      return(private$.get_obj_list("dgp"))
    },
    #' @description Add a new \code{Method} to the \code{Experiment}.
    #'
    #' @param method A \code{Method} object.
    #' @param name Name of \code{Method}.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{Method} added.
    add_method = function(method, name=NULL, ...) {
      private$.check_obj(method, "Method")
      private$.add_obj("method", method, name)
      invisible(self)
    },
    #' @description Update an existing \code{Method} in the \code{Experiment}.
    #'
    #' @param method A \code{Method} object.
    #' @param name Name of existing \code{Method} to update.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the updated \code{Method}.
    update_method = function(method, name, ...) {
      private$.check_obj(method, "Method")
      private$.update_obj("method", method, name)
      invisible(self)
    },
    #' @description Remove a \code{Method} from the \code{Experiment}.
    #'
    #' @param name Name of \code{Method} to remove.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{Method} removed.
    remove_method = function(name, ...) {
      private$.remove_obj("method", name)
      invisible(self)
    },
    #' @description Get all \code{Methods} in the \code{Experiment}.
    #'
    #' @return A list of \code{Method} objects in the \code{Experiment}.
    get_methods = function() {
      return(private$.get_obj_list("method"))
    },
    #' @description Add a new \code{Evaluator} to the \code{Experiment}.
    #'
    #' @param evaluator An \code{Evaluator} object.
    #' @param name Name of \code{Evaluator}.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{Evaluator} added.
    add_evaluator = function(evaluator, name = NULL, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.add_obj("evaluator", evaluator, name)
      invisible(self)
    },
    #' @description Update an existing \code{Evaluator} in the \code{Experiment}.
    #'
    #' @param evaluator An \code{Evaluator} object.
    #' @param name Name of existing \code{Evaluator} to update.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the updated \code{Evaluator}.
    update_evaluator = function(evaluator, name, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.update_obj("evaluator", evaluator, name)
      invisible(self)
    },
    #' @description Remove an \code{Evaluator} from the \code{Experiment}.
    #'
    #' @param name Name of \code{Evaluator} to remove.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{Evaluator} removed.
    remove_evaluator = function(name, ...) {
      private$.remove_obj("evaluator", name)
      invisible(self)
    },
    #' @description Get all \code{Evaluators} in the \code{Experiment}.
    #'
    #' @return A list of \code{Evaluator} objects in the \code{Experiment}.
    get_evaluators = function() {
      return(private$.get_obj_list("evaluator"))
    },
    #' @description Add a new \code{Plotter} to the \code{Experiment}.
    #'
    #' @param plotter A \code{Plotter} object.
    #' @param name Name of \code{Plotter}.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{Plotter} added.
    add_plotter = function(plotter, name=NULL, ...) {
      private$.check_obj(plotter, "Plotter")
      private$.add_obj("plotter", plotter, name)
      invisible(self)
    },
    #' @description Update an existing \code{Plotter} in the \code{Experiment}.
    #'
    #' @param plotter A \code{Plotter} object.
    #' @param name Name of existing \code{Plotter} to update.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the updated \code{Plotter}.
    update_plotter = function(plotter, name, ...) {
      private$.check_obj(plotter, "Plotter")
      private$.update_obj("plotter", plotter, name)
      invisible(self)
    },
    #' @description Remove a \code{Plotter} from the \code{Experiment}.
    #'
    #' @param name Name of \code{Plotter} to remove.
    #' @param ... Not used.
    #'
    #' @return The original \code{Experiment} object with the \code{Plotter} removed.
    remove_plotter = function(name, ...) {
      private$.remove_obj("plotter", name)
      invisible(self)
    },
    #' @description Get all \code{Plotters} in the \code{Experiment}.
    #'
    #' @return A list of \code{Plotter} objects in the \code{Experiment}.
    get_plotters = function() {
      return(private$.get_obj_list("plotter"))
    },
    #' @description Add a \code{vary_across} component to the \code{Experiment}.
    #'   When the \code{Experiment} is run, the \code{Experiment} is
    #'   systematically varied across values of the specified parameter in the
    #'   \code{DGP} or \code{Method} while all other parameters are held
    #'   constant at their baseline value.
    #'
    #' @param dgp Name of \code{DGP} to vary in the \code{Experiment}. Can also
    #'   be a \code{DGP} object that matches one in the \code{Experiment}. Must
    #'   provide either the \code{dgp} or \code{method} argument.
    #' @param method Name of \code{Method} to vary in the \code{Experiment}. Can
    #'   also be a \code{Method} object that matches one in the \code{Experiment}.
    #'   Must provide either the \code{dgp} or \code{method} argument.
    #' @param ... Any number of named arguments where names match an argument in
    #'   the user-specified \code{DGP} or \code{Method} function and values are
    #'   vectors (for scalar parameters) or lists (for arbitrary parameters).
    #'
    #' @return The original \code{Experiment} object with the
    #'   \code{vary_across} component added.
    add_vary_across = function(dgp, method, ...) {
      temp <- private$.check_vary_across(dgp, method, ...)
      dots_list <- temp$dots_list
      field_name <- temp$field_name
      obj_name <- temp$obj_name
      vary_across_sublist <- private$.vary_across_list[[field_name]][[obj_name]]
      if (is.null(vary_across_sublist)) {
        vary_across_sublist <- list()
      }
      for (arg_name in names(dots_list)) {
        if (is.null(vary_across_sublist[[arg_name]])) {
          vary_across_sublist[[arg_name]] <- dots_list[[arg_name]]
        } else {
          stop(
            sprintf(
              paste0(
                "The vary_across parameter for argument '%s' has already ",
                "been set for %s's %s_fun. Use update_vary_across instead."
              ),
              arg_name, obj_name, field_name
            ), call. = FALSE
          )
        }
      }
      private$.vary_across_list[[field_name]][[obj_name]] <- vary_across_sublist
      invisible(self)
    },
    #' @description Update the \code{vary_across} component in the
    #'   \code{Experiment}. Used if a \code{vary_across} component already
    #'   exists in the \code{Experiment}.
    #'
    #' @param dgp Name of \code{DGP} to vary in the \code{Experiment}. Can also
    #'   be a \code{DGP} object that matches one in the \code{Experiment}. Must
    #'   provide either the \code{dgp} or \code{method} argument.
    #' @param method Name of \code{Method} to vary in the \code{Experiment}. Can
    #'   also be a \code{Method} object that matches one in the \code{Experiment}.
    #'   Must provide either the \code{dgp} or \code{method} argument.
    #' @param ... Any number of named arguments where names match an argument in
    #'   the user-specified \code{DGP} or \code{Method} function and values are
    #'   vectors (for scalar parameters) or lists (for arbitrary parameters).
    #'
    #' @return The original \code{Experiment} object with the updated
    #'   \code{vary_across} component.
    update_vary_across = function(dgp, method, ...) {
      temp <- private$.check_vary_across(dgp, method, ...)
      dots_list <- temp$dots_list
      field_name <- temp$field_name
      obj_name <- temp$obj_name
      vary_across_sublist <- private$.vary_across_list[[field_name]][[obj_name]]
      if (is.null(vary_across_sublist)) {
        stop(
          sprintf(
            paste0(
              "The vary_across parameter has not been set for %s's %s_fun. ",
              "Use add_vary_across instead."
            ),
            obj_name, field_name
          ), call. = FALSE
        )
      }
      for (arg_name in names(dots_list)) {
        if (is.null(vary_across_sublist[[arg_name]])) {
          stop(
            sprintf(
              paste0(
                "The vary_across parameter for argument '%s' has not ",
                "been set for %s's %s_fun. Use add_vary_across instead."
              ),
              arg_name, obj_name, field_name
            ), call. = FALSE
          )
        } else {
          vary_across_sublist[[arg_name]] <- dots_list[[arg_name]]
        }
      }
      private$.vary_across_list[[field_name]][[obj_name]] <- vary_across_sublist
      invisible(self)
    },
    #' @description Remove part or all of the \code{vary_across} component for a
    #'   specific \code{DGP} or \code{Method} in the \code{Experiment}.
    #'
    #' @param dgp Name of \code{DGP} to vary in the \code{Experiment}. Can also
    #'   be a \code{DGP} object that matches one in the \code{Experiment}. Must
    #'   provide either the \code{dgp} or \code{method} argument.
    #' @param method Name of \code{Method} to vary in the \code{Experiment}. Can
    #'   also be a \code{Method} object that matches one in the \code{Experiment}.
    #'   Must provide either the \code{dgp} or \code{method} argument.
    #' @param param_names A character vector of parameter names to remove. If
    #'   not provided, the entire set of \code{vary_across} parameters will be
    #'   removed for the specified \code{DGP}/\code{Method}.
    #'
    #' @return The original \code{Experiment} object with the \code{vary_across}
    #'   component removed.
    remove_vary_across = function(dgp, method, param_names = NULL) {
      temp <- private$.check_vary_across(dgp, method)
      field_name <- temp$field_name
      obj_name <- temp$obj_name
      vary_across_sublist <- private$.vary_across_list[[field_name]][[obj_name]]
      if (is.null(vary_across_sublist)) {
        stop(
          sprintf(
            paste0(
              "Cannot remove vary_across parameter for %s's %s_fun ",
              "since the vary_across parameter has not been set."
            ),
            obj_name, field_name
          ), call. = FALSE
        )
      }

      for (arg_name in param_names) {
        if (is.null(vary_across_sublist[[arg_name]])) {
          stop(
            sprintf(
              paste0(
                "Cannot remove vary_across parameter for argument '%s' ",
                "in %s's %s_fun since the vary_across parameter has not been set."
              ),
              arg_name, obj_name, field_name
            ), call. = FALSE
          )
        } else {
          vary_across_sublist[[arg_name]] <- NULL
        }
      }
      if ((length(vary_across_sublist) == 0) || is.null(param_names)) {
        vary_across_sublist <- NULL
      }
      private$.vary_across_list[[field_name]][[obj_name]] <- vary_across_sublist
      invisible(self)
    },
    #' @description Get the \code{vary_across} component from the
    #'   \code{Experiment}.
    #'
    #' @return A list with two components: "dgp" and "method". Both components
    #'    are also lists that contain lists named for specific \code{DGP} or
    #'    \code{Method} objects in the \code{Experiment}. This final layer of
    #'    lists contains vectors or lists named for the arguments of the
    #'    corresponding user-specified \code{DGP}/\code{Method} function.
    get_vary_across = function() {
      return(private$.vary_across_list)
    },
    #' @description Set R Markdown options for displaying \code{Evaluator} or
    #'   \code{Plotter} outputs in the summary report.
    #'
    #' @param field_name One of "evaluator" or "plotter".
    #' @param name Name of \code{Evaluator} or \code{Plotter} to set R Markdown
    #'   options.
    #' @param show If \code{TRUE}, show output; if \code{FALSE}, hide output in
    #'   R Markdown report. Default \code{NULL} does not change the "show" field
    #'   in \code{Evaluator}/\code{Plotter}.
    #' @param ... Named R Markdown options to set. If \code{field_name = "plotter"},
    #'   options are "height" and "width". If \code{field_name = "evaluator"},
    #'   see options for [prettyDT()].
    #'
    #' @return The original \code{Experiment} object with the \code{rmd_options}
    #'   and/or \code{show} fields modified in the \code{Evaluator}/\code{Plotter}.
    set_rmd_options = function(field_name, name, show = NULL, ...) {
      obj_list <- private$.get_obj_list(field_name)
      if (!name %in% names(obj_list)) {
        stop(
          sprintf("The name '%s' isn't in the %s list. ",
                  name, field_name),
          sprintf("Use add_%s first.", field_name),
          call. = FALSE
        )
      }
      list_name <- paste0(".", field_name, "_list")
      if (!is.null(show)) {
        private[[list_name]][[name]]$show <- show
      }
      rmd_options <- list(...)
      if (length(rmd_options) > 0) {
        for (i in 1:length(rmd_options)) {
          private[[list_name]][[name]]$rmd_options[[names(rmd_options)[i]]] <-
            rmd_options[[i]]
        }
      }
      invisible(self)
    },
    #' @description Get the directory in which the \code{Experiment}'s results
    #'   are saved.
    #'
    #' @return The directory in which the \code{Experiment}'s results are saved.
    get_save_dir = function() {
      return(private$.save_dir)
    },
    #' @description Set the directory in which the \code{Experiment}'s results
    #'   are saved.
    #'
    #' @param save_dir The directory in which the \code{Experiment}'s results
    #'   will be saved.
    #'
    #' @return The original \code{Experiment} object with the updated saving
    #'   directory.
    set_save_dir = function(save_dir) {
      private$.save_dir <- save_dir
      invisible(self)
    },
    #' @description Save an \code{Experiment} object.
    #'
    #' @return The original \code{Experiment} object.
    save = function() {
      save_dir <- self$get_save_dir()
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      invisible(self)
    },
    #' @description Print an \code{Experiment} in a nice format, showing the
    #'   names of all \code{DGPs}, \code{Methods}, \code{Evaluators}, and
    #'   \code{Plotters} in addition to the directory where results are saved
    #'   and the \code{vary_across} component (if any).
    #'
    #' @return The original \code{Experiment} object.
    print = function() {
      cat("Experiment Name:", self$name, "\n")
      cat("   Saved results at:", private$.save_dir, "\n")
      cat("   DGPs:",
          paste(names(private$.get_obj_list("dgp")),
                sep = "", collapse = ", "), "\n")
      cat("   Methods:",
          paste(names(private$.get_obj_list("method")),
                sep = "", collapse = ", "), "\n")
      cat("   Evaluators:",
          paste(names(private$.get_obj_list("evaluator")),
                sep = "", collapse = ", "), "\n")
      cat("   Plotters:",
          paste(names(private$.get_obj_list("plotter")),
                sep = "", collapse = ", "), "\n")
      cat("   Vary Across: ")
      if (identical(private$.vary_across, list())) {
        cat("None")
      } else {
        if ("dgp" %in% names(private$.vary_across)) {
          cat("\n      DGP:", private$.vary_across$dgp, "\n")
        } else {
          cat("\n      Method:", private$.vary_across$method, "\n")
        }
        cat("      Parameter:", private$.vary_across$param_name, "\n")
        cat("      Parameter values: ")
        cat(str(private$.vary_across$param_values,
                indent.str = "        ", no.list = F))
      }
      invisible(self)
    }
  )
)

#' Create a new \code{Experiment}.
#'
#' @name create_experiment
#'
#' @param ... Passed to \code{Experiment$new()}.
#'
#' @return A new instance of \code{Experiment}.
#'
#' @export
create_experiment <- function(...) {
  Experiment$new(...)
}

#' Run the full \code{Experiment} pipeline (fitting, evaluating, and plotting).
#'
#' @name run_experiment
#'
#' @param experiment An \code{Experiment} object.
#' @param n_reps The number of replicates of the \code{Experiment} for this run.
#' @param ... Passed to \code{Experiment$run()}.
#'
#' @return A list of results from the simulation experiment.
#' \describe{
#' \item{fit_results}{A tibble containing results from the \code{fit}
#'   method. In addition to results columns, has columns named 'rep', 'dgp',
#'   'method', and the \code{vary_across} parameter name if applicable.}
#' \item{eval_results}{A list of tibbles containing results from the
#'   \code{evaluate} method, which evaluates each \code{Evaluator} in
#'   the \code{Experiment}. Length of list is equivalent to the number of
#'   \code{Evaluators}.}
#' \item{plot_results}{A list of tibbles containing results from the
#'   \code{plot} method, which plots each \code{Plotter} in
#'   the \code{Experiment}. Length of list is equivalent to the number of
#'   \code{Plotters}.}
#' }
#' @export
run_experiment <- function(experiment, n_reps=1, ...) {
  return(experiment$run(n_reps, ...))
}

#' Generate data from each \code{DGP} in the \code{Experiment}.
#'
#' @name generate_data
#'
#' @param experiment An \code{Experiment} object.
#' @param n_reps The number of datasets to generate per \code{DGP}.
#' @param ... Passed to \code{experiment$generate_data()}.
#'
#' @return A list of length equal to the number of \code{DGPs} in the
#'   \code{Experiment}. Each element in the list is a list of \code{n_reps}
#'   datasets generated by the given \code{DGP}.
#'
#' @export
generate_data <- function(experiment, n_reps=1, ...) {
  return(experiment$generate_data(n_reps, ...))
}

#' Fit an \code{Experiment}.
#'
#' @name fit_experiment
#' @description Fit \code{Methods} in the \code{Experiment} across all
#'   \code{DGPs} for \code{n_reps} repetitions and return results from fits.
#'
#' @param experiment An \code{Experiment} object.
#' @param n_reps The number of replicates of the \code{Experiment} for this run.
#' @param ... Passed to \code{Experiment$fit()}.
#'
#' @return A tibble containing the results from fitting all \code{Methods}
#'   across all \code{DGPs} for \code{n_reps} repetitions. In addition to
#'   results columns, has columns named 'rep', 'dgp', 'method', and the
#'   \code{vary_across} parameter name if applicable.
#'
#' @export
fit_experiment <- function(experiment, n_reps=1, ...) {
  return(experiment$fit(n_reps, ...))
}

#' Evaluate an \code{Experiment}.
#'
#' @name evaluate_experiment
#' @description Evaluate the performance of method(s) across all
#'   \code{Evaluators} in the \code{Experiment} and return results.
#'
#' @param experiment An \code{Experiment} object.
#' @param ... Passed to \code{Experiment$evaluate()}.
#'
#' @return A list of evaluation result tibbles, one for each
#'   \code{Evaluator}.
#'
#' @export
evaluate_experiment <- function(experiment, ...) {
  return(experiment$evaluate(...))
}

#' Plot results of an \code{Experiment}.
#'
#' @name plot_experiment
#' @description Plot the performance of methods and/or its evaluation metrics
#'   using all \code{Plotters} in the \code{Experiment} and return plot results.
#'
#' @param experiment An \code{Experiment} object.
#' @param ... Passed to \code{Experiment$plot()}.
#'
#' @return A list of plots, one for each \code{Plotter}.
#'
#' @export
plot_experiment <- function(experiment, ...) {
  return(experiment$plot(...))
}

#' Helper functions for adding components to an \code{Experiment}.
#'
#' @description Helper functions for adding \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Plotters} to an \code{Experiment}.
#'
#' @param experiment An \code{Experiment} object.
#' @param name A name to identify the object to be added.
#' @param dgp A \code{DGP} object.
#' @param method A \code{Method} object.
#' @param evaluator An \code{Evaluator} object.
#' @param plotter A \code{Plotter} object.
#' @param ... Not used.
#'
#' @return The original \code{Experiment} object passed to \code{add_*}.
#'
#' @name add_funs
#' @rdname add_funs
#'
NULL

#' @rdname add_funs
#'
#' @export
add_dgp <- function(experiment, dgp, name=NULL, ...) {
  experiment$add_dgp(dgp, name, ...)
}

#' @rdname add_funs
#'
#' @export
add_method <- function(experiment, method, name=NULL, ...) {
  experiment$add_method(method, name, ...)
}

#' @rdname add_funs
#'
#' @export
add_evaluator <- function(experiment, evaluator, name = NULL, ...) {
  experiment$add_evaluator(evaluator, name, ...)
}

#' @rdname add_funs
#'
#' @export
add_plotter <- function(experiment, plotter, name=NULL, ...) {
  experiment$add_plotter(plotter, name, ...)
}

#' Helper functions for updating components of an \code{Experiment}.
#'
#' @description Helper functions for updating \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Plotters} already added to an
#'   \code{Experiment}.
#'
#' @inheritParams add_funs
#' @param name A name to identify the object to be updated.
#'
#' @return The original \code{Experiment} object passed to \code{update_*}.
#'
#' @name update_funs
#' @rdname update_funs
#'
NULL

#' @rdname update_funs
#'
#' @export
update_dgp <- function(experiment, dgp, name, ...) {
  experiment$update_dgp(dgp, name, ...)
}

#' @rdname update_funs
#'
#' @export
update_method <- function(experiment, method, name, ...) {
  experiment$update_method(method, name, ...)
}


#' @rdname update_funs
#'
#' @export
update_evaluator <- function(experiment, evaluator, name, ...) {
  experiment$update_evaluator(evaluator, name, ...)
}

#' @rdname update_funs
#'
#' @export
update_plotter <- function(experiment, plotter, name, ...) {
  experiment$update_plotter(plotter, name, ...)
}

#' Helper functions for removing components of an \code{Experiment}.
#'
#' @description Helper functions for removing \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Plotters} already added to an
#'   \code{Experiment}.
#'
#' @param name A name to identify the object to be removed.
#' @param ... Not used.
#'
#' @return The original \code{Experiment} object passed to \code{remove_*}.
#'
#' @name remove_funs
#' @rdname remove_funs
#'
NULL

#' @rdname remove_funs
#'
#' @export
remove_dgp <- function(experiment, name, ...) {
  experiment$remove_dgp(name, ...)
}

#' @rdname remove_funs
#'
#' @export
remove_method <- function(experiment, name, ...) {
  experiment$remove_method(name, ...)
}

#' @rdname remove_funs
#'
#' @export
remove_evaluator <- function(experiment, name, ...) {
  experiment$remove_evaluator(name, ...)
}

#' @rdname remove_funs
#'
#' @export
remove_plotter <- function(experiment, name, ...) {
  experiment$remove_plotter(name, ...)
}

#' Varying across parameters in an \code{Experiment}.
#'
#' @description Helper functions for adding, updating, or removing a
#'   \code{vary_across} component in an \code{Experiment}. When a
#'   \code{vary_across} component is added and the \code{Experiment} is run, the
#'   \code{Experiment} is systematically varied across values of the specified
#'   parameter in the \code{DGP} or \code{Method} while all other parameters are
#'   held constant at their baseline value.
#'
#' @param experiment An \code{Experiment} object.
#' @param dgp Name of \code{DGP} to vary in the \code{Experiment}. Can also be a
#'   \code{DGP} object that matches one in the \code{Experiment}. Must provide
#'   either the \code{dgp} or \code{method} argument.
#' @param method Name of \code{Method} to vary in the \code{Experiment}. Can
#'   also be a \code{Method} object that matches one in the \code{Experiment}.
#'   Must provide either the \code{dgp} or \code{method} argument.
#' @param param_names A character vector of parameter names to remove. If
#'   not provided, the entire set of \code{vary_across} parameters will be
#'   removed for the specified \code{DGP}/\code{Method}.
#' @param ... Any number of named arguments where names match an argument in the
#'   user-specified \code{DGP} or \code{Method} function and values are vectors
#'   (for scalar parameters) or lists (for arbitrary parameters).
#'
#' @return The original \code{Experiment} object passed to
#'   \code{*_vary_across()}.
#'
#' @name vary_across
#' @rdname vary_across
#'
NULL

#' @rdname vary_across
#'
#' @export
add_vary_across <- function(experiment, ...) {
  experiment$add_vary_across(...)
}

#' @rdname vary_across
#'
#' @export
update_vary_across <- function(experiment, ...) {
  experiment$update_vary_across(...)
}

#' @rdname vary_across
#'
#' @export
remove_vary_across <- function(experiment, ...) {
  experiment$remove_vary_across(...)
}

#' Set R Markdown options for \code{Evaluator} and \code{Plotter} outputs in
#'   summary report
#'
#' @name set_rmd_options
#' @description Set R Markdown options for \code{Evaluator} or \code{Plotter}
#'   outputs in the summary report. Some options include the height/width of
#'   plots and number of digits to show in tables.
#'
#' @param experiment An \code{Experiment} object
#' @param field_name One of "evaluator" or "plotter".
#' @param name Name of \code{Evaluator} or \code{Plotter} to set R Markdown
#'   options.
#' @param show If \code{TRUE}, show output; if \code{FALSE}, hide output in
#'   R Markdown report. Default \code{NULL} does not change the "show" field
#'   in \code{Evaluator}/\code{Plotter}.
#' @param ... Named R Markdown options to set. If \code{field_name = "plotter"},
#'   options are "height" and "width". If \code{field_name = "evaluator"},
#'   see options for [prettyDT()].
#'
#' @return The original \code{Experiment} object with the \code{rmd_options}
#'   and/or \code{show} fields modified in the \code{Evaluator}/\code{Plotter}.
#'
#' @export
set_rmd_options <- function(experiment, field_name = c("evaluator", "plotter"),
                            name, show = NULL, ...) {
  field_name <- match.arg(field_name)
  experiment$set_rmd_options(field_name = field_name, name = name, show = show,
                             ...)
}

#' Set results directory for an \code{Experiment}.
#'
#' @name set_save_dir
#' @description Set the directory in which the \code{Experiment}'s results and
#'   plots are saved.
#'
#' @param experiment An \code{Experiment} object.
#' @param save_dir The directory in which the \code{Experiment}'s results
#'   will be saved.
#'
#' @return The original \code{Experiment} object with the updated saving
#'   directory.
#'
#' @export
set_save_dir <- function(experiment, save_dir) {
  experiment$set_save_dir(save_dir)
}

#' Save an \code{Experiment}.
#'
#' @name save_experiment
#' @description Save an \code{Experiment} object to a .rds file under the
#'   \code{Experiment}'s results directory (see \code{Experiment$get_save_dir()}).
#'
#' @param experiment An \code{Experiment} object.
#'
#' @return The original \code{Experiment} object passed to
#'   \code{save_experiment}.
#'
#' @export
save_experiment <- function(experiment) {
  experiment$save()
}

#' Create documentation template for the R Markdown results report.
#'
#' @name create_doc_template
#' @description Create documentation template (a series of .md files) to
#'   fill out for the R Markdown results report. The documentation files can
#'   be found in the \code{Experiment}'s results directory (see
#'   \code{Experiment$get_save_dir()}) under docs/.
#'
#' @param experiment An \code{Experiment} object.
#' @param experiment_dirname A directory where results from an \code{Experiment}
#'   were previously saved. Used if \code{experiment} was not provided.
#'
#' @return The original \code{Experiment} object passed to
#'   \code{create_doc_template}.
#'
#' @export
create_doc_template = function(experiment, experiment_dirname) {
  if (missing(experiment) && missing(experiment_dirname)) {
    stop("Must provide argument for one of experiment or experiment_dirname")
  }
  if (missing(experiment)) {
    # create dummy experiment
    experiment <- create_experiment()
  } else {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of pcs.sim.pkg::%s",
                         as.character(substitute(experiment)), "Experiment")
      stop(err_msg, call.=FALSE)
    }
    experiment_dirname <- experiment$get_save_dir()
  }
  experiment$create_doc_template(save_dir = experiment_dirname)
}

#' Create an R Markdown file summarizing the results of an \code{Experiment}.
#'
#' @name create_rmd
#' @description Knits an R Markdown file summarizing the results of an
#'   \code{Experiment}. Outputs an R Markdown-generated html file, saved in
#'   the \code{Experiment}'s root results directory (see
#'   \code{Experiment$get_save_dir()}).
#'
#' @param experiment An \code{Experiment} object.
#' @param experiment_dirname A directory where results from an \code{Experiment}
#'   were previously saved. Used if \code{experiment} argument was not provided.
#' @param open If \code{TRUE}, open the R Markdown-generated html file in a
#'   web browser.
#' @param verbose Level of verboseness (0, 1, 2) when knitting R Markdown.
#'   Default is 2.
#'
#' @return The original \code{Experiment} object passed to \code{create_rmd}.
#'
#' @export
create_rmd <- function(experiment, experiment_dirname,
                       open = TRUE, verbose = 2) {
  if (missing(experiment) && missing(experiment_dirname)) {
    stop("Must provide argument for one of experiment or experiment_dirname")
  }
  if (missing(experiment)) {
    if (!file.exists(file.path(experiment_dirname, "experiment.rds"))) {
      stop(sprintf("Cannot find saved experiment.rds file in %s",
                   experiment_dirname))
    }
    experiment <- readRDS(file.path(experiment_dirname, "experiment.rds"))
  } else {
    if (!inherits(experiment, "Experiment")) {
      err_msg <- sprintf("%s must be an instance of pcs.sim.pkg::%s",
                         as.character(substitute(experiment)), "Experiment")
      stop(err_msg, call.=FALSE)
    }
  }
  experiment$create_doc_template()
  experiment$create_rmd(open = open, verbose = verbose)
}
