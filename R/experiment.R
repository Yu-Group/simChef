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
    .vary_across = list(),
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
    },
    .get_cached_results = function(save_filename) {
      if (identical(private$.vary_across, list())) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- file.path(private$.save_dir,
                              paste0(private$.vary_across$dgp,
                                     private$.vary_across$method),
                              paste("Varying", private$.vary_across$param_name))
      }
      save_file <- file.path(save_dir, save_filename)
      if (file.exists(save_file)) {
        return(readRDS(save_file))
      } else {
        stop(
          sprintf("Cached results do not exist at %s. Set use_cached = FALSE.",
                  save_file),
          call. = FALSE)
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
    #' @param future.globals Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for a specific run of the
    #'    experiment, use the same argument in \code{Experiment$run}.
    #' @param future.packages Passed as the argument of the same name to
    #' code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for a specific run of the
    #'   experiment, use the same argument in \code{Experiment$run}.
    #' @Param clone_from An optional \code{Experiment} object to use as a base
    #'   for this one.
    #' @param save_dir An optional directory in which to save the experiment's
    #'   results.
    #' @param ... Not used.
    #'
    #' @return A new \code{Experiment} object.
    initialize = function(name = "experiment",
                          dgp_list = list(), method_list = list(),
                          evaluator_list = list(), plotter_list = list(),
                          future.globals = NULL, future.packages = NULL,
                          clone_from = NULL, save_dir = NULL, ...) {
      if (!is.null(clone_from)) {
        private$.check_obj(clone_from, "Experiment")
        clone <- clone_from$clone(deep = TRUE)
        dgp_list <- c(clone$get_dgps(), dgp_list)
        method_list <- c(clone$get_methods(), method_list)
        evaluator_list <- c(clone$get_evaluators(), evaluator_list)
        plotter_list <- c(clone$get_plots(), plotter_list)
      }
      private$.add_obj_list(dgp_list, "DGP")
      private$.add_obj_list(method_list, "Method")
      private$.add_obj_list(evaluator_list, "Evaluator")
      private$.add_obj_list(plotter_list, "Plotter")
      self$name <- name
      if (is.null(save_dir)) {
        save_dir <- file.path("results", name)
      }
      private$.save_dir <- R.utils::getAbsolutePath(save_dir)
    },
    #' @description Run the entire simulation experiment pipeline.
    #'
    #' @param n_reps The number of replicates of the \code{Experiment} for this
    #'   run.
    #' @param parallel_strategy A vector with some combination of "reps",
    #'   "dgps", or "methods". Determines how computation will be distributed
    #'   across available resources.
    #' @param future.globals Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for all runs of the
    #'   experiment, use the same argument during initialization.
    #' @param future.packages Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. See
    #'   \link{future.apply}{future_apply}. To set for all runs of the
    #'   experiment, use the same argument during initialization.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'   results.
    #' @param save If \code{TRUE}, save results to disk.
    #' @param ... Not used.
    #'
    #' @return A list of results from the simulation experiment.
    run = function(n_reps=1, parallel_strategy = c("reps", "dgps", "methods"),
                   future.globals = NULL, future.packages = NULL,
                   use_cached = FALSE, save = FALSE, ...) {
      if (!is.logical(use_cached)) {
        use_cached <- c("methods", "evaluators", "plots") %in% use_cached
      } else {
        use_cached <- rep(use_cached, 3)
      }
      if (!is.logical(save)) {
        save <- c("methods", "evaluators", "plots") %in% save
      } else {
        save <- rep(save, 3)
      }

      fit_results <- self$fit(n_reps, parallel_strategy = parallel_strategy,
                              future.globals = future.globals,
                              future.packages = future.packages,
                              use_cached = use_cached[1], save = save[1])
      eval_results <- self$evaluate(fit_results = fit_results,
                                    use_cached = use_cached[2], save = save[2])
      plot_results <- self$plot(fit_results = fit_results,
                                eval_results = eval_results,
                                use_cached = use_cached[3], save = save[3])
      return(list(fit_results = fit_results,
                  eval_results = eval_results,
                  plot_results = plot_results))
    },
    #' @description Generate data from each of the \code{DGPs} in the
    #'   \code{Experiment}.
    #'
    #' @param n_reps The number of datasets to generate per \code{DGP}.
    #' @param ... Passed to the generate.
    #'
    #' @return A list of length equal to the number of \code{DGPs} in the
    #'   \code{Experiment}. Each element is a list of datasets of length
    #'   \code{n_reps}.
    generate_data = function(n_reps = 1, ...) {
      # TODO: generate data that was used in run() or fit() (e.g., w/ same seed)
      dgp_list <- private$.get_obj_list("dgp")
      if (length(dgp_list) == 0) {
        private$.throw_empty_list_error("dgp", "generate data from")
      }
      dgp_results <- purrr::map(dgp_list, function(dgp) {
        replicates <- replicate(n_reps, {
          return(dgp$generate(...))
        }, simplify = FALSE)
      })
      return(dgp_results)
    },
    #' @description Return fit results from the \code{Methods} in the
    #'    \code{Experiment}.
    #'
    #' @param n_reps The number of replicates to run.
    #' @param parallel_strategy A vector with some combination of "reps",
    #'   "dgps", or "methods". Determines how computation will be distributed
    #'   across available resources.
    #' @param future.globals Character vector of names in the global environment
    #'   to pass to parallel workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions. To set for all
    #'   runs of the experiment, use the same argument during initialization.
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   code{future.apply::future_lapply} and related functions.
    #'   To set for all runs of the experiment, use the same argument during
    #'   initialization.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'   results.
    #' @param save If \code{TRUE}, save results to disk.
    #' @param ... Not used.
    #'
    #' @return A list of results from the simulation experiment.
    fit = function(n_reps=1, parallel_strategy = "reps",
                   future.globals = NULL, future.packages = NULL,
                   use_cached = FALSE, save = FALSE, ...) {
      if (use_cached) {
        return(private$.get_cached_results(
          save_filename = "fit_results.rds"
        ))
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

      future.globals <- c(future.globals, "list_to_tibble_row")

      if (identical(private$.vary_across, list())) {
        fit_results <- switch(
          parallel_strategy,
          "reps" = {
            results <- future.apply::future_replicate(n_reps, {
              purrr::map_dfr(dgp_list, function(dgp) {
                datasets <- dgp$generate()
                purrr::map_dfr(method_list, function(method) {
                  return(method$fit(datasets))
                }, .id = "method")
              }, .id = "dgp")
            }, simplify=FALSE,
            future.globals = future.globals,
            future.packages = future.packages)
            dplyr::bind_rows(results, .id = "rep")
          },
          "dgps" = {
            results <- future.apply::future_lapply(dgp_list, function(dgp) {
              reps <- replicate(n_reps, {
                datasets <- dgp$generate()
                purrr::map_dfr(method_list, function(method) {
                  return(method$fit(datasets))
                }, .id = "method")
              }, simplify=FALSE)
              dplyr::bind_rows(reps, .id = "rep")
            }, future.seed = TRUE,
            future.globals = future.globals,
            future.packages = future.packages)
            dplyr::bind_rows(results, .id = "dgp")
          },
          "methods" = {
            results <- future.apply::future_lapply(
              method_list, function(method) {
                reps <- replicate(n_reps, {
                  purrr::map_dfr(dgp_list, function(dgp) {
                    datasets <- dgp$generate()
                    return(method$fit(datasets))
                  }, .id = "dgp")
                }, simplify=FALSE)
                dplyr::bind_rows(reps, .id = "rep")
              }, future.seed = TRUE,
              future.globals = future.globals,
              future.packages = future.packages)
            dplyr::bind_rows(results, .id = "method")
          },
          "reps+dgps" = {
            dgp_names <- rep(names(dgp_list), times = n_reps)
            results <- future.apply::future_lapply(
              dgp_names,
              function(dgp_name) {
                datasets <- dgp_list[[dgp_name]]$generate()
                purrr::map_dfr(method_list, function(method) {
                  return(method$fit(datasets))
                }, .id = "method")
              }, future.seed = TRUE,
              future.globals = future.globals,
              future.packages = future.packages
            )
            names(results) <- dgp_names
            results <- dplyr::bind_rows(results, .id = "dgp")
            results$rep <- rep(
              1:n_reps, each = length(dgp_list)*length(method_list)
            )
            return(results)
          },
          "reps+methods" = {
            method_names <- rep(names(method_list), times = n_reps)
            results <- future.apply::future_lapply(
              method_names,
              function(method_name) {
                purrr::map_dfr(dgp_list, function(dgp) {
                  datasets <- dgp$generate()
                  return(method_list[[method_name]]$fit(datasets))
                }, .id = "dgp")
              },
              future.seed = TRUE,
              future.globals = future.globals,
              future.packages = future.packages
            )
            names(results) <- method_names
            results <- dplyr::bind_rows(results, .id = "method")
            results$rep <- rep(
              1:n_reps, each = length(dgp_list)*length(method_list)
            )
            return(results)
          },
          "dgps+methods" = {
            mapply_args <- expand.grid(
              dgps = names(dgp_list), methods = names(method_list)
            )
            # shuffle the inputs to avoid bad load balancing
            mapply_args <- mapply_args[sample(1:nrow(mapply_args)), ]
            results <- future.apply::future_mapply(
              function(dgp_name, method_name) {
                reps <- replicate(n_reps, {
                  datasets <- dgp_list[[dgp_name]]$generate()
                  method_list[[method_name]]$fit(datasets)
                }, simplify=FALSE)
                dplyr::bind_rows(reps, .id = "rep")
              },
              mapply_args$dgps, mapply_args$methods,
              future.seed = TRUE, SIMPLIFY = FALSE,
              future.globals = future.globals,
              future.packages = future.packages
            )
            for (i in 1:nrow(mapply_args)) {
              results[[i]]$dgp <- mapply_args[i, "dgps"]
              results[[i]]$method <- mapply_args[i, "methods"]
            }
            dplyr::bind_rows(results)
          },
          "reps+dgps+methods" = {
            mapply_args <- expand.grid(
              reps = 1:n_reps,
              dgps = names(dgp_list),
              methods = names(method_list)
            )
            # shuffle the inputs to avoid bad load balancing
            mapply_args <- mapply_args[sample(1:nrow(mapply_args)), ]
            results <- dplyr::bind_rows(
              future.apply::future_mapply(
                function(rep, dgp_name, method_name) {
                  datasets <- dgp_list[[dgp_name]]$generate()
                  method_list[[method_name]]$fit(datasets)
                },
                mapply_args$reps, mapply_args$dgps, mapply_args$methods,
                future.seed = TRUE, SIMPLIFY = FALSE,
                future.globals = future.globals,
                future.packages = future.packages
              )
            )
            results$rep <- mapply_args$rep
            results$dgp <- mapply_args$dgps
            results$method <- mapply_args$methods
            return(results)
          }
        )
      } else {
        # TODO: add parallelization
        # TODO: tweak to work for varying multiple parameters simultaneously
        # Q: do we want to vary parameters across multiple dgps/methods?
        param_name <- private$.vary_across$param_name
        param_values <- private$.vary_across$param_values
        if (!is.null(private$.vary_across$dgp)) {
          obj_name <- private$.vary_across$dgp
          fit_results <- future.apply::future_replicate(n_reps, {
            purrr::map_dfr(param_values, function(param_value) {
              input_param <- list(param = param_value) %>%
                setNames(param_name)
              datasets <- do.call(dgp_list[[obj_name]]$generate, input_param)
              purrr::map_dfr(method_list, function(method) {
                method$fit(datasets)
              }, .id = "method")
            }, .id = param_name) %>%
              dplyr::mutate(dgp = obj_name) %>%
              dplyr::relocate(dgp, .before = method)
          }, simplify = FALSE) %>%
            dplyr::bind_rows(.id = "rep")
        } else if (!is.null(private$.vary_across$method)) {
          obj_name <- private$.vary_across$method
          fit_results <- future.apply::future_replicate(n_reps, {
            purrr::map_dfr(dgp_list, function(dgp) {
              datasets <- dgp$generate()
              purrr::map_dfr(param_values, function(param_value) {
                input_param <- list(param = param_value) %>%
                  setNames(param_name)
                do.call(method_list[[obj_name]]$fit,
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
          if (is.list(param_values)) {
            attr(fit_results[[param_name]], "param_values") <- param_values
          } else {
            fit_results[[param_name]] <- param_values[
              fit_results[[param_name]]
            ]
          }
          attr(fit_results[[param_name]], "names") <- NULL
        }
      }

      if (save) {
        private$.save_results(fit_results,
                              save_filename = "fit_results.rds")
      }
      return(fit_results)
    },
    #' @description Evaluate the \code{Experiment}.
    #'
    #' @param fit_results A list of results, as returned by the \code{run}
    #'   method.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'  evaluation results.
    #' @param save If \code{TRUE}, save evaluation results to disk.
    #' @param ... Not used.
    #'
    #' @return A list of evaluation results.
    evaluate = function(fit_results, use_cached = FALSE, save = FALSE,
                        ...) {
      if (use_cached) {
        return(private$.get_cached_results(save_filename = "eval_results.rds"))
      }
      evaluator_list <- private$.get_obj_list("evaluator")
      if (length(evaluator_list) == 0) {
        private$.throw_empty_list_error("evaluator", "evaluate")
      }
      eval_results <- purrr::map(evaluator_list, function(evaluator) {
        evaluator$evaluate(fit_results = fit_results,
                           vary_param = private$.vary_across$param_name)
      })

      if (save) {
        private$.save_results(eval_results, save_filename = "eval_results.rds")
      }

      return(eval_results)
    },
    #' @description Plot the simulation experiment's method results or
    #'   evaluation results.
    #'
    #' @param fit_results A list of results, as returned by the \code{fit}
    #'   method.
    #' @param eval_results A list of results, as returned by the \code{evaluate}
    #'   method.
    #' @param use_cached If \code{TRUE}, find and return previously saved
    #'   results.
    #' @param save If \code{TRUE}, save plots to disk.
    #' @param ... Not used.
    #'
    #' @return A list of plots.
    plot = function(fit_results = NULL, eval_results = NULL,
                    use_cached = FALSE, save = FALSE, ...) {
      if (use_cached) {
        return(private$.get_cached_results(save_filename = "plot_results.rds"))
      }
      plotter_list <- private$.get_obj_list("plotter", "get_plots")
      if (length(plotter_list) == 0) {
        private$.throw_empty_list_error("plotter", "plot results from")
      }
      plot_results <- purrr::map(plotter_list, function(plotter) {
        plotter$plot(fit_results = fit_results,
                     eval_results = eval_results,
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
    create_rmd = function(open = TRUE, ...) {
      self$create_doc_template()
      input_fname <- system.file("rmd", "results.Rmd", package = packageName())
      output_fname <- file.path(private$.save_dir, paste0(self$name, ".html"))
      params_list <- list(sim_name = self$name, sim_path = private$.save_dir)
      rmarkdown::render(input = input_fname,
                        params = params_list,
                        output_file = output_fname)
      output_fname <- str_replace_all(output_fname, " ", "\\\\ ")
      if (open) {
        system(paste("open", output_fname))
      }
      return(invisible(self))
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
    add_evaluator = function(evaluator, name = NULL, ...) {
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
    add_vary_across = function(dgp = NULL, method = NULL,
                               param_name, param_values) {
      if (!identical(private$.vary_across, list())) {
        stop("The vary_across parameter has already been set. Use update_vary_across instead.",
             call. = FALSE)
      }
      if (is.null(dgp) & is.null(method)) {
        stop("Must specify either dgp or method.")
      } else if ((is.null(dgp) + is.null(method)) != 1) {
        stop("Must specify one of dgp or method, but not both")
      } else if (!is.null(dgp)) {
        dgp_list <- private$.get_obj_list("dgp")
        if (inherits(dgp, "DGP")) {
          obj_name <- sapply(dgp_list,
                             function(x) check_equal(x, dgp)) %>%
            which() %>%
            names()
        } else if (dgp %in% names(dgp_list)) {
          obj_name <- dgp
        } else {
          stop("dgp must either be a DGP object or the name of a dgp in the current experiment.")
        }
        dgp_args <- formalArgs(args(dgp_list[[obj_name]]$dgp_fun))
        if (!(param_name %in% dgp_args) & (!("..." %in% dgp_args))) {
          stop(
            sprintf("%s is not an argument in %s dgp", param_name, obj_name)
          )
        }
        private$.vary_across$dgp <- obj_name
        private$.vary_across$method <- NULL
      } else if (!is.null(method)) {
        method_list <- private$.get_obj_list("method")
        if (inherits(method, "Method")) {
          obj_name <- sapply(method_list,
                             function(x) check_equal(x, method)) %>%
            which() %>%
            names()
        } else if (method %in% names(method_list)) {
          obj_name <- method
        } else {
          stop("method must either be a Method object or the name of a method in the current experiment.")
        }
        method_args <- formalArgs(args(method_list[[obj_name]]$method_fun))
        if (!(param_name %in% method_args) & (!("..." %in% method_args))) {
          stop(
            sprintf("%s is not an argument in %s method", param_name, obj_name)
          )
        }
        private$.vary_across$method <- obj_name
        private$.vary_across$dgp <- NULL
      }
      private$.vary_across$param_name <- param_name
      private$.vary_across$param_values <- param_values
    },
    update_vary_across = function(dgp = NULL, method = NULL,
                                  param_name, param_values) {
      if (identical(private$.vary_across, list())) {
        stop("The vary_across parameter has not been added yet. Use add_vary_across instead.",
             call. = FALSE)
      }
      self$remove_vary_across()
      self$add_vary_across(dgp = dgp, method = method,
                           param_name = param_name, param_values = param_values)
    },
    remove_vary_across = function() {
      private$.vary_across <- list()
    },
    get_vary_across = function() {
      return(private$.vary_across)
    },
    get_save_dir = function() {
      return(private$.save_dir)
    },
    set_save_dir = function(save_dir) {
      private$.save_dir <- save_dir
    }
  )
)

#' Create a new \code{Experiment}.
#'
#' @name create_experiment
#'
#' @param ... Passed to experiment$new().
#'
#' @return A new instance of \code{Experiment}.
#'
#' @export
create_experiment <- function(...) {
  Experiment$new(...)
}

#' Run the full \code{Experiment} pipeline.
#'
#' @name run_experiment
#'
#' @param experiment An \code{Experiment} object.
#' @param n_reps The number of replicates of the \code{Experiment} for this run.
#' @param ... Passed to \code{experiment$run()}.
#'
#' @return A list of the simulation experiment's method results.
#'
#' @export
run_experiment <- function(experiment, n_reps=1, ...) {
  return(experiment$run(n_reps=1, ...))
}

#' Evaluate an \code{Experiment}
#'
#' @name generate_data
#'
#' @param experiment An \code{Experiment} object.
#' @param n_reps The number of datasets to generate per \code{DGP}.
#' @param ... Passed to \code{experiment$generate()}.
#'
#' @return A list of the simulation experiment's evaluation results.
#'
#' @export
generate_data <- function(experiment, n_reps=1, ...) {
  return(experiment$generate_data(n_reps, ...))
}

#' Fit an \code{Experiment}
#'
#' @name fit_experiment
#'
#' @param experiment An \code{Experiment} object.
#' @param n_reps The number of replicates of the \code{Experiment} for this run.
#' @param ... Passed to \code{experiment$fit()}.
#'
#' @return A list of the simulation experiment's evaluation results.
#'
#' @export
fit_experiment <- function(experiment, n_reps=1, ...) {
  return(experiment$fit(n_reps, ...))
}

#' Evaluate an \code{Experiment}
#'
#' @name evaluate_experiment
#'
#' @param experiment An \code{Experiment} object.
#' @param ... Passed to \code{experiment$evaluate()}.
#'
#' @return A list of the simulation experiment's evaluation results.
#'
#' @export
evaluate_experiment <- function(experiment, ...) {
  return(experiment$evaluate(...))
}

#' Plot an \code{Experiment}'s method or evaluation results.
#'
#' @param experiment An \code{Experiment} object.
#' @param ... Passed to \code{experiment$plot()}.
#'
#' @return A list of the simulation experiment's plots.
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
#' @param evaluator A \code{Evaluator} object.
#' @param plotter A \code{Plotter} object.
#' @param dgp_name The name of a \code{DGP} object in \code{experiment} with which to
#'   associate \code{evaluator}.
#' @param method_name The name of a \code{Method} object in \code{experiment} with
#'   which to associate \code{evaluator}.
#' @param ... Not currently used.
#'
#' @return The original `experiment` object passed to \code{add_*}.
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
  return(experiment)
}

#' @rdname add_funs
#'
#' @export
add_method <- function(experiment, method, name=NULL, ...) {
  experiment$add_method(method, name, ...)
  return(experiment)
}

#' @rdname add_funs
#'
#' @export
add_evaluator <- function(experiment, evaluator, name = NULL, ...) {
  experiment$add_evaluator(evaluator, name, ...)
  return(experiment)
}

#' @rdname add_funs
#'
#' @export
add_plot <- function(experiment, plotter, name=NULL, ...) {
  experiment$add_plot(plotter, name, ...)
  return(experiment)
}

#' @rdname add_funs
#'
#' @export
add_vary_across <- function(experiment, dgp = NULL, method = NULL,
                            param_name, param_values) {
  experiment$add_vary_across(dgp = dgp, method = method,
                             param_name = param_name,
                             param_values = param_values)
  return(experiment)
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
#' @return The original \code{experiment} object passed to \code{update_*}.
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
  return(experiment)
}

#' @rdname update_funs
#'
#' @export
update_method <- function(experiment, method, name, ...) {
  experiment$update_method(method, name, ...)
  return(experiment)
}


#' @rdname update_funs
#'
#' @export
update_evaluator <- function(experiment, evaluator, name, ...) {
  experiment$update_evaluator(evaluator, name, ...)
  return(experiment)
}

#' @rdname update_funs
#'
#' @export
update_plot <- function(experiment, plotter, name, ...) {
  experiment$update_plot(plotter, name, ...)
  return(experiment)
}

#' @rdname update_funs
#'
#' @export
update_vary_across <- function(experiment, dgp = NULL, method = NULL,
                               param_name, param_values) {
  experiment$update_vary_across(dgp = dgp, method = method,
                                param_name = param_name,
                                param_values = param_values)
  return(experiment)
}

#' Remove variable parameters from an \code{Experiment}.
#'
#' @param experiment An \code{Experiment} object.
#'
#' @return The original \code{experiment} object passed to \code{remove_vary_across}.
#'
#' @name remove_vary_across
#'
#' @export
remove_vary_across <- function(experiment) {
  experiment$remove_vary_across()
  return(experiment)
}

#' Set base directory to use for saving results and plots from an
#' \code{Experiment}.
#'
#' @param experiment An \code{Experiment} object.
#' @param save_dir A directory to use for saving `experiment's` results.
#'
#' @return The original \code{experiment} object passed to \code{set_save_dir}.
#'
#' @name set_save_dir
#'
#' @export
set_save_dir <- function(experiment, save_dir) {
  experiment$set_save_dir(save_dir)
  return(experiment)
}

#' Create an R Markdown file summarizing the results of an \code{Experiment}.
#'
#' @param experiment An \code{Experiment} object.
#' @param experiment_dirname A directory where results from an \code{Experiment}
#'   were previously saved. Used if \code{experiment} was not provided.
#' @param open If \code{TRUE}, open the R Markdown file in a web browser.
#'
#' @return The original \code{experiment} object passed to \code{create_rmd}.
#'
#' @name create_rmd
#'
#' @export
create_rmd <- function(experiment, experiment_dirname, open = TRUE) {
  if (missing(experiment) & missing(experiment_dirname)) {
    stop("Must provide argument for one of experiment or experiment_dirname")
  }
  if (missing(experiment)) {
    experiment <- readRDS(file.path(experiment_dirname, "experiment.rds"))
  }
  experiment$create_doc_template()
  experiment$create_rmd(open = open)
}
