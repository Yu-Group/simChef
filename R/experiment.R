#' \code{R6} class representing a simulation experiment.
#'
#' @docType class
#'
#' @description A simulation experiment with any number of \code{DGPs},
#'   \code{Methods}, \code{Evaluators}, and \code{Visualizers}.
#'
#' @details When run, an \code{Experiment} seamlessly combines \code{DGPs} and
#'   \code{Methods}, computing results in parallel. Those results can then be
#'   evaluated using \code{Evaluators} and visualized using \code{Visualizers}.
#'
#' @template experiment-template
#'
#' @export
Experiment <- R6::R6Class(
  classname = 'Experiment',
  private = list(
    .save_dir = NULL,
    .dgp_list = list(),
    .method_list = list(),
    .evaluator_list = list(),
    .visualizer_list = list(),
    .vary_across_list = list(
      dgp = list(),
      method = list()
    ),
    .fit_params = tibble::tibble(),
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
      list_name <- paste0(".", field_name, "_list")
      if (is.null(obj_name)) {
        private[[list_name]] <- list()
      } else if (is.null(obj_list[[obj_name]])) {
        stop(
          sprintf("Cannot remove '%s'. ", obj_name),
          sprintf("The name '%s' does not exist in the %s list. ",
                  obj_name, field_name),
          call. = FALSE
        )
      } else {
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
        err_msg <- sprintf("%s must be an instance of simChef::%s",
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
    .check_vary_across = function(.dgp, .method, ...) {
      dots_list <- rlang::list2(...)
      if (missing(.dgp) && missing(.method)) {
        stop("Must specify either dgp or method.")
      } else if ((!missing(.dgp)) + (!missing(.method)) != 1) {
        stop("Must specify one of dgp or method, but not both")
      } else if (!missing(.dgp)) {
        obj <- .dgp
        field_name <- "dgp"
        class_name <- "DGP"
      } else if (!missing(.method)) {
        obj <- .method
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
    .has_vary_across = function() {
      if ((length(private$.vary_across_list$dgp) == 0) &&
          (length(private$.vary_across_list$method) == 0)) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    },
    .get_vary_params = function(field_name = c("dgp", "method")) {
      field_name <- match.arg(field_name, several.ok = TRUE)
      param_names_ls <- purrr::map(private$.vary_across_list[field_name],
                                   function(x) {
                                     if (identical(x, list())) {
                                       return(NULL)
                                     } else {
                                       return(purrr::map(x, names) %>%
                                                purrr::reduce(c))
                                     }
                                   })
      param_names <- purrr::reduce(param_names_ls, c)
      # fix duplicate names in dgp and method vary across components
      if (all(c("dgp", "method") %in% field_name)) {
        same_names <- unique(intersect(param_names_ls[[1]], 
                                       param_names_ls[[2]]))
        if (length(same_names) >= 1) {
          unique_param_names <- setdiff(param_names, same_names)
          same_param_names <- c(paste0(same_names, "_dgp"),
                                paste0(same_names, "_method"))
          param_names <- c(unique_param_names, same_param_names)
        }
      }
      return(param_names)
    },
    .get_duplicate_param_names = function() {
      dgp_params <- private$.get_vary_params("dgp")
      method_params <- private$.get_vary_params("method")
      return(intersect(dgp_params, method_params))
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
        names(obj_params)[1] <- paste0(".", field_name, "_name")
        param_grid <- purrr::cross(obj_params)
        return(param_grid)
      }) %>% purrr::reduce(c)
      return(param_list)
    },
    .update_fit_params = function() {
      # update/set (dgp, method) fit parameter combinations
      dgp_list <- private$.get_obj_list("dgp")
      dgp_params_list <- private$.combine_vary_params("dgp")
      method_list <- private$.get_obj_list("method")
      method_params_list <- private$.combine_vary_params("method")
      fit_params <- purrr::cross2(dgp_params_list, method_params_list) %>%
        purrr::map_dfr(
          ~tibble::tibble(
            .dgp_name = .x[[1]]$.dgp_name,
            .dgp_fun = list(deparse(dgp_list[[.dgp_name]]$dgp_fun)),
            .dgp_params = list(dgp_list[[.dgp_name]]$dgp_params),
            .dgp = .x[1],
            .method_name = .x[[2]]$.method_name,
            .method_fun = list(deparse(method_list[[.method_name]]$method_fun)),
            .method_params = list(method_list[[.method_name]]$method_params),
            .method = .x[2]
          )
        )
      private$.fit_params <- fit_params
    },
    .get_fit_params = function(cached_params = NULL,
                               type = c("all", "cached", "new"),
                               n_reps = NULL, simplify = FALSE) {
      # get all/new/cached (dgp, method) fit parameter combinations
      type <- match.arg(type)
      fit_params <- private$.fit_params
      if (identical(type, "all")) {
        out_params <- fit_params
      } else {
        if (nrow(cached_params$fit) == 0) {
          if (identical(type, "cached")) {
            out_params <- cached_params$fit
          } else if (identical(type, "new")) {
            out_params <- fit_params
          }
        } else {
          cached_idxs <- dplyr::bind_rows(
            fit_params,
            cached_params$fit %>%
              dplyr::filter(as.numeric(.n_reps) >= n_reps) %>%
              dplyr::select(-.n_reps)
          ) %>%
            duplicated(fromLast = TRUE)
          if (identical(type, "cached")) {
            out_params <- fit_params[cached_idxs[1:nrow(fit_params)], ]
          } else if (identical(type, "new")) {
            out_params <- fit_params[!cached_idxs[1:nrow(fit_params)], ]
          }
        }
      }

      if (simplify) {
        duplicate_param_names <- private$.get_duplicate_param_names()
        for (param_name in private$.get_vary_params("dgp")) {
          # fix naming if also in method vary across
          col_name <- ifelse(param_name %in% duplicate_param_names,
                             paste0(param_name, "_dgp"),
                             param_name)
          out_params[[col_name]] <- purrr::map(out_params$.dgp,
                                               ~.x[[param_name]])
        }
        for (param_name in private$.get_vary_params("method")) {
          # fix naming if also in dgp vary across
          col_name <- ifelse(param_name %in% duplicate_param_names,
                             paste0(param_name, "_method"),
                             param_name)
          out_params[[col_name]] <- purrr::map(out_params$.method,
                                               ~.x[[param_name]])
        }
        out_params <- out_params %>%
          dplyr::select(-.dgp, -.dgp_fun, -.dgp_params,
                        -.method, -.method_fun, -.method_params) %>%
          simplify_tibble()
      }
      return(out_params)
    },
    .get_new_dgp_params = function(method_params, new_fit_params) {
      # get new dgp parameter combinations given method parameter set
      dgp_params_list <- new_fit_params %>%
        dplyr::filter(sapply(.method, identical, method_params)) %>%
        dplyr::pull(.dgp)
      return(dgp_params_list)
    },
    .get_new_method_params = function(dgp_params, new_fit_params) {
      # get new method parameter combinations given dgp parameter set
      method_params_list <- new_fit_params %>%
        dplyr::filter(sapply(.dgp, identical, dgp_params)) %>%
        dplyr::pull(.method)
      return(method_params_list)
    },
    .get_new_obj_list = function(cached_params,
                                 field_name = c("dgp", "method",
                                                "evaluator", "visualizer"),
                                 new_fit_params = NULL) {
      # get new uncached objects for a certain class in the Experiment
      field_name <- match.arg(field_name)
      if (field_name %in% c("dgp", "method")) {
        return(unique(new_fit_params[[paste0(".", field_name)]]))
      } else if (identical(field_name, "evaluator")) {
        evaluator_list <- private$.get_obj_list("evaluator")
        evaluate_params <- tibble::tibble(
          .eval_name = names(evaluator_list),
          .eval_fun = purrr::map(evaluator_list, ~deparse(.x$eval_fun)),
          .eval_params = purrr::map(evaluator_list, "eval_params")
        )
        cached_idxs <- dplyr::bind_rows(evaluate_params,
                                        cached_params$evaluate) %>%
          duplicated(fromLast = TRUE)
        return(evaluator_list[
          evaluate_params$.eval_name[!cached_idxs[1:nrow(evaluate_params)]]
        ])
      } else if (identical(field_name, "visualizer")) {
        visualizer_list <- private$.get_obj_list("visualizer")
        visualize_params <- tibble::tibble(
          .viz_name = names(visualizer_list),
          .viz_fun = purrr::map(visualizer_list, ~deparse(.x$viz_fun)),
          .viz_params = purrr::map(visualizer_list, "viz_params")
        )
        cached_idxs <- dplyr::bind_rows(visualize_params,
                                        cached_params$visualize) %>%
          duplicated(fromLast = TRUE)
        return(visualizer_list[
          visualize_params$.viz_name[!cached_idxs[1:nrow(visualize_params)]]
        ])
      }
    },
    .n_reps_cached = function(cached_fit_params) {
      if (nrow(cached_fit_params) == 0) {
        return(0)
      }

      fit_params <- private$.get_fit_params()
      fit_cached <- compare_tibble_rows(
        fit_params, 
        cached_fit_params %>% dplyr::select(-.n_reps),
        op = "contained_in"
      )
      
      if (!fit_cached) {
        return(0)
      } else {
        n_reps_complete <- fit_params %>%
          dplyr::left_join(y = cached_fit_params, by = colnames(fit_params)) %>%
          dplyr::pull(.n_reps) %>%
          as.numeric()
        return(min(n_reps_complete))
      }
    },
    .is_fully_cached = function(cached_params,
                                results_type = c("fit", "eval", "viz"),
                                n_reps) {
      # has the Experiment been completely cached? Returns TRUE, FALSE, or NULL,
      # where NULL means the cached experimental setup is a mismatch and need
      # to switch from use_cached = TRUE to use_cached = FALSE
      results_type <- match.arg(results_type)

      if (identical(results_type, "fit")) {
        n_reps_cached <- private$.n_reps_cached(cached_params$fit)
        return(n_reps_cached >= n_reps)
      }

      if (nrow(cached_params$fit) == 0) {
          return(NULL)
      }

      cached_fit_params <- cached_params$fit %>%
        dplyr::filter(as.numeric(.n_reps) == n_reps) %>%
        dplyr::select(-.n_reps)
      fit_cached_op <- "equal"
      if (identical(results_type, "eval")) {
        eval_cached_op <- "contained_in"
      } else if (identical(results_type, "viz")) {
        eval_cached_op <- "equal"
        viz_cached_op <- "contained_in"
      }

      fit_params <- private$.get_fit_params()
      fit_cached <- compare_tibble_rows(fit_params, cached_fit_params,
                                        op = fit_cached_op)
      if (!fit_cached) {
        return(NULL)
      }

      evaluator_list <- private$.get_obj_list("evaluator")
      evaluate_params <- tibble::tibble(
        .eval_name = names(evaluator_list),
        .eval_fun = purrr::map(evaluator_list, ~deparse(.x$eval_fun)),
        .eval_params = purrr::map(evaluator_list, "eval_params")
      )
      eval_cached <- compare_tibble_rows(evaluate_params,
                                         cached_params$evaluate,
                                         op = eval_cached_op)
      if (identical(results_type, "eval")) {
        return(eval_cached)
      } else if (!eval_cached) {
        return(NULL)
      }

      visualizer_list <- private$.get_obj_list("visualizer")
      visualize_params <- tibble::tibble(
        .viz_name = names(visualizer_list),
        .viz_fun = purrr::map(visualizer_list, ~deparse(.x$viz_fun)),
        .viz_params = purrr::map(visualizer_list, "viz_params")
      )
      visualize_cached <- compare_tibble_rows(visualize_params,
                                              cached_params$visualize,
                                              op = viz_cached_op)
      return(visualize_cached)
    },
    .get_cached_results = function(results_type = c("experiment",
                                                    "experiment_cached_params",
                                                    "fit", "eval", "viz"),
                                   verbose = 1) {
      results_type <- match.arg(results_type)
      if (verbose >= 1) {
        if (results_type %in% c("fit", "eval", "viz")) {
          message(sprintf("Reading in cached %s results...", results_type))
        } else if (identical(results_type, "experiment")) {
          message(sprintf("Reading in the cached experiment..."))
        } else if (identical(results_type, "experiment_cached_params")) {
          message(sprintf("Reading in the cached experiment parameters..."))
        }
      }
      if (!private$.has_vary_across()) {
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
      if (results_type %in% c("fit", "eval", "viz")) {
        save_file <- file.path(save_dir, paste0(results_type, "_results.rds"))
        if (results_type == "fit") {
          save_file2 <- file.path(save_dir, 
                                  paste0(results_type,
                                         "_results_extra_cached_reps.rds"))
        }
      } else {
        save_file <- file.path(save_dir, paste0(results_type, ".rds"))
      }
      if (file.exists(save_file)) {
        res <- readRDS(save_file)
        if (results_type == "fit") {
          if (file.exists(save_file2)) {
            res <- dplyr::bind_rows(res, readRDS(save_file2))
          }
        }
        return(res)
      } else {
        if (verbose >= 1) {
          if (results_type %in% c("fit", "eval", "viz")) {
            message(sprintf("Cannot find cached %s results.", results_type))
          } else {
            message("Cannot find cache.")
          }
        }
        return(NULL)
      }
    },
    .clear_cache = function() {
      if (!private$.has_vary_across()) {
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
      params_fpath <- file.path(save_dir, "experiment_cached_params.rds")
      fits_fpath <- file.path(save_dir, "fit_results_extra_cached_reps.rds")
      if (file.exists(params_fpath)) {
        file.remove(params_fpath)
      }
      if (file.exists(fits_fpath)) {
        file.remove(fits_fpath)
      }
    },
    .get_cache = function(results_type = c("all", "fit", "evaluate", 
                                           "visualize")) {
      results_type <- match.arg(results_type)
      cached_params <- private$.get_cached_results("experiment_cached_params",
                                                   verbose = 0)
      if (is.null(cached_params)) {
        cached_params <- list(
          fit = list(fit = tibble::tibble()),
          evaluate = list(fit = tibble::tibble(),
                          evaluate = tibble::tibble()),
          visualize = list(fit = tibble::tibble(),
                           evaluate = tibble::tibble(),
                           visualize = tibble::tibble())
        )
      }
      if (identical(results_type, "all")) {
        return(cached_params)
      } else {
        return(cached_params[[results_type]])
      }
    },
    .update_cache = function(results_type = c("fit", "eval", "viz"),
                             n_reps = NULL) {
      results_type <- match.arg(results_type)
      cached_params <- list()
      cached_params_all <- private$.get_cache("all")
      cached_params$fit <- private$.get_fit_params() %>%
        dplyr::mutate(.n_reps = n_reps)
      if (identical(results_type, "fit")) {
        if (nrow(cached_params_all$fit$fit) > 0) {
          cached_params_all$fit$fit <- cached_params$fit %>%
            dplyr::left_join(cached_params_all$fit$fit,
                             by = c(".dgp_name", ".dgp_fun", ".dgp_params",
                                    ".dgp", ".method_name", ".method_fun",
                                    ".method_params", ".method")) %>%
            dplyr::mutate(.n_reps = pmax(.n_reps.x, .n_reps.y, na.rm = T)) %>%
            dplyr::select(-.n_reps.x, -.n_reps.y)
        } else {
          cached_params_all$fit$fit <- cached_params$fit
        }
        return(cached_params_all)
      }

      evaluator_list <- private$.get_obj_list("evaluator")
      cached_params$evaluate <- tibble::tibble(
        .eval_name = names(evaluator_list),
        .eval_fun = purrr::map(evaluator_list, ~deparse(.x$eval_fun)),
        .eval_params = purrr::map(evaluator_list, "eval_params")
      )
      if (identical(results_type, "eval")) {
        cached_params_all$evaluate <- cached_params
        return(cached_params_all)
      }

      visualizer_list <- private$.get_obj_list("visualizer")
      cached_params$visualize <- tibble::tibble(
        .viz_name = names(visualizer_list),
        .viz_fun = purrr::map(visualizer_list, ~deparse(.x$viz_fun)),
        .viz_params = purrr::map(visualizer_list, "viz_params")
      )
      cached_params_all$visualize <- cached_params
      return(cached_params_all)
    },
    .save_results = function(results,
                             results_type = c("fit", "eval", "viz"),
                             n_reps, verbose = 1, checkpoint = FALSE) {
      results_type <- match.arg(results_type)
      if (verbose >= 1) {
        if (checkpoint) {
          message(sprintf("Saving %s results checkpoint...", results_type))
        } else {
          message(sprintf("Saving %s results...", results_type))
        }
        start_time <- Sys.time()
      }
      if (!private$.has_vary_across()) {
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
      save_file2 <- file.path(save_dir, 
                              paste0(results_type, 
                                     "_results_extra_cached_reps.rds"))
      if (!dir.exists(dirname(save_file))) {
        dir.create(dirname(save_file), recursive = TRUE)
      }
      cached_params <- private$.update_cache(results_type = results_type,
                                             n_reps = n_reps)
      saveRDS(cached_params,
              file.path(save_dir, "experiment_cached_params.rds"))
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      if (results_type == "fit") {
        main_results <- results %>% dplyr::filter(as.numeric(.rep) <= n_reps)
        saveRDS(main_results, save_file)
        if (nrow(main_results) < nrow(results)) {
          extra_results <- results %>% dplyr::filter(as.numeric(.rep) > n_reps)
          saveRDS(extra_results, save_file2)
        } else if (file.exists(save_file2)) {
          file.remove(save_file2)
        }
      } else {
        saveRDS(results, save_file)
      }
      if (verbose >= 1) {
        message(sprintf("%s results saved | time taken: %f seconds",
                        R.utils::capitalize(results_type),
                        difftime(Sys.time(), start_time, units = "secs")))
      }
    },
    .in_error_state = FALSE,
    .do_call_handler = function(name, fun, params = list(), verbose = 1) {
      # the goal of this function is to catch and return errors from
      # do_call_handler() and to set .in_error_state to TRUE for the worker's
      # copy of this Experiment, which will be checked prior to costly
      # computation in Experiment$fit
      result <- tryCatch(
        error = identity,
        do_call_handler(name, fun, params, verbose)
      )
      if ("simChefError" %in% class(result)) {
        private$.in_error_state <- TRUE
      }
      return(result)
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
    name = NULL,
    initialize = function(name = "experiment",
                          dgp_list = list(), method_list = list(),
                          evaluator_list = list(), visualizer_list = list(),
                          future.globals = TRUE, future.packages = NULL,
                          clone_from = NULL, save_dir = NULL, ...) {
      if (!is.null(clone_from)) {
        private$.check_obj(clone_from, "Experiment")
        clone <- clone_from$clone(deep = TRUE)
        dgp_list <- c(clone$get_dgps(), dgp_list)
        method_list <- c(clone$get_methods(), method_list)
        evaluator_list <- c(clone$get_evaluators(), evaluator_list)
        visualizer_list <- c(clone$get_visualizers(), visualizer_list)
        if (is.null(save_dir)) {
          save_dir <- clone$get_save_dir()
        }
      }
      private$.add_obj_list(dgp_list, "DGP")
      private$.add_obj_list(method_list, "Method")
      private$.add_obj_list(evaluator_list, "Evaluator")
      private$.add_obj_list(visualizer_list, "Visualizer")
      private$.future.globals <- future.globals
      private$.future.packages <- future.packages
      self$name <- name
      if (is.null(save_dir)) {
        save_dir <- file.path("results", name)
      }
      private$.save_dir <- R.utils::getAbsolutePath(save_dir)
    },
    run = function(n_reps = 1, parallel_strategy = c("reps"),
                   future.globals = NULL, future.packages = NULL,
                   future.seed = TRUE, use_cached = FALSE, 
                   return_all_cached_reps = FALSE, save = FALSE,
                   checkpoint_n_reps = 0, verbose = 1, ...) {
      if (!is.logical(save)) {
        save <- c("fit", "eval", "viz") %in% save
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
                              future.seed = future.seed,
                              use_cached = use_cached,
                              return_all_cached_reps = return_all_cached_reps,
                              save = save[1], 
                              checkpoint_n_reps = checkpoint_n_reps,
                              verbose = verbose, ...)
      eval_results <- self$evaluate(fit_results = fit_results %>%
                                      dplyr::filter(as.numeric(.rep) <= n_reps),
                                    use_cached = use_cached, save = save[2],
                                    verbose = verbose, ...)
      viz_results <- self$visualize(fit_results = fit_results %>%
                                      dplyr::filter(as.numeric(.rep) <= n_reps),
                                eval_results = eval_results,
                                use_cached = use_cached, save = save[3],
                                verbose = verbose, ...)
      return(list(fit_results = fit_results,
                  eval_results = eval_results,
                  viz_results = viz_results))
    },
    generate_data = function(n_reps = 1, ...) {
      # TODO: generate data that was used in run() or fit() (e.g., w/ same seed)
      dgp_list <- private$.get_obj_list("dgp")
      if (length(dgp_list) == 0) {
        private$.throw_empty_list_error("dgp", "generate data from")
      }

      if (!private$.has_vary_across()) {
        dgp_results <- purrr::map(dgp_list, function(dgp) {
          replicates <- replicate(n_reps, {
            return(dgp$generate())
          }, simplify = FALSE)
        })
      } else {
        dgp_params_list <- private$.combine_vary_params("dgp")
        dgp_names <- purrr::map_chr(dgp_params_list, ".dgp_name") %>%
          unique() %>%
          setNames(., .)
        dgp_results <- purrr::map(dgp_names, function(dgp_name) {
          keep_dgps <- purrr::map_chr(dgp_params_list, ".dgp_name") == dgp_name
          keep_dgp_params_list <- dgp_params_list[keep_dgps]
          purrr::map(keep_dgp_params_list, function(dgp_params) {
            dgp_params$.dgp_name <- NULL
            replicates <- replicate(n_reps, {
              sim_data <- do.call(dgp_list[[dgp_name]]$generate, dgp_params)
              return(sim_data)
            }, simplify = FALSE)
            attr(replicates, "params") <- dgp_params
            replicates
          })
        })
      }
      return(dgp_results)
    },
    fit = function(n_reps = 1, parallel_strategy = c("reps"),
                   future.globals = NULL, future.packages = NULL,
                   future.seed = TRUE, use_cached = FALSE,
                   return_all_cached_reps = FALSE, save = FALSE,
                   checkpoint_n_reps = 0, verbose = 1, ...) {

      dgp_list <- private$.get_obj_list("dgp")
      method_list <- private$.get_obj_list("method")
      if (length(dgp_list) == 0) {
        private$.throw_empty_list_error("dgp", "generate data from")
      }
      if (length(method_list) == 0) {
        private$.throw_empty_list_error("method", "fit methods in")
      }
      private$.update_fit_params()

      if (!is.numeric(checkpoint_n_reps)) {
        checkpoint <- FALSE
      } else {
        checkpoint <- isTRUE(checkpoint_n_reps > 0)
      }

      n_reps_cached <- 0
      n_reps_total <- n_reps
      fit_results <- data.frame()
      if (checkpoint) {
        n_reps <- round(checkpoint_n_reps)
      }

      if (use_cached || checkpoint) {
        cached_params <- private$.get_cache("fit")
        n_reps_cached <- private$.n_reps_cached(cached_params$fit)
        if (n_reps_cached > 0) {
          results <- private$.get_cached_results("fit", verbose = verbose)
          fit_params <- private$.get_fit_params(simplify = TRUE)
          fit_results <- dplyr::inner_join(x = results,
                                           y = fit_params,
                                           by = colnames(fit_params)) %>%
            dplyr::arrange(as.numeric(.rep), .dgp_name, .method_name)
          if (save) {
            n_reps_cached <- min(n_reps_total, n_reps_cached)
            private$.save_results(fit_results, "fit", n_reps_cached, verbose)
          }
          if (n_reps_cached >= n_reps_total) {
            if (verbose >= 1) {
              message("==============================")
            }
            if (use_cached && return_all_cached_reps) {
              return(fit_results)
            } else {
              return(fit_results %>%
                       dplyr::filter(as.numeric(.rep) <= n_reps_total))
            }
          }
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

      if (is.null(future.packages)) {
        future.packages <- private$.future.packages
      }
      if (is.null(future.globals)) {
        future.globals <- private$.future.globals
      }

      dgp_params_list <- private$.combine_vary_params("dgp")
      method_params_list <- private$.combine_vary_params("method")

      check_cache <- FALSE
      if (use_cached) {
        new_fit_params <- private$.get_fit_params(
          cached_params, "new", n_reps
        )
        n_params <- nrow(new_fit_params)
        new_fit <- n_params == nrow(private$.get_fit_params())
        if (!new_fit) {
          # get only the new dgps and methods that are not cached
          dgp_params_list <- private$.get_new_obj_list(
            cached_params, "dgp", new_fit_params = new_fit_params
          )
          method_params_list <- private$.get_new_obj_list(
            cached_params, "method", new_fit_params = new_fit_params
          )
          if (length(dgp_params_list)*length(method_params_list) != n_params) {
            # case when not all combos of (dgp_params_list, method_params_list)
            # need to be rerun so need to check cache ids when fitting
            check_cache <- TRUE
          }
        }
      }

      duplicate_param_names <- private$.get_duplicate_param_names()
      progressr::handlers("progress")

      while (n_reps_cached < n_reps_total) {
        n_reps <- min(n_reps, n_reps_total - n_reps_cached)

        new_fit_results <- switch(
          parallel_strategy,
          "reps" = {
            p <- progressr::progressor(steps = n_reps)
            results <- future.apply::future_replicate(n_reps, {
              p()
              if (private$.in_error_state) {
                return(NULL)
              }
              purrr::map(dgp_params_list, function(dgp_params) {
                if (private$.in_error_state) {
                  return(NULL)
                }
                if (check_cache) {
                  method_params_list <- private$.get_new_method_params(
                    dgp_params, new_fit_params
                  )
                }
                dgp_name <- dgp_params$.dgp_name
                dgp_params$.dgp_name <- NULL
                data_list <- private$.do_call_handler(
                  dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                )
                if ("simChefError" %in% class(data_list)) {
                  return(tibble::tibble(.err = list(data_list)))
                }
                purrr::map(method_params_list, function(method_params) {
                  if (private$.in_error_state) {
                    return(NULL)
                  }
                  method_name <- method_params$.method_name
                  param_df <- fix_duplicate_param_names(
                    dgp_params = c(.dgp_name = dgp_name, dgp_params),
                    method_params = method_params,
                    duplicate_param_names = duplicate_param_names
                  ) %>%
                    list_to_tibble_row() %>%
                    simplify_tibble()
                  method_params$.method_name <- NULL
                  method_params$data_list <- data_list
                  result <- private$.do_call_handler(
                    method_name, method_list[[method_name]]$fit, method_params, verbose
                  )
                  if ("simChefError" %in% class(result)) {
                    return(tibble::tibble(.err = list(result)))
                  }
                  result <- result %>%
                    tibble::add_column(
                      param_df, .before = 1,
                      .name_repair = ~check_results_names(., method_name)
                    )
                  return(result)
                }) %>%
                  data.table::rbindlist(fill = TRUE)
              }) %>%
                data.table::rbindlist(fill = TRUE)
            },
            simplify = FALSE,
            future.globals = future.globals,
            future.packages = future.packages,
            future.seed = future.seed) %>%
              dplyr::bind_rows(.id = ".rep") %>%
              tibble::as_tibble()
          },
          "dgps" = {
            p <- progressr::progressor(steps = length(dgp_params_list)*n_reps)
            results <- future.apply::future_lapply(
              dgp_params_list, function(dgp_params) {
                if (private$.in_error_state) {
                  return(NULL)
                }
                if (check_cache) {
                  method_params_list <- private$.get_new_method_params(
                    dgp_params, new_fit_params
                  )
                }
                reps <- replicate(n_reps, {
                  p()
                  if (private$.in_error_state) {
                    return(NULL)
                  }
                  dgp_name <- dgp_params$.dgp_name
                  dgp_params$.dgp_name <- NULL
                  data_list <- private$.do_call_handler(
                    dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                  )
                  if ("simChefError" %in% class(data_list)) {
                    return(tibble::tibble(.err = list(data_list)))
                  }
                  purrr::map(method_params_list, function(method_params) {
                    if (private$.in_error_state) {
                      return(NULL)
                    }
                    method_name <- method_params$.method_name
                    param_df <- fix_duplicate_param_names(
                      dgp_params = c(.dgp_name = dgp_name, dgp_params),
                      method_params = method_params,
                      duplicate_param_names = duplicate_param_names
                    ) %>%
                      list_to_tibble_row() %>%
                      simplify_tibble()
                    method_params$.method_name <- NULL
                    method_params$data_list <- data_list
                    result <- private$.do_call_handler(
                      method_name, method_list[[method_name]]$fit, method_params,
                      verbose
                    )
                    if ("simChefError" %in% class(result)) {
                      return(tibble::tibble(.err = list(result)))
                    }
                    result <- result %>%
                      tibble::add_column(
                        param_df, .before = 1,
                        .name_repair = ~check_results_names(., method_name)
                      )
                    return(result)
                  }) %>%
                    data.table::rbindlist(fill = TRUE)
                }, simplify=FALSE)
                dplyr::bind_rows(reps, .id = ".rep")
              },
              future.globals = future.globals,
              future.packages = future.packages,
              future.seed = future.seed)
            data.table::rbindlist(results, fill = TRUE) %>%
              tibble::as_tibble()
          },
          "methods" = {
            p <- progressr::progressor(steps = length(method_params_list)*n_reps)
            results <- future.apply::future_lapply(
              method_params_list, function(method_params) {
                if (private$.in_error_state) {
                  return(NULL)
                }
                if (check_cache) {
                  dgp_params_list <- private$.get_new_dgp_params(
                    method_params, new_fit_params
                  )
                }
                reps <- replicate(n_reps, {
                  p()
                  purrr::map(dgp_params_list, function(dgp_params) {
                    if (private$.in_error_state) {
                      return(NULL)
                    }
                    dgp_name <- dgp_params$.dgp_name
                    dgp_params$.dgp_name <- NULL
                    data_list <- private$.do_call_handler(
                      dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                    )
                    if ("simChefError" %in% class(data_list)) {
                      return(tibble::tibble(.err = list(data_list)))
                    }
                    method_name <- method_params$.method_name
                    param_df <- fix_duplicate_param_names(
                      dgp_params = c(.dgp_name = dgp_name, dgp_params),
                      method_params = method_params,
                      duplicate_param_names = duplicate_param_names
                    ) %>%
                      list_to_tibble_row() %>%
                      simplify_tibble()
                    method_params$.method_name <- NULL
                    method_params$data_list <- data_list
                    result <- private$.do_call_handler(
                      method_name, method_list[[method_name]]$fit, method_params,
                      verbose
                    )
                    if ("simChefError" %in% class(result)) {
                      return(tibble::tibble(.err = list(result)))
                    }
                    result <- result %>%
                      tibble::add_column(
                        param_df, .before = 1,
                        .name_repair = ~check_results_names(., method_name)
                      )
                    return(result)
                  }) %>%
                    data.table::rbindlist(fill = TRUE)
                }, simplify = FALSE)
                dplyr::bind_rows(reps, .id = ".rep")
              },
              future.globals = future.globals,
              future.packages = future.packages,
              future.seed = future.seed)
            data.table::rbindlist(results, fill = TRUE) %>%
              tibble::as_tibble()
          },
          "reps+dgps" = {
            n_dgps <- length(dgp_params_list)
            dgp_params_list <- rep(dgp_params_list, times = n_reps)
            p <- progressr::progressor(along = dgp_params_list)
            results <- future.apply::future_lapply(
              dgp_params_list,
              function(dgp_params) {
                p()
                if (private$.in_error_state) {
                  return(NULL)
                }
                if (check_cache) {
                  method_params_list <- private$.get_new_method_params(
                    dgp_params, new_fit_params
                  )
                }
                dgp_name <- dgp_params$.dgp_name
                dgp_params$.dgp_name <- NULL
                data_list <- private$.do_call_handler(
                  dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                )
                if ("simChefError" %in% class(data_list)) {
                  return(tibble::tibble(.err = list(data_list)))
                }
                purrr::map(method_params_list, function(method_params) {
                  if (private$.in_error_state) {
                    return(NULL)
                  }
                  method_name <- method_params$.method_name
                  param_df <- fix_duplicate_param_names(
                    dgp_params = c(.dgp_name = dgp_name, dgp_params),
                    method_params = method_params,
                    duplicate_param_names = duplicate_param_names
                  ) %>%
                    list_to_tibble_row() %>%
                    simplify_tibble()
                  method_params$.method_name <- NULL
                  method_params$data_list <- data_list
                  result <- private$.do_call_handler(
                    method_name, method_list[[method_name]]$fit, method_params,
                    verbose
                  )
                  if ("simChefError" %in% class(result)) {
                    return(tibble::tibble(.err = list(result)))
                  }
                  result <- result %>%
                    tibble::add_column(
                      param_df, .before = 1,
                      .name_repair = ~check_results_names(., method_name)
                    )
                  return(result)
                }) %>%
                  data.table::rbindlist(fill = TRUE)
              },
              future.seed = future.seed,
              future.globals = future.globals,
              future.packages = future.packages
            )
            # get correct rep number
            results <- purrr::map(1:length(results),
                                  function(i) {
                                    results[[i]]$.rep <- i
                                    return(results[[i]])
                                  })
            results <- data.table::rbindlist(results, fill = TRUE) %>%
              tibble::as_tibble() %>%
              dplyr::mutate(.rep = (.rep - 1) %/% n_dgps + 1)
            results
          },
          "reps+methods" = {
            n_methods <- length(method_params_list)
            method_params_list <- rep(method_params_list, times = n_reps)
            p <- progressr::progressor(along = method_params_list)
            results <- future.apply::future_lapply(
              method_params_list,
              function(method_params) {
                p()
                if (private$.in_error_state) {
                  return(NULL)
                }
                if (check_cache) {
                  dgp_params_list <- private$.get_new_dgp_params(
                    method_params, new_fit_params
                  )
                }
                purrr::map(dgp_params_list, function(dgp_params) {
                  if (private$.in_error_state) {
                    return(NULL)
                  }
                  dgp_name <- dgp_params$.dgp_name
                  dgp_params$.dgp_name <- NULL
                  data_list <- private$.do_call_handler(
                    dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                  )
                  if ("simChefError" %in% class(data_list)) {
                    return(tibble::tibble(.err = list(data_list)))
                  }
                  method_name <- method_params$.method_name
                  param_df <- fix_duplicate_param_names(
                    dgp_params = c(.dgp_name = dgp_name, dgp_params),
                    method_params = method_params,
                    duplicate_param_names = duplicate_param_names
                  ) %>%
                    list_to_tibble_row() %>%
                    simplify_tibble()
                  method_params$.method_name <- NULL
                  method_params$data_list <- data_list
                  result <- private$.do_call_handler(
                    method_name, method_list[[method_name]]$fit, method_params,
                    verbose
                  )
                  if ("simChefError" %in% class(result)) {
                    return(tibble::tibble(.err = list(result)))
                  }
                  result <- result %>%
                    tibble::add_column(
                      param_df, .before = 1,
                      .name_repair = ~check_results_names(., method_name)
                    )
                  return(result)
                }) %>%
                  data.table::rbindlist(fill = TRUE)
              },
              future.seed = future.seed,
              future.globals = future.globals,
              future.packages = future.packages
            )
            # get correct rep number
            results <- purrr::map(1:length(results),
                                  function(i) {
                                    results[[i]]$.rep <- i
                                    return(results[[i]])
                                  })
            results <- data.table::rbindlist(results, fill = TRUE) %>%
              tibble::as_tibble() %>%
              dplyr::mutate(.rep = (.rep - 1) %/% n_methods + 1)
            results
          },
          "dgps+methods" = {
            if (check_cache) {
              mapply_args <- purrr::map2(new_fit_params$.dgp,
                                         new_fit_params$.method,
                                         ~c(list(.x), list(.y)))
            } else {
              mapply_args <- purrr::cross2(
                dgp_params_list, method_params_list
              )
            }
            # shuffle the inputs to avoid bad load balancing
            mapply_args <- mapply_args[sample(1:length(mapply_args))]
            dgp_mapply_args <- lapply(mapply_args, `[[`, 1)
            method_mapply_args <- lapply(mapply_args, `[[`, 2)
            p <- progressr::progressor(steps = length(dgp_mapply_args)*n_reps)
            results <- future.apply::future_mapply(
              function(dgp_params, method_params) {
                if (private$.in_error_state) {
                  return(NULL)
                }
                reps <- replicate(n_reps, {
                  p()
                  if (private$.in_error_state) {
                    return(NULL)
                  }
                  dgp_name <- dgp_params$.dgp_name
                  dgp_params$.dgp_name <- NULL
                  data_list <- private$.do_call_handler(
                    dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                  )
                  if ("simChefError" %in% class(data_list)) {
                    return(tibble::tibble(.err = list(data_list)))
                  }
                  method_name <- method_params$.method_name
                  param_df <- fix_duplicate_param_names(
                    dgp_params = c(.dgp_name = dgp_name, dgp_params),
                    method_params = method_params,
                    duplicate_param_names = duplicate_param_names
                  ) %>%
                    list_to_tibble_row() %>%
                    simplify_tibble()
                  method_params$.method_name <- NULL
                  method_params$data_list <- data_list
                  result <- private$.do_call_handler(
                    method_name, method_list[[method_name]]$fit, method_params,
                    verbose
                  )
                  if ("simChefError" %in% class(result)) {
                    return(tibble::tibble(.err = list(result)))
                  }
                  result <- result %>%
                    tibble::add_column(
                      param_df, .before = 1,
                      .name_repair = ~check_results_names(., method_name)
                    )
                  return(result)
                }, simplify=FALSE)
                dplyr::bind_rows(reps, .id = ".rep")
              },
              dgp_mapply_args, method_mapply_args,
              future.seed = future.seed, SIMPLIFY = FALSE,
              future.globals = future.globals,
              future.packages = future.packages
            )
            data.table::rbindlist(results, fill = TRUE) %>%
              tibble::as_tibble()
          },
          "reps+dgps+methods" = {
            if (check_cache) {
              mapply_args <- purrr::map2(new_fit_params$.dgp,
                                         new_fit_params$.method,
                                         ~c(list(.x), list(.y)))
              mapply_args <- purrr::map(1:n_reps,
                                        function(i) {
                                          purrr::map(mapply_args,
                                                     ~c(list(i), .x))
                                        }) %>%
                purrr::reduce(c)
            } else {
              mapply_args <- purrr::cross3(
                1:n_reps, dgp_params_list, method_params_list
              )
            }
            # shuffle the inputs to avoid bad load balancing
            mapply_args <- mapply_args[sample(1:length(mapply_args))]
            reps <- sapply(mapply_args, `[[`, 1)
            dgp_mapply_args <- lapply(mapply_args, `[[`, 2)
            method_mapply_args <- lapply(mapply_args, `[[`, 3)
            p <- progressr::progressor(along = dgp_mapply_args)
            results <- future.apply::future_mapply(
              function(dgp_params, method_params) {
                p()
                if (private$.in_error_state) {
                  return(NULL)
                }
                dgp_name <- dgp_params$.dgp_name
                dgp_params$.dgp_name <- NULL
                data_list <- private$.do_call_handler(
                  dgp_name, dgp_list[[dgp_name]]$generate, dgp_params, verbose
                )
                if ("simChefError" %in% class(data_list)) {
                  return(tibble::tibble(.err = list(data_list)))
                }
                method_name <- method_params$.method_name
                param_df <- fix_duplicate_param_names(
                  dgp_params = c(.dgp_name = dgp_name, dgp_params),
                  method_params = method_params,
                  duplicate_param_names = duplicate_param_names
                ) %>%
                  list_to_tibble_row() %>%
                  simplify_tibble()
                method_params$.method_name <- NULL
                method_params$data_list <- data_list
                result <- private$.do_call_handler(
                  method_name, method_list[[method_name]]$fit, method_params,
                  verbose
                )
                if ("simChefError" %in% class(result)) {
                  return(tibble::tibble(.err = list(result)))
                }
                result <- result %>%
                  tibble::add_column(
                    param_df, .before = 1,
                    .name_repair = ~check_results_names(., method_name)
                  )
                return(result)
              },
              dgp_mapply_args, method_mapply_args,
              future.seed = future.seed, SIMPLIFY = FALSE,
              future.globals = future.globals,
              future.packages = future.packages
            )
            result_non_null <- !sapply(results, is.null)
            results <- data.table::rbindlist(results, fill = TRUE) %>%
              tibble::as_tibble() %>%
              dplyr::mutate(.rep = reps[result_non_null])
            results
          }
        ) %>%
          dplyr::mutate(.rep = as.numeric(.rep) + n_reps_cached)

        if (".err" %in% colnames(new_fit_results)) {
          private$.in_error_state <- FALSE
          if (save) {
            # TODO: save to disk
          }
          is_err <- sapply(new_fit_results$.err, function(err) {
            "simChefError" %in% class(err)
          })
          # get one of the errors
          err <- new_fit_results$.err[is_err][[1]]
          # filter out empty rows
          if (".dgp_name" %in% colnames(new_fit_results)) {
            not_err <- !sapply(new_fit_results$.dgp_name, is.na)
            new_fit_results <- new_fit_results[is_err | not_err, ]
          } else {
            new_fit_results <- new_fit_results[is_err, ]
          }
          # end the simulation with an error
          rlang::abort(
            paste0(
              "Error(s) encountered while running the simulation, ",
              "including:\n\n", err$message,
              "\n\nUse `rlang::last_error()$partial_results` to return partial results."
            ),
            class = "simChefError", partial_results=new_fit_results
          )
        }

        n_reps_cached <- n_reps_cached + n_reps

        attr(new_fit_results, ".internal.selfref") <- NULL
        col_diff <- setdiff(
          private$.get_vary_params(), colnames(new_fit_results)
        )
        for (col in col_diff) {
          new_fit_results[[col]] <- NA
        }
        fit_results <- simplify_tibble(new_fit_results) %>%
          dplyr::mutate(.rep = as.character(.rep)) %>%
          dplyr::select(.rep, .dgp_name, .method_name,
                        private$.get_vary_params(),
                        tidyselect::everything()) %>%
          dplyr::bind_rows(fit_results) %>%
          dplyr::arrange(as.numeric(.rep), .dgp_name, .method_name)

        if (use_cached && !new_fit) {
          fit_params_cached <- private$.get_fit_params(cached_params, "cached",
                                                       n_reps_total, TRUE)
          fit_results_cached <- private$.get_cached_results("fit",
                                                            verbose = verbose) %>%
            dplyr::inner_join(y = fit_params_cached,
                              by = colnames(fit_params_cached))
          if (verbose >= 1) {
            message("Appending cached results to the new fit results...")
          }
          fit_params <- private$.get_fit_params(simplify = TRUE)
          fit_params_cols <- colnames(fit_params)
          fit_results <- dplyr::bind_rows(fit_results, fit_results_cached) %>%
            dplyr::inner_join(y = fit_params, by = fit_params_cols) %>%
            dplyr::arrange(as.numeric(.rep), .dgp_name, .method_name)
        }

        if (verbose >= 1) {
          message(sprintf("%s reps completed (totals: %s/%s) | time taken: %f minutes",
                          n_reps, n_reps_cached, n_reps_total,
                          difftime(Sys.time(), start_time, units = "mins")))
        }
        if (save || checkpoint) {
          private$.save_results(
            fit_results, "fit", n_reps_cached, verbose,
            checkpoint && n_reps_cached < n_reps_total
          )
        }
      }
      if (verbose >= 1) {
        message("==============================")
      }

      if (use_cached && return_all_cached_reps) {
        return(fit_results)
      } else {
        return(fit_results %>% dplyr::filter(as.numeric(.rep) <= n_reps_total))
      }
    },
    evaluate = function(fit_results, use_cached = FALSE, save = FALSE,
                        verbose = 1, ...) {
      evaluator_list <- private$.get_obj_list("evaluator")
      evaluator_names <- names(evaluator_list)
      if (length(evaluator_list) == 0) {
        if (verbose >= 1) {
          message("No evaluators to evaluate. Skipping evaluation.")
          message("==============================")
        }
        return(NULL)
      }

      n_reps <- max(as.numeric(fit_results$.rep))
      private$.update_fit_params()
      if (use_cached) {
        cached_params <- private$.get_cache("evaluate")
        is_cached <- private$.is_fully_cached(cached_params, "eval", n_reps)
        if (isTRUE(is_cached)) {
          results <- private$.get_cached_results("eval", verbose = verbose)
          results <- results[names(private$.get_obj_list("evaluator"))]
          if (save) {
            if (!setequal(names(private$.get_obj_list("evaluator")),
                          names(results))) {
              private$.save_results(results, "eval", n_reps, verbose)
            }
          }
          if (verbose >= 1) {
            message("==============================")
          }
          return(results)
        } else if (is.null(is_cached)) {
          use_cached <- FALSE
        } else {
          evaluator_list <- private$.get_new_obj_list(cached_params,
                                                      "evaluator")
        }
      }

      if (verbose >= 1) {
        message(sprintf("Evaluating %s...", self$name))
        start_time <- Sys.time()
      }
      eval_results <- purrr::map2(
        names(evaluator_list), evaluator_list,
        function(name, evaluator) {
          do_call_handler(
            name, evaluator$evaluate,
            list(fit_results = fit_results,
                 vary_params = private$.get_vary_params()),
            verbose
          )
        }
      )
      names(eval_results) <- names(evaluator_list)
      if (use_cached && !setequal(names(evaluator_list), evaluator_names)) {
        eval_results_cached <- private$.get_cached_results("eval",
                                                           verbose = verbose)
        if (verbose >= 1) {
          message("Appending cached results to the new evaluation results...")
        }
        eval_results <- c(eval_results, eval_results_cached)[evaluator_names]
      }
      if (verbose >= 1) {
        message(sprintf("Evaluation completed | time taken: %f minutes",
                        difftime(Sys.time(), start_time, units = "mins")))
      }
      if (save) {
        private$.save_results(eval_results, "eval", n_reps, verbose)
      }
      if (verbose >= 1) {
        message("==============================")
      }

      return(eval_results)
    },
    visualize = function(fit_results, eval_results = NULL,
                         use_cached = FALSE, save = FALSE, verbose = 1, ...) {

      visualizer_list <- private$.get_obj_list("visualizer")
      visualizer_names <- names(visualizer_list)
      if (length(visualizer_list) == 0) {
        if (verbose >= 1) {
          message("No visualizers to visualize. Skipping visualization.")
          message("==============================")
        }
        return(NULL)
      }

      n_reps <- max(as.numeric(fit_results$.rep))
      private$.update_fit_params()
      if (use_cached) {
        cached_params <- private$.get_cache("visualize")
        is_cached <- private$.is_fully_cached(cached_params, "viz", n_reps)
        if (isTRUE(is_cached)) {
          results <- private$.get_cached_results("viz", verbose = verbose)
          results <- results[names(private$.get_obj_list("visualizer"))]
          if (save) {
            if (!setequal(names(private$.get_obj_list("visualizer")),
                          names(results))) {
              private$.save_results(results, "viz", n_reps, verbose)
            }
          }
          if (verbose >= 1) {
            message("==============================")
          }
          return(results)
        } else if (is.null(is_cached)) {
          use_cached <- FALSE
        } else {
          visualizer_list <- private$.get_new_obj_list(cached_params,
                                                       "visualizer")
        }
      }

      if (verbose >= 1) {
        message(sprintf("Visualizing %s...", self$name))
        start_time <- Sys.time()
      }
      viz_results <- purrr::map2(
        names(visualizer_list), visualizer_list,
        function(name, visualizer) {
          do_call_handler(
            name, visualizer$visualize,
            list(fit_results = fit_results,
                 eval_results = eval_results,
                 vary_params = private$.get_vary_params()),
            verbose
          )
        }
      )
      names(viz_results) <- names(visualizer_list)
      if (use_cached && !setequal(names(visualizer_list), visualizer_names)) {
        viz_results_cached <- private$.get_cached_results("viz",
                                                                verbose = verbose)
        if (verbose >= 1) {
          message("Appending cached results to the new visualization results...")
        }
        viz_results <- c(viz_results,
                               viz_results_cached)[visualizer_names]
      }
      if (verbose >= 1) {
        message(sprintf("Visualization completed | time taken: %f minutes",
                        difftime(Sys.time(), start_time, units = "mins")))
      }
      if (save) {
        private$.save_results(viz_results, "viz", n_reps, verbose)
      }
      if (verbose >= 1) {
        message("==============================")
      }

      return(viz_results)
    },
    add_dgp = function(dgp, name=NULL, ...) {
      private$.check_obj(dgp, "DGP")
      private$.add_obj("dgp", dgp, name)
      invisible(self)
    },
    update_dgp = function(dgp, name, ...) {
      private$.check_obj(dgp, "DGP")
      private$.update_obj("dgp", dgp, name)
      invisible(self)
    },
    remove_dgp = function(name = NULL, ...) {
      if (is.null(name)) {
        private$.vary_across_list[["dgp"]] <- list()
      } else {
        vary_across_sublist <- private$.vary_across_list[["dgp"]][[name]]
        if (!is.null(vary_across_sublist)) {
          self$remove_vary_across(dgp = name)
        }
      }
      private$.remove_obj("dgp", name)
      invisible(self)
    },
    get_dgps = function() {
      return(private$.get_obj_list("dgp"))
    },
    add_method = function(method, name=NULL, ...) {
      private$.check_obj(method, "Method")
      private$.add_obj("method", method, name)
      invisible(self)
    },
    update_method = function(method, name, ...) {
      private$.check_obj(method, "Method")
      private$.update_obj("method", method, name)
      invisible(self)
    },
    remove_method = function(name = NULL, ...) {
      if (is.null(name)) {
        private$.vary_across_list[["method"]] <- list()
      } else {
        vary_across_sublist <- private$.vary_across_list[["method"]][[name]]
        if (!is.null(vary_across_sublist)) {
          self$remove_vary_across(method = name)
        }
      }
      private$.remove_obj("method", name)
      invisible(self)
    },
    get_methods = function() {
      return(private$.get_obj_list("method"))
    },
    add_evaluator = function(evaluator, name = NULL, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.add_obj("evaluator", evaluator, name)
      invisible(self)
    },
    update_evaluator = function(evaluator, name, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.update_obj("evaluator", evaluator, name)
      invisible(self)
    },
    remove_evaluator = function(name = NULL, ...) {
      private$.remove_obj("evaluator", name)
      invisible(self)
    },
    get_evaluators = function() {
      return(private$.get_obj_list("evaluator"))
    },
    add_visualizer = function(visualizer, name=NULL, ...) {
      private$.check_obj(visualizer, "Visualizer")
      private$.add_obj("visualizer", visualizer, name)
      invisible(self)
    },
    update_visualizer = function(visualizer, name, ...) {
      private$.check_obj(visualizer, "Visualizer")
      private$.update_obj("visualizer", visualizer, name)
      invisible(self)
    },
    remove_visualizer = function(name = NULL, ...) {
      private$.remove_obj("visualizer", name)
      invisible(self)
    },
    get_visualizers = function() {
      return(private$.get_obj_list("visualizer"))
    },
    add_vary_across = function(.dgp, .method, ...) {
      temp <- private$.check_vary_across(.dgp, .method, ...)
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
    update_vary_across = function(.dgp, .method, ...) {
      temp <- private$.check_vary_across(.dgp, .method, ...)
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
    remove_vary_across = function(dgp, method, param_names = NULL) {
      if (missing(dgp) && missing(method)) {
        if (!private$.has_vary_across()) {
          stop(
            paste("Cannot remove all vary_across parameters ",
                  "since the vary_across parameter has not been set."),
            call. = FALSE
          )
        } else {
          private$.vary_across_list <- list(
            dgp = list(),
            method = list()
          )
          return(invisible(self))
        }
      }
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
      if (length(private$.vary_across_list[[field_name]]) == 0) {
        private$.vary_across_list[[field_name]] <- list()
      }
      invisible(self)
    },
    get_vary_across = function() {
      return(private$.vary_across_list)
    },
    clear_cache = function() {
      private$.clear_cache()
      return(invisible(self))
    },
    get_cached_results = function(results_type, verbose = 0) {
      return(private$.get_cached_results(results_type = results_type,
                                         verbose = verbose))
    },
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
        private[[list_name]][[name]]$rmd_show <- show
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
    get_save_dir = function() {
      return(private$.save_dir)
    },
    set_save_dir = function(save_dir) {
      private$.save_dir <- R.utils::getAbsolutePath(save_dir)
      invisible(self)
    },
    save = function() {
      save_dir <- self$get_save_dir()
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      invisible(self)
    },
    print = function() {
      cat("Experiment Name:", self$name, "\n")
      cat("   Saved results at:",
          R.utils::getRelativePath(private$.save_dir), "\n")
      cat("   DGPs:",
          paste(names(private$.get_obj_list("dgp")),
                sep = "", collapse = ", "), "\n")
      cat("   Methods:",
          paste(names(private$.get_obj_list("method")),
                sep = "", collapse = ", "), "\n")
      cat("   Evaluators:",
          paste(names(private$.get_obj_list("evaluator")),
                sep = "", collapse = ", "), "\n")
      cat("   Visualizers:",
          paste(names(private$.get_obj_list("visualizer")),
                sep = "", collapse = ", "), "\n")
      cat("   Vary Across: ")
      if (!private$.has_vary_across()) {
        cat("None\n")
      } else {
        vary_across_list <- private$.vary_across_list
        if (!is.null(names(vary_across_list$dgp)) |
            !is.null(names(vary_across_list$method))) {
          cat("\n")
        }
        for (dgp in names(vary_across_list$dgp)) {
          cat("      DGP:", dgp, "\n")
          for (param_name in names(vary_across_list$dgp[[dgp]])) {
            cat(paste0("         ", param_name, ": "))
            cat(str(vary_across_list$dgp[[dgp]][[param_name]],
                    indent.str = "           ", no.list = F))
          }
        }
        for (method in names(vary_across_list$method)) {
          cat("      Method:", method, "\n")
          for (param_name in names(vary_across_list$method[[method]])) {
            cat(paste0("         ", param_name, ": "))
            cat(str(vary_across_list$method[[method]][[param_name]],
                indent.str = "           ", no.list = F))
          }
        }
      }
      invisible(self)
    }
  )
)
