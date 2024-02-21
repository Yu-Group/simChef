#' @include utils.R
NULL

#' `R6` class representing a simulation experiment.
#'
#' @name Experiment
#'
#' @docType class
#'
#' @description A simulation experiment with any number of [DGP],
#'   [Method], [Evaluator], and [Visualizer] objects.
#'
#'   Generally speaking, users won't directly interact with the `Experiment` R6
#'   class, but instead indirectly through [create_experiment()] and the tidy
#'   `Experiment` helpers listed in below in the **See also** section.
#'
#' @details When run, an `Experiment` seamlessly combines `DGPs` and
#'   `Methods`, computing results in parallel. Those results can then be
#'   evaluated using `Evaluators` and visualized using `Visualizers`.
#'
#' @seealso The following tidy helpers take an `Experiment` object as their
#'   first argument: [create_experiment()], [generate_data()],
#'   [fit_experiment()], [evaluate_experiment()], [visualize_experiment()],
#'   [run_experiment()], [clear_cache()], [get_cached_results()],
#'   [get_save_dir()], [set_save_dir()], [save_experiment()],
#'   [export_visualizers()], [`add_*()`](add_funs.html),
#'   [`update_*()`](update_funs.html), [`remove_*()`](remove_funs.html),
#'   [`get_*()`](get_funs.html), and [`*_vary_across()`](vary_across.html).
#'
#' @export
Experiment <- R6::R6Class(
  classname = 'Experiment',
  private = list(

    # private fields
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

    # private methods
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
        abort(
          sprintf(
            paste("The name '%s' already exists in the %s list.",
                  "Use `update_%s` instead."),
            obj_name, field_name, field_name,
            ),
          call = rlang::caller_env()
        )
      } else {
        list_name <- paste0(".", field_name, "_list")
        private[[list_name]][[obj_name]] <- obj
      }
    },

    .update_obj = function(field_name, obj, obj_name, ...) {
      obj_list <- private$.get_obj_list(field_name, ...)
      if (!obj_name %in% names(obj_list)) {
        abort(
          sprintf(
            paste("The name '%s' isn't in the %s list.",
                  "Use `add_%s` instead."),
            obj_name, field_name, field_name
          ),
          call = rlang::caller_env()
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
        abort(
          sprintf(
            paste("Cannot remove '%s'.",
                  "The name '%s' does not exist in the %s list."),
            obj_name, obj_name, field_name
          ),
          call = rlang::caller_env()
        )
      } else {
        private[[list_name]][[obj_name]] <- NULL
      }
    },

    .throw_empty_list_error = function(field_name, action_name = "run") {
      abort(
        sprintf(
          paste("No %s has been added yet. ",
                "Use add_%s before trying to %s the experiment."),
          field_name, field_name, action_name
        ),
        call = rlang::caller_env()
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
        abort(err_msg, call = rlang::caller_env())
      }
    },

    .add_obj_list = function(obj_list, expected_class) {
      if (length(obj_list) > 0) {
        lapply(obj_list, function(obj) {
          if (!inherits(obj, expected_class)) {
            abort(
              sprintf(
                paste("Expected all objects in %s_list to be instances of %s,",
                      "but found an object with the following class(es): %s"),
                tolower(expected_class), expected_class,
                paste0(class(obj), collapse=", ")
              ),
              call = rlang::caller_env()
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
      if (missing(.dgp) && missing(.method)) {
        abort("Must specify either '.dgp' or '.method'.",
              call = rlang::caller_env())
      } else if (!missing(.dgp) && !missing(.method)) {
        abort("Must specify one of '.dgp' or '.method', but not both.",
              call = rlang::caller_env())
      } else if (!missing(.dgp)) {
        obj <- .dgp
        field <- "dgp"
      } else if (!missing(.method)) {
        obj <- .method
        field <- "method"
      }
      is_multi <- FALSE
      if (is.vector(obj)) {  # note: is.vector("abc") returns TRUE
        if (!((length(obj) == 1) && is.character(obj))) {  # check that not obj = "abc" case
          is_multi <- TRUE
          objs <- obj
        }
      }
      if (!is_multi) {
        if (field == "dgp") {
          objs <- list(.dgp)
        } else if (field == "method") {
          objs <- list(.method)
        }
      }
      out <- list()
      for (i in seq_along(objs)) {
        obj <- objs[[i]]
        out[[i]] <- private$.check_each_vary_across(
          obj = obj, field_name = field, ...
        )
      }
      return(out)
    },

    .check_each_vary_across = function(obj, field_name, ...) {
      dots_list <- rlang::list2(...)
      if (field_name == "dgp") {
        class_name <- "DGP"
      } else if (field_name == "method") {
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
        abort(
          sprintf(
            "%s must either be a %s object or the name of a %s in the current Experiment.",
            field_name, class_name, class_name
          ),
          call = rlang::caller_env()
        )
      }
      obj_fun_args <- methods::formalArgs(
        obj_list[[obj_name]][[paste0(field_name, "_fun")]]
      )
      dots_list_valid_names <- names(dots_list) %in% obj_fun_args
      if (!all(dots_list_valid_names) && (!("..." %in% obj_fun_args))) {
        invalid_names <- names(dots_list)[!dots_list_valid_names]
        invalid_names <- paste0(invalid_names, collapse=", ")
        abort(
          sprintf("%s: not valid argument(s) to %s's %s_fun",
                  invalid_names, obj_name, field_name),
          call = rlang::caller_env()
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
      params_list <- purrr::map(obj_names, function(obj_name) {
        obj_params <- private$.vary_across_list[[field_name]][[obj_name]]
        if (is.null(obj_params)) {
          obj_params <- list()
        }
        obj_params <- c(obj_name, obj_params)
        names(obj_params)[1] <- paste0(".", field_name, "_name")
        param_grid <- tidyr::expand_grid(!!!obj_params) %>%
          dplyr::transmute(params = purrr::pmap(., list))
        return(param_grid$params)
      }) %>%
        unlist(recursive = FALSE)
      return(params_list)
    },

    .update_fit_params = function() {
      # update/set (dgp, method) fit parameter combinations
      dgp_list <- private$.get_obj_list("dgp")
      dgp_params_list <- private$.combine_vary_params("dgp")
      method_list <- private$.get_obj_list("method")
      method_params_list <- private$.combine_vary_params("method")

      fit_params <- tidyr::crossing(.dgp = dgp_params_list,
                                    .method = method_params_list) %>%
        dplyr::mutate(
          .dgp_name = purrr::map_chr(.dgp, ~.x$.dgp_name),
          .dgp_fun = purrr::map(
            .dgp, ~removeSource(dgp_list[[.x$.dgp_name]]$dgp_fun)
          ),
          .dgp_params = purrr::map(
            .dgp, ~dgp_list[[.x$.dgp_name]]$dgp_params
          ),
          .method_name = purrr::map_chr(.method, ~.x$.method_name),
          .method_fun = purrr::map(
            .method, ~removeSource(method_list[[.x$.method_name]]$method_fun)
          ),
          .method_params = purrr::map(
            .method, ~method_list[[.x$.method_name]]$method_params
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
            out_params <- fit_params[cached_idxs[seq_len(nrow(fit_params))], ]
          } else if (identical(type, "new")) {
            out_params <- fit_params[!cached_idxs[seq_len(nrow(fit_params))], ]
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
    .get_obj_params = function(field_name = c("evaluator", "visualizer"),
                               obj_list) {
      field_name <- match.arg(field_name)
      field_name <- dplyr::case_when(
        field_name == "evaluator" ~ "eval",
        field_name == "visualizer" ~ "viz"
      )
      obj_params <- tibble::tibble(
        name = names(obj_list),
        fun = purrr::map(
          obj_list, ~removeSource(.x[[sprintf("%s_fun", field_name)]])
        ),
        params = purrr::map(
          obj_list, sprintf("%s_params", field_name)
        )
      ) %>%
        dplyr::rename_with(~sprintf(".%s_%s", field_name, .x))
      return(obj_params)
    },

    .get_new_obj_list = function(cached_params,
                                 field_name = c("dgp", "method",
                                                "evaluator", "visualizer"),
                                 new_fit_params = NULL) {
      # get new uncached objects for a certain class in the Experiment
      field_name <- match.arg(field_name)
      if (field_name %in% c("dgp", "method")) {
        return(unique(new_fit_params[[paste0(".", field_name)]]))
      } else {
        obj_verb <- dplyr::case_when(
          field_name == "evaluator" ~ "evaluate",
          field_name == "visualizer" ~ "visualize"
        )
        obj_name_col <- dplyr::case_when(
          field_name == "evaluator" ~ ".eval_name",
          field_name == "visualizer" ~ ".viz_name"
        )
        obj_list <- private$.get_obj_list(field_name)
        obj_params <- private$.get_obj_params(field_name, obj_list)
        cached_idxs <- dplyr::bind_rows(
          obj_params, cached_params[[obj_verb]]
        ) %>%
          duplicated(fromLast = TRUE)
        return(obj_list[
          obj_params[[obj_name_col]][!cached_idxs[1:nrow(obj_params)]]
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
        n_reps_complete <- get_matching_rows(
          id = fit_params, x = cached_fit_params
        ) %>%
          dplyr::pull(.n_reps)
        return(min(as.numeric(n_reps_complete)))
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
      evaluate_params <- private$.get_obj_params("evaluator", evaluator_list)
      eval_cached <- compare_tibble_rows(evaluate_params,
                                         cached_params$evaluate,
                                         op = eval_cached_op)
      if (identical(results_type, "eval")) {
        return(eval_cached)
      } else if (!eval_cached) {
        return(NULL)
      }

      visualizer_list <- private$.get_obj_list("visualizer")
      visualize_params <- private$.get_obj_params("visualizer", visualizer_list)
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
          inform(sprintf("Reading in cached %s results...", results_type))
        } else if (identical(results_type, "experiment")) {
          inform("Reading in the cached experiment...")
        } else if (identical(results_type, "experiment_cached_params")) {
          inform("Reading in the cached experiment parameters...")
        }
      }
      if (!private$.has_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- private$.get_vary_across_dir()
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
            inform(sprintf("Cannot find cached %s results.", results_type))
          } else {
            inform("Cannot find cache.")
          }
        }
        return(NULL)
      }
    },

    .clear_cache = function() {
      if (!private$.has_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- private$.get_vary_across_dir()
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
          cached_n_reps <- purrr::map_int(
            1:nrow(cached_params$fit),
            function(i) {
              n_reps0 <- cached_params$fit$.n_reps[i]
              n_reps_all <- get_matching_rows(
                id = cached_params$fit[i, ] %>% dplyr::select(-.n_reps),
                x = cached_params_all$fit$fit
              ) %>%
                dplyr::pull(.n_reps)
              return(max(n_reps0, n_reps_all, na.rm = TRUE))
            }
          )
          cached_params_all$fit$fit <- cached_params$fit %>%
            dplyr::mutate(.n_reps = cached_n_reps)
        } else {
          cached_params_all$fit$fit <- cached_params$fit
        }
        return(cached_params_all)
      }

      evaluator_list <- private$.get_obj_list("evaluator")
      cached_params$evaluate <- private$.get_obj_params(
        "evaluator", evaluator_list
      )
      if (identical(results_type, "eval")) {
        cached_params_all$evaluate <- cached_params
        return(cached_params_all)
      }

      visualizer_list <- private$.get_obj_list("visualizer")
      cached_params$visualize <- private$.get_obj_params(
        "visualizer", visualizer_list
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
          inform(sprintf("Saving %s results checkpoint...", results_type))
        } else {
          inform(sprintf("Saving %s results...", results_type))
        }
        start_time <- Sys.time()
      }
      if (!private$.has_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- private$.get_vary_across_dir()
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
        inform(sprintf("%s results saved | time taken: %f seconds",
                       R.utils::capitalize(results_type),
                       difftime(Sys.time(), start_time, units = "secs")))
      }
    },

    .get_vary_across_dir = function() {
      obj_names <- purrr::map(private$.vary_across_list, names) %>%
        purrr::reduce(c) %>%
        paste(collapse = "-")
      param_names <- private$.get_vary_params() %>%
        paste(collapse = "-")
      save_dir <- file.path(private$.save_dir, obj_names,
                            paste("Varying", param_names))
      return(save_dir)
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

    #' @field name The name of the `Experiment`.
    name = NULL,

    # NOTE: R6 methods can't use the `@inheritParams` tag. If you want to update
    # the `@param` tags for the public methods below, do so in the appropriate
    # docs in experiment-helpers.R, then copy-paste the corresponding `@param`
    # tags below.

    #' @description Initialize a new `Experiment` object.
    #'
    #' @param name The name of the `Experiment`.
    #' @param dgp_list An optional list of [DGP] objects.
    #' @param method_list An optional list of [Method] objects.
    #' @param evaluator_list An optional list of [Evaluator] objects.
    #' @param visualizer_list An optional list of [Visualizer] objects.
    #' @param future.globals Character vector of names in the global environment to
    #'   pass to parallel workers. Passed as the argument of the same name to
    #'   [future.apply::future_lapply()] and related functions. To set for a
    #'   specific run of the experiment, use the same argument in
    #'   [run_experiment()].
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   [future.apply::future_lapply()] and related functions. To set for a
    #'   specific run of the experiment, use the same argument in
    #'   [run_experiment()].
    #' @param clone_from An optional `Experiment` object to use as a base for
    #'   this one.
    #' @param save_dir An optional directory in which to save the experiment's
    #'   results. If `NULL`, results are saved in the current working directory
    #'   in a directory called "results" with a sub-directory named after
    #'   `Experiment$name` when using [run_experiment()] or [fit_experiment()]
    #'   with `save=TRUE`.
    #' @param ... Not used.
    #'
    #' @return A new instance of `Experiment`.
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

    #' @description Run the full `Experiment` pipeline (fitting, evaluating,
    #'   and visualizing).
    #'
    #' @param n_reps The number of replicates of the `Experiment` for this run.
    #' @param parallel_strategy A vector with some combination of "reps", "dgps", or
    #'   "methods". Determines how computation will be distributed across available
    #'   resources. Currently only the default, "reps", is supported.
    #' @param future.globals Character vector of names in the global environment to
    #'   pass to parallel workers. Passed as the argument of the same name to
    #'   `future.apply::future_lapply` and related functions. To set for all runs of
    #'   the experiment, use the same argument during initialization.
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   `future.apply::future_lapply` and related functions. To set for all runs of
    #'   the experiment, use the same argument during initialization.
    #' @param future.seed Passed as the argument of the same name in
    #'   `future.apply::future_apply`.
    #' @param use_cached Logical. If `TRUE`, find and return previously saved
    #'   results. If cached results cannot be found, continue as if `use_cached` was
    #'   `FALSE`.
    #' @param return_all_cached_reps Logical. If `FALSE` (default), returns
    #'   only the fit results for the requested `n_reps`. If `TRUE`,
    #'   returns fit results for the requested `n_reps` plus any additional
    #'   cached replicates from the (`DGP`, `Method`) combinations in the
    #'   `Experiment`. Note that even if `return_all_cached_reps = TRUE`,
    #'   only the `n_reps` replicates are used when evaluating and visualizing
    #'   the `Experiment`.
    #' @param save If `TRUE`, save outputs to disk.
    #' @param checkpoint_n_reps The number of experiment replicates to compute
    #'   before saving results to disk. If 0 (the default), no checkpoints are
    #'   saved.
    #' @param verbose Level of verbosity. Default is 1, which prints out messages
    #'   after major checkpoints in the experiment. If 2, prints additional
    #'   debugging information for warnings and messages from user-defined functions
    #'   (in addition to error debugging information). If 0, no messages are printed
    #'   other than user-defined function error debugging information.
    #' @param ... Not used.
    #'
    #' @return A named list of results from the simulation experiment with the
    #'   following entries:
    #' \describe{
    #' \item{fit_results}{A tibble containing results from the `fit`
    #'   method. In addition to results columns, has columns named '.rep', '.dgp_name',
    #'   '.method_name', and the `vary_across` parameter names if applicable.}
    #' \item{eval_results}{A list of tibbles containing results from the
    #'   `evaluate` method, which evaluates each `Evaluator` in
    #'   the `Experiment`. Length of list is equivalent to the number of
    #'   `Evaluators`.}
    #' \item{viz_results}{A list of tibbles containing results from the
    #'   `visualize` method, which visualizes each `Visualizer` in
    #'   the `Experiment`. Length of list is equivalent to the number of
    #'   `Visualizers`.}
    #' }
    run = function(n_reps = 1, parallel_strategy = "reps",
                   future.globals = NULL, future.packages = NULL,
                   future.seed = TRUE, use_cached = FALSE,
                   return_all_cached_reps = FALSE, save = FALSE,
                   checkpoint_n_reps = 0, verbose = 1, ...) {

      if (!is.logical(save)) {
        save <- c("fit", "eval", "viz") %in% save
      } else {
        if (length(save) > 1) {
          warn("The input save is a logical vector of length > 1. Only the first element of save is used.")
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

    #' @description Generate sample data from all `DGP` objects that were added
    #'   to the `Experiment`, including their varied params. Primarily useful
    #'   for debugging. Note that results are not generated in parallel.
    #'
    #' @param n_reps The number of datasets to generate per `DGP`.
    #' @param ... Not used.
    #'
    #' @return A list of length equal to the number of `DGPs` in the
    #'   `Experiment`. If the `Experiment` does not have a
    #'   `vary_across` component, then each element in the list is a list
    #'   of `n_reps` datasets generated by the given `DGP`. If the
    #'   `Experiment` does have a `vary_across` component, then each
    #'   element in the outermost list is a list of lists. The second layer of
    #'   lists corresponds to a specific parameter setting within the
    #'   `vary_across` scheme, and the innermost layer of lists is of
    #'   length `n_reps` with the dataset replicates, generated by the
    #'   `DGP`.
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

    #' @description Fit `Methods` in the `Experiment` across all
    #'   `DGPs` for `n_reps` repetitions and return results from fits.
    #'
    #' @param n_reps The number of replicates of the `Experiment` for this run.
    #' @param parallel_strategy A vector with some combination of "reps", "dgps", or
    #'   "methods". Determines how computation will be distributed across available
    #'   resources. Currently only the default, "reps", is supported.
    #' @param future.globals Character vector of names in the global environment to
    #'   pass to parallel workers. Passed as the argument of the same name to
    #'   `future.apply::future_lapply` and related functions. To set for all runs of
    #'   the experiment, use the same argument during initialization.
    #' @param future.packages Character vector of packages required by parallel
    #'   workers. Passed as the argument of the same name to
    #'   `future.apply::future_lapply` and related functions. To set for all runs of
    #'   the experiment, use the same argument during initialization.
    #' @param future.seed Passed as the argument of the same name in
    #'   `future.apply::future_apply`.
    #' @param use_cached Logical. If `TRUE`, find and return previously saved
    #'   results. If cached results cannot be found, continue as if `use_cached` was
    #'   `FALSE`.
    #' @param return_all_cached_reps Logical. If `FALSE` (default), returns
    #'   only the fit results for the requested `n_reps`. If `TRUE`,
    #'   returns fit results for the requested `n_reps` plus any additional
    #'   cached replicates from the (`DGP`, `Method`) combinations in the
    #'   `Experiment`.
    #' @param save If `TRUE`, save outputs to disk.
    #' @param checkpoint_n_reps The number of experiment replicates to compute
    #'   before saving results to disk. If 0 (the default), no checkpoints are
    #'   saved.
    #' @param verbose Level of verbosity. Default is 1, which prints out messages
    #'   after major checkpoints in the experiment. If 2, prints additional
    #'   debugging information for warnings and messages from user-defined functions
    #'   (in addition to error debugging information). If 0, no messages are printed
    #'   other than user-defined function error debugging information.
    #' @param ... Additional `future.*` arguments to pass to [future.apply]
    #'   functions. See [future.apply::future_lapply()] and
    #'   [future.apply::future_mapply()].
    #'
    #' @return A tibble containing the results from fitting all `Methods`
    #'   across all `DGPs` for `n_reps` repetitions. In addition to
    #'   results columns, has columns named '.rep', '.dgp_name', '.method_name', and the
    #'   `vary_across` parameter names if applicable.
    fit = function(n_reps = 1, parallel_strategy = "reps",
                   future.globals = NULL, future.packages = NULL,
                   future.seed = TRUE, use_cached = FALSE,
                   return_all_cached_reps = FALSE, save = FALSE,
                   checkpoint_n_reps = 0, verbose = 1, ...) {

      parallel_strategy <- unique(parallel_strategy)

      valid_strategies <- c(
        "reps"
        # TODO: currently unimplemented:
        #, "dgps", "methods", "dgps+reps", "methods+reps",
        # "dgps+methods", "dgps+methods+reps"
      )

      if (length(parallel_strategy) == 0) {
        parallel_strategy <- "reps"

      } else if (length(parallel_strategy) > 1) {

        parallel_strategy <- sapply(
          parallel_strategy, match.arg, choices = c("reps", "dgps", "methods")
        )

        strategy_string <- NULL

        if ("reps" %in% parallel_strategy) {
          strategy_string <- "reps"
        }
        if ("methods" %in% parallel_strategy) {
          strategy_string <- paste0(c("methods", strategy_string), collapse="+")
        }
        if ("dgps" %in% parallel_strategy) {
          strategy_string <- paste0(c("dgps", strategy_string), collapse="+")
        }

        parallel_strategy <- strategy_string
      }

      if (!parallel_strategy %in% valid_strategies) {
        abort(
          sprintf(
            paste("`parallel_strategy` '%s' is currently unimplemented.",
                  "Please use one of the supported values: %s."),
            parallel_strategy,
            paste0("'", valid_strategies, "'", collapse = "', '")
          )
        )
      }

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

          fit_results <- get_matching_rows(id = fit_params, x = results) %>%
            dplyr::select(.rep, tidyselect::everything()) %>%
            dplyr::arrange(as.numeric(.rep), .dgp_name, .method_name)

          if (save) {
            n_reps_cached <- min(n_reps_total, n_reps_cached)
            private$.save_results(fit_results, "fit", n_reps_cached, verbose)
          }

          if (n_reps_cached >= n_reps_total) {
            if (verbose >= 1) {
              inform("==============================")
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
        inform(sprintf("Fitting %s...", self$name))
        start_time <- Sys.time()
      }

      if (is.null(future.packages)) {
        future.packages <- private$.future.packages
      }

      if (is.null(future.globals)) {
        future.globals <- private$.future.globals
      }

      dgp_params_list <- private$.combine_vary_params("dgp")
      method_params_list <- private$.combine_vary_params("method")

      # if new_fit_params is not NULL after the if statement below, then not all
      # combos of (dgp_params_list, method_params_list) need to be rerun so need
      # to check cache ids when fitting
      new_fit_params <- NULL

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

          n_new <- length(dgp_params_list) * length(method_params_list)

          if (n_new == n_params) {
            new_fit_params <- NULL
          }
        }
      }

      duplicate_param_names <- private$.get_duplicate_param_names()

      # simulation loop
      while (n_reps_cached < n_reps_total) {

        n_reps <- min(n_reps, n_reps_total - n_reps_cached)

        new_fit_results <- local({

          # create an env with objs/funcs that the future workers need
          workenv <- rlang::new_environment(
            data = list(
              verbose = verbose,
              dgp_list = dgp_list,
              method_list = method_list,
              new_fit_params = new_fit_params,
              dgp_params_list = dgp_params_list,
              method_params_list = method_params_list,
              duplicate_param_names = duplicate_param_names,
              do_call_wrapper = function(name,
                                         fun,
                                         params,
                                         verbose,
                                         call) {
                tryCatch(
                  do_call_handler(
                    name, fun, params, verbose, call
                  ),
                  error = identity
                )
              }
            ),
            parent = rlang::ns_env()
          )

          # get the experiment compute fun
          compute_fun <- switch(
            parallel_strategy,
            "reps" = compute_rep,
            "dgps" = compute_dgp,
            "methods" = compute_method,
            "dgps+reps" = compute_dgp_rep,
            "methods+reps" = compute_method_rep,
            "dgps+methods" = compute_dgp_method,
            "dgps+methods+reps" = compute_dgp_method_rep
          )

          environment(compute_fun) <- workenv

          # compute the experiment
          compute_fun(n_reps,
                      future.globals,
                      future.packages,
                      future.seed)
        })

        gc()

        new_fit_results <- new_fit_results %>%
          dplyr::mutate(
            .rep = as.character(as.numeric(.rep) + n_reps_cached)
          ) %>%
          simplify_tibble()

        if (".err" %in% colnames(new_fit_results)) {

          errors <- new_fit_results %>%
            dplyr::filter(
              purrr::map_lgl(
                .err, ~!is.null(.x)
              )
            ) %>%
            dplyr::select(.dgp, .dgp_name, .dgp_params,
                          .method, .method_name, .method_params,
                          .method_output, .err, .pid, .gc) %>%
            dplyr::arrange(.dgp_name, .method_name)

          # filter out errors
          new_fit_results <- new_fit_results %>%
            dplyr::filter(purrr::map_lgl(.err, is.null)) %>%
            dplyr::select(-c(.dgp, .dgp_params, .method, .method_params,
                             .method_output, .err))

          if (isFALSE(getOption("simChef.debug", FALSE))) {
            new_fit_results <- new_fit_results %>%
              dplyr::select(-c(.pid, .gc))
          }

          # TODO: add fail_fast -- immediately throw an error or continue

          # end the simulation with an error
          abort(
            paste0(
              "Error(s) encountered while running the simulation, ",
              "including:\n\n", errors$.err[[1]]$message,
              "\n\nRun `rlang::last_error()$partial_results`",
              "to return partial simulation results.\n",
              "Run `rlang::last_error()$errors` to inspect each error, ",
              "along with the params,\n  `DGP`, `Method`, and ",
              "inputs/outputs before the error occurred."
            ),
            partial_results = new_fit_results,
            errors = errors
          )

        }

        n_reps_cached <- n_reps_cached + n_reps

        col_diff <- setdiff(
          private$.get_vary_params(), colnames(new_fit_results)
        )

        for (col in col_diff) {
          new_fit_results[[col]] <- NA
        }

        fit_results <- new_fit_results %>%
          dplyr::select(.rep, .dgp_name, .method_name,
                        private$.get_vary_params(),
                        tidyselect::everything()) %>%
          dplyr::bind_rows(fit_results) %>%
          dplyr::arrange(as.numeric(.rep), .dgp_name, .method_name)

        if (use_cached && !new_fit) {
          fit_params_cached <- private$.get_fit_params(cached_params, "cached",
                                                       n_reps_total, TRUE)
          fit_results_cached <- private$.get_cached_results(
            "fit", verbose = verbose
          ) %>%
            get_matching_rows(id = fit_params_cached, x = .)
          if (verbose >= 1) {
            inform("Appending cached results to the new fit results...")
          }
          fit_params <- private$.get_fit_params(simplify = TRUE)
          fit_results <- dplyr::bind_rows(fit_results, fit_results_cached) %>%
            get_matching_rows(id = fit_params, x = .) %>%
            dplyr::arrange(as.numeric(.rep), .dgp_name, .method_name) %>%
            dplyr::select(.rep, tidyselect::everything())
        }

        if (save || checkpoint) {
          private$.save_results(
            fit_results, "fit", n_reps_cached, verbose,
            checkpoint && n_reps_cached < n_reps_total
          )
        }

        if (verbose >= 1) {
          inform(sprintf("%s reps completed (totals: %s/%s) | time taken: %f minutes",
                         n_reps, n_reps_cached, n_reps_total,
                         difftime(Sys.time(), start_time, units = "mins")))
        }

      } # while (n_reps_cached < n_reps_total) {

      if (verbose >= 1) {
        inform("==============================")
      }

      if (use_cached && return_all_cached_reps) {
        return(fit_results)
      } else {
        return(fit_results %>% dplyr::filter(as.numeric(.rep) <= n_reps_total))
      }

    },

    #' @description Evaluate the performance of method(s) across all
    #'   [Evaluator] objects in the `Experiment` and return results.
    #'
    #' @param fit_results A tibble, as returned by [fit_experiment()].
    #' @param use_cached Logical. If `TRUE`, find and return previously saved
    #'   results. If cached results cannot be found, continue as if `use_cached` was
    #'   `FALSE`.
    #' @param save If `TRUE`, save outputs to disk.
    #' @param verbose Level of verbosity. Default is 1, which prints out messages
    #'   after major checkpoints in the experiment. If 2, prints additional
    #'   debugging information for warnings and messages from user-defined functions
    #'   (in addition to error debugging information). If 0, no messages are printed
    #'   other than user-defined function error debugging information.
    #' @param ... Not used.
    #'
    #' @return A list of evaluation result tibbles, one for each
    #'   `Evaluator`.
    evaluate = function(fit_results, use_cached = FALSE, save = FALSE,
                        verbose = 1, ...) {
      evaluator_list <- private$.get_obj_list("evaluator")
      evaluator_names <- names(evaluator_list)
      if (length(evaluator_list) == 0) {
        if (verbose >= 1) {
          inform("No evaluators to evaluate. Skipping evaluation.")
          inform("==============================")
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
            inform("==============================")
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
        inform(sprintf("Evaluating %s...", self$name))
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
          inform("Appending cached results to the new evaluation results...")
        }
        eval_results <- c(eval_results, eval_results_cached)[evaluator_names]
      }
      if (verbose >= 1) {
        inform(sprintf("Evaluation completed | time taken: %f minutes",
                       difftime(Sys.time(), start_time, units = "mins")))
      }
      if (save) {
        private$.save_results(eval_results, "eval", n_reps, verbose)
      }
      if (verbose >= 1) {
        inform("==============================")
      }

      return(eval_results)
    },

    #' @description Visualize the performance of methods and/or its evaluation metrics
    #'   using all [Visualizer] objects in the `Experiment` and return
    #'   visualization results.
    #'
    #' @param fit_results A tibble, as returned by [fit_experiment()].
    #' @param eval_results A list of result tibbles, as returned by
    #'   [evaluate_experiment()].
    #' @param use_cached Logical. If `TRUE`, find and return previously saved
    #'   results. If cached results cannot be found, continue as if `use_cached` was
    #'   `FALSE`.
    #' @param save If `TRUE`, save outputs to disk.
    #' @param verbose Level of verbosity. Default is 1, which prints out messages
    #'   after major checkpoints in the experiment. If 2, prints additional
    #'   debugging information for warnings and messages from user-defined functions
    #'   (in addition to error debugging information). If 0, no messages are printed
    #'   other than user-defined function error debugging information.
    #' @param ... Not used.
    #'
    #' @return A list of visualizations, one for each `Visualizer`.
    visualize = function(fit_results, eval_results = NULL,
                         use_cached = FALSE, save = FALSE, verbose = 1, ...) {

      visualizer_list <- private$.get_obj_list("visualizer")
      visualizer_names <- names(visualizer_list)
      if (length(visualizer_list) == 0) {
        if (verbose >= 1) {
          inform("No visualizers to visualize. Skipping visualization.")
          inform("==============================")
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
            inform("==============================")
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
        inform(sprintf("Visualizing %s...", self$name))
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
          inform("Appending cached results to the new visualization results...")
        }
        viz_results <- c(viz_results,
                         viz_results_cached)[visualizer_names]
      }
      if (verbose >= 1) {
        inform(sprintf("Visualization completed | time taken: %f minutes",
                       difftime(Sys.time(), start_time, units = "mins")))
      }
      if (save) {
        private$.save_results(viz_results, "viz", n_reps, verbose)
      }
      if (verbose >= 1) {
        inform("==============================")
      }

      return(viz_results)
    },

    #' @description Add a [DGP] object to the `Experiment`.
    #'
    #' @param dgp A `DGP` object.
    #' @param name A name to identify the `DGP`.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    add_dgp = function(dgp, name = NULL, ...) {
      private$.check_obj(dgp, "DGP")
      private$.add_obj("dgp", dgp, name)
      invisible(self)
    },

    #' @description Update a [DGP] object in the `Experiment`.
    #'
    #' @param dgp A `DGP` object.
    #' @param name An existing name identifying the `DGP` to be updated.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    update_dgp = function(dgp, name, ...) {
      private$.check_obj(dgp, "DGP")
      private$.update_obj("dgp", dgp, name)
      invisible(self)
    },

    #' @description Remove a [DGP] object from the `Experiment`.
    #'
    #' @param name An existing name identifying the `DGP` to be removed.
    #' @param ... Not used
    #'
    #' @return The `Experiment` object, invisibly.
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

    #' @description Retrieve the [DGP] objects associated with the `Experiment`.
    #'
    #' @return A named list of the `DGP` objects in the `Experiment`.
    get_dgps = function() {
      return(private$.get_obj_list("dgp"))
    },

    #' @description Add a [Method] object to the `Experiment`.
    #'
    #' @param method A `Method` object.
    #' @param name A name to identify the `Method` to be updated.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    add_method = function(method, name = NULL, ...) {
      private$.check_obj(method, "Method")
      private$.add_obj("method", method, name)
      invisible(self)
    },

    #' @description Update a [Method] object in the `Experiment`.
    #'
    #' @param method A `Method` object.
    #' @param name An existing name identifying the `Method` to be updated.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    update_method = function(method, name, ...) {
      private$.check_obj(method, "Method")
      private$.update_obj("method", method, name)
      invisible(self)
    },

    #' @description Remove a [Method] object from the `Experiment`.
    #'
    #' @param name An existing name identifying the `Method` to be removed.
    #' @param ... Not used
    #'
    #' @return The `Experiment` object, invisibly.
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

    #' @description Retrieve the [Method] objects associated with the `Experiment`.
    #'
    #' @return A named list of the `Method` objects in the `Experiment`.
    get_methods = function() {
      return(private$.get_obj_list("method"))
    },

    #' @description Add an [Evaluator] object to the `Experiment`.
    #'
    #' @param evaluator An `Evaluator` object.
    #' @param name A name to identify the `Evaluator`.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    add_evaluator = function(evaluator, name = NULL, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.add_obj("evaluator", evaluator, name)
      invisible(self)
    },

    #' @description Update an [Evaluator] object in the `Experiment`.
    #'
    #' @param evaluator An `Evaluator` object.
    #' @param name An existing name identifying the `Evaluator` to be updated.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    update_evaluator = function(evaluator, name, ...) {
      private$.check_obj(evaluator, "Evaluator")
      private$.update_obj("evaluator", evaluator, name)
      invisible(self)
    },

    #' @description Remove an [Evaluator] object from the `Experiment`.
    #'
    #' @param name An existing name identifying the `Evaluator` to be removed.
    #' @param ... Not used
    #'
    #' @return The `Experiment` object, invisibly.
    remove_evaluator = function(name = NULL, ...) {
      private$.remove_obj("evaluator", name)
      invisible(self)
    },

    #' @description Retrieve the [Evaluator] objects associated with the `Experiment`.
    #'
    #' @return A named list of the `Evaluator` objects in the `Experiment`.
    get_evaluators = function() {
      return(private$.get_obj_list("evaluator"))
    },

    #' @description Add a [Visualizer] object to the `Experiment`.
    #'
    #' @param visualizer A `Visualizer` object.
    #' @param name A name to identify the `Visualizer`.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    add_visualizer = function(visualizer, name = NULL, ...) {
      private$.check_obj(visualizer, "Visualizer")
      private$.add_obj("visualizer", visualizer, name)
      invisible(self)
    },

    #' @description Update a [Visualizer] object in the `Experiment`.
    #'
    #' @param visualizer A `Visualizer` object.
    #' @param name An existing name identifying the `Visualizer` to be updated.
    #' @param ... Not used.
    #'
    #' @return The `Experiment` object, invisibly.
    update_visualizer = function(visualizer, name, ...) {
      private$.check_obj(visualizer, "Visualizer")
      private$.update_obj("visualizer", visualizer, name)
      invisible(self)
    },

    #' @description Remove a [Visualizer] object from the `Experiment`.
    #'
    #' @param name An existing name identifying the `Visualizer` to be removed.
    #' @param ... Not used
    #'
    #' @return The `Experiment` object, invisibly.
    remove_visualizer = function(name = NULL, ...) {
      private$.remove_obj("visualizer", name)
      invisible(self)
    },

    #' @description Retrieve the [Visualizer] objects associated with the `Experiment`.
    #'
    #' @return A named list of the `Visualizer` objects in the `Experiment`.
    get_visualizers = function() {
      return(private$.get_obj_list("visualizer"))
    },

    # @description Add a `vary_across` component in the `Experiment`. When a
    #'   `vary_across` component is added and the `Experiment` is run, the
    #'   `Experiment` is systematically varied across values of the specified
    #'   parameter in the `DGP` or `Method` while all other parameters are
    #'   held constant.
    #'
    #' @details One of the `.dgp` or `.method` arguments (but not both) must
    #'   be provided.
    #'
    #' @param .dgp Name of `DGP` to vary in the `Experiment`. Can also be a
    #'   `DGP` object that matches one in the `Experiment` or even a
    #'   vector/list of `DGP` names/objects, assuming they all support the
    #'   target arguments provided via `...`.
    #' @param .method Name of `Method` to vary in the `Experiment`. Can also be a
    #'   `Method` object that matches one in the `Experiment` or even a
    #'   vector/list of `Method` names/objects, assuming they all support the
    #'   target arguments provided via `...`.
    #' @param ... Any number of named arguments where names match an argument in the
    #'   user-specified `DGP` or `Method` function and values are vectors (for
    #'   scalar parameters) or lists (for arbitrary parameters).
    #'
    #' @return The `Experiment` object, invisibly.
    add_vary_across = function(.dgp, .method, ...) {
      objs <- private$.check_vary_across(.dgp = .dgp, .method = .method, ...)
      for (obj in objs) {
        dots_list <- obj$dots_list
        field_name <- obj$field_name
        obj_name <- obj$obj_name
        vary_across_sublist <- private$.vary_across_list[[field_name]][[obj_name]]
        if (is.null(vary_across_sublist)) {
          vary_across_sublist <- list()
        }
        for (arg_name in names(dots_list)) {
          if (is.null(vary_across_sublist[[arg_name]])) {
            vary_across_sublist[[arg_name]] <- dots_list[[arg_name]]
          } else {
            abort(
              sprintf(
                paste0(
                  "The vary_across parameter for argument '%s' has already ",
                  "been set for %s's %s_fun. Use update_vary_across instead."
                ),
                arg_name, obj_name, field_name
              )
            )
          }
        }
        private$.vary_across_list[[field_name]][[obj_name]] <- vary_across_sublist
      }
      invisible(self)
    },

    #' @description Update a `vary_across` component in the `Experiment`.
    #'
    #' @details One of the `.dgp` or `.method` arguments (but not both) must
    #'   be provided.
    #'
    #' @param .dgp Name of `DGP` to vary in the `Experiment`. Can also be a
    #'   `DGP` object that matches one in the `Experiment` or even a
    #'   vector/list of `DGP` names/objects, assuming they all support the
    #'   target arguments provided via `...`.
    #' @param .method Name of `Method` to vary in the `Experiment`. Can also be a
    #'   `Method` object that matches one in the `Experiment` or even a
    #'   vector/list of `Method` names/objects, assuming they all support the
    #'   target arguments provided via `...`.
    #' @param ... Any number of named arguments where names match an argument in the
    #'   user-specified `DGP` or `Method` function and values are vectors (for
    #'   scalar parameters) or lists (for arbitrary parameters).
    #'
    #' @return The `Experiment` object, invisibly.
    update_vary_across = function(.dgp, .method, ...) {
      objs <- private$.check_vary_across(.dgp = .dgp, .method = .method, ...)
      for (obj in objs) {
        dots_list <- obj$dots_list
        field_name <- obj$field_name
        obj_name <- obj$obj_name
        vary_across_sublist <- private$.vary_across_list[[field_name]][[obj_name]]
        if (is.null(vary_across_sublist)) {
          abort(
            sprintf(
              paste0(
                "The vary_across parameter has not been set for %s's %s_fun. ",
                "Use add_vary_across instead."
              ),
              obj_name, field_name
            )
          )
        }
        for (arg_name in names(dots_list)) {
          if (is.null(vary_across_sublist[[arg_name]])) {
            abort(
              sprintf(
                paste0(
                  "The vary_across parameter for argument '%s' has not ",
                  "been set for %s's %s_fun. Use add_vary_across instead."
                ),
                arg_name, obj_name, field_name
              )
            )
          } else {
            vary_across_sublist[[arg_name]] <- dots_list[[arg_name]]
          }
        }
        private$.vary_across_list[[field_name]][[obj_name]] <- vary_across_sublist
      }
      invisible(self)
    },

    #' @description Remove a `vary_across` component in the `Experiment`.
    #'
    #' @details If both the `dgp` and `method` arguments are not provided,
    #'   then all `vary_across` parameters from the experiment are removed.
    #'
    #' @param dgp Name of `DGP` to vary in the `Experiment`. Can also be a
    #'   `DGP` object that matches one in the `Experiment` or even a
    #'   vector/list of `DGP` names/objects.
    #' @param method Name of `Method` to vary in the `Experiment`. Can also be a
    #'   `Method` object that matches one in the `Experiment` or even a
    #'   vector/list of `Method` names/objects.
    #' @param param_names A character vector of parameter names to remove. If not
    #'   provided, the entire set of `vary_across` parameters will be removed for
    #'   the specified `DGP`/`Method`.
    #'
    #' @return The `Experiment` object, invisibly.
    remove_vary_across = function(dgp, method, param_names = NULL) {
      if (missing(dgp) && missing(method)) {
        if (!private$.has_vary_across()) {
          abort(
            paste("Cannot remove all vary_across parameters ",
                  "since the vary_across parameter has not been set.")
          )
        } else {
          # TODO: check param_names?
          private$.vary_across_list <- list(
            dgp = list(),
            method = list()
          )
          return(invisible(self))
        }
      }
      objs <- private$.check_vary_across(.dgp = dgp, .method = method)
      for (obj in objs) {
        field_name <- obj$field_name
        obj_name <- obj$obj_name
        vary_across_sublist <- private$.vary_across_list[[field_name]][[obj_name]]
        if (is.null(vary_across_sublist)) {
          abort(
            sprintf(
              paste0(
                "Cannot remove vary_across parameter for %s's %s_fun ",
                "since the vary_across parameter has not been set."
              ),
              obj_name, field_name
            )
          )
        }

        for (arg_name in param_names) {
          if (is.null(vary_across_sublist[[arg_name]])) {
            abort(
              sprintf(
                paste0(
                  "Cannot remove vary_across parameter for argument '%s' ",
                  "in %s's %s_fun since the vary_across parameter has not been set."
                ),
                arg_name, obj_name, field_name
              )
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
      }
      invisible(self)
    },

    #' @description Retrieve the parameters to vary across for each `DGP` and
    #'   `Method` in the `Experiment`.
    #'
    #' @return a nested list with entries "dgp" and "method".
    get_vary_across = function() {
      return(private$.vary_across_list)
    },

    #' @description Clear (or delete) cached results from an `Experiment` to
    #'   start the experiment fresh/from scratch.
    #'
    #' @return The `Experiment` object, invisibly.
    clear_cache = function() {
      private$.clear_cache()
      return(invisible(self))
    },

    #' @description Read in cached results from disk from a previously saved
    #'   `Experiment` run.
    #'
    #' @param results_type Character string indicating the type of results to read
    #'   in. Must be one of "experiment", "experiment_cached_params", "fit", "eval",
    #'   or "viz".
    #' @param verbose Level of verbosity. Default is 1, which prints out messages
    #'   after major checkpoints in the experiment. If 2, prints additional
    #'   debugging information for warnings and messages from user-defined functions
    #'   (in addition to error debugging information). If 0, no messages are printed
    #'   other than user-defined function error debugging information.
    #'
    #' @return The cached results, specifically the cached `Experiment` object
    #'   if `results_type = "experiment"`, the cached fit results if
    #'   `results_type = "fit"`, the cached evaluation results if
    #'   `results_type = "eval"`, the cached visualization results if
    #'   `results_type = "viz"`, and the experiment parameters used in
    #'   the cache if `results_type = "experiment_cached_params"`.
    get_cached_results = function(results_type, verbose = 0) {
      return(private$.get_cached_results(results_type = results_type,
                                         verbose = verbose))
    },

    #' @description Set R Markdown options for `Evaluator` or `Visualizer`
    #'   outputs in the summary report. Some options include the height/width of
    #'   plots and number of digits to show in tables.
    #'
    #' @param field_name One of "evaluator" or "visualizer".
    #' @param name Name of `Evaluator` or `Visualizer` to set R Markdown
    #'   options.
    #' @param show If `TRUE`, show output; if `FALSE`, hide output in
    #'   R Markdown report. Default `NULL` does not change the "doc_show" field
    #'   in `Evaluator`/`Visualizer`.
    #' @param nrows Maximum number of rows to show in the `Evaluator`'s results
    #'   table in the R Markdown report. If `NULL`, shows all rows. Default does
    #'   not change the "doc_nrows" field in the `Evaluator`. Argument is
    #'   ignored if `field_name = "visualizer"`.
    #' @param ... Named R Markdown options to set. If `field_name = "visualizer"`,
    #'   options are "height" and "width". If `field_name = "evaluator"`,
    #'   see options for [vthemes::pretty_DT()].
    #'
    #' @return The `Experiment` object, invisibly.
    set_doc_options = function(field_name, name, show = NULL, nrows, ...) {
      field_name <- match.arg(field_name, c("evaluator", "visualizer"))
      obj_list <- private$.get_obj_list(field_name)
      if (!name %in% names(obj_list)) {
        abort(
          sprintf(
            paste("The name '%s' isn't in the %s list.",
                  "Use add_%s first."),
            name, field_name, field_name
          )
        )
      }
      list_name <- paste0(".", field_name, "_list")
      if (!is.null(show)) {
        private[[list_name]][[name]]$doc_show <- show
      }
      if (field_name == "evaluator") {
        if (!missing(nrows)) {
          private[[list_name]][[name]]$doc_nrows <- nrows
        }
      }
      doc_options <- list(...)
      if (length(doc_options) > 0) {
        for (i in 1:length(doc_options)) {
          private[[list_name]][[name]]$doc_options[[names(doc_options)[i]]] <-
            doc_options[[i]]
        }
      }
      invisible(self)
    },

    #' @description Get the directory in which the `Experiment`'s results and
    #'   visualizations are saved.
    #'
    #' @return The relative path to where the `Experiment`'s results and
    #'   visualizations are saved.
    get_save_dir = function() {
      return(private$.save_dir)
    },

    #' @description Set the directory in which the `Experiment`'s results and
    #'   visualizations are saved.
    #'
    #' @param save_dir The directory in which the `Experiment`'s results
    #'   will be saved.
    #'
    #' @return The `Experiment` object, invisibly.
    set_save_dir = function(save_dir) {
      private$.save_dir <- R.utils::getAbsolutePath(save_dir)
      invisible(self)
    },

    #' @description Save the `Experiment` object to a .rds file under the
    #'   `Experiment`'s results directory (see [get_save_dir()]).
    #'
    #' @return The `Experiment` object, invisibly.
    save = function() {
      if (!private$.has_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- private$.get_vary_across_dir()
      }
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      saveRDS(self, file.path(save_dir, "experiment.rds"))
      invisible(self)
    },

    #' @description Export all cached `Visualizer` results from an
    #'   `Experiment` to images in the `viz_results/` directory under the
    #'   `Experiment`'s results directory (see [get_save_dir()]).
    #'
    #' @param device See `device` argument of [ggplot2::ggsave()].
    #' @param width See `width` argument of [ggplot2::ggsave()].
    #' @param height See `height` argument of [ggplot2::ggsave()].
    #' @param ... Additional arguments to pass to [ggplot2::ggsave()].
    #'
    #' @return The `Experiment` object, invisibly.
    export_visualizers = function(device = "pdf", width = "auto", height = "auto",
                                  ...) {
      rlang::check_installed("ggplot2",
        reason = "to export visualizers to image.")
      viz_list <- self$get_visualizers()
      if (length(viz_list) == 0) {
        return(invisible(self))
      }
      viz_results <- private$.get_cached_results("viz", verbose = 0)

      if (!private$.has_vary_across()) {
        save_dir <- private$.save_dir
      } else {
        save_dir <- private$.get_vary_across_dir()
      }
      save_dir <- file.path(save_dir, "viz_results")
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }

      ggsave_args <- list(device = device, ...)
      for (viz_name in names(viz_results)) {
        viz <- viz_list[[viz_name]]
        if (identical(height, "auto")) {
          ggsave_args[["height"]] <- viz$doc_options$height
        }
        if (identical(width, "auto")) {
          ggsave_args[["width"]] <- viz$doc_options$width
        }
        fname <- file.path(save_dir, sprintf("%s.%s", viz_name, device))
        tryCatch({
          do.call(ggplot2::ggsave,
            args = c(list(filename = fname,
              plot = viz_results[[viz_name]]),
              ggsave_args))
        }, error = function(err) {
          rlang::warn(sprintf(
            "Could not save %s as image using ggplot2::ggsave.", viz_name
          ))
          if (file.exists(fname)) {
            file.remove(fname)
          }
        })
      }
      invisible(self)
    },

    #' @description Print the `Experiment` in a nice format, showing the
    #'   `DGP`, `Method`, `Evaluator`, `Visualizers`, and varied parameters
    #'   involved in the `Experiment`.
    #'
    #' @return The original `Experiment` object, invisibly.
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
        if (!is.null(names(vary_across_list$dgp)) ||
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
