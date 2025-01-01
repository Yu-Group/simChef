#' Arguments that are shared by multiple Experiment helper funs.
#'
#' @name shared_experiment_helpers_args
#'
#' @param checkpoint_n_reps The number of experiment replicates to compute
#'   before saving results to disk. If 0 (the default), no checkpoints are
#'   saved.
#' @param dgp A `DGP` object.
#' @param evaluator An `Evaluator` object.
#' @param eval_results A list of result tibbles, as returned by
#'   [evaluate_experiment()].
#' @param experiment An `Experiment` object.
#' @param fit_results A tibble, as returned by [fit_experiment()].
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
#' @param method A `Method` object.
#' @param n_reps The number of replicates of the `Experiment` for this run.
#' @param parallel_strategy A vector with some combination of "reps", "dgps", or
#'   "methods". Determines how computation will be distributed across available
#'   resources. Currently only the default, "reps", is supported.
#' @param save If `TRUE`, save outputs to disk.
#' @param use_cached Logical. If `TRUE`, find and return previously saved
#'   results. If cached results cannot be found, continue as if `use_cached` was
#'   `FALSE`.
#' @param vary_params A vector of `DGP` or `Method` parameter names that are
#'   varied across in the `Experiment`.
#' @param verbose Level of verbosity. Default is 1, which prints out messages
#'   after major checkpoints in the experiment. If 2, prints additional
#'   debugging information for warnings and messages from user-defined functions
#'   (in addition to error debugging information). If 0, no messages are printed
#'   other than user-defined function error debugging information.
#' @param visualizer A `Visualizer` object.
#' @param ... Not used.
#'
#' @keywords internal
NULL

#' Create a new `Experiment`.
#'
#' @name create_experiment
#'
#' @inheritParams shared_experiment_helpers_args
#' @param name The name of the `Experiment`.
#' @param dgp_list An optional list of `DGP` objects.
#' @param method_list An optional list of `Method` objects.
#' @param evaluator_list An optional list of `Evaluator` objects.
#' @param visualizer_list An optional list of `Visualizer` objects.
#' @param future.globals Character vector of names in the global environment to
#'   pass to parallel workers. Passed as the argument of the same name to
#'   `future.apply::future_lapply` and related functions. To set for a
#'   specific run of the experiment, use the same argument in
#'   `Experiment$run`.
#' @param future.packages Character vector of packages required by parallel
#'   workers. Passed as the argument of the same name to
#'   `future.apply::future_lapply` and related functions. To set for a
#'   specific run of the experiment, use the same argument in
#'   `Experiment$run`.
#' @param clone_from An optional `Experiment` object to use as a base for
#'   this one.
#' @param save_dir An optional directory in which to save the experiment's
#'   results. If `NULL`, results are saved in the current working directory
#'   in a directory called "results" with a sub-directory named after
#'   `Experiment$name`.
#'
#' @return A new `Experiment` object.
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 10
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 10
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Mean(x)"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_mean_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#'
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = "SD of Mean(x)") {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "SD of Mean(x) Plot"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' experiment <- create_experiment(
#'   name = "Experiment Name",
#'   dgp_list = list(`Normal DGP` = normal_dgp, `Bernoulli DGP` = bernoulli_dgp),
#'   method_list = list(`Mean(x)` = mean_method),
#'   evaluator_list = list(`SD of Mean(x)` = sd_mean_eval),
#'   visualizer_list = list(`SD of Mean(x) Plot` = sd_mean_plot)
#' )
#'
#' # initialize empty experiment with user-defined directory for saving results
#' experiment <- create_experiment(
#'   name = "Experiment Name",
#'   dgp_list = list(`Normal DGP` = normal_dgp, `Bernoulli DGP` = bernoulli_dgp),
#'   method_list = list(`Mean(x)` = mean_method),
#'   evaluator_list = list(`SD of Mean(x)` = sd_mean_eval),
#'   visualizer_list = list(`SD of Mean(x) Plot` = sd_mean_plot),
#'   save_dir = 'path/to/directory'
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |>
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#'
#' # run experiment with 2 replicates
#' results <- run_experiment(experiment, n_reps = 2)
#' # uncomment below to view results
#' # results
#'
#' # run experiment with varying number of samples n
#' experiment <- experiment |>
#'   add_vary_across(
#'     .dgp = c("Normal DGP", "Bernoulli DGP"), n = c(1, 10)
#'   )
#' # run vary-across experiment with 2 replicates
#' results <- run_experiment(experiment, n_reps = 2)
#' # uncomment below to view results
#' # results
#'
#' # `run_experiment()` above is equivalent to the following sequence of calls
#' fit_results <- fit_experiment(experiment, n_reps = 2)
#' eval_results <- evaluate_experiment(experiment, fit_results)
#' viz_results <- visualize_experiment(experiment, fit_results, eval_results)
#'
#' # generate data from all DGPs (and vary across components) in experiment
#' data_out <- generate_data(experiment, n_reps = 1)
#'
#' @export
create_experiment <- function(name = "experiment",
                              dgp_list = list(), method_list = list(),
                              evaluator_list = list(), visualizer_list = list(),
                              future.globals = TRUE, future.packages = NULL,
                              clone_from = NULL, save_dir = NULL, ...) {
  Experiment$new(name = name, dgp_list = dgp_list, method_list = method_list,
                 evaluator_list = evaluator_list, visualizer_list = visualizer_list,
                 future.globals = future.globals, future.packages = future.packages,
                 clone_from = clone_from, save_dir = save_dir, ...)
}

#' Run the full `Experiment` pipeline (fitting, evaluating, and visualizing).
#'
#' @name run_experiment
#'
#' @inheritParams shared_experiment_helpers_args
#' @param return_all_cached_reps Logical. If `FALSE` (default), returns
#'   only the fit results for the requested `n_reps`. If `TRUE`,
#'   returns fit results for the requested `n_reps` plus any additional
#'   cached replicates from the (`DGP`, `Method`) combinations in the
#'   `Experiment`. Note that even if `return_all_cached_reps = TRUE`,
#'   only the `n_reps` replicates are used when evaluating and visualizing
#'   the `Experiment`.
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
#'
#' @inherit create_experiment examples
#' @export
run_experiment <- function(experiment, n_reps = 1,
                           parallel_strategy = c("reps"), future.globals = NULL,
                           future.packages = NULL, future.seed = TRUE,
                           use_cached = FALSE, return_all_cached_reps = FALSE,
                           save = FALSE, checkpoint_n_reps = 0, verbose = 1,
                           ...) {
  return(experiment$run(n_reps = n_reps,
                        parallel_strategy = parallel_strategy,
                        future.globals = future.globals,
                        future.packages = future.packages,
                        future.seed = future.seed, use_cached = use_cached,
                        save = save, checkpoint_n_reps = checkpoint_n_reps,
                        verbose = verbose, ...))
}

#' Generate data from each `DGP` in the `Experiment`.
#'
#' @name generate_data
#'
#' @description Generate sample data from all `DGP` objects that were added
#'   to the `Experiment`, including their varied params. Primarily useful
#'   for debugging. Note that results are not generated in parallel.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param n_reps The number of datasets to generate per `DGP`.
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
#'
#' @examples
#' # create DGP to generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 10
#' )
#' # create DGP to generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 10
#' )
#'
#' # initialize experiment with toy DGPs only
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp)
#'
#' # generate data from all DGPs (and vary across components if applicable)
#' # in experiment
#' generate_data(experiment, n_reps = 2)
#' @export
generate_data <- function(experiment, n_reps=1, ...) {
  return(experiment$generate_data(n_reps = n_reps, ...))
}

#' Fit an `Experiment`.
#'
#' @name fit_experiment
#' @description Fit `Methods` in the `Experiment` across all
#'   `DGPs` for `n_reps` repetitions and return results from fits.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param return_all_cached_reps Logical. If `FALSE` (default), returns
#'   only the fit results for the requested `n_reps`. If `TRUE`,
#'   returns fit results for the requested `n_reps` plus any additional
#'   cached replicates from the (`DGP`, `Method`) combinations in the
#'   `Experiment`.
#' @param ... Additional `future.*` arguments to pass to [future.apply]
#'   functions. See [future.apply::future_lapply()] and
#'   [future.apply::future_mapply()].
#'
#' @return A tibble containing the results from fitting all `Methods`
#'   across all `DGPs` for `n_reps` repetitions. In addition to
#'   results columns, has columns named '.rep', '.dgp_name', '.method_name', and the
#'   `vary_across` parameter names if applicable.
#'
#' @inherit create_experiment examples
#' @export
fit_experiment <- function(experiment, n_reps=1, parallel_strategy = c("reps"),
                           future.globals = NULL, future.packages = NULL,
                           future.seed = TRUE, use_cached = FALSE,
                           return_all_cached_reps = FALSE, save = FALSE,
                           checkpoint_n_reps = 0, verbose = 1, ...) {
  return(experiment$fit(n_reps = n_reps,
                        parallel_strategy = parallel_strategy,
                        future.globals = future.globals,
                        future.packages = future.packages,
                        future.seed = future.seed, use_cached = use_cached,
                        save = save, checkpoint_n_reps = checkpoint_n_reps,
                        verbose = verbose, ...))
}

#' Evaluate an `Experiment`.
#'
#' @name evaluate_experiment
#' @description Evaluate the performance of method(s) across all
#'   `Evaluators` in the `Experiment` and return results.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return A list of evaluation result tibbles, one for each
#'   `Evaluator`.
#'
#' @inherit create_experiment examples
#' @export
evaluate_experiment <- function(experiment, fit_results, use_cached = FALSE,
                                save = FALSE, verbose = 1, ...) {
  return(experiment$evaluate(fit_results = fit_results, use_cached = use_cached,
                             save = save, verbose = verbose, ...))
}

#' Visualize results of an `Experiment`.
#'
#' @name visualize_experiment
#' @description Visualize the performance of methods and/or its evaluation metrics
#'   using all `Visualizers` in the `Experiment` and return visualization results.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return A list of visualizations, one for each `Visualizer`.
#'
#' @inherit create_experiment examples
#' @export
visualize_experiment <- function(experiment, fit_results, eval_results = NULL,
                                 use_cached = FALSE, save = FALSE, verbose = 1,
                                 ...) {
  return(experiment$visualize(fit_results = fit_results,
                              eval_results = eval_results,
                              use_cached = use_cached, save = save,
                              verbose = verbose, ...))
}

# TODO: add @details
#' Helper functions for adding components to an `Experiment`.
#'
#' @description Helper functions for adding `DGPs`, `Methods`,
#'   `Evaluators`, and `Visualizers` to an `Experiment`.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param name A name to identify the object to be added.
#'
#' @return The original `Experiment` object passed to `add_*`.
#'
#' @name add_funs
#' @rdname add_funs
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 100
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 100
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Mean(x)"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_mean_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = "SD of Mean(x)") {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "SD of Mean(x) Plot"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |> and add_* functions
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#' print(experiment)
#'
#' # this is equivalent to
#' experiment <- create_experiment(
#'   name = "Experiment Name",
#'   dgp_list = list(`Normal DGP` = normal_dgp, `Bernoulli DGP` = bernoulli_dgp),
#'   method_list = list(`Mean(x)` = mean_method),
#'   evaluator_list = list(`SD of Mean(x)` = sd_mean_eval),
#'   visualizer_list = list(`SD of Mean(x) Plot` = sd_mean_plot)
#' )
#'
NULL

#' @rdname add_funs
#'
#' @inherit add_funs examples
#' @export
add_dgp <- function(experiment, dgp, name=NULL, ...) {
  experiment$add_dgp(dgp, name, ...)
}

#' @rdname add_funs
#'
#' @inherit add_funs examples
#' @export
add_method <- function(experiment, method, name=NULL, ...) {
  experiment$add_method(method, name, ...)
}

#' @rdname add_funs
#'
#' @inherit add_funs examples
#' @export
add_evaluator <- function(experiment, evaluator, name = NULL, ...) {
  experiment$add_evaluator(evaluator, name, ...)
}

#' @rdname add_funs
#'
#' @inherit add_funs examples
#' @export
add_visualizer <- function(experiment, visualizer, name=NULL, ...) {
  experiment$add_visualizer(visualizer, name, ...)
}

# TODO: add @details
#' Helper functions for updating components of an `Experiment`.
#'
#' @description Helper functions for updating `DGPs`, `Methods`,
#'   `Evaluators`, and `Visualizers` already added to an
#'   `Experiment`.
#'
#' @inheritParams add_funs
#' @param name An existing name identifying the object to be updated.
#'
#' @return The original `Experiment` object passed to `update_*`.
#'
#' @name update_funs
#' @rdname update_funs
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with 100 samples
#' dgp1 <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "DGP", n = 100
#' )
#' # generate data from normal distribution with 500 samples
#' dgp2 <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "DGP", n = 500
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Method"
#' )
#' # compute mean of data
#' median_method <- create_method(
#'   .method_fun = function(x) list(mean = median(x)), .name = "Method"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "Evaluator"
#' )
#' # evaluate Variance of mean(x) across simulation replicates
#' var_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(var = var(mean), .groups = "keep")
#'   },
#'   .name = "Evaluator"
#' )
#'
#' # plot SD of method results across simulation replicates
#' sd_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = 1) {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "Visualizer"
#' )
#' # plot variance of method results across simulation replicates
#' var_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = 1) {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = var, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = var)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "Visualizer"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |> and add_* functions
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(dgp1) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_eval) |>
#'   add_visualizer(sd_plot)
#' print(experiment)
#'
#' # example usage of update_* functions
#' experiment <- experiment |>
#'   update_dgp(dgp2, "DGP") |>
#'   update_method(median_method, "Method") |>
#'   update_evaluator(var_eval, "Evaluator") |>
#'   update_visualizer(var_plot, "Visualizer")
#'
NULL

#' @rdname update_funs
#'
#' @inherit update_funs examples
#' @export
update_dgp <- function(experiment, dgp, name, ...) {
  experiment$update_dgp(dgp, name, ...)
}

#' @rdname update_funs
#'
#' @inherit update_funs examples
#' @export
update_method <- function(experiment, method, name, ...) {
  experiment$update_method(method, name, ...)
}

#' @rdname update_funs
#'
#' @inherit update_funs examples
#' @export
update_evaluator <- function(experiment, evaluator, name, ...) {
  experiment$update_evaluator(evaluator, name, ...)
}

#' @rdname update_funs
#'
#' @inherit update_funs examples
#' @export
update_visualizer <- function(experiment, visualizer, name, ...) {
  experiment$update_visualizer(visualizer, name, ...)
}

# TODO: add @details
#' Helper functions for removing components of an `Experiment`.
#'
#' @description Helper functions for removing `DGPs`, `Methods`,
#'   `Evaluators`, and `Visualizers` already added to an
#'   `Experiment`.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param name A name to identify the object to be removed. If `NULL`
#'   (default), remove all objects of that class from the experiment. For
#'   example, `remove_dgp()` will remove all DGPs from the experiment.
#'
#' @return The original `Experiment` object passed to `remove_*`.
#'
#' @name remove_funs
#' @rdname remove_funs
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 100
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 100
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Mean(x)"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_mean_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = "SD of Mean(x)") {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "SD of Mean(x) Plot"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |> and add_* functions
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#' print(experiment)
#'
#' # example usage of removing DGPs, Methods, Evaluators, and Visualizers
#' experiment <- experiment |>
#'   remove_dgp("Normal DGP") |>
#'   remove_dgp("Bernoulli DGP") |>
#'   remove_method("Mean(x)") |>
#'   remove_evaluator("SD of Mean(x)") |>
#'   remove_visualizer("SD of Mean(x) Plot")
#' print(experiment)
#'
NULL

#' @rdname remove_funs
#'
#' @inherit remove_funs examples
#' @export
remove_dgp <- function(experiment, name = NULL, ...) {
  experiment$remove_dgp(name, ...)
}

#' @rdname remove_funs
#'
#' @inherit remove_funs examples
#' @export
remove_method <- function(experiment, name = NULL, ...) {
  experiment$remove_method(name, ...)
}

#' @rdname remove_funs
#'
#' @inherit remove_funs examples
#' @export
remove_evaluator <- function(experiment, name = NULL, ...) {
  experiment$remove_evaluator(name, ...)
}

#' @rdname remove_funs
#'
#' @inherit remove_funs examples
#' @export
remove_visualizer <- function(experiment, name = NULL, ...) {
  experiment$remove_visualizer(name, ...)
}

# TODO: add @details
#' Helper functions for getting components in an `Experiment`.
#'
#' @description Helper functions for getting or retrieving `DGPs`,
#'   `Methods`, `Evaluators`, and `Visualizers` from an
#'   `Experiment`.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return A named list of `DGPs`, `Methods`, `Evaluators`, or `Visualizers`
#'
#' @name get_funs
#' @rdname get_funs
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 100
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 100
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Mean(x)"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_mean_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = "SD of Mean(x)") {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "SD of Mean(x) Plot"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |> and add_* functions
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#'
#' # get DGPs, Methods, Evaluators, and Visualizers
#' get_dgps(experiment)
#' get_methods(experiment)
#' get_evaluators(experiment)
#' get_visualizers(experiment)
#'
NULL

#' @rdname get_funs
#'
#' @inherit get_funs examples
#' @export
get_dgps <- function(experiment, ...) {
  experiment$get_dgps()
}

#' @rdname get_funs
#'
#' @inherit get_funs examples
#' @export
get_methods <- function(experiment, ...) {
  experiment$get_methods()
}

#' @rdname get_funs
#'
#' @inherit get_funs examples
#' @export
get_evaluators <- function(experiment, ...) {
  experiment$get_evaluators()
}

#' @rdname get_funs
#'
#' @inherit get_funs examples
#' @export
get_visualizers <- function(experiment, ...) {
  experiment$get_visualizers()
}

#' Varying across parameters in an `Experiment`.
#'
#' @description Helper functions for adding, updating, removing, or getting a
#'   `vary_across` component in an `Experiment`. When a
#'   `vary_across` component is added and the `Experiment` is run, the
#'   `Experiment` is systematically varied across values of the specified
#'   parameter in the `DGP` or `Method` while all other parameters are
#'   held constant.
#'
#' @param .experiment,experiment An `Experiment` object.
#' @param .dgp,dgp Name of `DGP` to vary in the `Experiment`. Can also be a
#'   `DGP` object that matches one in the `Experiment` or even a vector/list of
#'   `DGP` names/objects, assuming they can all take in the specified
#'   `param_names`.
#' @param .method,method Name of `Method` to vary in the `Experiment`. Can also
#'   be a `Method` object that matches one in the `Experiment` or even a
#'   vector/list of `Method` names/objects, assuming they all support the target
#'   arguments provided via `...`.
#' @param param_names A character vector of parameter names to remove. If not
#'   provided, the entire set of `vary_across` parameters will be removed for
#'   the specified `DGP`/`Method`.
#' @param ... Any number of named arguments where names match an argument in the
#'   user-specified `DGP` or `Method` function and values are vectors (for
#'   scalar parameters) or lists (for arbitrary parameters).
#'
#' @details One of the `.dgp` or `.method` arguments (but not both) must
#'   be provided when using `add_vary_across()` and
#'   `update_vary_across`. For `remove_vary_across()`, if both the
#'   `dgp` and `method` arguments are not provided, then all
#'   `vary_across` parameters from the experiment are removed.
#'
#' @return In the case of `get_vary_across`, a nested list with entries
#'   "dgp" and "method" that contains the parameters to vary across for each
#'   `DGP` and `Method` in the `Experiment`. Otherwise, the
#'   original `Experiment` object passed to `*_vary_across()`.
#'
#' @name vary_across
#' @rdname vary_across
#'
#' @examples
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 100
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 100
#' )
#'
#' # compute weighted mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x, ...) list(mean = mean(x, ...)),
#'   .name = "Mean"
#' )
#'
#' # initialize experiment with toy DGPs and Methods
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method)
#'
#' # vary across n for normal DGP
#' experiment <- experiment |>
#'   add_vary_across(.dgp = "Normal DGP", n = c(100, 200, 300))
#' get_vary_across(experiment)
#' print(experiment)
#'
#' # remove vary across for normal DGP
#' experiment <- experiment |>
#'   remove_vary_across(dgp = "Normal DGP")
#' get_vary_across(experiment)
#' print(experiment)
#'
#' # can add vary across for multiple DGPs at once
#' experiment <- experiment |>
#'   add_vary_across(.dgp = c("Normal DGP", "Bernoulli DGP"), n = c(100, 200, 300))
#' get_vary_across(experiment)
#' print(experiment)
#'
#' # can update vary across for DGPs
#' experiment <- experiment |>
#'   update_vary_across(.dgp = "Normal DGP", n = c(100, 300)) |>
#'   update_vary_across(.dgp = "Bernoulli DGP", n = c(100, 200))
#' get_vary_across(experiment)
#' print(experiment)
#'
#' # can also add/update/remove vary across for methods
#' experiment <- experiment |>
#'   add_vary_across(.method = "Mean", trim = list(0, 0.1, 0.2))
#' print(experiment)
#' experiment <- experiment |>
#'   update_vary_across(
#'     .method = "Mean", trim = list(0, 0.1, 0.2, 0.3)
#'   )
#' print(experiment)
#' experiment <- experiment |>
#'   remove_vary_across(method = "Mean")
#' print(experiment)
#'
#' # can remove all vary across parameters
#' experiment <- experiment |>
#'   remove_vary_across()
#' get_vary_across(experiment)
#' print(experiment)
#'
NULL

#' @rdname vary_across
#'
#' @inherit vary_across examples
#' @export
add_vary_across <- function(.experiment, .dgp, .method, ...) {
  .experiment$add_vary_across(.dgp, .method, ...)
}

#' @rdname vary_across
#'
#' @inherit vary_across examples
#' @export
update_vary_across <- function(.experiment, .dgp, .method, ...) {
  .experiment$update_vary_across(.dgp, .method, ...)
}

#' @rdname vary_across
#'
#' @inherit vary_across examples
#' @export
remove_vary_across <- function(experiment, dgp, method, param_names = NULL) {
  experiment$remove_vary_across(dgp, method, param_names)
}

#' @rdname vary_across
#'
#' @inherit vary_across examples
#' @export
get_vary_across <- function(experiment) {
  experiment$get_vary_across()
}

#' Clear cached results from `Experiment`.
#'
#' @name clear_cache
#' @description Clear (or delete) cached results from an `Experiment` to
#'   start the experiment fresh/from scratch.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The original `Experiment` object with cache cleared.
#'
#' @examples
#' \dontrun{
#' clear_cache(experiment)}
#'
#' @export
clear_cache <- function(experiment) {
  experiment$clear_cache()
}

#' Retrieve cached results from previously saved `Experiment`.
#'
#' @name get_cached_results
#' @description Read in cached results from disk from a previously saved
#'   `Experiment` run.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param results_type Character string indicating the type of results to read
#'   in. Must be one of "experiment", "experiment_cached_params", "fit", "eval",
#'   or "viz".
#'
#' @return The cached results, specifically the cached `Experiment` object
#'   if `results_type = "experiment"`, the cached fit results if
#'   `results_type = "fit"`, the cached evaluation results if
#'   `results_type = "eval"`, the cached visualization results if
#'   `results_type = "viz"`, and the experiment parameters used in
#'   the cache if `results_type = "experiment_cached_params"`.
#'
#' @examples
#' \dontrun{
#' fit_results <- get_cached_results(experiment, "fit")
#' eval_results <- get_cached_results(experiment, "eval")
#' viz_results <- get_cached_results(experiment, "viz")
#' cached_params <- get_cached_results(experiment, "experiment_cached_params")}
#'
#' @export
get_cached_results <- function(experiment, results_type, verbose = 0) {
  experiment$get_cached_results(results_type = results_type, verbose = verbose)
}

#' Set R Markdown options for `Evaluator` and `Visualizer` outputs in
#'   summary report.
#'
#' @name set_doc_options
#' @description Set R Markdown options for `Evaluator` or `Visualizer`
#'   outputs in the summary report. Some options include the height/width of
#'   plots and number of digits to show in tables.
#'
#' @inheritParams shared_experiment_helpers_args
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
#' @return The original `Experiment` object with the `doc_options`
#'   and/or `show` fields modified in the `Evaluator`/`Visualizer`.
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 100
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 100
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Mean(x)"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_mean_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = "SD of Mean(x)") {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "SD of Mean(x) Plot"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |> and add_* functions
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#'
#' # set R Markdown options for Evaluator/Visualizer (in this case, Visualizer)
#' experiment <- experiment |>
#'   set_doc_options(
#'     field_name = "visualizer", name = "SD of Mean(x) Plot",
#'     height = 10, width = 8
#'   )
#'
#' @export
set_doc_options <- function(experiment, field_name = c("evaluator", "visualizer"),
                            name, show = NULL, nrows, ...) {
  field_name <- match.arg(field_name)
  experiment$set_doc_options(field_name = field_name, name = name, show = show,
                             nrows = nrows, ...)
}

#' Set R Markdown options for `Evaluator` and `Visualizer` outputs in
#'   summary report.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `set_rmd_options()` was renamed to `set_doc_options()` to create a more
#' consistent API.
#'
#' @inheritParams set_doc_options
#' @inherit set_doc_options examples
#'
#' @keywords internal
#' @export
set_rmd_options <- function(experiment, field_name = c("evaluator", "visualizer"),
                            name, show = NULL, ...) {
  lifecycle::deprecate_warn("0.1.0", "set_rmd_options()", "set_doc_options()")
  set_doc_options(experiment, field_name, name, show, ...)
}

#' Set results directory for an `Experiment`.
#'
#' @name set_save_dir
#' @description Set the directory in which the `Experiment`'s results and
#'   visualizations are saved.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param save_dir The directory in which the `Experiment`'s results
#'   will be saved.
#'
#' @return The original `Experiment` object with the updated saving
#'   directory.
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate data from normal distribution with n samples
#' normal_dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "Normal DGP", n = 100
#' )
#' # generate data from binomial distribution with n samples
#' bernoulli_dgp <- create_dgp(
#'   .dgp_fun = function(n) rbinom(n, 1, 0.5), .name = "Bernoulli DGP", n = 100
#' )
#'
#' # compute mean of data
#' mean_method <- create_method(
#'   .method_fun = function(x) list(mean = mean(x)), .name = "Mean(x)"
#' )
#'
#' # evaluate SD of mean(x) across simulation replicates
#' sd_mean_eval <- create_evaluator(
#'   .eval_fun = function(fit_results, vary_params = NULL) {
#'     group_vars <- c(".dgp_name", ".method_name", vary_params)
#'     fit_results |>
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_plot <- create_visualizer(
#'   .viz_fun = function(fit_results, eval_results, vary_params = NULL,
#'                       eval_name = "SD of Mean(x)") {
#'     if (!is.null(vary_params)) {
#'       add_aes <- ggplot2::aes(
#'         x = .data[[unique(vary_params)]], y = sd, color = .dgp_name
#'       )
#'     } else {
#'       add_aes <- ggplot2::aes(x = .dgp_name, y = sd)
#'     }
#'     plt <- ggplot2::ggplot(eval_results[[eval_name]]) +
#'       add_aes +
#'       ggplot2::geom_point()
#'     if (!is.null(vary_params)) {
#'       plt <- plt + ggplot2::geom_line()
#'     }
#'     return(plt)
#'   },
#'   .name = "SD of Mean(x) Plot"
#' )
#'
#' # initialize experiment with toy DGPs, Methods, Evaluators, and Visualizers
#' # using piping |> and add_* functions
#' experiment <- create_experiment(name = "Experiment Name") |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#'
#' # set custom save directory (i.e., where to save results)
#' experiment <- experiment |>
#'   set_save_dir("path/to/results")
#'
#' # this is equivalent to:
#' experiment <- create_experiment(
#'   name = "Experiment Name", save_dir = "path/to/results"
#' ) |>
#'   add_dgp(normal_dgp) |>
#'   add_dgp(bernoulli_dgp) |>
#'   add_method(mean_method) |>
#'   add_evaluator(sd_mean_eval) |>
#'   add_visualizer(sd_mean_plot)
#'
#' # get save dir via:
#' get_save_dir(experiment)
#'
#' @export
set_save_dir <- function(experiment, save_dir) {
  experiment$set_save_dir(save_dir)
}

#' Get results directory for an `Experiment`.
#'
#' @name get_save_dir
#' @description Get the directory in which the `Experiment`'s results and
#'   visualizations are saved.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The relative path to where the `Experiment`'s results and
#'   visualizations are saved.
#'
#' @inherit set_save_dir examples
#'
#' @export
get_save_dir <- function(experiment) {
  experiment$get_save_dir()
}

#' Save an `Experiment`.
#'
#' @name save_experiment
#' @description Save an `Experiment` object to a .rds file under the
#'   `Experiment`'s results directory (see [get_save_dir()]).
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The original `Experiment` object passed to
#'   `save_experiment`.
#'
#' @examples
#' \dontrun{
#' save_experiment(experiment)}
#'
#' @export
save_experiment <- function(experiment) {
  experiment$save()
}

#' Export cached `Visualizer` results to image.
#'
#' @name export_visualizers
#' @description Export all cached `Visualizer` results from an `Experiment` to
#'   images in the `viz_results/` directory under the `Experiment`'s results
#'   directory (see [get_save_dir()]).
#'
#' @inheritParams shared_experiment_helpers_args
#' @param ... Additional arguments to pass to [ggplot2::ggsave()]
#'
#' @return The original `Experiment` object passed to
#'   `export_visualizers`.
#'
#' @examples
#' \dontrun{
#' export_visualizers(experiment)}
#'
#' @export
export_visualizers <- function(experiment, ...) {
  experiment$export_visualizers(...)
}
