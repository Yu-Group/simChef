#' Arguments that are shared by multiple Experiment helper funs.
#'
#' @name shared_experiment_helpers_args
#'
#' @param checkpoint_n_reps The number of experiment replicates to compute
#'   before saving results to disk. If 0 (the default), no checkpoints are
#'   saved.
#' @param dgp A \code{DGP} object.
#' @param evaluator An \code{Evaluator} object.
#' @param eval_results A list of result tibbles, as returned by the
#'   \code{evaluate} method.
#' @param experiment An \code{Experiment} object.
#' @param fit_results A tibble, as returned by the \code{fit} method.
#' @param future.globals Character vector of names in the global environment to
#'   pass to parallel workers. Passed as the argument of the same name to
#'   \code{future.apply::future_lapply} and related functions. To set for all
#'   runs of the experiment, use the same argument during initialization.
#' @param future.packages Character vector of packages required by parallel
#'   workers. Passed as the argument of the same name to
#'   \code{future.apply::future_lapply} and related functions. To set for all
#'   runs of the experiment, use the same argument during initialization.
#' @param future.seed Passed as the argument of the same name in
#'   \code{future.apply::future_apply}.
#' @param method A \code{Method} object.
#' @param n_reps The number of replicates of the \code{Experiment} for this run.
#' @param parallel_strategy A vector with some combination of "reps", "dgps", or
#'   "methods". Determines how computation will be distributed across available
#'   resources. Default is "reps".
#' @param save If \code{TRUE}, save outputs to disk.
#' @param use_cached Logical. If \code{TRUE}, find and return previously saved
#'   results. If cached results cannot be found, continue as if
#'   \code{use_cached} was \code{FALSE}.
#' @param vary_params A vector of parameter names that are varied across in the
#'   \code{Experiment}.
#' @param verbose Level of verbosity. Default is 1, which prints out messages
#'   after major checkpoints in the experiment. If 2, prints additional
#'   debugging information for warnings and messages from user-defined functions
#'   (in addition to error debugging information). If 0, no messages are printed
#'   other than user-defined function error debugging information.
#' @param visualizer A \code{Visualizer} object.
#' @param ... Not used.
#'
#' @keywords internal
NULL

#' Create a new \code{Experiment}.
#'
#' @name create_experiment
#'
#' @inheritParams shared_experiment_helpers_args
#' @param name The name of the \code{Experiment}.
#' @param dgp_list An optional list of \code{DGP} objects.
#' @param method_list An optional list of \code{Method} objects.
#' @param evaluator_list An optional list of \code{Evaluator} objects.
#' @param visualizer_list An optional list of \code{Visualizer} objects.
#' @param future.globals Character vector of names in the global environment to
#'   pass to parallel workers. Passed as the argument of the same name to
#'   \code{future.apply::future_lapply} and related functions. To set for a
#'   specific run of the experiment, use the same argument in
#'   \code{Experiment$run}.
#' @param future.packages Character vector of packages required by parallel
#'   workers. Passed as the argument of the same name to
#'   \code{future.apply::future_lapply} and related functions. To set for a
#'   specific run of the experiment, use the same argument in
#'   \code{Experiment$run}.
#' @param clone_from An optional \code{Experiment} object to use as a base for
#'   this one.
#' @param save_dir An optional directory in which to save the experiment's
#'   results. If \code{NULL}, results are saved in the current working directory
#'   in a directory called "results" with a sub-directory named after
#'   \code{Experiment$name}.
#'
#' @return A new \code{Experiment} object.
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

#' Run the full \code{Experiment} pipeline (fitting, evaluating, and visualizing).
#'
#' @name run_experiment
#'
#' @inheritParams shared_experiment_helpers_args
#' @param return_all_cached_reps Logical. If \code{FALSE} (default), returns
#'   only the fit results for the requested \code{n_reps}. If \code{TRUE},
#'   returns fit results for the requested \code{n_reps} plus any additional
#'   cached replicates from the (\code{DGP}, \code{Method}) combinations in the
#'   \code{Experiment}. Note that even if \code{return_all_cached_reps = TRUE},
#'   only the \code{n_reps} replicates are used when evaluating and visualizing
#'   the \code{Experiment}.
#'
#' @return A list of results from the simulation experiment.
#' \describe{
#' \item{fit_results}{A tibble containing results from the \code{fit}
#'   method. In addition to results columns, has columns named '.rep', '.dgp_name',
#'   '.method_name', and the \code{vary_across} parameter names if applicable.}
#' \item{eval_results}{A list of tibbles containing results from the
#'   \code{evaluate} method, which evaluates each \code{Evaluator} in
#'   the \code{Experiment}. Length of list is equivalent to the number of
#'   \code{Evaluators}.}
#' \item{viz_results}{A list of tibbles containing results from the
#'   \code{visualize} method, which visualizes each \code{Visualizer} in
#'   the \code{Experiment}. Length of list is equivalent to the number of
#'   \code{Visualizers}.}
#' }
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

#' Generate data from each \code{DGP} in the \code{Experiment}.
#'
#' @name generate_data
#'
#' @inheritParams shared_experiment_helpers_args
#' @param n_reps The number of datasets to generate per \code{DGP}.
#'
#' @return A list of length equal to the number of \code{DGPs} in the
#'   \code{Experiment}. If the \code{Experiment} does not have a
#'   \code{vary_across} component, then each element in the list is a list
#'   of \code{n_reps} datasets generated by the given \code{DGP}. If the
#'   \code{Experiment} does have a \code{vary_across} component, then each
#'   element in the outermost list is a list of lists. The second layer of
#'   lists corresponds to a specific parameter setting within the
#'   \code{vary_across} scheme, and the innermost layer of lists is of
#'   length \code{n_reps} with the dataset replicates, generated by the
#'   \code{DGP}.
#' @export
generate_data <- function(experiment, n_reps=1, ...) {
  return(experiment$generate_data(n_reps = n_reps, ...))
}

#' Fit an \code{Experiment}.
#'
#' @name fit_experiment
#' @description Fit \code{Methods} in the \code{Experiment} across all
#'   \code{DGPs} for \code{n_reps} repetitions and return results from fits.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param return_all_cached_reps Logical. If \code{FALSE} (default), returns
#'   only the fit results for the requested \code{n_reps}. If \code{TRUE},
#'   returns fit results for the requested \code{n_reps} plus any additional
#'   cached replicates from the (\code{DGP}, \code{Method}) combinations in the
#'   \code{Experiment}.
#' @param ... Additional `future.*` arguments to pass to [future.apply]
#'   functions. See [future.apply::future_lapply()] and
#'   [future.apply::future_mapply()].
#'
#' @return A tibble containing the results from fitting all \code{Methods}
#'   across all \code{DGPs} for \code{n_reps} repetitions. In addition to
#'   results columns, has columns named '.rep', '.dgp_name', '.method_name', and the
#'   \code{vary_across} parameter names if applicable.
#'
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

#' Evaluate an \code{Experiment}.
#'
#' @name evaluate_experiment
#' @description Evaluate the performance of method(s) across all
#'   \code{Evaluators} in the \code{Experiment} and return results.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return A list of evaluation result tibbles, one for each
#'   \code{Evaluator}.
#'
#' @export
evaluate_experiment <- function(experiment, fit_results, use_cached = FALSE,
                                save = FALSE, verbose = 1, ...) {
  return(experiment$evaluate(fit_results = fit_results, use_cached = use_cached,
                             save = save, verbose = verbose, ...))
}

#' Visualize results of an \code{Experiment}.
#'
#' @name visualize_experiment
#' @description Visualize the performance of methods and/or its evaluation metrics
#'   using all \code{Visualizers} in the \code{Experiment} and return visualization results.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return A list of visualizations, one for each \code{Visualizer}.
#'
#' @export
visualize_experiment <- function(experiment, fit_results, eval_results = NULL,
                                 use_cached = FALSE, save = FALSE, verbose = 1,
                                 ...) {
  return(experiment$visualize(fit_results = fit_results,
                              eval_results = eval_results,
                              use_cached = use_cached, save = save,
                              verbose = verbose, ...))
}

#' Helper functions for adding components to an \code{Experiment}.
#'
#' @description Helper functions for adding \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Visualizers} to an \code{Experiment}.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param name A name to identify the object to be added.
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
add_visualizer <- function(experiment, visualizer, name=NULL, ...) {
  experiment$add_visualizer(visualizer, name, ...)
}

#' Helper functions for updating components of an \code{Experiment}.
#'
#' @description Helper functions for updating \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Visualizers} already added to an
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
update_visualizer <- function(experiment, visualizer, name, ...) {
  experiment$update_visualizer(visualizer, name, ...)
}

#' Helper functions for removing components of an \code{Experiment}.
#'
#' @description Helper functions for removing \code{DGPs}, \code{Methods},
#'   \code{Evaluators}, and \code{Visualizers} already added to an
#'   \code{Experiment}.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param name A name to identify the object to be removed. If \code{NULL}
#'   (default), remove all objects of that class from the experiment. For
#'   example, \code{remove_dgp()} will remove all DGPs from the experiment.
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
remove_dgp <- function(experiment, name = NULL, ...) {
  experiment$remove_dgp(name, ...)
}

#' @rdname remove_funs
#'
#' @export
remove_method <- function(experiment, name = NULL, ...) {
  experiment$remove_method(name, ...)
}

#' @rdname remove_funs
#'
#' @export
remove_evaluator <- function(experiment, name = NULL, ...) {
  experiment$remove_evaluator(name, ...)
}

#' @rdname remove_funs
#'
#' @export
remove_visualizer <- function(experiment, name = NULL, ...) {
  experiment$remove_visualizer(name, ...)
}

#' Helper functions for getting components in an \code{Experiment}.
#'
#' @description Helper functions for getting or retrieving \code{DGPs},
#'   \code{Methods}, \code{Evaluators}, and \code{Visualizers} from an
#'   \code{Experiment}.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The original \code{Experiment} object passed to \code{get_*}.
#'
#' @name get_funs
#' @rdname get_funs
#'
NULL

#' @rdname get_funs
#'
#' @export
get_dgps <- function(experiment, ...) {
  experiment$get_dgps()
}

#' @rdname get_funs
#'
#' @export
get_methods <- function(experiment, ...) {
  experiment$get_methods()
}

#' @rdname get_funs
#'
#' @export
get_evaluators <- function(experiment, ...) {
  experiment$get_evaluators()
}

#' @rdname get_funs
#'
#' @export
get_visualizers <- function(experiment, ...) {
  experiment$get_visualizers()
}

#' Varying across parameters in an \code{Experiment}.
#'
#' @description Helper functions for adding, updating, removing, or getting a
#'   \code{vary_across} component in an \code{Experiment}. When a
#'   \code{vary_across} component is added and the \code{Experiment} is run, the
#'   \code{Experiment} is systematically varied across values of the specified
#'   parameter in the \code{DGP} or \code{Method} while all other parameters are
#'   held constant at their baseline value.
#'
#' @param .experiment,experiment An \code{Experiment} object.
#' @param .dgp,dgp Name of \code{DGP} to vary in the \code{Experiment}. Can also be a
#'   \code{DGP} object that matches one in the \code{Experiment} or even a
#'   vector/list of \code{DGP} names/objects, assuming they can all take in the
#'   specified \code{param_names}.
#' @param .method,method Name of \code{Method} to vary in the \code{Experiment}. Can
#'   also be a \code{Method} object that matches one in the \code{Experiment} or
#'   even a vector/listo f \code{Method} names/objects, assuming they can all
#'   take in the specified \code{param_names}.
#' @param param_names A character vector of parameter names to remove. If
#'   not provided, the entire set of \code{vary_across} parameters will be
#'   removed for the specified \code{DGP}/\code{Method}.
#' @param ... Any number of named arguments where names match an argument in the
#'   user-specified \code{DGP} or \code{Method} function and values are vectors
#'   (for scalar parameters) or lists (for arbitrary parameters).
#'
#' @details One of the \code{.dgp} or \code{.method} arguments (but not both) must
#'   be provided when using \code{add_vary_across()} and
#'   \code{update_vary_across}. For \code{remove_vary_across()}, if both the
#'   \code{dgp} and \code{method} arguments are not provided, then all
#'   \code{vary_across} parameters from the experiment are removed.
#'
#' @return In the case of \code{get_vary_across}, a nested list with entries
#'   "dgp" and "method" that contains the parameters to vary across for each
#'   \code{DGP} and \code{Method} in the \code{Experiment}. Otherwise, the
#'   original \code{Experiment} object passed to \code{*_vary_across()}.
#'
#' @name vary_across
#' @rdname vary_across
#'
NULL

#' @rdname vary_across
#'
#' @export
add_vary_across <- function(.experiment, .dgp, .method, ...) {
  .experiment$add_vary_across(.dgp, .method, ...)
}

#' @rdname vary_across
#'
#' @export
update_vary_across <- function(.experiment, .dgp, .method, ...) {
  .experiment$update_vary_across(.dgp, .method, ...)
}

#' @rdname vary_across
#'
#' @export
remove_vary_across <- function(experiment, dgp, method, param_names = NULL) {
  experiment$remove_vary_across(dgp, method, param_names)
}

#' @rdname vary_across
#'
#' @export
get_vary_across <- function(experiment) {
  experiment$get_vary_across()
}

#' Clear cached results from \code{Experiment}.
#'
#' @name clear_cache
#' @description Clear (or delete) cached results from an \code{Experiment} to
#'   start the experiment fresh/from scratch.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The original \code{Experiment} object with cache cleared.
#'
#' @export
clear_cache <- function(experiment) {
  experiment$clear_cache()
}

#' Retrieve cached results from previously saved \code{Experiment}.
#'
#' @name get_cached_results
#' @description Read in cached results from disk from a previously saved
#'   \code{Experiment}.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param results_type Character string indicating the type of results to read
#'   in. Must be one of "experiment", "experiment_cached_params", "fit", "eval",
#'   or "viz".
#'
#' @return The cached results, specifically the cached \code{Experiment} object
#'   if \code{results_type = "experiment"}, the cached fit results if
#'   \code{results_type = "fit"}, the cached evaluation results if
#'   \code{results_type = "eval"}, the cached visualization results if
#'   \code{results_type = "viz"}, and the experiment parameters used in
#'   the cache if \code{results_type = "experiment_cached_params"}.
#'
#' @export
get_cached_results <- function(experiment, results_type, verbose = 0) {
  experiment$get_cached_results(results_type = results_type, verbose = verbose)
}

#' Set R Markdown options for \code{Evaluator} and \code{Visualizer} outputs in
#'   summary report.
#'
#' @name set_doc_options
#' @description Set R Markdown options for \code{Evaluator} or \code{Visualizer}
#'   outputs in the summary report. Some options include the height/width of
#'   plots and number of digits to show in tables.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param field_name One of "evaluator" or "visualizer".
#' @param name Name of \code{Evaluator} or \code{Visualizer} to set R Markdown
#'   options.
#' @param show If \code{TRUE}, show output; if \code{FALSE}, hide output in
#'   R Markdown report. Default \code{NULL} does not change the "show" field
#'   in \code{Evaluator}/\code{Visualizer}.
#' @param ... Named R Markdown options to set. If \code{field_name = "visualizer"},
#'   options are "height" and "width". If \code{field_name = "evaluator"},
#'   see options for [vthemes::pretty_DT()].
#'
#' @return The original \code{Experiment} object with the \code{doc_options}
#'   and/or \code{show} fields modified in the \code{Evaluator}/\code{Visualizer}.
#'
#' @export
set_doc_options <- function(experiment, field_name = c("evaluator", "visualizer"),
                            name, show = NULL, ...) {
  field_name <- match.arg(field_name)
  experiment$set_doc_options(field_name = field_name, name = name, show = show,
                             ...)
}

#' Set R Markdown options for \code{Evaluator} and \code{Visualizer} outputs in
#'   summary report.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `set_rmd_options()` was renamed to `set_doc_options()` to create a more
#' consistent API.
#'
#' @keywords internal
#' @export
set_rmd_options <- function(experiment, field_name = c("evaluator", "visualizer"),
                            name, show = NULL, ...) {
  lifecycle::deprecate_warn("0.1.0", "set_rmd_options()", "set_doc_options()")
  set_doc_options(experiment, field_name, name, show, ...)
}

#' Set results directory for an \code{Experiment}.
#'
#' @name set_save_dir
#' @description Set the directory in which the \code{Experiment}'s results and
#'   visualizations are saved.
#'
#' @inheritParams shared_experiment_helpers_args
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

#' Get results directory for an \code{Experiment}.
#'
#' @name get_save_dir
#' @description Get the directory in which the \code{Experiment}'s results and
#'   visualizations are saved.
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The relative path to where the \code{Experiment}'s results and
#'   visualizations are saved.
#'
#' @export
get_save_dir <- function(experiment) {
  experiment$get_save_dir()
}

#' Save an \code{Experiment}.
#'
#' @name save_experiment
#' @description Save an \code{Experiment} object to a .rds file under the
#'   \code{Experiment}'s results directory (see \code{Experiment$get_save_dir()}).
#'
#' @inheritParams shared_experiment_helpers_args
#'
#' @return The original \code{Experiment} object passed to
#'   \code{save_experiment}.
#'
#' @export
save_experiment <- function(experiment) {
  experiment$save()
}

#' Export cached \code{Visualizer} results to image.
#'
#' @name export_visualizers
#' @description Export all cached \code{Visualizer} results from an
#'   \code{Experiment} to images in viz_results/ under the \code{Experiment}'s
#'   results directory (see \code{Experiment$get_save_dir()}).
#'
#' @inheritParams shared_experiment_helpers_args
#' @param ... Additional arguments to pass to [ggplot2::ggsave()]
#'
#' @return The original \code{Experiment} object passed to
#'   \code{export_visualizers}.
#'
#' @export
export_visualizers <- function(experiment, ...) {
  experiment$export_visualizers(...)
}
