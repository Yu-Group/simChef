# NOTE: R6 methods can't use the `@inheritParams` tag. If you update the
# `@param` tags below then be sure to manually replace the corresponding tags
# above `Visualizer$initialize()`.

#' Create a new `Visualizer`
#'
#' @name create_visualizer
#'
#' @description Create a [Visualizer] which can `visualize()` outputs and/or
#'   evaluation metrics from [Experiment] runs.
#'
#' @inherit Visualizer details
#'
#' @param .viz_fun The user-defined visualization function.
#' @param .name (Optional) The name of the `Visualizer`.
#' @param .doc_options (Optional) List of options to control the aesthetics of
#'   the `Visualizer`'s visualization in the knitted R Markdown report.
#'   Currently, possible options are "height" and "width" (in inches). The
#'   argument must be specified by position or typed out in whole; no partial
#'   matching is allowed for this argument.
#' @param .doc_show If `TRUE` (default), show the resulting visualization in the
#'   R Markdown report; if `FALSE`, hide output in the R Markdown report.
#' @param ... User-defined default arguments to pass into `.viz_fun()`.
#'
#' @return A new [Visualizer] object.
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate toy data
#' dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "DGP", n = 100
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
#'     fit_results %>%
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) %>%
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#'
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_viz <- create_visualizer(
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
#' experiment <- create_experiment(name = "Experiment Name") %>%
#'   add_dgp(dgp) %>%
#'   add_method(mean_method) %>%
#'   add_evaluator(sd_mean_eval) %>%
#'   add_visualizer(sd_mean_viz)
#'
#' # fit, evaluate, and visualize (i.e., run) experiment
#' fit_results <- fit_experiment(experiment, n_reps = 10)
#'
#' eval_results <- evaluate_experiment(experiment, fit_results)
#'
#' viz_results <- visualize_experiment(experiment, fit_results, eval_results)
#'
#' # example usage of generate_dgp which generates one replicate
#' data_out <- dgp$generate()
#'
#' # example usage of fit_method on a single data replicate
#' method_out <- mean_method$fit(data_out)
#'
#' # example usage of evaluate_evaluator and visualize_visualizer
#' eval_out <- sd_mean_eval$evaluate(fit_results)
#' all.equal(eval_out, eval_results[[1]])
#'
#' viz_out <- sd_mean_viz$visualize(fit_results, eval_results)
#' all.equal(viz_out, viz_results[[1]])
#'
#' @export
create_visualizer <- function(.viz_fun, .name = NULL, .doc_options = list(),
                              .doc_show = TRUE, ...) {
  Visualizer$new(.viz_fun, .name, .doc_options, .doc_show, ...)
}

#' `R6` class representing a visualizer
#'
#' @name Visualizer
#'
#' @docType class
#'
#' @description `Visualizer` which can `visualize()` outputs and/or evaluation
#'   metrics from [Experiment] runs.
#'
#'   Generally speaking, users won't directly interact with the `Visualizer` R6
#'   class, but instead indirectly through [create_visualizer()] and the
#'   following `Experiment` helpers:
#'
#'   - [add_visualizer()]
#'   - [update_visualizer()]
#'   - [remove_visualizer()]
#'   - [get_visualizers()]
#'   - [visualize_experiment()]
#'
#' @details When visualizing or running the `Experiment` (see
#'   [visualize_experiment()] and [run_experiment()]), the named arguments
#'   `fit_results`, `eval_results`, and `vary_params` are automatically passed
#'   into the `Visualizer` function `.viz_fun()` and serve as placeholders for
#'   the [fit_experiment()] results, the [evaluate_experiment()] results, and
#'   the name of the varying parameter, respectively.
#'
#'   To visualize the performance of a method's fit and/or its evaluation
#'   metrics then, the `Visualizer` function `.viz_fun()` should take in the
#'   named arguments `fit_results` and/or `eval_results`. See [fit_experiment()]
#'   for details on the format of `fit_results`. See [evaluate_experiment()] for
#'   details on the format of `eval_results`. If the `Visualizer` is used within
#'   an `Experiment` with varying parameters, `vary_params` should be used as a
#'   stand in for the name of this varying parameter.
#'
#' @seealso [create_visualizer]
#'
#' @examples
#' ## create toy DGPs, Methods, Evaluators, and Visualizers
#'
#' # generate toy data
#' dgp <- create_dgp(
#'   .dgp_fun = function(n) rnorm(n), .name = "DGP", n = 100
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
#'     fit_results %>%
#'       dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) %>%
#'       dplyr::summarise(sd = sd(mean), .groups = "keep")
#'   },
#'   .name = "SD of Mean(x)"
#' )
#'
#' # plot SD of mean(x) across simulation replicates
#' sd_mean_viz <- Visualizer$new(
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
#' experiment <- create_experiment(name = "Experiment Name") %>%
#'   add_dgp(dgp) %>%
#'   add_method(mean_method) %>%
#'   add_evaluator(sd_mean_eval) %>%
#'   add_visualizer(sd_mean_viz)
#'
#' # fit, evaluate, and visualize (i.e., run) experiment
#' fit_results <- fit_experiment(experiment, n_reps = 10)
#'
#' eval_results <- evaluate_experiment(experiment, fit_results)
#'
#' viz_results <- visualize_experiment(experiment, fit_results, eval_results)
#'
#' # example usage of generate_dgp which generates one replicate
#' data_out <- dgp$generate()
#'
#' # example usage of fit_method on a single data replicate
#' method_out <- mean_method$fit(data_out)
#'
#' # example usage of evaluate_evaluator and visualize_visualizer
#' eval_out <- sd_mean_eval$evaluate(fit_results)
#' all.equal(eval_out, eval_results[[1]])
#'
#' viz_out <- sd_mean_viz$visualize(fit_results, eval_results)
#' all.equal(viz_out, viz_results[[1]])
#'
#' @export
Visualizer <- R6::R6Class(
  classname = 'Visualizer',
  public = list(

    #' @field name The name of the `Visualizer`.
    name = NULL,

    #' @field viz_fun The user-defined visualization function.
    viz_fun = NULL,

    #' @field viz_params A (named) list of default parameters to input into
    #'   the visualization function.
    viz_params = NULL,

    #' @field doc_options List of options to control the aesthetics of
    #'   the `Visualizer`'s visualization in the knitted R Markdown report.
    doc_options = list(height = 6, width = 10),

    #' @field doc_show Boolean indicating whether or not the resulting
    #'   visualizations are shown in the R Markdown report.
    doc_show = TRUE,

    # NOTE: R6 methods can't use the `@inheritParams` tag. If you want to update
    # the `@param` tags below, do so in the `create_visualizer()` docs above and
    # then copy-paste the corresponding `@param` tags below.

    #' @description Initialize a new `Visualizer` object.
    #'
    #' @param .viz_fun The user-defined visualization function.
    #' @param .name (Optional) The name of the `Visualizer`.
    #' @param .doc_options (Optional) List of options to control the aesthetics of
    #'   the `Visualizer`'s visualization in the knitted R Markdown report.
    #'   Currently, possible options are "height" and "width" (in inches). The
    #'   argument must be specified by position or typed out in whole; no partial
    #'   matching is allowed for this argument.
    #' @param .doc_show If `TRUE` (default), show the resulting visualization in the
    #'   R Markdown report; if `FALSE`, hide output in the R Markdown report.
    #' @param ... User-defined default arguments to pass into `.viz_fun()`.
    #'
    #' @return A new instance of `Visualizer`.
    initialize = function(.viz_fun, .name = NULL, .doc_options = list(),
                          .doc_show = TRUE, ...) {
      self$viz_fun <- .viz_fun
      self$name <- .name
      for (opt in names(.doc_options)) {
        self$doc_options[[opt]] <- .doc_options[[opt]]
      }
      self$doc_show <- .doc_show
      self$viz_params <- rlang::list2(...)
    },

    #' @description Visualize the performance of methods and/or their evaluation
    #'   metrics from the \code{Experiment} using the \code{Visualizer} and the
    #'   provided parameters.
    #'
    #' @param fit_results A tibble, as returned by [fit_experiment()].
    #' @param eval_results A list of result tibbles, as returned by
    #'   [evaluate_experiment()].
    #' @param vary_params A vector of `DGP` or `Method` parameter names that are
    #'   varied across in the `Experiment`.
    #'
    #' @param ... Not used.
    #'
    #' @return Result of `Visualizer$viz_fun()`.
    visualize = function(fit_results = NULL, eval_results = NULL,
                         vary_params = NULL, ...) {
      args_list <- list(fit_results = fit_results,
        eval_results = eval_results,
        vary_params = vary_params)
      if (!identical(self$viz_params, list())) {
        always_args_list <- self$viz_params
      } else {
        always_args_list <- NULL
      }
      viz_results <- R.utils::doCall(self$viz_fun,
        args = args_list,
        alwaysArgs = always_args_list)
      return(viz_results)
    },

    #' @description Print the `Visualizer` in a nice format, showing the
    #'   `Visualizer`'s name, function, parameters, and R Markdown options.
    #'
    #' @return The original `Visualizer` object, invisibly.
    print = function() {
      if (is.null(self$name)) {
        cat("Visualizer Name: NULL \n")
      } else {
        cat("Visualizer Name:", self$name, "\n")
      }
      cat("   Function: ")
      cat(str(self$viz_fun, give.attr = F))
      cat("   Parameters: ")
      cat(str(self$viz_params,
        indent.str = "     ", no.list = F))
      cat("   R Markdown Options: ")
      cat(str(self$doc_options,
        indent.str = "     ", no.list = F))
      cat("   Show in R Markdown:", self$doc_show, "\n")
      invisible(self)
    }
  )
)

