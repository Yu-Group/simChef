#' \code{R6} class representing a visualizer.
#'
#' @docType class
#'
#' @description A visualizer (or visualization function) to **visualize** the performance of
#'   methods and/or its evaluation metrics from the \code{Experiment} run.
#'
#' @template visualizer-template
#'
#' @export
Visualizer <- R6::R6Class(
  classname = 'Visualizer',
  public = list(
    name = NULL,
    viz_fun = NULL,
    viz_params = NULL,
    doc_options = list(height = 6, width = 10),
    doc_show = TRUE,
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

#' Create a new \code{Visualizer}.
#'
#' @name create_visualizer
#'
#' @param .viz_fun The visualization function.
#' @param .name (Optional) The name of the \code{Visualizer}.
#' @param .doc_options (Optional) List of options to control the aesthetics of
#'   the \code{Visualizer}'s visualization in the knitted R Markdown report.
#'   Currently, possible options are "height" and "width" (in inches). The
#'   argument must be specified by position or typed out in whole; no partial
#'   matching is allowed for this argument.
#' @param .doc_show If \code{TRUE} (default), show the resulting visualization in
#'   the R Markdown report; if \code{FALSE}, hide output in the R Markdown
#'   report.
#' @param ... Arguments to pass into \code{.viz_fun()}.
#'
#' @details When visualizing or running the \code{Experiment} (see
#'   \code{Experiment$visualize()} or \code{Experiment$run()}), the named
#'   arguments \code{fit_results}, \code{eval_results}, and
#'   \code{vary_params} are automatically passed into the \code{Visualizer}
#'   function \code{.viz_fun()} and serve as placeholders for the
#'   \code{Experiment$fit()} results, the \code{Experiment$evaluate()}
#'   results, and the name of the varying parameter, respectively.
#'   To visualize the performance of a method(s) fit and/or its evaluation
#'   metrics then, the \code{Visualizer} function \code{.viz_fun()} should
#'   take in the named arguments \code{fit_results} and/or
#'   \code{eval_results}. See \code{Experiment$fit()} or
#'   \code{fit_experiment()} for details on the format of \code{fit_results}.
#'   See \code{Experiment$evaluate()} or \code{evaluate_experiment()} for
#'   details on the format of \code{eval_results}. If the \code{Visualizer}
#'   is used for \code{Experiments} with varying parameters,
#'   \code{vary_params} should be used as a stand in for the name of this
#'   varying parameter.
#'
#' @return A new instance of \code{Visualizer}.
#'
#' @examples
#' # create an example Visualizer function
#' power_plot_fun <- function(fit_results, vary_params = NULL, col = "X1") {
#'
#'   if (!is.null(vary_params)) {
#'     # deal with the case when we vary across a parameter that is vector-valued
#'     if (is.list(fit_results[[vary_params]])) {
#'       fit_results[[vary_params]] <- list_col_to_chr(fit_results[[vary_params]],
#'                                                     name = vary_params,
#'                                                     verbatim = TRUE)
#'     }
#'   }
#'
#'   plt <- ggplot2::ggplot(fit_results) +
#'     ggplot2::aes(x = .data[[paste(col, "p-value")]],
#'                  color = as.factor(.method_name)) +
#'     ggplot2::geom_abline(slope = 1, intercept = 0,
#'                          color = "darkgray", linetype = "solid", size = 1) +
#'     ggplot2::stat_ecdf(size = 1) +
#'     ggplot2::scale_x_continuous(limits = c(0, 1)) +
#'     ggplot2::labs(x = "t", y = "P( p-value \u2264 t )",
#'                   linetype = "", color = "Method")
#'   if (!is.null(vary_params)) {
#'     plt <- plt + ggplot2::facet_wrap(~ .data[[vary_params]])
#'   }
#'   return(plt)
#' }
#'
#' # create Visualizer using the default arguments (i.e., col = "X1")
#' power_plot1 <- create_visualizer(.viz_fun = power_plot_fun,
#'                                  .name = "Power Plot (X1)")
#' # create Visualizer using non-default arguments (i.e., col = "X2")
#' power_plot2 <- create_visualizer(.viz_fun = power_plot_fun,
#'                                  .name = "Power Plot (X2)",
#'                                  # additional named parameters to pass to power_plot_fun()
#'                                  col = "X2")
#'
#' # create Visualizer from a function in the built-in Visualizer library
#' pred_err_plot <- create_visualizer(.viz_fun = plot_pred_err,
#'                                    .name = "Prediction Error Plot",
#'                                    # additional named parameters to pass to plot_pred_err()
#'                                    truth_col = "y", estimate_col = "predictions")
#'
#' # change figure height/width when displaying Visualizer in Rmd report
#' pred_err_plot <- create_visualizer(.viz_fun = plot_pred_err,
#'                                    .name = "Prediction Error Plot",
#'                                    .doc_options = list(height = 8, width = 12),
#'                                    # additional named parameters to pass to plot_pred_err()
#'                                    truth_col = "y", estimate_col = "predictions")
#'
#' @export
create_visualizer <- function(.viz_fun, .name = NULL, .doc_options = list(),
                              .doc_show = TRUE, ...) {
  Visualizer$new(.viz_fun, .name, .doc_options, .doc_show, ...)
}

#' Visualize a \code{Visualizer}.
#'
#' @name visualize_visualizer
#' @description Visualize the performance of methods and/or their evaluation
#'   metrics from the \code{Experiment} using the \code{Visualizer} and the
#'   provided parameters.
#'
#' @inheritParams shared_experiment_helpers_args
#' @param visualizer A \code{Visualizer} object.
#' @param ... Not used.
#'
#' @return Result of \code{Visualizer$viz_fun()}.
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
#' experiment <- create_experiment(name = "Experiment Name") %>%
#'   add_dgp(dgp) %>%
#'   add_method(mean_method) %>%
#'   add_evaluator(sd_mean_eval) %>%
#'   add_visualizer(sd_mean_plot)
#'
#' # fit, evaluate, and visualize (i.e., run) experiment
#' fit_results <- fit_experiment(experiment, n_reps = 10)
#' eval_results <- evaluate_experiment(experiment, fit_results)
#' viz_results <- visualize_experiment(experiment, fit_results, eval_results)
#'
#' # example usage of generate_dgp which generates one replicate
#' data_out <- generate_dgp(dgp)
#'
#' # example usage of fit_method on a single data replicate
#' method_out <- fit_method(mean_method, data_out)
#'
#' # example usage of evaluate_evaluator and visualize_visualizer
#' eval_out <- evaluate_evaluator(sd_mean_eval, fit_results)
#' all.equal(eval_out, eval_results[[1]])
#' viz_out <- visualize_visualizer(sd_mean_plot, fit_results, eval_results)
#' all.equal(viz_out, viz_results[[1]])
#'
#' @export
visualize_visualizer <- function(visualizer, fit_results = NULL,
                                 eval_results = NULL, vary_params = NULL, ...) {
  visualizer$visualize(fit_results, eval_results, vary_params, ...)
}
