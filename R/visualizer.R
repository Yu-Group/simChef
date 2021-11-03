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
    rmd_options = list(height = 6, width = 10),
    show = TRUE,
    initialize = function(viz_fun, name = NULL, rmd_options = list(),
                          show = TRUE, ...) {
      dots_list <- list(...)
      if (".args_list" %in% names(dots_list)) {
        args_list <- dots_list[[".args_list"]]
      } else {
        args_list <- make_initialize_arg_list(viz_fun, name = name,
                                              rmd_options = rmd_options,
                                              show = show, ..., which = -2)
      }
      self$viz_fun <- args_list$viz_fun
      self$name <- args_list$name
      rmd_options <- args_list$rmd_options
      for (opt in names(rmd_options)) {
        self$rmd_options[[opt]] <- rmd_options[[opt]]
      }
      self$show <- args_list$show
      args_list$viz_fun <- NULL
      args_list$name <- NULL
      args_list$rmd_options <- NULL
      args_list$show <- NULL
      self$viz_params <- args_list
    },
    # @description Visualize the performance of methods and/or their evaluation
    #   metrics from the \code{Experiment} using the \code{Visualizer} and the
    #   provided parameters.
    #
    # @param fit_results A tibble, typically returned by the
    #   \code{Experiment$fit()} method.
    # @param eval_results A list of tibbles, typically returned by the
    #   \code{Experiment$evaluate()} method.
    # @param vary_params Name of parameter/argument that was varied in the
    #   \code{Experiment} (see \code{Experiment$get_vary_across()}).
    #   Use \code{NULL} (default) if no \code{vary_across} component in
    #   \code{Experiment} run.
    # @param ... Not used.
    #
    # @return Result of \code{viz_fun()}.
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
    # @description Print a \code{Visualizer} in a nice format, showing the 
    #   \code{Visualizer}'s name, function, parameters, and R Markdown options.
    #
    # @return The original \code{Visualizer} object.
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
      cat(str(self$rmd_options,
              indent.str = "     ", no.list = F))
      cat("   Show in R Markdown:", self$show)
      invisible(self)
    }
  )
)

#' Create a new \code{Visualizer}.
#'
#' @name create_visualizer
#'
#' @param viz_fun The visualization function.
#' @param name (Optional) The name of the \code{Visualizer}. The argument must
#'   be specified by position or typed out in whole; no partial matching is
#'   allowed for this argument.
#' @param rmd_options (Optional) List of options to control the aesthetics of
#'   the \code{Visualizer}'s visualization in the knitted R Markdown report.
#'   Currently, possible options are "height" and "width" (in inches). The
#'   argument must be specified by position or typed out in whole; no partial
#'   matching is allowed for this argument.
#' @param show If \code{TRUE} (default), show the resulting visualization in the R
#'   Markdown report; if \code{FALSE}, hide output in the R Markdown report.
#' @param ... Arguments to pass into \code{viz_fun()}.
#'
#' @details When visualizing or running the \code{Experiment} (see
#'   \code{Experiment$visualize()} or \code{Experiment$run()}), the named
#'   arguments \code{fit_results}, \code{eval_results}, and
#'   \code{vary_params} are automatically passed into the \code{Visualizer}
#'   function \code{viz_fun()} and serve as placeholders for the
#'   \code{Experiment$fit()} results, the \code{Experiment$evaluate()}
#'   results, and the name of the varying parameter, respectively.
#'   To visualize the performance of a method(s) fit and/or its evaluation
#'   metrics then, the \code{Visualizer} function \code{viz_fun()} should
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
#' power_plot1 <- create_visualizer(viz_fun = power_plot_fun, 
#'                                  name = "Power Plot (X1)")
#' # create Visualizer using non-default arguments (i.e., col = "X2")
#' power_plot2 <- create_visualizer(viz_fun = power_plot_fun,
#'                                  name = "Power Plot (X2)",
#'                                  # additional named parameters to pass to power_plot_fun()
#'                                  col = "X2")
#' 
#' # create Visualizer from a function in the built-in Visualizer library
#' pred_err_plot <- create_visualizer(viz_fun = plot_pred_err,
#'                                    name = "Prediction Error Plot",
#'                                    # additional named parameters to pass to plot_pred_err()
#'                                    truth_col = "y", estimate_col = "predictions")
#'                                   
#' # change figure height/width when displaying Visualizer in Rmd report
#' pred_err_plot <- create_visualizer(viz_fun = plot_pred_err,
#'                                    name = "Prediction Error Plot",
#'                                    rmd_options = list(height = 8, width = 12),
#'                                    # additional named parameters to pass to plot_pred_err()
#'                                    truth_col = "y", estimate_col = "predictions")
#'
#' @export
create_visualizer <- function(viz_fun, name = NULL,
                              rmd_options = list(), show = TRUE, ...) {
  args_list <- make_initialize_arg_list(viz_fun, name = name,
                                        rmd_options = rmd_options, show = show,
                                        ...)
  do.call(Visualizer$new, list(.args_list = args_list))
}
