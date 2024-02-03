#' Plot prediction error according to various metrics.
#'
#' @description Plot the raw or summarized prediction errors as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars.
#'
#' @inheritParams shared_viz_lib_args
#' @inheritParams shared_experiment_helpers_args
#' @param metrics A \code{metric_set} object indicating the metrics to plot.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   use the default metrics in [yardstick::metrics()].
#'
#' @inherit plot_eval_constructor return
#'
#' @family prediction_error_funs
#'
#' @examples
#' # generate example fit_results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4, FUN = function(x) rnorm(100)),
#'   # predicted response
#'   predictions = lapply(1:4, FUN = function(x) rnorm(100))
#' )
#'
#' # generate example eval_results data
#' eval_results <- list(
#'   `Prediction Errors` = summarize_pred_err(
#'     fit_results, truth_col = "y", estimate_col = "predictions"
#'   )
#' )
#'
#' # create errorbar plot using pre-computed evaluation results
#' plt <- plot_pred_err(eval_results = eval_results,
#'                      eval_name = "Prediction Errors",
#'                      show = c("point", "errorbar"))
#' # or alternatively, create the same plot directly from fit results
#' plt <- plot_pred_err(fit_results = fit_results,
#'                      show = c("point", "errorbar"),
#'                      eval_fun_options = list(truth_col = "y",
#'                                              estimate_col = "predictions"))
#'
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' plt <- plot_pred_err(fit_results = fit_results, eval_results = eval_results,
#'                      eval_name = "Prediction Errors",
#'                      show = c("point", "errorbar"),
#'                      color_str = NULL,
#'                      facet_formula = .method_name ~ .metric,
#'                      facet_type = "grid")
#'
#' @export
plot_pred_err <- function(fit_results = NULL,
                          eval_results = NULL, eval_name = NULL,
                          eval_fun = "summarize_pred_err",
                          eval_fun_options = NULL,
                          vary_params = NULL, metrics = NULL,
                          show = c("point", "line"), ...) {
  .metric <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_dot_args(
    user_args = rlang::list2(...),
    default_args = list(eval_id = "pred_err",
                        facet_formula = ~ .metric,
                        facet_type = "wrap",
                        facet_args = list(scales = "free"))
  )

  plot_data <- get_plot_data(
    fit_results = fit_results,
    eval_results = eval_results,
    eval_name = eval_name,
    eval_fun = eval_fun,
    eval_fun_options = c(eval_fun_options, list(metrics = metrics))
  )

  if (!is.null(metrics)) {
    if (!inherits(metrics, "metric_set")) {
      abort("Unknown metrics. metrics must be of class 'yardstick::metric_set' or NULL.")
    }
    metric_names <- names(attr(metrics, "metrics"))
    plot_data <- plot_data %>%
      dplyr::filter(.metric %in% metric_names)
  }

  plt <- do.call(
    plot_eval_constructor,
    args = c(
      list(plot_data = plot_data, vary_params = vary_params, show = show),
      arg_list
    )
  )
  return(plt)
}

#' Plot ROC/PR curves.
#'
#' @description Plot ROC/PR curves or some summary thereof across experimental
#'   replicates.
#'
#' @inheritParams shared_viz_lib_args
#' @inheritParams shared_experiment_helpers_args
#'
#' @inherit plot_eval_constructor return
#'
#' @family prediction_error_funs
#'
#' @examples
#' # generate example fit_results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4,
#'              FUN = function(x) {
#'                as.factor(sample(0:1, size = 100, replace = TRUE))
#'              }),
#'   # predicted class probabilities
#'   class_probs = lapply(1:4, FUN = function(x) runif(n = 100, min = 0, max = 1))
#' )
#'
#' # generate example eval_results data
#' eval_results <- list(
#'   ROC = summarize_pred_curve(
#'     fit_results, truth_col = "y", prob_cols = "class_probs", curve = "ROC"
#'   ),
#'   PR = summarize_pred_curve(
#'     fit_results, truth_col = "y", prob_cols = "class_probs", curve = "PR"
#'   )
#' )
#'
#' # create summary ROC/PR plots using pre-computed evaluation results
#' roc_plt <- plot_pred_curve(eval_results = eval_results,
#'                            eval_name = "ROC", curve = "ROC",
#'                            show = c("line", "ribbon"))
#' pr_plt <- plot_pred_curve(eval_results = eval_results,
#'                           eval_name = "PR", curve = "PR",
#'                           show = c("line", "ribbon"))
#' # or alternatively, create the same plots directly from fit results
#' roc_plt <- plot_pred_curve(fit_results = fit_results,
#'                            show = c("line", "ribbon"), curve = "ROC",
#'                            eval_fun_options = list(truth_col = "y",
#'                                                    prob_cols = "class_probs"))
#' pt_plt <- plot_pred_curve(fit_results = fit_results,
#'                           show = c("line", "ribbon"), curve = "PR",
#'                           eval_fun_options = list(truth_col = "y",
#'                                                   prob_cols = "class_probs"))
#'
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' roc_plt <- plot_pred_curve(eval_results = eval_results,
#'                            eval_name = "ROC", curve = "ROC",
#'                            show = c("line", "ribbon"),
#'                            plot_by = ".dgp_name")
#'
#' @export
plot_pred_curve <- function(fit_results = NULL,
                            eval_results = NULL, eval_name = NULL,
                            eval_fun = "summarize_pred_curve",
                            eval_fun_options = NULL,
                            vary_params = NULL, curve = c("ROC", "PR"),
                            show = c("line", "ribbon"), ...) {
  curve <- match.arg(curve)
  if (curve == "ROC") {
    eval_id <- "TPR"
    x_str <- "FPR"
  } else if (curve == "PR") {
    eval_id <- "precision"
    x_str <- "recall"
  }
  arg_list <- get_dot_args(
    user_args = rlang::list2(...),
    default_args = list(
      eval_id = eval_id,
      x_str = x_str,
      ribbon_args = list(alpha = 0.2),
      add_ggplot_layers = list(ggplot2::coord_cartesian(ylim = c(0, 1)))
    )
  )

  plot_data <- get_plot_data(
    fit_results = fit_results,
    eval_results = eval_results,
    eval_name = eval_name,
    eval_fun = eval_fun,
    eval_fun_options = c(eval_fun_options, list(curve = curve))
  )

  plt <- do.call(
    plot_eval_constructor,
    args = c(
      list(plot_data = plot_data, vary_params = vary_params, show = show),
      arg_list
    )
  )
  return(plt)
}
