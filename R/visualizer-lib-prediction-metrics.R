#' Plot prediction error according to various metrics.
#' 
#' @description Plot the raw or summarized prediction errors as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @param metrics A \code{metric_set} object indicating the metrics to plot.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   use the default metrics in [yardstick::metrics()].
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_pred_err()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family prediction_error_funs
#' 
#' @export
plot_pred_err <- function(fit_results, eval_results, evaluator_name = NULL,
                          vary_params = NULL, metrics = NULL,
                          show = c("point", "line"), ...) {
  .metric <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_dot_args(
    user_args = list(...), 
    default_args = list(eval_id = "pred_err",
                        eval_fun = "summarize_pred_err",
                        facet_wrap_formula = ~ .metric,
                        facet_args = list(scales = "free"))
  )
  
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  eval_tib <- NULL
  if (!is.null(evaluator_name)) {
    eval_tib <- eval_results[[evaluator_name]]
    if (!is.null(metrics)) {
      metric_names <- names(attr(metrics, "metrics"))
      eval_tib <- eval_tib %>%
        dplyr::filter(.metric %in% metric_names)
    }
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_tib = eval_tib,
                  vary_params = vary_params, show = show, metrics = metrics),
             arg_list)
  )
  return(plt)
}

#' Plot ROC/PR curves.
#' 
#' @description Plot ROC/PR curves or some summary thereof across experimental
#'   replicates.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_pred_curve()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family prediction_error_funs
#' 
#' @export
plot_pred_curve <- function(fit_results, eval_results, evaluator_name = NULL,
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
    user_args = list(...), 
    default_args = list(eval_id = eval_id,
                        eval_fun = "summarize_pred_curve",
                        x_str = x_str,
                        ribbon_args = list(alpha = 0.2))
  )
  
  eval_tib <- NULL
  if (!is.null(evaluator_name)) {
    eval_tib <- eval_results[[evaluator_name]]
    if (("recall" %in% colnames(eval_tib)) && (identical(curve, "ROC"))) {
      eval_tib <- NULL
    } else if (("FPR" %in% colnames(eval_tib)) && (identical(curve, "PR"))) {
      eval_tib <- NULL
    }
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_tib = eval_tib,
                  vary_params = vary_params, show = show, curve = curve),
             arg_list)
  )
  return(plt)
}
