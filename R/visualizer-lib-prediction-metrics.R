#' Plot prediction error according to various metrics.
#' 
#' @description Plot raw or summarized prediction errors as a boxplot, scatter
#'   plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams plot_eval_summary
#' @param metrics Prediction error metrics for which to plot results. If 
#'   \code{evaluator_name} is provided, then this argument is optional, and the
#'   default is to use all metrics found in the Evaluator's prediction error
#'   results. Otherwise, this argument is required.
#' @param group (Optional) vector of group ids (as a factor) to use for
#'   assessing within-group prediction errors.
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}.
#' 
#' @return A ggplot object or list of ggplot objects.
#' 
#' @export
plot_pred_err <- function(fit_results, eval_results, evaluator_name = NULL,
                          vary_params = NULL, metrics = NULL, group = NULL,
                          show = c("boxplot", "point", "line", "bar", 
                                   "errorbar"), ...) {
  eval_out <- NULL
  if (!is.null(evaluator_name)) {
    eval_out <- eval_results[[evaluator_name]]
    if (!is.null(evaluator_name) & is.null(metrics)) {
      metrics <- unique(eval_out$metric)
    } else if (is.null(metrics)) {
      stop("Must specify which metrics to plot.")
    }
    eval_out <- eval_out %>%
      dplyr::filter(metric %in% metrics)
  }
  arg_list <- get_args(
    user_args = list(...), 
    default_args = list(eval_id = "pred_err",
                        eval_fun = "summarize_pred_err",
                        facet_wrap_formula = ~ metric,
                        facet_args = list(scales = "free"))
  )
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, eval_out = eval_out,
                  vary_params = vary_params, show = show, 
                  metrics = metrics, group = group),
             arg_list)
  )
  return(plt)
}

#' Plot ROC/PR curves.
#' 
#' @description Plot ROC/PR curves or some summary thereof across experimental
#'   repetitions.
#' 
#' @inheritParams plot_eval_summary
#' @param metric A character string. Either "ROC" or "PR".
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}.
#' 
#' @return A ggplot object or list of ggplot objects.
#' 
plot_auc_curve <- function(fit_results, eval_results, evaluator_name = NULL,
                           vary_params = NULL, metric = c("ROC", "PR"),
                           show = c("boxplot", "point", "line", "bar", 
                                    "errorbar", "ribbon"), ...) {
  metric <- match.arg(metric)
  eval_out <- NULL
  if (!is.null(evaluator_name)) {
    eval_out <- eval_results[[evaluator_name]]
    if (!is.null(evaluator_name) & is.null(metric)) {
      metric <- unique(eval_out$metric)
    } else if (is.null(metric)) {
      stop("Must specify which metric to plot.")
    }
    m <- metric
    eval_out <- eval_out %>%
      dplyr::filter(metric == m)
  }
  if (metric == "ROC") {
    eval_id <- "TPR"
    x_str <- "FPR"
  } else if (metric == "PR") {
    eval_id <- "Precision"
    x_str <- "Recall"
  }
  arg_list <- get_args(
    user_args = list(...), 
    default_args = list(eval_id = eval_id,
                        eval_fun = "summarize_auc_curve",
                        x_str = x_str)
  )
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, eval_out = eval_out,
                  vary_params = vary_params, show = show, metric = metric),
             arg_list)
  )
  return(plt)
}
