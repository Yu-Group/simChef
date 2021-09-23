#' Plot feature importances.
#' 
#' @description Plot raw or summarized feature importances as a boxplot, scatter
#'   plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams plot_eval_summary
#' @param feature_col Character string. Name of the feature column.
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}.
#' 
#' @return A ggplot object or list of ggplot objects.
#' 
#' @export
plot_feature_imp <- function(fit_results, eval_results, evaluator_name = NULL, 
                             vary_params = NULL, feature_col = "feature",
                             show = c("boxplot", "point", "line", "bar",
                                      "errorbar"), ...) {
  arg_list <- get_args(
    user_args = list(...), 
    default_args = list(eval_id = "feature_imp",
                        eval_fun = "summarize_feature_importances",
                        x_str = feature_col)
  )
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, vary_params = vary_params,
                  show = show),
             arg_list)
  )
  return(plt)
}

#' Plot feature recovery error according to various metrics
#' 
#' @description Plot raw or summarized feature recovery errors as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams plot_eval_summary
#' @param metrics Prediction error metrics for which to plot results. If 
#'   \code{evaluator_name} is provided, then this argument is optional, and the
#'   default is to use all metrics found in the Evaluator's prediction error
#'   results. Otherwise, this argument is required.
#' @param metrics Character vector of the support recovery metrics to plot. If
#'   \code{evaluator_name} is provided, then this argument is optional, and the
#'   default is to use all metrics found in the Evaluator's feature recover
#'   results. Otherwise, this argument is required.
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}.
#' 
#' @return A ggplot object or list of ggplot objects.
#' 
#' @export
plot_feature_recovery <- function(fit_results, eval_results, 
                                  evaluator_name = NULL, vary_params = NULL,
                                  metrics = c("TPR", "FPR", "FDR", "AUROC",
                                              "AUPRC"), 
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
    default_args = list(eval_id = "feature_recovery",
                        eval_fun = "summarize_feature_recovery",
                        facet_wrap_formula = ~ metric,
                        facet_args = list(scales = "free"))
  )
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, eval_out = eval_out,
                  vary_params = vary_params, show = show, metrics = metrics),
             arg_list)
  )
  return(plt)
}
