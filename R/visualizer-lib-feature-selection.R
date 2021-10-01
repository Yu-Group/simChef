#' Plot feature importances.
#' 
#' @description Plot the raw or summarized feature importances as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @inheritParams summarize_feature_importances
#' @param max_features Maximum number of features to plot.
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_feature_importances()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family feature_selection_funs
#' 
#' @importFrom rlang .data
#' @export
plot_feature_imp <- function(fit_results, eval_results, evaluator_name = NULL, 
                             vary_params = NULL, max_features = NULL, feature,
                             show = c("errorbar", "bar"), ...) {
  imp <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_args(
    user_args = list(...), 
    default_args = list(eval_id = "feature_imp",
                        eval_fun = "summarize_feature_importances",
                        x_str = feature)
  )
  
  eval_out <- get_eval_tibble(fit_results, eval_results, evaluator_name,
                              arg_list$eval_id, arg_list$eval_fun, 
                              vary_params, show, arg_list$y_str, 
                              feature = feature, ...)
  
  if (!is.null(max_features) && 
      (paste0("mean_", arg_list$eval_id) %in% colnames(eval_out))) {
    keep_features <- eval_out %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(feature))) %>%
      dplyr::summarise(imp = max(.data[[paste0("mean_", arg_list$eval_id)]],
                                 na.rm = TRUE)) %>%
      dplyr::arrange(-imp) %>%
      dplyr::slice(1:min(max_features, nrow(.data))) %>%
      dplyr::pull(tidyselect::all_of(feature))
    eval_out <- eval_out %>%
      dplyr::filter(.data[[feature]] %in% keep_features) %>%
      dplyr::mutate({{feature}} := factor(.data[[feature]], 
                                          levels = keep_features))
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_out = eval_out,
                  vary_params = vary_params, show = show, feature = feature),
             arg_list)
  )
  return(plt)
}

#' Plot feature recovery error according to various metrics
#' 
#' @description Plot the raw or summarized feature recovery errors as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams plot_pred_err
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_feature_recovery()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family feature_selection_funs
#' 
#' @export
plot_feature_recovery <- function(fit_results, eval_results, 
                                  evaluator_name = NULL, vary_params = NULL,
                                  metrics = NULL, 
                                  show = c("point", "line", "errorbar"), ...) {
  .metric <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_args(
    user_args = list(...), 
    default_args = list(eval_id = "feature_recovery",
                        eval_fun = "summarize_feature_recovery",
                        facet_wrap_formula = ~ .metric,
                        facet_args = list(scales = "free"))
  )
  
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  eval_out <- NULL
  if (!is.null(evaluator_name)) {
    eval_out <- eval_results[[evaluator_name]]
    if (!is.null(metrics)) {
      metric_names <- names(attr(metrics, "metrics"))
      eval_out <- eval_out %>%
        dplyr::filter(.metric %in% metric_names)
    }
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_out = eval_out,
                  vary_params = vary_params, show = show, metrics = metrics),
             arg_list)
  )
  return(plt)
}

#' Plot ROC/PR curves for feature recovery.
#' 
#' @description Plot ROC/PR curves for feature recovery or some summary thereof
#'   across experimental replicates.
#' 
#' @inheritParams plot_pred_curve
#' 
#' @inherit plot_eval_summary return
#' 
#' @family feature_selection_funs
#' 
#' @export
plot_feature_recovery_curve <- function(fit_results, eval_results,
                                        evaluator_name = NULL,
                                        vary_params = NULL, 
                                        metric = c("ROC", "PR"),
                                        show = c("line", "ribbon"), ...) {
  metric <- match.arg(metric)
  if (metric == "ROC") {
    eval_id <- "TPR"
    x_str <- "FPR"
  } else if (metric == "PR") {
    eval_id <- "precision"
    x_str <- "recall"
  }
  arg_list <- get_args(
    user_args = list(...), 
    default_args = list(eval_id = eval_id,
                        eval_fun = "summarize_feature_recovery_curve",
                        x_str = x_str,
                        ribbon_args = list(alpha = 0.2))
  )
  
  eval_out <- NULL
  if (!is.null(evaluator_name)) {
    eval_out <- eval_results[[evaluator_name]]
    if (("recall" %in% colnames(eval_out)) && (identical(metric, "ROC"))) {
      eval_out <- NULL
    } else if (("FPR" %in% colnames(eval_out)) && (identical(metric, "PR"))) {
      eval_out <- NULL
    }
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_out = eval_out,
                  vary_params = vary_params, show = show, metric = metric),
             arg_list)
  )
  return(plt)
}

