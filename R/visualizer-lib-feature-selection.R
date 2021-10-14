#' Plot feature importances.
#' 
#' @description Plot the raw or summarized feature importances as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @inheritParams summarize_feature_importance
#' @param show_max_features Maximum number of features to plot.
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_feature_importance()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family feature_selection_funs
#' 
#' @importFrom rlang .data
#' @export
plot_feature_importance <- function(fit_results, eval_results, 
                                    evaluator_name = NULL, vary_params = NULL,
                                    feature_col, show_max_features = NULL, 
                                    show = c("errorbar", "bar"), ...) {
  .imp_est <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_dot_args(
    user_args = list(...), 
    default_args = list(eval_id = "feature_importance",
                        eval_fun = "summarize_feature_importance",
                        x_str = feature_col)
  )
  
  if (!is.null(evaluator_name)) {
    eval_tib <- eval_results[[evaluator_name]]
  } else {
    eval_tib <- NULL
  }
  eval_tib <- get_eval_tibble(fit_results = fit_results, eval_tib = eval_tib,
                              eval_id = arg_list$eval_id, 
                              eval_fun = arg_list$eval_fun, 
                              vary_params = vary_params, show = show, 
                              y_str = arg_list$y_str, feature_col = feature_col,
                              ...)
  if (!is.null(arg_list$eval_id)) {
    eval_id <- paste0("_", arg_list$eval_id)
  }
  
  # filter to only keep top important features if show_max_features is set
  if (!is.null(show_max_features) && 
      (paste0("mean", eval_id) %in% colnames(eval_tib))) {
    keep_features <- eval_tib %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(feature_col))) %>%
      dplyr::summarise(.imp_est = max(.data[[paste0("mean", eval_id)]],
                                      na.rm = TRUE)) %>%
      dplyr::arrange(-.imp_est) %>%
      dplyr::slice(1:min(show_max_features, nrow(.data))) %>%
      dplyr::pull(tidyselect::all_of(feature_col))
    eval_tib <- eval_tib %>%
      dplyr::filter(.data[[feature_col]] %in% keep_features) %>%
      dplyr::mutate({{feature_col}} := factor(.data[[feature_col]], 
                                              levels = keep_features))
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_tib = eval_tib,
                  vary_params = vary_params, show = show, 
                  feature_col = feature_col),
             arg_list)
  )
  return(plt)
}

#' Plot feature selection error according to various metrics.
#' 
#' @description Plot the raw or summarized feature selection errors as a 
#'   boxplot, scatter plot, line plot, or bar plot with or without 1 SD error 
#'   bars. 
#' 
#' @inheritParams plot_pred_err
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_feature_selection_err()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family feature_selection_funs
#' 
#' @export
plot_feature_selection_err <- function(fit_results, eval_results, 
                                       evaluator_name = NULL, 
                                       vary_params = NULL, metrics = NULL, 
                                       show = c("point", "line", "errorbar"),
                                       ...) {
  arg_list <- get_dot_args(
    user_args = list(...),
    default_args = list(eval_id = "feature_selection",
                        eval_fun = "summarize_feature_selection_err")
  )
  plt <- do.call(
    plot_pred_err,
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, vary_params = vary_params,
                  metrics = metrics, show = show),
             arg_list)
  )
  return(plt)
}

#' Plot ROC/PR curves for feature selection.
#' 
#' @description Plot ROC/PR curves for feature selection or some summary thereof
#'   across experimental replicates.
#' 
#' @inheritParams plot_pred_curve
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_feature_selection_curve()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family feature_selection_funs
#' 
#' @export
plot_feature_selection_curve <- function(fit_results, eval_results,
                                         evaluator_name = NULL,
                                         vary_params = NULL, 
                                         curve = c("ROC", "PR"),
                                         show = c("line", "ribbon"), ...) {
  arg_list <- get_dot_args(
    user_args = list(...),
    default_args = list(eval_fun = "summarize_feature_selection_curve")
  )
  plt <- do.call(
    plot_pred_curve,
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, vary_params = vary_params,
                  curve = curve, show = show),
             arg_list)
  )
  return(plt)
}

