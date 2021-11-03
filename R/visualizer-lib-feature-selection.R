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
#' @examples
#' # generate example fit_results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   feature_info = lapply(
#'     1:4,
#'     FUN = function(i) {
#'       tibble::tibble(
#'         # feature names
#'         feature = c("featureA", "featureB", "featureC"),
#'         # estimated feature importance scores
#'         est_importance = c(10, runif(2, min = -2, max = 2))  
#'       )
#'     }
#'   )
#' )
#' 
#' # generate example eval_results data
#' eval_results <- list(
#'   `Feature Importance` = summarize_feature_importance(
#'     fit_results,
#'     nested_data = "feature_info",
#'     feature_col = "feature",
#'     imp_col = "est_importance"
#'   )
#' )
#' 
#' # create bar plot using pre-computed evaluation results
#' plt <- plot_feature_importance(fit_results = fit_results,
#'                                eval_results = eval_results,
#'                                evaluator_name = "Feature Importance",
#'                                feature_col = "feature")
#' # or alternatively, create the same plot without pre-computing evaluation results
#' plt <- plot_feature_importance(fit_results,
#'                                nested_data = "feature_info",
#'                                feature_col = "feature",
#'                                imp_col = "est_importance")
#' 
#' # can customize plot (see plot_eval_summary() for possible arguments)
#' plt <- plot_feature_importance(fit_results = fit_results,
#'                                eval_results = eval_results,
#'                                evaluator_name = "Feature Importance",
#'                                feature_col = "feature",
#'                                errorbar_args = list(width = .5, position = "dodge"),
#'                                bar_args = list(width = .5))
#' 
#' @importFrom rlang .data
#' @export
plot_feature_importance <- function(fit_results, eval_results = NULL, 
                                    evaluator_name = NULL, vary_params = NULL,
                                    feature_col, show_max_features = NULL, 
                                    show = c("errorbar", "bar"), ...) {
  .imp_est <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_dot_args(
    user_args = list(...), 
    default_args = list(eval_id = "feature_importance",
                        eval_fun = "summarize_feature_importance",
                        x_str = feature_col,
                        errorbar_args = list(position = "dodge"))
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
#' @examples
#' # generate example fit_results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   feature_info = lapply(
#'     1:4,
#'     FUN = function(i) {
#'       tibble::tibble(
#'         # feature names
#'         feature = c("featureA", "featureB", "featureC"),  
#'         # true feature support
#'         true_support = c(TRUE, FALSE, TRUE),  
#'         # estimated feature support
#'         est_support = c(TRUE, FALSE, FALSE),  
#'         # estimated feature importance scores
#'         est_importance = c(10, runif(2, min = -2, max = 2))  
#'       )
#'     }
#'   )
#' )
#' 
#' # generate example eval_results data
#' eval_results <- list(
#'   `Feature Selection Errors` = summarize_feature_selection_err(
#'     fit_results, 
#'     nested_data = "feature_info",
#'     truth_col = "true_support", 
#'     estimate_col = "est_support",
#'     imp_col = "est_importance"
#'   )
#' )
#' 
#' # create bar plot using pre-computed evaluation results
#' plt <- plot_feature_selection_err(fit_results = fit_results, 
#'                                   eval_results = eval_results,
#'                                   evaluator_name = "Feature Selection Errors",
#'                                   show = c("bar"))
#' # or alternatively, create the same plot without pre-computing evaluation results
#' plt <- plot_feature_selection_err(fit_results,
#'                                   show = c("bar"),
#'                                   nested_data = "feature_info",
#'                                   truth_col = "true_support",
#'                                   estimate_col = "est_support",
#'                                   imp_col = "est_importance")
#' 
#' # can customize plot (see plot_eval_summary() for possible arguments)
#' plt <- plot_feature_selection_err(fit_results = fit_results, 
#'                                   eval_results = eval_results,
#'                                   evaluator_name = "Feature Selection Errors",
#'                                   show = c("bar"),
#'                                   color_str = ".dgp_name",
#'                                   interactive = TRUE)
#' 
#' @export
plot_feature_selection_err <- function(fit_results, eval_results = NULL, 
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
#' @examples
#' # generate example fit_results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   feature_info = lapply(
#'     1:4,
#'     FUN = function(i) {
#'       tibble::tibble(
#'         # feature names
#'         feature = c("featureA", "featureB", "featureC"),  
#'         # true feature support
#'         true_support = c(TRUE, FALSE, TRUE),  
#'         # estimated feature support
#'         est_support = c(TRUE, FALSE, FALSE),  
#'         # estimated feature importance scores
#'         est_importance = c(10, runif(2, min = -2, max = 2))  
#'       )
#'     }
#'   )
#' )
#' 
#' # generate example eval_results data
#' eval_results <- list(
#'   ROC = summarize_feature_selection_curve(
#'     fit_results, 
#'     curve = "ROC",
#'     nested_data = "feature_info",
#'     truth_col = "true_support", 
#'     imp_col = "est_importance"
#'   ),
#'   PR = summarize_feature_selection_curve(
#'     fit_results, 
#'     curve = "PR",
#'     nested_data = "feature_info",
#'     truth_col = "true_support", 
#'     imp_col = "est_importance"
#'   )
#' )
#' 
#' # create summary ROC/PR plots using pre-computed evaluation results
#' roc_plt <- plot_feature_selection_curve(fit_results = fit_results, 
#'                                         eval_results = eval_results,
#'                                         evaluator_name = "ROC", curve = "ROC",
#'                                         show = c("line", "ribbon"))
#' pr_plt <- plot_feature_selection_curve(fit_results = fit_results, 
#'                                        eval_results = eval_results,
#'                                        evaluator_name = "PR", curve = "PR",
#'                                        show = c("line", "ribbon"))
#' # or alternatively, create the same plots without pre-computing evaluation results
#' roc_plt <- plot_feature_selection_curve(fit_results, show = c("line", "ribbon"),
#'                                         nested_data = "feature_info",
#'                                         truth_col = "true_support",
#'                                         imp_col = "est_importance",
#'                                         curve = "ROC")
#' pr_plt <- plot_feature_selection_curve(fit_results, show = c("line", "ribbon"),
#'                                        nested_data = "feature_info",
#'                                        truth_col = "true_support",
#'                                        imp_col = "est_importance",
#'                                        curve = "PR")
#' 
#' # can customize plot (see plot_eval_summary() for possible arguments)
#' roc_plt <- plot_feature_selection_curve(fit_results = fit_results, 
#'                                         eval_results = eval_results,
#'                                         evaluator_name = "ROC", curve = "ROC",
#'                                         show = c("line", "ribbon"),
#'                                         plot_by = ".dgp_name")
#' 
#' @export
plot_feature_selection_curve <- function(fit_results, eval_results = NULL,
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

