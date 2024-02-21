#' Plot feature importances.
#' 
#' @description Plot the raw or summarized feature importances as a boxplot,
#'   scatter plot, line plot, or bar plot with or without 1 SD error bars. 
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @inheritParams summarize_feature_importance
#' @inheritDotParams plot_eval_constructor -eval_results -eval_names -plot_data
#'   -vary_params -show
#' @param show_max_features Maximum number of features to plot.
#' 
#' @inherit plot_eval_constructor return
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
#'     nested_cols = "feature_info",
#'     feature_col = "feature",
#'     imp_col = "est_importance"
#'   )
#' )
#' 
#' # create bar plot using pre-computed evaluation results
#' plt <- plot_feature_importance(eval_results = eval_results,
#'                                eval_name = "Feature Importance",
#'                                feature_col = "feature")
#' # or alternatively, create the same plot directly from fit results
#' plt <- plot_feature_importance(fit_results = fit_results,
#'                                feature_col = "feature",
#'                                eval_fun_options = list(
#'                                  nested_cols = "feature_info",
#'                                  imp_col = "est_importance"
#'                                ))
#' 
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' plt <- plot_feature_importance(eval_results = eval_results,
#'                                eval_name = "Feature Importance",
#'                                feature_col = "feature",
#'                                errorbar_args = list(width = .5, position = "dodge"),
#'                                bar_args = list(width = .5))
#' 
#' @importFrom rlang .data
#' @export
plot_feature_importance <- function(fit_results = NULL,
                                    eval_results = NULL, eval_name = NULL,
                                    eval_fun = "summarize_feature_importance",
                                    eval_fun_options = NULL,
                                    vary_params = NULL,
                                    feature_col, show_max_features = NULL, 
                                    show = c("errorbar", "bar"), ...) {
  .imp_est <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_dot_args(
    user_args = rlang::list2(...),
    default_args = list(eval_id = "feature_importance",
                        x_str = feature_col,
                        errorbar_args = list(position = "dodge"))
  )
  
  plot_data <- get_plot_data(
    fit_results = fit_results,
    eval_results = eval_results,
    eval_name = eval_name,
    eval_fun = eval_fun,
    eval_fun_options = c(eval_fun_options, list(feature_col = feature_col))
  )

  # filter to only keep top important features if show_max_features is set
  if (!is.null(show_max_features)) {
    if (!is.null(arg_list$eval_id)) {
      eval_id <- paste0("_", arg_list$eval_id)
    }
    if (paste0("mean", eval_id) %in% colnames(plot_data)) {
      keep_features <- plot_data %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(feature_col))) %>%
        dplyr::summarise(.imp_est = max(.data[[paste0("mean", eval_id)]],
                                        na.rm = TRUE)) %>%
        dplyr::arrange(-.imp_est) %>%
        dplyr::slice(1:min(show_max_features, nrow(.data))) %>%
        dplyr::pull(tidyselect::all_of(feature_col))
      plot_data <- plot_data %>%
        dplyr::filter(.data[[feature_col]] %in% keep_features) %>%
        dplyr::mutate({{feature_col}} := factor(.data[[feature_col]],
                                                levels = keep_features))
    }
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

#' Plot feature selection error according to various metrics.
#' 
#' @description Plot the raw or summarized feature selection errors as a 
#'   boxplot, scatter plot, line plot, or bar plot with or without 1 SD error 
#'   bars. 
#' 
#' @inheritParams plot_pred_err
#' @inheritDotParams plot_eval_constructor -eval_results -eval_names -plot_data
#'   -vary_params -show
#' 
#' @inherit plot_eval_constructor return
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
#'     nested_cols = "feature_info",
#'     truth_col = "true_support", 
#'     estimate_col = "est_support",
#'     imp_col = "est_importance"
#'   )
#' )
#' 
#' # create bar plot using pre-computed evaluation results
#' plt <- plot_feature_selection_err(eval_results = eval_results,
#'                                   eval_name = "Feature Selection Errors",
#'                                   show = c("bar"))
#' # or alternatively, create the same plot directly from fit results
#' plt <- plot_feature_selection_err(fit_results = fit_results, show = c("bar"),
#'                                   eval_fun_options = list(
#'                                     nested_cols = "feature_info",
#'                                     truth_col = "true_support",
#'                                     estimate_col = "est_support",
#'                                     imp_col = "est_importance"
#'                                   ))
#' 
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' plt <- plot_feature_selection_err(eval_results = eval_results,
#'                                   eval_name = "Feature Selection Errors",
#'                                   show = c("bar"),
#'                                   color_str = ".dgp_name",
#'                                   interactive = TRUE)
#' 
#' @export
plot_feature_selection_err <- function(fit_results = NULL,
                                       eval_results = NULL, eval_name = NULL,
                                       eval_fun = "summarize_feature_selection_err",
                                       eval_fun_options = NULL,
                                       vary_params = NULL, metrics = NULL,
                                       show = c("point", "line", "errorbar"),
                                       ...) {
  arg_list <- get_dot_args(
    user_args = rlang::list2(...),
    default_args = list(eval_id = "feature_selection")
  )
  plt <- do.call(
    plot_pred_err,
    args = c(
      list(fit_results = fit_results,
           eval_results = eval_results, eval_name = eval_name,
           eval_fun = eval_fun, eval_fun_options = eval_fun_options,
           vary_params = vary_params, metrics = metrics, show = show),
      arg_list
    )
  )
  return(plt)
}

#' Plot ROC/PR curves for feature selection.
#' 
#' @description Plot ROC/PR curves for feature selection or some summary thereof
#'   across experimental replicates.
#' 
#' @inheritParams plot_pred_curve
#' @inheritDotParams plot_eval_constructor -eval_results -eval_names -plot_data
#'   -vary_params -show
#' 
#' @inherit plot_eval_constructor return
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
#'     nested_cols = "feature_info",
#'     truth_col = "true_support", 
#'     imp_col = "est_importance"
#'   ),
#'   PR = summarize_feature_selection_curve(
#'     fit_results, 
#'     curve = "PR",
#'     nested_cols = "feature_info",
#'     truth_col = "true_support", 
#'     imp_col = "est_importance"
#'   )
#' )
#' 
#' # create summary ROC/PR plots using pre-computed evaluation results
#' roc_plt <- plot_feature_selection_curve(eval_results = eval_results,
#'                                         eval_name = "ROC", curve = "ROC",
#'                                         show = c("line", "ribbon"))
#' pr_plt <- plot_feature_selection_curve(eval_results = eval_results,
#'                                        eval_name = "PR", curve = "PR",
#'                                        show = c("line", "ribbon"))
#' # or alternatively, create the same plots directly from fit results
#' roc_plt <- plot_feature_selection_curve(fit_results = fit_results,
#'                                         show = c("line", "ribbon"),
#'                                         curve = "ROC",
#'                                         eval_fun_options = list(
#'                                           nested_cols = "feature_info",
#'                                           truth_col = "true_support",
#'                                           imp_col = "est_importance"
#'                                         ))
#' pr_plt <- plot_feature_selection_curve(fit_results = fit_results,
#'                                        show = c("line", "ribbon"),
#'                                        curve = "PR",
#'                                        eval_fun_options = list(
#'                                          nested_cols = "feature_info",
#'                                          truth_col = "true_support",
#'                                          imp_col = "est_importance"
#'                                        ))
#'
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' roc_plt <- plot_feature_selection_curve(eval_results = eval_results,
#'                                         eval_name = "ROC", curve = "ROC",
#'                                         show = c("line", "ribbon"),
#'                                         plot_by = ".dgp_name")
#' 
#' @export
plot_feature_selection_curve <- function(fit_results = NULL,
                                         eval_results = NULL, eval_name = NULL,
                                         eval_fun = "summarize_feature_selection_curve",
                                         eval_fun_options = NULL,
                                         vary_params = NULL, 
                                         curve = c("ROC", "PR"),
                                         show = c("line", "ribbon"), ...) {
  arg_list <- rlang::list2(...)
  plt <- do.call(
    plot_pred_curve,
    args = c(
      list(fit_results = fit_results,
           eval_results = eval_results, eval_name = eval_name,
           eval_fun = eval_fun, eval_fun_options = eval_fun_options,
           vary_params = vary_params, curve = curve, show = show),
      arg_list
    )
  )
  return(plt)
}

