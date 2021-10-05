#' Evaluate feature support recovery.
#' 
#' @description Evaluate various support recovery metrics, given the true
#'   feature support and the estimated feature support.
#' 
#' @inheritParams shared_eval_lib_args
#' @param data A \code{data.frame} containing the \code{truth} and 
#'   \code{estimate} columns. Each row in this \code{data.frame} typically
#'   corresponds to a feature or variable.
#' @param truth A character string identifying the column with the true 
#'   support. The column should be binary, where \code{TRUE} or \code{1} means 
#'   the feature (corresponding to that row) is in the support and \code{FALSE} 
#'   or \code{0} means the feature is not in the support.
#' @param estimate A character string identifying the column with the estimated 
#'   support, where a non-zero entry means that the feature (corresponding to
#'   that row) is part of the estimated support and a zero entry means that 
#'   feature is not part of the estimated support. Moreover, a higher magnitude
#'   indicates a more important feature.
#' @param metrics A \code{metric_set} object indicating the metrics to evaluate.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   evaluate the following: number of true positives (\code{tp}), number of
#'   false positives (\code{fp}), sensitivity (\code{sens}), specificity
#'   (\code{spec}), positive predictive value (\code{ppv}), number of features
#'   in the estimated support (\code{pos}), number of features not in the 
#'   estimated support (\code{neg}), AUROC (\code{roc_auc}), and AUPRC 
#'   (\code{pr_auc}).
#' 
#' @returns A two column \code{tibble} with the following columns:
#' \describe{
#' \item{.metric}{Name of feature support recovery metric.}
#' \item{.estimate}{Feature support recovery value for the given metric.}
#' }
#' 
#' @family feature_selection_funs
#' 
#' @importFrom rlang .data
#' @export
eval_feature_recovery <- function(data, truth, estimate, metrics = NULL,
                                  na_rm = FALSE) {
  .estimator <- NULL  # to fix no visible binding for global variable error
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  names <- names(data)
  truth <- tidyselect::vars_pull(names, tidyselect::all_of(truth))
  estimate <- tidyselect::vars_pull(names, tidyselect::all_of(estimate))
  
  data <- data %>%
    dplyr::mutate(prob = abs(.data[[estimate]])) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c(truth, estimate)),
                                ~factor(as.integer(as.numeric(.x) != 0),
                                        levels = 0:1)))
  
  if (is.null(metrics)) {
    metrics <- yardstick::metric_set(tp, fp, 
                                     yardstick::sens, yardstick::spec,
                                     yardstick::ppv, 
                                     pos, neg,
                                     yardstick::roc_auc, yardstick::pr_auc)
  }
  
  res <- metrics(data = data, truth = !!truth, estimate = !!estimate,
                 tidyselect::all_of("prob"),
                 na_rm = na_rm, event_level = "second") %>%
    dplyr::select(-.estimator)
  return(res)
}

#' Evaluate ROC or PR curves for feature support recovery.
#' 
#' @description Evaluate the ROC or PR curves for feature support recovery and
#'   return a tibble with the results.
#' 
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_feature_recovery
#' @inheritParams yardstick::roc_curve
#' 
#' @inherit eval_pred_curve return
#' 
#' @family feature_selection_funs
#' 
#' @export
eval_feature_recovery_curve <- function(data, truth, estimate, 
                                        metric = c("ROC", "PR"), 
                                        options = list(), na_rm = FALSE) {
  data <- data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(truth),
                                ~factor(as.integer(as.numeric(.x) != 0), 
                                        levels = 1:0)))
  curve_df <- eval_pred_curve(data = data, truth = truth, probs = estimate,
                              metric = metric, options = options, na_rm = na_rm)
  return(curve_df)
}

#' Show feature importance scores.
#' 
#' @description Show feature importance scores across all repetitions, methods,
#'   and DGPs in an \code{Experiment}.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @param data_col Character string or vector specifying the name of column(s) 
#'   in \code{fit_results} with the feature importance score information.
#' 
#' @return A \code{tibble} with the columns \code{rep}, \code{dgp_name}, and
#'   \code{method_name} in addition to the columns specified by 
#'   \code{vary_params} and \code{data_col}, where the \code{data_col} column(s)
#'   has been unnested.
#' 
#' @family feature_selection_funs
#' 
#' @export
eval_feature_importances <- function(fit_results, vary_params = NULL, 
                                     data_col) {
  id_cols <- c("rep", "dgp_name", "method_name", vary_params)
  fi_out <- fit_results %>%
    dplyr::select(tidyselect::all_of(c(id_cols, data_col))) %>%
    tidyr::unnest(tidyselect::all_of(data_col))
  return(fi_out)
}

#' Summarize feature recovery evaluation results.
#' 
#' @description Summarize feature recovery evaluation results for a variety of
#'   evaluation metrics across experimental repetitions.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_feature_recovery
#' @param data_col Character string specifying the name of the list-type column 
#'   in \code{fit_results} with the feature support \code{data.frames} or 
#'   \code{tibbles} that contain the \code{truth} and \code{estimate} columns.
#'   Each row in these \code{data.frames} or \code{tibbles} typically
#'   corresponds to a feature or variable.
#' 
#' @return A grouped \code{tibble} containing both identifying information
#'   and the feature recovery results aggregated over experimental replicates.
#'   Specifically, the identifier columns include \code{dgp_name},
#'   \code{method_name}, any columns specified by \code{vary_params}, and 
#'   \code{.metric}. In addition, there are results columns corresponding to the
#'   requested statistics in \code{summary_funs} and \code{custom_summary_funs}.
#'   These columns end in the suffix "_feature_recovery".
#' 
#' @family feature_selection_funs
#' 
#' @importFrom rlang .data
#' @export
summarize_feature_recovery <- function(fit_results, vary_params = NULL, 
                                       data_col, truth, estimate, 
                                       metrics = NULL, na_rm = FALSE,
                                       summary_funs = c("mean", "median", "min",
                                                        "max", "sd", "raw"),
                                       custom_summary_funs = NULL) {
  eval_out <- NULL  # to fix no visible binding for global variable error
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  
  id_vars <- c("rep", "dgp_name", "method_name", vary_params)
  group_vars <- c("dgp_name", "method_name", vary_params, ".metric")
  
  eval_results <- fit_results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      eval_out = list(
        eval_feature_recovery(
          data = .data[[data_col]], 
          truth = !!truth, estimate = !!estimate, metrics = !!metrics,
          na_rm = na_rm
        )
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), eval_out) %>%
    tidyr::unnest(eval_out) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- eval_summary_constructor(
    eval_results, id = "feature_recovery", value = ".estimate",
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Summarize the ROC/PR curves for feature support recovery.
#' 
#' @description Summarize the ROC or PR curves for feature support recovery
#'   across experimental repetitions.
#' 
#' @inheritParams summarize_feature_recovery
#' @inheritParams eval_feature_recovery_curve
#' @inheritParams summarize_pred_curve
#' 
#' @inherit summarize_pred_curve return
#' 
#' @family feature_selection_funs
#' 
#' @importFrom rlang .data
#' @export
summarize_feature_recovery_curve <- function(fit_results, vary_params = NULL,
                                             data_col, truth, estimate, 
                                             metric = c("ROC", "PR"),
                                             options = list(), na_rm = FALSE,
                                             x_grid = seq(0, 1, by = 1e-2),
                                             summary_funs = c("mean", "median",
                                                              "min", "max",
                                                              "sd", "raw"),
                                             custom_summary_funs = NULL) {
  curve_df <- NULL  # to fix no visible binding for global variable error
  metric <- match.arg(metric)
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  if (metric == "PR") {
    xvar <- "recall"
    yvar <- "precision"
  } else if (metric == "ROC") {
    xvar <- "FPR"
    yvar <- "TPR"
  }
  id_vars <- c("rep", "dgp_name", "method_name", vary_params)
  group_vars <- c("dgp_name", "method_name", vary_params, xvar)
  
  eval_and_rescale_curve <- function(data) {
    # evaluate ROC/PR curve and transform data frame to have same x scale
    curve_df <- eval_feature_recovery_curve(
      data = data, truth = truth, estimate = estimate,
      metric = metric, options = options, na_rm = na_rm
    )
    curve_df = data.frame(
      .x_auc = x_grid,
      .y_auc = purrr::map_dbl(x_grid,
                              ~max(curve_df[curve_df[[xvar]] <= .x, yvar]))
    ) %>%
      stats::setNames(c(xvar, yvar))
    return(curve_df)
  }
  
  eval_results <- fit_results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      curve_df = list(eval_and_rescale_curve(.data[[data_col]]))
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), curve_df) %>%
    tidyr::unnest(curve_df) %>%
    dplyr::group_by(dplyr::across({{group_vars}}))
  
  eval_summary <- eval_summary_constructor(
    eval_results, id = yvar, value = yvar,
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Summarize feature importance results.
#' 
#' @description Summarize feature importance results across experimental
#'   repetitions.
#'
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @param data_col Character string specifying the name of the list-type column 
#'   in \code{fit_results} with the feature importance \code{data.frames} or 
#'   \code{tibbles} that contain the \code{feature} and \code{estimate} columns.
#' @param feature A character string identifying the column with the feature
#'   names.
#' @param estimate A character string identifying the column with the estimated 
#'   feature importance scores.
#' 
#' @return A grouped \code{tibble} containing both identifying information
#'   and the feature importance results aggregated over experimental replicates.
#'   Specifically, the identifier columns include \code{dgp_name},
#'   \code{method_name}, any columns specified by \code{vary_params}, and 
#'   the column specified by \code{feature}. In addition, there are results
#'   columns corresponding to the requested statistics in \code{summary_funs} 
#'   and \code{custom_summary_funs}. These columns end in the suffix
#'   "_feature_imp".
#'
#' @family feature_selection_funs
#' 
#' @export
summarize_feature_importances <- function(fit_results, vary_params = NULL, 
                                          data_col, feature, estimate, 
                                          na_rm = FALSE,
                                          summary_funs = c("mean", "median",
                                                           "min", "max", "sd",
                                                           "raw"),
                                          custom_summary_funs = NULL) {
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  group_vars <- c("dgp_name", "method_name", vary_params, feature)
  
  eval_results <- eval_feature_importances(fit_results = fit_results,
                                           vary_params = vary_params,
                                           data_col = data_col) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- eval_summary_constructor(
    eval_results, id = "feature_imp", value = estimate,
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}
