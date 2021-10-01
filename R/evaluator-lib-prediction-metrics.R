#' Evaluate prediction error.
#' 
#' @description Evaluate various prediction error metrics, given the true 
#'   responses and the predicted (or estimated) responses.
#'   
#' @inheritParams shared_eval_lib_args
#' @param data A \code{data.frame} containing the \code{truth} and 
#'   \code{estimate} columns. Each row in this \code{data.frame} typically
#'   corresponds to a sample or observation in the data.
#' @param truth A character string identifying the column with the true 
#'   responses. The column should be numeric for a regression problem and a 
#'   factor for a classification problem.
#' @param estimate A character string identifying the column with the estimated 
#'   or predicted responses. The column should be numeric for a regression
#'   problem and a factor (with the predicted classes) for a classification
#'   problem.
#' @param probs A character string or vector identifying the column(s) with the
#'   columns containing class probabilities. If \code{truth} is binary, only
#'   1 column name should be provided. Otherwise, the length of the character
#'   vector should be equal to the number of factor levels of \code{truth}.
#'   This argument is not used when evaluating numeric metrics.
#' @param metrics A \code{metric_set} object indicating the metrics to evaluate.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   use the default metrics in [yardstick::metrics()].
#' @param groups (Optional) vector of group IDs to group observations by before
#'   evaluating prediction errors. This is useful for assessing within-group
#'   prediction errors. Note: the (unstratified) prediction errors, aggregated
#'   across the full data set, are computed in addition to these stratified
#'   within-group errors.
#' @inheritParams yardstick::roc_auc
#' 
#' @return A \code{tibble} with the following columns:
#' \describe{
#' \item{.group}{If the \code{groups} argument is not \code{NULL}, the name of
#'   the group over which the prediction error is being computed. Here, ".all" 
#'   is used to indicate the full unstratified data set (with no groups). If
#'   \code{groups} is \code{NULL}, this column is not returned.}
#' \item{.metric}{Name of prediction error metric}
#' \item{.estimate}{Prediction error value for the given metric.}
#' }
#' 
#' @family prediction_error_funs
#' 
#' @export
eval_pred_err <- function(data, truth, estimate, probs = NULL, 
                          metrics = NULL, groups = NULL, 
                          options = list(), na_rm = FALSE) {
  .estimator <- NULL  # to fix no visible binding for global variable error
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  names <- names(data)
  truth <- tidyselect::vars_pull(names, tidyselect::all_of(truth))
  estimate <- tidyselect::vars_pull(names, tidyselect::all_of(estimate))
  probs <- intersect(names, probs)
  
  if (!is.null(groups)) {
    data <- dplyr::bind_rows(data, data) %>%
      dplyr::group_by(.group = tidyselect::all_of(c(rep(".all", nrow(data)),
                                                    groups)))
  }
  
  if (is.null(metrics)) {
    res <- yardstick::metrics(data = data, 
                              truth = !!truth, estimate = !!estimate, 
                              tidyselect::all_of(probs), 
                              options = options, na_rm = na_rm)
  } else {
    is_class <- is.factor(data[[truth]]) || 
      inherits(data[[truth]], "class_pred")
    if (is_class) {
      res <- yardstick::metrics(data = data, 
                                truth = !!truth, estimate = !!estimate,
                                tidyselect::all_of(probs),
                                na_rm = na_rm)
    } else {
      res <- yardstick::metrics(data, truth = !!truth, estimate = !!estimate,
                                na_rm = na_rm)
    }
  }
  return(res %>% dplyr::select(-.estimator))
}

#' Evaluate ROC or PR curves.
#' 
#' @description Evaluate the ROC or PR curves and return a tibble with the
#'   results.
#'   
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_pred_err
#'   
#' @returns If \code{metric = "ROC"}, returns a \code{tibble} with the columns
#'   \code{.threshold}, \code{FPR}, and \code{TPR} for the threshold, false
#'   positive rate, and true positive rate, respectively. If 
#'   \code{metric = "PR"}, returns a \code{tibble} with columns 
#'   \code{.threshold}, \code{recall}, and \code{precision}.
#' 
#' @family prediction_error_funs
#' 
#' @export
eval_pred_curve <- function(data, truth, probs, metric = c("ROC", "PR"), 
                            groups = NULL, options = list(), na_rm = FALSE) {
  specificity <- NULL  # to fix no visible binding for global variable error
  sensitivity <- NULL
  FPR <- NULL
  metric <- match.arg(metric)
  if (!is.null(groups)) {
    data <- dplyr::bind_rows(data, data) %>%
      dplyr::group_by(.group = tidyselect::all_of(c(rep(".all", nrow(data)),
                                                    groups)))
  }
  
  if (identical(metric, "ROC")) {
    curve_df <- yardstick::roc_curve(data = data, truth = !!truth, 
                                     tidyselect::all_of(probs),
                                     options = options, na_rm = na_rm) %>%
      dplyr::rename(FPR = specificity, TPR = sensitivity) %>%
      dplyr::mutate(FPR = 1 - FPR)
  } else if (identical(metric, "PR")) {
    curve_df <- yardstick::pr_curve(data = data, truth = !!truth, 
                                    tidyselect::all_of(probs), na_rm = na_rm)
  }
  return(curve_df)
}

#' Summarize prediction error evaluation results.
#' 
#' @description Summarize prediction error evaluation results for a variety of
#'   evaluation metrics across experimental repetitions.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_pred_err
#' 
#' @return A grouped \code{tibble} containing both identifying information
#'   and the prediction error results aggregated over experimental replicates.
#'   Specifically, the identifier columns include \code{dgp_name},
#'   \code{method_name}, any columns specified by \code{vary_params}, and 
#'   \code{.metric}. In addition, there are results columns corresponding to the
#'   requested statistics in \code{summary_funs} and \code{custom_summary_funs}.
#'   These columns end in the suffix "_pred_err".
#' 
#' @family prediction_error_funs
#' 
#' @export
summarize_pred_err <- function(fit_results, vary_params = NULL,
                               truth, estimate, probs = NULL, 
                               metrics = NULL, groups = NULL, 
                               options = list(), na_rm = FALSE,
                               summary_funs = c("mean", "median", "min", "max",
                                                "sd", "raw"),
                               custom_summary_funs = NULL) {
  eval_out <- NULL  # to fix no visible binding for global variable error
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  id_vars <- c("rep", "dgp_name", "method_name", vary_params)
  if (!is.null(groups)) {
    group_vars <- c("dgp_name", "method_name", vary_params, ".group", ".metric")
  } else {
    group_vars <- c("dgp_name", "method_name", vary_params, ".metric")
  }
  eval_results <- fit_results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      eval_out = list(
        eval_pred_err(
          data = dplyr::cur_data(), truth = !!truth, 
          estimate = !!estimate, probs = !!probs,
          metrics = !!metrics, groups = !!groups, 
          options = options, na_rm = na_rm
        )
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), eval_out) %>%
    tidyr::unnest(eval_out) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- eval_summary_constructor(
    eval_results, id = "pred_err", value = ".estimate",
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Summarize ROC/PR curves.
#' 
#' @description Summarize ROC/PR curves across experimental repetitions.
#' 
#' @inheritParams summarize_pred_err
#' @inheritParams eval_pred_curve
#' @param x_grid Vector of values between 0 and 1 at which to evaluate the ROC 
#'   or PR curve. If \code{metric = "ROC"}, the provided vector of values are
#'   the FPR values at which to evaluate the TPR, and if \code{metric = "PR"},
#'   the values are the recall values at which to evaluate the precision.
#' 
#' @return A grouped \code{tibble} containing both identifying information
#'   and the feature recovery curve results aggregated over experimental
#'   replicates. Specifically, the identifier columns include \code{dgp_name},
#'   \code{method_name}, and any columns specified by \code{vary_params}. In
#'   addition, there are results columns corresponding to the
#'   requested statistics in \code{summary_funs} and \code{custom_summary_funs}.
#'   If \code{metric = "ROC"}, these results columns include \code{FPR} and
#'   others that end in the suffix "_TPR". If \code{metric = "PR"}, the results
#'   columns include \code{recall} and others that end in the suffix 
#'   "_precision".
#' 
#' @family prediction_error_funs
#' 
#' @export
summarize_pred_curve <- function(fit_results, vary_params = NULL,
                                 truth, probs, metric = c("ROC", "PR"), 
                                 groups = NULL, options = list(), na_rm = FALSE,
                                 x_grid = seq(0, 1, by = 1e-2),
                                 summary_funs = c("mean", "median", "min", "max",
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
    curve_df <- eval_pred_curve(
      data = data, truth = truth, probs = probs,
      metric = metric, groups = groups, options = options, na_rm = na_rm
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
      curve_df = list(eval_and_rescale_curve(dplyr::cur_data()))
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
