#' Evaluate and/or summarize feature selection errors.
#' 
#' @name eval_feature_selection_err_funs
#' @description Evaluate various feature selection metrics, given the true 
#'   feature support and the estimated feature support.
#'   \code{eval_feature_selection_err()} evaluates the various feature selection
#'   metrics for each experimental replicate separately.. 
#'   \code{summarize_feature_selection_err()} summarizes the various feature
#'   selection metrics across experimental replicates.
#'   
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @param truth_col A character string identifying the column in 
#'   \code{fit_results} with the true feature support data. Each element in this
#'   column should be an array of length \code{p}, where \code{p} is the number 
#'   of features. Elements in this array should be binary with \code{TRUE} or
#'   \code{1} meaning the feature (corresponding to that slot) is in the support 
#'   and \code{FALSE} or \code{0} meaning the feature is not in the support.
#' @param estimate_col An (optional) character string identifying the column in
#'   \code{fit_results} with the estimated feature support data. Each element in
#'   this column should be an array of length \code{p}, where \code{p} is the 
#'   number of features and the feature order aligns with that of 
#'   \code{truth_col}. Elements in this array should be binary with \code{TRUE} 
#'   or \code{1} meaning the feature (corresponding to that slot) is in the 
#'   estimated support and \code{FALSE} or \code{0} meaning the feature is not 
#'   in the estimated support. If \code{NULL} (default), the non-zero elements 
#'   of \code{imp_col} are used as the estimated feature support.
#' @param imp_col A character string identifying the column in 
#'   \code{fit_results} with the estimated feature importance data. Each element 
#'   in this column should be an array of length \code{p}, where \code{p} is the 
#'   number of features and the feature order aligns with that of 
#'   \code{truth_col}. Elements in this array should be numeric where a higher
#'   magnitude indicates a more important feature.
#' @param metrics A \code{metric_set} object indicating the metrics to evaluate.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   evaluate the following: number of true positives (\code{tp}), number of
#'   false positives (\code{fp}), sensitivity (\code{sens}), specificity
#'   (\code{spec}), positive predictive value (\code{ppv}), number of features
#'   in the estimated support (\code{pos}), number of features not in the 
#'   estimated support (\code{neg}), AUROC (\code{roc_auc}), and AUPRC 
#'   (\code{pr_auc}).
#' 
#' @returns 
#' The output of \code{eval_feature_selection_err()} is a \code{tibble} with the
#' following columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{.metric}{Name of the evaluation metric.}
#' \item{.estimate}{Value of the evaluation metric.}
#' }
#' as well as any columns specified by \code{vary_params}.
#' 
#' The output of \code{summarize_feature_selection_err()} is a grouped
#' \code{tibble} containing both identifying information and the feature 
#' selection results aggregated over experimental replicates. Specifically, the 
#' identifier columns include \code{.dgp_name}, \code{.method_name}, any columns
#' specified by \code{vary_params}, and \code{.metric}. In addition, there are 
#' results columns corresponding to the requested statistics in 
#' \code{summary_funs} and \code{custom_summary_funs}. These columns end in the 
#' suffix "_feature_selection".
#' 
#' @family feature_selection_funs
#' 
NULL

#' @rdname eval_feature_selection_err_funs
#' 
#' @importFrom rlang .data
#' @export
eval_feature_selection_err <- function(fit_results, vary_params = NULL,
                                       nested_data = NULL, truth_col, 
                                       estimate_col = NULL, imp_col, 
                                       metrics = NULL, na_rm = FALSE) {
  .estimator <- NULL  # to fix no visible binding for global variable error
  .eval_res <- NULL
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  eval_feature_selection_err_rowwise <- function(data) {
    if (!is.null(nested_data)) {
      data <- data %>% tidyr::unnest(tidyselect::all_of(nested_data))
    }
    cols <- colnames(data)
    truth_col <- tidyselect::vars_pull(cols, tidyselect::all_of(truth_col))
    imp_col <- tidyselect::vars_pull(cols, tidyselect::all_of(imp_col))
    estimate_col <- intersect(cols, estimate_col)
    if (is.null(estimate_col)) {
      estimate_col <- imp_col
    }
    data <- data %>%
      tidyr::unnest(tidyselect::all_of(c(truth_col, imp_col, estimate_col)))
    
    data <- data %>%
      dplyr::mutate(.imp_est = abs(.data[[imp_col]])) %>%
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(c(truth_col, estimate_col)),
                      ~factor(as.integer(as.numeric(.x) != 0), levels = 0:1))
      )
    
    if (is.null(metrics)) {
      metrics <- yardstick::metric_set(tp, fp, 
                                       yardstick::sens, yardstick::spec,
                                       yardstick::ppv, 
                                       pos, neg,
                                       yardstick::roc_auc, yardstick::pr_auc)
    }
    
    res <- metrics(data = data, truth = !!truth_col, estimate = !!estimate_col,
                   tidyselect::all_of(".imp_est"), na_rm = na_rm, 
                   event_level = "second") %>%
      dplyr::select(-.estimator)
    return(res)
  }
  
  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  eval_tib <- fit_results %>%
    dplyr::mutate(
      .eval_res = purrr::map(
        1:nrow(fit_results),
        ~eval_feature_selection_err_rowwise(data = fit_results[.x, ])
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), .eval_res) %>%
    tidyr::unnest(.eval_res)
  return(eval_tib)
}

#' @rdname eval_feature_selection_err_funs
#' 
#' @export
summarize_feature_selection_err <- function(fit_results, vary_params = NULL,
                                            nested_data = NULL, truth_col,
                                            estimate_col = NULL, imp_col,
                                            metrics = NULL, na_rm = FALSE,
                                            summary_funs = c("mean", "median",
                                                             "min", "max", 
                                                             "sd", "raw"),
                                            custom_summary_funs = NULL) {
  group_vars <- c(".dgp_name", ".method_name", vary_params, ".metric")
  eval_tib <- eval_feature_selection_err(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, 
    estimate_col = estimate_col, imp_col = imp_col, 
    metrics = metrics, na_rm = na_rm
  ) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
    eval_data = eval_tib, eval_id = "feature_selection", value_col = ".estimate",
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Evaluate and/or summarize ROC or PR curves for feature selection.
#' 
#' @name eval_feature_selection_curve_funs
#' @description Evaluate the ROC or PR curves corresponding to the selected
#'   features, given the true feature support and the estimated feature 
#'   importances. \code{eval_feature_selection_curve()} evaluates the ROC or PR
#'   curve for each experimental replicate separately. 
#'   \code{summarize_feature_selection_curve()} summarizes the ROC or PR curve
#'   across experimental replicates.
#' 
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_feature_selection_err
#' 
#' @returns
#' The output of \code{eval_feature_selection_curve()} is a \code{tibble} with 
#' the following columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{curve_estimate}{A list of tibbles with x and y coordinate values for 
#'   the ROC/PR curve for the given experimental replicate. If 
#'   \code{curve = "ROC"}, the \code{tibble} has the columns \code{.threshold}, 
#'   \code{FPR}, and \code{TPR} for the threshold, false positive rate, and true
#'   positive rate, respectively. If \code{curve = "PR"}, the \code{tibble} has 
#'   the columns \code{.threshold}, \code{recall}, and \code{precision}.}
#' }
#' as well as any columns specified by \code{vary_params}.
#' 
#' The output of \code{summarize_feature_selection_curve()} is a grouped
#' \code{tibble} containing both identifying information and the 
#' feature selection curve results aggregated over experimental replicates. 
#' Specifically, the identifier columns include \code{.dgp_name},
#' \code{.method_name}, and any columns specified by \code{vary_params}. In
#' addition, there are results columns corresponding to the requested statistics 
#' in \code{summary_funs} and \code{custom_summary_funs}. If 
#' \code{curve = "ROC"}, these results columns include \code{FPR} and others 
#' that end in the suffix "_TPR". If \code{curve = "PR"}, the results columns 
#' include \code{recall} and others that end in the suffix "_precision".
#' 
#' @family feature_selection_funs
#' 
NULL

#' @rdname eval_feature_selection_curve_funs
#' 
#' @importFrom rlang .data
#' @export
eval_feature_selection_curve <- function(fit_results, vary_params = NULL,
                                         nested_data = NULL, truth_col, imp_col,
                                         curve = c("ROC", "PR"), 
                                         options = list(), na_rm = FALSE) {
  if (is.null(nested_data) | (truth_col %in% names(fit_results))) {
    fit_results <- fit_results %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        {{truth_col}} := factor(
          as.integer(as.numeric(.data[[truth_col]]) != 0), levels = 1:0
        )
      )
  } else {
    fit_results[[nested_data]] <- purrr::map(
      fit_results[[nested_data]],
      ~.x %>%
        dplyr::mutate(
          {{truth_col}} := factor(
            as.integer(as.numeric(.data[[truth_col]]) != 0), levels = 1:0
          )
        )
    )
  }
  
  eval_tib <- eval_pred_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, prob_cols = imp_col,
    curve = curve, options = options, na_rm = na_rm
  )
  return(eval_tib)
}


#' @rdname eval_feature_selection_curve_funs
#' 
#' @export
summarize_feature_selection_curve <- function(fit_results, vary_params = NULL,
                                              nested_data = NULL, truth_col,
                                              imp_col, curve = c("ROC", "PR"), 
                                              options = list(), na_rm = FALSE,
                                              x_grid = seq(0, 1, by = 1e-2),
                                              summary_funs = c("mean", "median",
                                                               "min", "max", 
                                                               "sd", "raw"),
                                              custom_summary_funs = NULL) {
  curve_estimate <- NULL  # to fix no visible binding for global variable error
  if (curve == "PR") {
    xvar <- "recall"
    yvar <- "precision"
  } else if (curve == "ROC") {
    xvar <- "FPR"
    yvar <- "TPR"
  }
  group_vars <- c(".dgp_name", ".method_name", vary_params, xvar)
  
  rescale_curve <- function(curve_data) {
    # map curves onto same x-axis scale
    curve_data = data.frame(
      .x_coord = x_grid,
      .y_coord = purrr::map_dbl(
        x_grid, ~max(curve_data[curve_data[[xvar]] <= .x, yvar])
      )
    ) %>%
      stats::setNames(c(xvar, yvar))
    return(curve_data)
  }
  
  eval_tib <- eval_feature_selection_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, imp_col = imp_col, 
    curve = curve, options = options, na_rm = na_rm
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(curve_estimate = list(rescale_curve(curve_estimate))) %>%
    tidyr::unnest(curve_estimate) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
    eval_data = eval_tib, eval_id = yvar, value_col = yvar,
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Evaluate and/or summarize feature importance scores.
#' 
#' @name eval_feature_importance_funs
#' @description Evaluate the estimated feature importance scores against the 
#'   true feature support. \code{eval_feature_importance} evaluates the 
#'   feature importances for each experimental replicate separately.
#'   \code{summarize_feature_importance} summarizes the feature importances
#'   across experimental replicates.
#'   
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_feature_selection_err
#' 
#' @returns
#' The output of \code{eval_feature_importance()} is a \code{tibble} with 
#' the columns \code{.rep}, \code{.dgp_name}, and \code{.method_name} in addition 
#' to the columns specified by \code{vary_params}, \code{feature_col}, and
#' \code{imp_col}.
#' 
#' The output of \code{summarize_feature_importance()} is a grouped 
#' \code{tibble} containing both identifying information and the feature 
#' importance results aggregated over experimental replicates. Specifically, the
#' identifier columns include \code{.dgp_name}, \code{.method_name}, any columns
#' specified by \code{vary_params}, and the column specified by 
#' \code{feature_col}. In addition, there are results columns corresponding to 
#' the requested statistics in \code{summary_funs} and 
#' \code{custom_summary_funs}. These columns end in the suffix
#' "_feature_importance".
#' 
#' @family feature_selection_funs
#' 
NULL

#' @rdname eval_feature_importance_funs
#' 
#' @export
eval_feature_importance <- function(fit_results, vary_params = NULL,
                                    nested_data = NULL, feature_col, imp_col) {
  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  if (!is.null(nested_data)) {
    fit_results <- fit_results %>% 
      tidyr::unnest(tidyselect::all_of(nested_data))
  }
  eval_tib <- fit_results %>%
    dplyr::select(tidyselect::all_of(c(id_vars, feature_col, imp_col))) %>%
    tidyr::unnest(tidyselect::all_of(c(feature_col, imp_col)))
  return(eval_tib)
}

#' @rdname eval_feature_importance_funs
#' 
#' @export
summarize_feature_importance <- function(fit_results, vary_params = NULL,
                                         nested_data = NULL, feature_col, 
                                         imp_col, na_rm = FALSE,
                                         summary_funs = c("mean", "median",
                                                          "min", "max", 
                                                          "sd", "raw"),
                                         custom_summary_funs = NULL) {
  group_vars <- c(".dgp_name", ".method_name", vary_params, feature_col)
  eval_tib <- eval_feature_importance(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, feature_col = feature_col, imp_col = imp_col
  ) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
    eval_data = eval_tib, eval_id = "feature_importance", value_col = imp_col,
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}
