#' Evaluate and/or summarize feature selection errors.
#'
#' @name eval_feature_selection_err_funs
#' @description Evaluate various feature selection metrics, given the true
#'   feature support and the estimated feature support.
#'   \code{eval_feature_selection_err()} evaluates the various feature selection
#'   metrics for each experimental replicate separately.
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
#'   (\code{pr_auc}). If \code{na_rm = TRUE}, the number of NA values
#'   (\code{num_na}) is also computed.
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
#' as well as any columns specified by \code{group_cols} and \code{vary_params}.
#'
#' The output of \code{summarize_feature_selection_err()} is a grouped
#' \code{tibble} containing both identifying information and the feature
#' selection results aggregated over experimental replicates. Specifically, the
#' identifier columns include \code{.dgp_name}, \code{.method_name}, any columns
#' specified by \code{group_cols} and \code{vary_params}, and \code{.metric}.
#' In addition, there are results columns corresponding to the requested
#' statistics in \code{summary_funs} and \code{custom_summary_funs}. These
#' columns end in the suffix specified by \code{eval_id}.
#'
#' @family feature_selection_funs
#'
#' @examples
#' # generate example fit_results data for a feature selection problem
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
#' # evaluate feature selection (using all default metrics) for each replicate
#' eval_results <- eval_feature_selection_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   estimate_col = "est_support",
#'   imp_col = "est_importance"
#' )
#' # summarize feature selection error (using all default metric) across replicates
#' eval_results_summary <- summarize_feature_selection_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   estimate_col = "est_support",
#'   imp_col = "est_importance"
#' )
#'
#' # evaluate/summarize feature selection errors using specific yardstick metrics
#' metrics <- yardstick::metric_set(yardstick::sens, yardstick::spec)
#' eval_results <- eval_feature_selection_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   estimate_col = "est_support",
#'   imp_col = "est_importance",
#'   metrics = metrics
#' )
#' eval_results_summary <- summarize_feature_selection_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   estimate_col = "est_support",
#'   imp_col = "est_importance",
#'   metrics = metrics
#' )
#'
#' # summarize feature selection errors using specific summary metric
#' range_fun <- function(x) return(max(x) - min(x))
#' eval_results_summary <- summarize_feature_selection_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   estimate_col = "est_support",
#'   imp_col = "est_importance",
#'   custom_summary_funs = list(range_feature_selection = range_fun)
#' )
#'
NULL

#' @rdname eval_feature_selection_err_funs
#'
#' @importFrom rlang .data
#' @export
eval_feature_selection_err <- function(fit_results, vary_params = NULL,
                                       nested_cols = NULL, truth_col,
                                       estimate_col = NULL, imp_col,
                                       group_cols = NULL, metrics = NULL,
                                       na_rm = FALSE) {

  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    abort("Unknown metrics. metrics must be of class 'yardstick::metric_set' or NULL.")
  }

  eval_feature_selection_err_fun <- function(data, truth_col, estimate_col,
                                             imp_col, metrics, na_rm) {

    data <- data %>%
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

    out <- metrics(data = data, truth = !!truth_col, estimate = !!estimate_col,
                   !!imp_col, na_rm = na_rm, event_level = "second") %>%
      add_na_counts(data = data, value_col = imp_col, na_rm = na_rm) %>%
      dplyr::select(-.estimator)
    return(out)
  }

  eval_tib <- eval_constructor(
    fit_results = fit_results, vary_params = vary_params,
    fun = eval_feature_selection_err_fun, nested_cols = nested_cols,
    truth_col = truth_col, estimate_col = estimate_col, imp_col = imp_col,
    group_cols = group_cols, fun_options = list(metrics = metrics), na_rm = na_rm
  ) %>%
    tidyr::unnest(.eval_result)
  return(eval_tib)
}

#' @rdname eval_feature_selection_err_funs
#'
#' @export
summarize_feature_selection_err <- function(fit_results, vary_params = NULL,
                                            nested_cols = NULL, truth_col,
                                            estimate_col = NULL, imp_col,
                                            group_cols = NULL, metrics = NULL,
                                            na_rm = FALSE,
                                            summary_funs = c("mean", "median",
                                                             "min", "max",
                                                             "sd", "raw"),
                                            custom_summary_funs = NULL,
                                            eval_id = "feature_selection") {
  group_vars <- c(".dgp_name", ".method_name", vary_params,
                  group_cols, ".metric")
  eval_tib <- eval_feature_selection_err(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, truth_col = truth_col,
    estimate_col = estimate_col, imp_col = imp_col, group_cols = group_cols,
    metrics = metrics, na_rm = na_rm
  ) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(group_vars)))

  eval_summary <- eval_summarizer(
    eval_data = eval_tib, eval_id = eval_id, value_col = ".estimate",
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
#' as well as any columns specified by \code{group_cols} and \code{vary_params}.
#'
#' The output of \code{summarize_feature_selection_curve()} is a grouped
#' \code{tibble} containing both identifying information and the
#' feature selection curve results aggregated over experimental replicates.
#' Specifically, the identifier columns include \code{.dgp_name},
#' \code{.method_name}, and any columns specified by \code{group_cols} and
#' \code{vary_params}. In addition, there are results columns corresponding to
#' the requested statistics in \code{summary_funs} and
#' \code{custom_summary_funs}. If \code{curve = "ROC"}, these results columns
#' include \code{FPR} and others that end in the suffix "_TPR". If
#' \code{curve = "PR"}, the results columns include \code{recall} and others
#' that end in the suffix "_precision".
#'
#' @family feature_selection_funs
#'
#' @examples
#' # generate example fit_results data for a feature selection problem
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
#'         # estimated feature importance scores
#'         est_importance = c(10, runif(2, min = -2, max = 2))
#'       )
#'     }
#'   )
#' )
#'
#' # evaluate feature selection ROC/PR curves for each replicate
#' roc_results <- eval_feature_selection_curve(
#'   fit_results,
#'   curve = "ROC",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   imp_col = "est_importance"
#' )
#' pr_results <- eval_feature_selection_curve(
#'   fit_results,
#'   curve = "PR",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   imp_col = "est_importance"
#' )
#' # summarize feature selection ROC/PR curves across replicates
#' roc_summary <- summarize_feature_selection_curve(
#'   fit_results,
#'   curve = "ROC",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   imp_col = "est_importance"
#' )
#' pr_summary <- summarize_feature_selection_curve(
#'   fit_results,
#'   curve = "PR",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   imp_col = "est_importance"
#' )
#'
NULL

#' @rdname eval_feature_selection_curve_funs
#'
#' @importFrom rlang .data
#' @export
eval_feature_selection_curve <- function(fit_results, vary_params = NULL,
                                         nested_cols = NULL, truth_col, imp_col,
                                         group_cols = NULL,
                                         curve = c("ROC", "PR"),
                                         na_rm = FALSE) {
  if (is.null(nested_cols) || (truth_col %in% names(fit_results))) {
    fit_results <- fit_results %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        {{truth_col}} := factor(
          as.integer(as.numeric(.data[[truth_col]]) != 0), levels = 1:0
        )
      )
  } else {
    fit_results[[nested_cols]] <- purrr::map(
      fit_results[[nested_cols]],
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
    nested_cols = nested_cols, truth_col = truth_col, prob_cols = imp_col,
    group_cols = group_cols, curve = curve, na_rm = na_rm
  )
  return(eval_tib)
}


#' @rdname eval_feature_selection_curve_funs
#'
#' @export
summarize_feature_selection_curve <- function(fit_results, vary_params = NULL,
                                              nested_cols = NULL, truth_col,
                                              imp_col, group_cols = NULL,
                                              curve = c("ROC", "PR"),
                                              na_rm = FALSE,
                                              x_grid = seq(0, 1, by = 1e-2),
                                              summary_funs = c("mean", "median",
                                                               "min", "max",
                                                               "sd", "raw"),
                                              custom_summary_funs = NULL,
                                              eval_id = ifelse(curve == "PR",
                                                               "precision",
                                                               "TPR")) {
  curve_estimate <- NULL  # to fix no visible binding for global variable error
  if (curve == "PR") {
    xvar <- "recall"
    yvar <- "precision"
  } else if (curve == "ROC") {
    xvar <- "FPR"
    yvar <- "TPR"
  }
  group_vars <- c(".dgp_name", ".method_name", vary_params, group_cols, xvar)

  eval_tib <- eval_feature_selection_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, truth_col = truth_col, imp_col = imp_col,
    group_cols = group_cols, curve = curve, na_rm = na_rm
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(curve_estimate = list(rescale_curve(curve_estimate,
                                                      x_grid = x_grid,
                                                      xvar = xvar,
                                                      yvar = yvar))) %>%
    tidyr::unnest(curve_estimate) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(group_vars)))

  eval_summary <- eval_summarizer(
    eval_data = eval_tib, eval_id = eval_id, value_col = yvar,
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
#' the columns \code{.rep}, \code{.dgp_name}, and \code{.method_name} in
#' addition to the columns specified by \code{group_cols}, \code{vary_params},
#' \code{feature_col}, and \code{imp_col}.
#'
#' The output of \code{summarize_feature_importance()} is a grouped
#' \code{tibble} containing both identifying information and the feature
#' importance results aggregated over experimental replicates. Specifically, the
#' identifier columns include \code{.dgp_name}, \code{.method_name}, any columns
#' specified by \code{group_cols} and \code{vary_params}, and the column
#' specified by \code{feature_col}. In addition, there are results columns
#' corresponding to the requested statistics in \code{summary_funs} and
#' \code{custom_summary_funs}. These columns end in the suffix
#' specified by \code{eval_id}.
#'
#' @family feature_selection_funs
#'
#' @examples
#' # generate example fit_results data for a feature selection problem
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
#' # evaluate feature importances (using all default metrics) for each replicate
#' eval_results <- eval_feature_importance(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   feature_col = "feature",
#'   imp_col = "est_importance"
#' )
#' # summarize feature importances (using all default metric) across replicates
#' eval_results_summary <- summarize_feature_importance(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   feature_col = "feature",
#'   imp_col = "est_importance"
#' )
#'
NULL

#' @rdname eval_feature_importance_funs
#'
#' @export
eval_feature_importance <- function(fit_results, vary_params = NULL,
                                    nested_cols = NULL, feature_col, imp_col,
                                    group_cols = NULL) {
  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  if (!is.null(nested_cols)) {
    fit_results <- fit_results %>%
      tidyr::unnest(tidyselect::all_of(nested_cols))
  } else {
    fit_results <- fit_results %>%
      tidyr::unnest(tidyselect::all_of(c(feature_col, imp_col, group_cols)))
  }
  eval_tib <- fit_results %>%
    dplyr::select(
      tidyselect::all_of(c(id_vars, feature_col, imp_col, group_cols))
    )
  return(eval_tib)
}

#' @rdname eval_feature_importance_funs
#'
#' @export
summarize_feature_importance <- function(fit_results, vary_params = NULL,
                                         nested_cols = NULL, feature_col,
                                         imp_col, group_cols = NULL,
                                         na_rm = FALSE,
                                         summary_funs = c("mean", "median",
                                                          "min", "max",
                                                          "sd", "raw"),
                                         custom_summary_funs = NULL,
                                         eval_id = "feature_importance") {
  group_vars <- c(".dgp_name", ".method_name", vary_params,
                  group_cols, feature_col)
  eval_tib <- eval_feature_importance(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, feature_col = feature_col, imp_col = imp_col,
    group_cols = group_cols
  ) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars)))

  eval_summary <- eval_summarizer(
    eval_data = eval_tib, eval_id = eval_id, value_col = imp_col,
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}
