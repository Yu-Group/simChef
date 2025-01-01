#' Evaluate and/or summarize error metrics when conducting multiple hypothesis
#'   tests.
#'
#' @name eval_testing_err_funs
#' @description Evaluate various testing error metrics, given the true feature
#'   support and the estimated p-values at pre-specified significance level
#'   thresholds. `eval_testing_err()` evaluates the various testing error
#'   metrics for each experimental replicate separately.
#'   `summarize_testing_err()` summarizes the various testing error metrics
#'   across experimental replicates.
#'
#' @inheritParams eval_feature_selection_err
#' @param pval_col A character string identifying the column in
#'   `fit_results` with the estimated p-values data. Each element in
#'   this column should be an array of length `p`, where `p` is the
#'   number of features and the feature order aligns with that of
#'   `truth_col`.
#' @param metrics A `metric_set` object indicating the metrics to evaluate.
#'   See [yardstick::metric_set()] for more details. Default `NULL` will
#'   evaluate the following: number of true positives (`tp`), number of
#'   false positives (`fp`), sensitivity (`sens`), specificity
#'   (`spec`), positive predictive value (`ppv`), number of tests that
#'   were rejected (`pos`), number of tests that were not rejected
#'   (`neg`), AUROC (`roc_auc`), and AUPRC (`pr_auc`).
#' @param alphas Vector of significance levels at which to evaluate
#'   the various metrics. Default is `alphas = 0.05`.
#'
#' @returns
#' The output of `eval_testing_err()` is a `tibble` with the following
#' columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{.alpha}{Level of significance.}
#' \item{.metric}{Name of the evaluation metric.}
#' \item{.estimate}{Value of the evaluation metric.}
#' }
#' as well as any columns specified by `group_cols` and `vary_params`.
#'
#' The output of `summarize_testing_err()` is a grouped `tibble`
#' containing both identifying information and the evaluation results
#' aggregated over experimental replicates. Specifically, the identifier columns
#' include `.dgp_name`, `.method_name`, any columns specified by
#' `group_cols` and `vary_params`, and `.metric`. In addition,
#' there are results columns corresponding to the requested statistics in
#' `summary_funs` and `custom_summary_funs`. These columns end in the
#' suffix specified by `eval_id`.
#'
#' @family inference_funs
#'
#' @examples
#' # generate example fit_results data for an inference problem
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
#'         # estimated p-values
#'         pval = 10^(sample(-3:0, 3, replace = TRUE))
#'       )
#'     }
#'   )
#' )
#'
#' # evaluate feature selection (using all default metrics and alpha = 0.05) for each replicate
#' eval_results <- eval_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' # summarize feature selection error (using all default metric and alpha = 0.05) across replicates
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#'
#' # evaluate/summarize feature selection (at alpha = 0.05) using specific yardstick metrics
#' metrics <- yardstick::metric_set(yardstick::sens, yardstick::spec)
#' eval_results <- eval_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   metrics = metrics
#' )
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   metrics = metrics
#' )
#'
#' # can evaluate/summarize feature selection at multiple values of alpha
#' eval_results <- eval_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   alphas = c(0.05, 0.1)
#' )
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   alphas = c(0.05, 0.1)
#' )
#'
#' # summarize feature selection (at alpha = 0.05) using specific summary metric
#' range_fun <- function(x) return(max(x) - min(x))
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   custom_summary_funs = list(range_testing_err = range_fun)
#' )
#'
NULL

#' @rdname eval_testing_err_funs
#'
#' @importFrom rlang .data
#' @export
eval_testing_err <- function(fit_results, vary_params = NULL,
                             nested_cols = NULL, truth_col, pval_col = NULL,
                             group_cols = NULL, metrics = NULL, alphas = 0.05,
                             na_rm = FALSE) {

  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    abort("Unknown metrics. metrics must be of class 'yardstick::metric_set' or NULL.")
  }

  eval_testing_err_fun <- function(data, truth_col, pval_col,
                                   metrics, alphas, na_rm) {

    data <- data |>
      dplyr::mutate(
        {{truth_col}} := factor(as.integer(as.numeric(.data[[truth_col]]) != 0),
                                levels = 0:1),
        # temporarily invert p-value for *_auc computation
        .pval_imp = -.data[[pval_col]]
      )

    if (is.null(alphas)) {
      alphas <- sort(unique(c(0, data[[pval_col]], 1)))
    }
    if (is.null(metrics)) {
      metrics <- yardstick::metric_set(tp, fp,
                                       yardstick::sens, yardstick::spec,
                                       yardstick::ppv,
                                       pos, neg,
                                       yardstick::roc_auc, yardstick::pr_auc)
    }

    out <- purrr::map(
      alphas,
      function(alpha) {
        data_alpha <- data |>
          dplyr::mutate(
            {{pval_col}} := factor(as.integer(.data[[pval_col]] <= !!alpha),
                                   levels = 0:1)
          )
        out <- metrics(data = data_alpha, truth = !!truth_col,
                       estimate = !!pval_col, .pval_imp,
                       na_rm = na_rm, event_level = "second") |>
          dplyr::mutate(.alpha = !!alpha) |>
          add_na_counts(data = data, value_col = pval_col, na_rm = na_rm,
                        .alpha = !!alpha) |>
          dplyr::select(.alpha, .metric, .estimate)
        return(out)
      }
    ) |>
      purrr::list_rbind()
    return(out)
  }

  eval_tib <- eval_constructor(
    fit_results = fit_results, vary_params = vary_params,
    fun = eval_testing_err_fun, nested_cols = nested_cols,
    truth_col = truth_col, pval_col = pval_col, group_cols = group_cols,
    fun_options = list(metrics = metrics, alphas = alphas), na_rm = na_rm
  ) |>
    tidyr::unnest(.eval_result)
  return(eval_tib)
}


#' @rdname eval_testing_err_funs
#'
#' @importFrom rlang .data
#' @export
summarize_testing_err <- function(fit_results, vary_params = NULL,
                                  nested_cols = NULL, truth_col,
                                  pval_col = NULL, group_cols = NULL,
                                  metrics = NULL, alphas = 0.05, na_rm = FALSE,
                                  summary_funs = c("mean", "median", "min",
                                                   "max", "sd", "raw"),
                                  custom_summary_funs = NULL,
                                  eval_id = "testing_err") {
  group_vars <- c(".dgp_name", ".method_name", vary_params, group_cols,
                  ".metric", ".alpha")
  eval_tib <- eval_testing_err(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, truth_col = truth_col, pval_col = pval_col,
    group_cols = group_cols, metrics = metrics, alphas = alphas, na_rm = na_rm
  ) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(group_vars)))

  eval_summary <- eval_summarizer(
    eval_data = eval_tib, eval_id = eval_id, value_col = ".estimate",
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Evaluate and/or summarize ROC or PR curves for feature rankings, ranked by
#'   p-value.
#'
#' @name eval_testing_curve_funs
#' @description Evaluate the ROC or PR curves corresponding to the feature
#'   importances as ranked by their p-values. `eval_testing_curve()`
#'   evaluates the ROC or PR curve for each experimental replicate separately.
#'   `summarize_testing_curve()` summarizes the ROC or PR curve across
#'   experimental replicates.
#'
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_testing_err
#'
#' @returns
#' The output of `eval_testing_curve()` is a `tibble` with the
#' following columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{curve_estimate}{A list of tibbles with x and y coordinate values for
#'   the ROC/PR curve for the given experimental replicate. If
#'   `curve = "ROC"`, the `tibble` has the columns `.threshold`,
#'   `FPR`, and `TPR` for the threshold, false positive rate, and true
#'   positive rate, respectively. If `curve = "PR"`, the `tibble` has
#'   the columns `.threshold`, `recall`, and `precision`.}
#' }
#' as well as any columns specified by `group_cols` and `vary_params`.
#'
#' The output of `summarize_testing_curve()` is a grouped `tibble`
#' containing both identifying information and the evaluation curve
#' results aggregated over experimental replicates. Specifically, the identifier
#' columns include `.dgp_name`, `.method_name`, and any columns
#' specified by `group_cols` and `vary_params`. In addition, there are
#' results columns corresponding to the requested statistics in
#' `summary_funs` and `custom_summary_funs`. If `curve = "ROC"`,
#' these results columns include `FPR` and others that end in the suffix
#' "_TPR". If `curve = "PR"`, the results columns include `recall` and
#' others that end in the suffix "_precision".
#'
#' @family inference_funs
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
#'         # estimated p-values
#'         pval = 10^(sample(-3:0, 3, replace = TRUE))
#'       )
#'     }
#'   )
#' )
#'
#' # evaluate feature selection ROC/PR curves for each replicate
#' roc_results <- eval_testing_curve(
#'   fit_results,
#'   curve = "ROC",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' pr_results <- eval_testing_curve(
#'   fit_results,
#'   curve = "PR",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' # summarize feature selection ROC/PR curves across replicates
#' roc_summary <- summarize_testing_curve(
#'   fit_results,
#'   curve = "ROC",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' pr_summary <- summarize_testing_curve(
#'   fit_results,
#'   curve = "PR",
#'   nested_cols = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#'
NULL

#' @rdname eval_testing_curve_funs
#'
#' @importFrom rlang .data
#' @export
eval_testing_curve <- function(fit_results, vary_params = NULL,
                               nested_cols = NULL, truth_col, pval_col,
                               group_cols = NULL, curve = c("ROC", "PR"),
                               na_rm = FALSE) {
  curve_estimate <- NULL  # to fix no visible binding for global variable error
  if (is.null(nested_cols) || (pval_col %in% names(fit_results))) {
    fit_results <- fit_results |>
      dplyr::rowwise() |>
      dplyr::mutate({{pval_col}} := -.data[[pval_col]])
  } else {
    fit_results[[nested_cols]] <- purrr::map(
      fit_results[[nested_cols]],
      ~.x |> dplyr::mutate({{pval_col}} := -.data[[pval_col]])
    )
  }

  eval_tib <- eval_feature_selection_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, truth_col = truth_col, imp_col = pval_col,
    group_cols = group_cols, curve = curve, na_rm = na_rm
  ) |>
    dplyr::mutate(
      curve_estimate = purrr::map(
        curve_estimate,
        ~.x |> dplyr::mutate(.threshold = -.threshold)
      )
    )
  return(eval_tib)
}

#' @rdname eval_testing_curve_funs
#'
#' @export
summarize_testing_curve <- function(fit_results, vary_params = NULL,
                                    nested_cols = NULL, truth_col, pval_col,
                                    group_cols = NULL, curve = c("ROC", "PR"),
                                    na_rm = FALSE,
                                    x_grid = seq(0, 1, by = 1e-2),
                                    summary_funs = c("mean", "median", "min",
                                                     "max", "sd", "raw"),
                                    custom_summary_funs = NULL,
                                    eval_id = ifelse(curve == "PR",
                                                     "precision", "TPR")) {
  curve_estimate <- NULL  # to fix no visible binding for global variable error
  if (curve == "PR") {
    xvar <- "recall"
    yvar <- "precision"
  } else if (curve == "ROC") {
    xvar <- "FPR"
    yvar <- "TPR"
  }
  group_vars <- c(".dgp_name", ".method_name", vary_params, group_cols, xvar)

  eval_tib <- eval_testing_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, truth_col = truth_col, pval_col = pval_col,
    group_cols = group_cols, curve = curve, na_rm = na_rm
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(curve_estimate = list(rescale_curve(curve_estimate,
                                                      x_grid = x_grid,
                                                      xvar = xvar,
                                                      yvar = yvar))) |>
    tidyr::unnest(curve_estimate) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars)))

  eval_summary <- eval_summarizer(
    eval_data = eval_tib, eval_id = eval_id, value_col = yvar,
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Evaluate the rejection probability of a hypothesis test.
#'
#' @description Evaluate the probability of rejecting the null hypothesis
#'   across various levels of significance (possibly for multiple hypothesis
#'   tests, one for each feature).
#'
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_testing_err
#' @param alphas (Optional) Vector of significance levels at which to evaluate
#'   the rejection probability. By default, `alphas` is `NULL`, which
#'   evaluates the full empirical cumulative distribution of the p-values, i.e.,
#'   the rejection probability is evaluated at all possible significance levels.
#'
#' @return A grouped `tibble` containing both identifying information
#'   and the rejection probability results aggregated over experimental
#'   replicates. Specifically, the identifier columns include `.dgp_name`,
#'   `.method_name`, any columns specified by `group_cols` and
#'   `vary_params`, and the feature names given in `feature_col` if
#'   applicable. In addition, there are results columns `.alpha` and
#'   `reject_prob`, which respectively give the significance level and the
#'   estimated rejection probabilities (averaged across experimental
#'   replicates).
#'
#' @family inference_funs
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
#'         # estimated p-values
#'         pval = 10^(sample(-3:0, 3, replace = TRUE))
#'       )
#'     }
#'   )
#' )
#'
#' # evaluate rejection probabilities for each feature across all possible values of alpha
#' eval_results <- eval_reject_prob(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   feature_col = "feature",
#'   pval_col = "pval"
#' )
#'
#' # evaluate rejection probability for each feature at specific values of alpha
#' eval_results <- eval_reject_prob(
#'   fit_results,
#'   nested_cols = "feature_info",
#'   feature_col = "feature",
#'   pval_col = "pval",
#'   alphas = c(0.05, 0.1)
#' )
#'
#' @importFrom rlang .data
#' @export
eval_reject_prob <- function(fit_results, vary_params = NULL,
                             nested_cols = NULL, feature_col = NULL, pval_col,
                             group_cols = NULL, alphas = NULL, na_rm = FALSE) {
  .alpha <- NULL  # to fix no visible binding for global variable error
  group_vars <- c(".dgp_name", ".method_name", vary_params,
                  group_cols, feature_col)
  if (!is.null(nested_cols)) {
    fit_results <- fit_results |>
      tidyr::unnest(tidyselect::all_of(nested_cols))
  } else {
    fit_results <- fit_results |>
      tidyr::unnest(tidyselect::all_of(c(feature_col, pval_col, group_cols)))
  }

  if (is.null(alphas)) {
    eval_tib <- fit_results |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
      dplyr::reframe(
        .alpha = sort(unique(c(0, .data[[pval_col]], 1))),
        reject_prob = stats::ecdf(.data[[pval_col]])(.alpha)
      ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars)))
  } else {
    eval_tib <- fit_results |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) |>
      dplyr::reframe(
        .alpha = sort(alphas),
        reject_prob = purrr::map_dbl(.alpha, ~mean(.data[[pval_col]] <= .x))
      ) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars)))
  }
  return(eval_tib)
}
