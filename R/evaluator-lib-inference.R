#' Evaluate and/or summarize error metrics when conducting multiple hypothesis
#'   tests.
#' 
#' @name eval_testing_err_funs
#' @description Evaluate various testing error metrics, given the true feature 
#'   support and the estimated p-values at pre-specified significance level 
#'   thresholds. \code{eval_testing_err()} evaluates the various testing error
#'   metrics for each experimental replicate separately. 
#'   \code{summarize_testing_err()} summarizes the various testing error metrics
#'   across experimental replicates.
#'   
#' @inheritParams eval_feature_selection_err
#' @param pval_col A character string identifying the column in 
#'   \code{fit_results} with the estimated p-values data. Each element in
#'   this column should be an array of length \code{p}, where \code{p} is the 
#'   number of features and the feature order aligns with that of 
#'   \code{truth_col}. 
#' @param metrics A \code{metric_set} object indicating the metrics to evaluate.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   evaluate the following: number of true positives (\code{tp}), number of
#'   false positives (\code{fp}), sensitivity (\code{sens}), specificity
#'   (\code{spec}), positive predictive value (\code{ppv}), number of tests that
#'   were rejected (\code{pos}), number of tests that were not rejected 
#'   (\code{neg}), AUROC (\code{roc_auc}), and AUPRC (\code{pr_auc}).
#' @param alphas Vector of significance levels at which to evaluate
#'   the various metrics. Default is \code{alphas = 0.05}.
#'   
#' @returns 
#' The output of \code{eval_testing_err()} is a \code{tibble} with the following
#' columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{.alpha}{Level of significance.}
#' \item{.metric}{Name of the evaluation metric.}
#' \item{.estimate}{Value of the evaluation metric.}
#' }
#' as well as any columns specified by \code{vary_params}.
#' 
#' The output of \code{summarize_testing_err()} is a grouped \code{tibble}
#' containing both identifying information and the evaluation results 
#' aggregated over experimental replicates. Specifically, the identifier columns
#' include \code{.dgp_name}, \code{.method_name}, any columns specified by
#' \code{vary_params}, and \code{.metric}. In addition, there are results columns
#' corresponding to the requested statistics in \code{summary_funs} and 
#' \code{custom_summary_funs}. These columns end in the suffix "_testing_err".
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
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' # summarize feature selection error (using all default metric and alpha = 0.05) across replicates
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' 
#' # evaluate/summarize feature selection (at alpha = 0.05) using specific yardstick metrics
#' metrics <- yardstick::metric_set(yardstick::sens, yardstick::spec)
#' eval_results <- eval_testing_err(
#'   fit_results,
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   metrics = metrics
#' )
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   metrics = metrics
#' )
#' 
#' # can evaluate/summarize feature selection at multiple values of alpha
#' eval_results <- eval_testing_err(
#'   fit_results,
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   alphas = c(0.05, 0.1)
#' )
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval",
#'   alphas = c(0.05, 0.1)
#' )
#' 
#' # summarize feature selection (at alpha = 0.05) using specific summary metric
#' range_fun <- function(x) return(max(x) - min(x))
#' eval_results_summary <- summarize_testing_err(
#'   fit_results,
#'   nested_data = "feature_info",
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
                             nested_data = NULL, truth_col, pval_col = NULL,
                             metrics = NULL, alphas = 0.05, na_rm = FALSE) {
  .estimate <- NULL  # to fix no visible binding for global variable error
  .alpha <- NULL
  .metric <- NULL
  .eval_res <- NULL
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  eval_testing_err_rowwise <- function(data) {
    if (!is.null(nested_data)) {
      data <- data %>% tidyr::unnest(tidyselect::all_of(nested_data))
    }
    cols <- colnames(data)
    truth_col <- tidyselect::vars_pull(cols, tidyselect::all_of(truth_col))
    pval_col <- tidyselect::vars_pull(cols, tidyselect::all_of(pval_col))
    data <- data %>%
      tidyr::unnest(tidyselect::all_of(c(truth_col, pval_col)))
    
    data <- data %>%
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
    
    res <- purrr::map_dfr(
      alphas,
      function(alpha) {
        data_alpha <- data %>%
          dplyr::mutate(
            {{pval_col}} := factor(as.integer(.data[[pval_col]] <= !!alpha),
                                   levels = 0:1)
          )
        res <- metrics(data = data_alpha, truth = !!truth_col, 
                       estimate = !!pval_col, tidyselect::all_of(".pval_imp"),
                       na_rm = na_rm, event_level = "second") %>%
          dplyr::mutate(.alpha = !!alpha) %>%
          dplyr::select(.alpha, .metric, .estimate)
        if (na_rm) {
          res <- res %>%
            dplyr::add_row(.alpha = !!alpha, 
                           .metric = "num_na", 
                           .estimate = sum(is.na(data_alpha[[pval_col]])))
        }
        return(res)
      }
    )
    return(res)
  }
  
  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  eval_tib <- fit_results %>%
    dplyr::mutate(
      .eval_res = purrr::map(
        1:nrow(fit_results),
        ~eval_testing_err_rowwise(data = fit_results[.x, ])
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), .eval_res) %>%
    tidyr::unnest(.eval_res)
  return(eval_tib)
}


#' @rdname eval_testing_err_funs
#' 
#' @importFrom rlang .data
#' @export
summarize_testing_err <- function(fit_results, vary_params = NULL,
                                  nested_data = NULL, truth_col, 
                                  pval_col = NULL, metrics = NULL, 
                                  alphas = 0.05, na_rm = FALSE,
                                  summary_funs = c("mean", "median", "min",
                                                   "max", "sd", "raw"),
                                  custom_summary_funs = NULL,
                                  eval_id = "testing_err") {
  group_vars <- c(".dgp_name", ".method_name", vary_params, ".metric", ".alpha")
  eval_tib <- eval_testing_err(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, pval_col = pval_col,
    metrics = metrics, alphas = alphas, na_rm = na_rm
  ) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
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
#'   importances as ranked by their p-values. \code{eval_testing_curve()} 
#'   evaluates the ROC or PR curve for each experimental replicate separately.
#'   \code{summarize_testing_curve()} summarizes the ROC or PR curve across
#'   experimental replicates.
#' 
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_testing_err
#' 
#' @returns 
#' The output of \code{eval_testing_curve()} is a \code{tibble} with the 
#' following columns:
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
#' The output of \code{summarize_testing_curve()} is a grouped \code{tibble} 
#' containing both identifying information and the evaluation curve 
#' results aggregated over experimental replicates. Specifically, the identifier
#' columns include \code{.dgp_name}, \code{.method_name}, and any columns 
#' specified by \code{vary_params}. In addition, there are results columns 
#' corresponding to the requested statistics in \code{summary_funs} and 
#' \code{custom_summary_funs}. If \code{curve = "ROC"}, these results columns 
#' include \code{FPR} and others that end in the suffix "_TPR". If 
#' \code{curve = "PR"}, the results columns include \code{recall} and others 
#' that end in the suffix "_precision".
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
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' pr_results <- eval_testing_curve(
#'   fit_results,
#'   curve = "PR",
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' # summarize feature selection ROC/PR curves across replicates
#' roc_summary <- summarize_testing_curve(
#'   fit_results,
#'   curve = "ROC",
#'   nested_data = "feature_info",
#'   truth_col = "true_support",
#'   pval_col = "pval"
#' )
#' pr_summary <- summarize_testing_curve(
#'   fit_results,
#'   curve = "PR",
#'   nested_data = "feature_info",
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
                               nested_data = NULL, truth_col, pval_col,
                               curve = c("ROC", "PR"), options = list(), 
                               na_rm = FALSE) {
  curve_estimate <- NULL  # to fix no visible binding for global variable error
  if (is.null(nested_data) | (pval_col %in% names(fit_results))) {
    fit_results <- fit_results %>%
      dplyr::rowwise() %>%
      dplyr::mutate({{pval_col}} := -.data[[pval_col]])
  } else {
    fit_results[[nested_data]] <- purrr::map(
      fit_results[[nested_data]],
      ~.x %>% dplyr::mutate({{pval_col}} := -.data[[pval_col]])
    )
  }
  
  eval_tib <- eval_feature_selection_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, imp_col = pval_col,
    curve = curve, options = options, na_rm = na_rm
  ) %>%
    dplyr::mutate(
      curve_estimate = purrr::map(
        curve_estimate, 
        ~.x %>% dplyr::mutate(.threshold = -.threshold)
      )
    )
  return(eval_tib)
}

#' @rdname eval_testing_curve_funs
#' 
#' @export
summarize_testing_curve <- function(fit_results, vary_params = NULL,
                                    nested_data = NULL, truth_col, pval_col, 
                                    curve = c("ROC", "PR"), 
                                    options = list(), na_rm = FALSE, 
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
  group_vars <- c(".dgp_name", ".method_name", vary_params, xvar)
  
  eval_tib <- eval_testing_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, pval_col = pval_col, 
    curve = curve, options = options, na_rm = na_rm
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(curve_estimate = list(rescale_curve(curve_estimate,
                                                      x_grid = x_grid,
                                                      xvar = xvar,
                                                      yvar = yvar))) %>%
    tidyr::unnest(curve_estimate) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
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
#'   the rejection probability. By default, \code{alphas} is \code{NULL}, which
#'   evaluates the full empirical cumulative distribution of the p-values, i.e.,
#'   the rejection probability is evaluated at all possible significance levels.
#' 
#' @return A grouped \code{tibble} containing both identifying information
#'   and the rejection probability results aggregated over experimental 
#'   replicates. Specifically, the identifier columns include \code{.dgp_name},
#'   \code{.method_name}, any columns specified by \code{vary_params}, and the
#'   feature names given in \code{feature_col} if applicable. In addition, there
#'   are results columns \code{.alpha} and \code{reject_prob}, which 
#'   respectively give the significance level and the estimated rejection 
#'   probabilities (averaged across experimental replicates).
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
#'   nested_data = "feature_info",
#'   feature_col = "feature",
#'   pval_col = "pval"
#' )
#' 
#' # evaluate rejection probability for each feature at specific values of alpha
#' eval_results <- eval_reject_prob(
#'   fit_results,
#'   nested_data = "feature_info",
#'   feature_col = "feature",
#'   pval_col = "pval",
#'   alphas = c(0.05, 0.1)
#' )
#'
#' @importFrom rlang .data
#' @export
eval_reject_prob <- function(fit_results, vary_params = NULL,
                             nested_data = NULL, feature_col = NULL, pval_col,
                             alphas = NULL, na_rm = FALSE) {
  .alpha <- NULL  # to fix no visible binding for global variable error
  group_vars <- c(".dgp_name", ".method_name", vary_params, feature_col)
  if (!is.null(nested_data)) {
    fit_results <- fit_results %>% 
      tidyr::unnest(tidyselect::all_of(nested_data))
  }
  fit_results <- fit_results %>%
    tidyr::unnest(tidyselect::all_of(c(feature_col, pval_col)))
  if (is.null(alphas)) {
    eval_tib <- fit_results %>%
      dplyr::group_by(dplyr::across({{group_vars}})) %>%
      dplyr::summarise(
        .alpha = sort(unique(c(0, .data[[pval_col]], 1))),
        reject_prob = stats::ecdf(.data[[pval_col]])(.alpha),
        .groups = "keep"
      )
  } else {
    eval_tib <- fit_results %>%
      dplyr::group_by(dplyr::across({{group_vars}})) %>%
      dplyr::summarise(
        .alpha = sort(alphas),
        reject_prob = purrr::map_dbl(.alpha, ~mean(.data[[pval_col]] <= .x)),
        .groups = "keep"
      )
  }
  return(eval_tib)
}