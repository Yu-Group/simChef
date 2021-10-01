#' Evaluate error metrics when conducting multiple tests.
#' 
#' @description Evaluate various error metrics, given the true
#'   feature support and the estimated p-values from multiple tests at
#'   pre-specified significance level thresholds.
#' 
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_feature_recovery
#' @param data A \code{data.frame} containing the \code{truth} and 
#'   \code{p_value} columns. Each row in this \code{data.frame} typically
#'   corresponds to a feature or variable.
#' @param p_value A character string identifying the column with the estimated 
#'   p-values.
#' @param alphas (Optional) Vector of significance levels at which to evaluate
#'   the various metrics. By default, \code{alphas = 0.05}. 
#' 
#' @returns A two column \code{tibble} with the following columns:
#' \describe{
#' \item{.alpha}{Level of significance}
#' \item{.metric}{Name of error metric.}
#' \item{.estimate}{Error value for the given metric.}
#' }
#' 
#' @family inference_funs
#' 
#' @importFrom rlang .data
#' @export
eval_testing_err <- function(data, truth, p_value, metrics = NULL,
                             alphas = 0.05, na_rm = FALSE) {
  .estimate <- NULL  # to fix no visible binding for global variable error
  .alpha <- NULL
  .metric <- NULL
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  names <- names(data)
  truth <- tidyselect::vars_pull(names, tidyselect::all_of(truth))
  p_value <- tidyselect::vars_pull(names, tidyselect::all_of(p_value))
  
  data <- data %>%
    dplyr::mutate(
      prob = -log10(.data[[p_value]]),
      {{truth}} := factor(as.integer(as.numeric(.data[[truth]] != 0)),
                          levels = 0:1)
    )
  
  if (is.null(alphas)) {
    alphas <- sort(unique(c(0, data[[p_value]], 1)))
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
          {{p_value}} := factor(as.integer(.data[[p_value]] <= !!alpha),
                                levels = 0:1)
        )
      res <- metrics(data = data_alpha, truth = !!truth, estimate = !!p_value,
                     tidyselect::all_of("prob"),
                     na_rm = na_rm, event_level = "second") %>%
        dplyr::mutate(.alpha = !!alpha) %>%
        dplyr::select(.alpha, .metric, .estimate)
      return(res)
    }
  )
  
  return(res)
}

#' Evaluate ROC or PR curves for feature rankings from p-values.
#' 
#' @description Evaluate the ROC or PR curves for the feature rankings 
#'   determined by the p-values and return a tibble with the results.
#' 
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_feature_recovery
#' @inheritParams eval_testing_err
#' @inheritParams yardstick::roc_curve
#' @param data A \code{data.frame} containing the \code{truth} and 
#'   \code{p_value} columns. Each row in this \code{data.frame} typically
#'   corresponds to a feature or variable.
#' 
#' @inherit eval_pred_curve return
#' 
#' @family inference_funs
#' 
#' @importFrom rlang .data
#' @export
eval_pval_ranking_curve <- function(data, truth, p_value, 
                                    metric = c("ROC", "PR"), 
                                    options = list(), na_rm = FALSE) {
  data <- data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(truth),
                                ~factor(as.integer(as.numeric(.x) != 0), 
                                        levels = 1:0))) %>%
    dplyr::mutate({{p_value}} := -log10(.data[[p_value]]))
  curve_df <- eval_pred_curve(data = data, truth = truth, probs = p_value,
                              metric = metric, options = options, na_rm = na_rm)
  return(curve_df)
}

#' Evaluate rejection probability of a test.
#' 
#' @description Evaluate the probability of rejecting the null hypothesis
#'   across various levels of significance.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @param data_col (Optional) Character string specifying the name of the 
#'   list-type column in \code{fit_results} with the feature support 
#'   \code{data.frames} or \code{tibbles} that contain the \code{truth},
#'   \code{feature}, and \code{p_value} columns. Each row in these 
#'   \code{data.frames} or \code{tibbles} typically corresponds to a feature or
#'   variable. Default \code{NULL} is used in the case where the p-value column
#'   is a column in \code{fit_results} and no feature information is needed.
#' @param feature (Optional) A character string identifying the column with the
#'   feature names nested within the \code{data_col} column. Default \code{NULL}
#'   is used when no feature information is needed.
#' @param p_value A character string identifying the column with the estimated 
#'   p-values. Can either be a column nested in \code{data_col} or a column
#'   directly in the \code{fit_results} tibble.
#' @param alphas (Optional) Vector of significance levels at which to evaluate
#'   the rejection probability. By default, \code{alphas} is \code{NULL}, which
#'   evaluates the full empirical cumulative distribution of the p-values, i.e.,
#'   the rejection probability is evaluated at all possible signficance levels.
#'   
#' @return A grouped \code{tibble} containing both identifying information
#'   and the rejection probability results aggregated over experimental 
#'   replicates. Specifically, the identifier columns include \code{dgp_name},
#'   \code{method_name}, any columns specified by \code{vary_params}, and the
#'   feature names given in \code{feature} if applicable. In addition, there are 
#'   results columns \code{.alpha} and "Rejection Prob", which respectively give
#'   the significance level and rejection probabilities (averaged across
#'   experimental replicates).
#'
#' @family inference_funs
#'
#' @importFrom rlang .data
#' @export
eval_reject_prob <- function(fit_results, vary_params = NULL, 
                             data_col = NULL, feature = NULL, p_value,
                             alphas = NULL, na_rm = FALSE) {
  .alpha <- NULL  # to fix no visible binding for global variable error
  group_vars <- c("dgp_name", "method_name", vary_params, feature)
  if (!is.null(data_col)) {
    fit_results <- fit_results %>%
      tidyr::unnest(tidyselect::all_of(data_col))
  }
  if (is.null(alphas)) {
    eval_results <- fit_results %>%
      dplyr::group_by(dplyr::across({{group_vars}})) %>%
      dplyr::summarise(
        .alpha = sort(unique(c(0, .data[[p_value]], 1))),
        `Rejection Prob` = stats::ecdf(.data[[p_value]])(.alpha),
        .groups = "keep"
      )
  } else {
    eval_results <- fit_results %>%
      dplyr::group_by(dplyr::across({{group_vars}})) %>%
      dplyr::summarise(
        .alpha = sort(alphas),
        `Rejection Prob` = purrr::map_dbl(.alpha, 
                                          ~mean(.data[[p_value]] <= .x)),
        .groups = "keep"
      )
  }
  return(eval_results)
}

#' Summarize error metrics when conducting multiple tests.
#' 
#' @description Summarize testing error evaluation results for a variety of
#'   evaluation metrics across experimental repetitions.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_testing_err
#' @inheritParams eval_reject_prob
#' 
#' @return A grouped \code{tibble} containing both identifying information
#'   and the feature recovery results aggregated over experimental replicates.
#'   Specifically, the identifier columns include \code{dgp_name},
#'   \code{method_name}, any columns specified by \code{vary_params}, and 
#'   \code{.metric}. In addition, there are results columns corresponding to the
#'   requested statistics in \code{summary_funs} and \code{custom_summary_funs}.
#'   These columns end in the suffix "_testing_err".
#' 
#' @family inference_funs
#' 
#' @importFrom rlang .data
#' @export
summarize_testing_err <- function(fit_results, vary_params = NULL, 
                                  data_col, truth, p_value, metrics = NULL, 
                                  alphas = 0.05, na_rm = FALSE,
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
        eval_testing_err(
          data = .data[[data_col]], 
          truth = !!truth, p_value = !!p_value, metrics = !!metrics,
          alphas = !!alphas, na_rm = na_rm
        )
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), eval_out) %>%
    tidyr::unnest(eval_out) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- eval_summary_constructor(
    eval_results, id = "testing_err", value = ".estimate",
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Summarize the ROC/PR curves for feature rankings from p-values.
#' 
#' @description Summarize the ROC or PR curves for the feature rankings 
#'   determined by the p-values and aggregate across experimental repetitions.
#' 
#' @inheritParams eval_pval_ranking_curve
#' @inheritParams summarize_pred_curve
#' @inheritParams summarize_testing_err
#' 
#' @inherit summarize_pred_curve return
#' 
#' @family inference_funs
#' 
#' @importFrom rlang .data
#' @export
summarize_pval_ranking_curve <- function(fit_results, vary_params = NULL,
                                         data_col, truth, p_value, 
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
    curve_df <- eval_pval_ranking_curve(
      data = data, truth = truth, p_value = p_value,
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

