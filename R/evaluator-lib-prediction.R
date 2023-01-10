#' Evaluate and/or summarize prediction errors.
#'
#' @name eval_pred_err_funs
#' @description Evaluate various prediction error metrics, given the true
#'   responses and the predicted (or estimated) responses.
#'   \code{eval_pred_err()} evaluates the various prediction error metrics for
#'   each experimental replicate separately. \code{summarize_pred_err()}
#'   summarizes the various prediction error metrics across experimental
#'   replicates.
#'
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @param truth_col A character string identifying the column with the true
#'   responses. The column should be numeric for a regression problem and a
#'   factor for a classification problem.
#' @param estimate_col A character string identifying the column with the
#'   estimated or predicted responses. The column should be numeric for a
#'   regression problem and a factor (with the predicted classes) for a
#'   classification problem.
#' @param prob_cols A character string or vector identifying the column(s)
#'   containing class probabilities. If the \code{truth_col} column is binary,
#'   only 1 column name should be provided. Otherwise, the length of the
#'   \code{prob_cols} should be equal to the number of factor levels of
#'   the \code{truth_col} column. This argument is not used when evaluating
#'   numeric metrics.
#' @param metrics A \code{metric_set} object indicating the metrics to evaluate.
#'   See [yardstick::metric_set()] for more details. Default \code{NULL} will
#'   use the default metrics in [yardstick::metrics()].
#' @param groups (Optional) vector of group IDs to group observations by before
#'   evaluating prediction errors. This is useful for assessing within-group
#'   prediction errors. Note: the (unstratified) prediction errors, aggregated
#'   across the full data set, are computed in addition to these stratified
#'   within-group errors.
#'
#' @returns
#' The output of \code{eval_pred_err()} is a \code{tibble} with the following
#' columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{.group}{If \code{groups} is not \code{NULL}, this column specifies the
#'   name of the group under evaluation. Otherwise, this column is not returned.}
#' \item{.metric}{Name of the evaluation metric.}
#' \item{.estimate}{Value of the evaluation metric.}
#' }
#' as well as any columns specified by \code{vary_params}.
#'
#' The output of \code{summarize_pred_err()} is a grouped \code{tibble}
#' containing both identifying information and the prediction error results
#' aggregated over experimental replicates. Specifically, the identifier columns
#' include \code{.dgp_name}, \code{.method_name}, any columns specified by
#' \code{vary_params}, and  \code{.metric}. In addition, there are results
#' columns corresponding to the requested statistics in \code{summary_funs} and
#' \code{custom_summary_funs}. These columns end in the suffix "_pred_err".
#'
#' @family prediction_error_funs
#'
#' @examples
#' ############################
#' #### Regression Problem ####
#' ############################
#'
#' # generate example fit_results data for a regression problem
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4, FUN = function(x) rnorm(100)),
#'   # predicted response
#'   predictions = lapply(1:4, FUN = function(x) rnorm(100))
#' )
#'
#' # evaluate prediction error (using all default metrics) for each replicate
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions")
#' # summarize prediction error (using all default metric) across replicates
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions")
#'
#' # evaluate/summarize prediction error within subgroups
#' group_ids <- rep(c("a", "b"), length.out = 100)
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions",
#'                               groups = group_ids)
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions",
#'                                            groups = group_ids)
#'
#' # evaluate/summarize prediction errors using specific yardstick metrics
#' metrics <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions",
#'                               metrics = metrics)
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions",
#'                                            metrics = metrics)
#'
#' # summarize prediction errors using specific summary metric
#' range_fun <- function(x) return(max(x) - min(x))
#' eval_results_summary <- summarize_pred_err(
#'   fit_results,
#'   truth_col = "y",
#'   estimate_col = "predictions",
#'   custom_summary_funs = list(range_pred_err = range_fun)
#' )
#'
#' #######################################
#' #### Binary Classification Problem ####
#' #######################################
#' # generate example fit_results data for a binary classification problem
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4,
#'              FUN = function(x) {
#'                as.factor(sample(0:1, size = 100, replace = TRUE))
#'              }),
#'   # predicted class probabilities
#'   class_probs = lapply(1:4, FUN = function(x) runif(n = 100, min = 0, max = 1)),
#'   # predicted class responses
#'   predictions = lapply(class_probs,
#'                        FUN = function(x) as.factor(ifelse(x > 0.5, 1, 0)))
#' )
#'
#' # evaluate prediction error (using all default metrics) for each replicate
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions",
#'                               prob_cols = "class_probs")
#' # summarize prediction error (using all default metric) across replicates
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions",
#'                                            prob_cols = "class_probs")
#'
#' # can also evaluate results using only class predictions (without class probs.)
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions")
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions")
#'
#' ############################################
#' #### Multi-class Classification Problem ####
#' ############################################
#' # generate example fit_results data for a multi-class classification problem
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4,
#'              FUN = function(x) {
#'                as.factor(sample(c("a", "b", "c"), size = 100, replace = TRUE))
#'              }),
#'   # predicted class probabilities
#'   class_probs = lapply(1:4,
#'                        FUN = function(x) {
#'                          tibble::tibble(a = runif(n = 100, min = 0, max = 0.5),
#'                                         b = runif(n = 100, min = 0, max = 0.5),
#'                                         c = 1 - a - b)
#'                        }),
#'   # predicted class responses
#'   predictions = lapply(class_probs,
#'                        FUN = function(x) {
#'                          yhat <- apply(x, 1,
#'                                        FUN = function(xi) names(which.max(xi)))
#'                          return(as.factor(yhat))
#'                        })
#' )
#'
#' # evaluate prediction error (using all default metrics) for each replicate
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions",
#'                               prob_cols = c("a", "b", "c"),
#'                               nested_data = "class_probs")
#' #' summarize prediction error (using all default metric) across replicates
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions",
#'                                            prob_cols = c("a", "b", "c"),
#'                                            nested_data = "class_probs")
#'
#' # can also evaluate results using only class predictions (without class probs.)
#' eval_results <- eval_pred_err(fit_results,
#'                               truth_col = "y",
#'                               estimate_col = "predictions")
#' eval_results_summary <- summarize_pred_err(fit_results,
#'                                            truth_col = "y",
#'                                            estimate_col = "predictions")
#'
NULL

#' @rdname eval_pred_err_funs
#'
#' @importFrom rlang .data
#' @export
eval_pred_err <- function(fit_results, vary_params = NULL, nested_data = NULL,
                          truth_col, estimate_col, prob_cols = NULL,
                          metrics = NULL, groups = NULL, options = list(),
                          na_rm = FALSE) {
  .estimator <- NULL  # to fix no visible binding for global variable error
  .eval_res <- NULL
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    abort("Unknown metrics. metrics must be of class 'yardstick::metric_set' or NULL.")
  }

  eval_pred_err_rowwise <- function(data) {
    if (!is.null(nested_data)) {
      data <- data %>% tidyr::unnest(tidyselect::all_of(nested_data))
    }
    cols <- colnames(data)
    truth_col <- tidyselect::vars_pull(cols, tidyselect::all_of(truth_col))
    estimate_col <- tidyselect::vars_pull(cols,
                                          tidyselect::all_of(estimate_col))
    prob_cols <- intersect(cols, prob_cols)
    data <- data %>%
      tidyr::unnest(tidyselect::all_of(c(truth_col, estimate_col, prob_cols)))

    if (!is.null(groups)) {
      group_id_vec <- c(rep(".all", nrow(data)), groups)
      data <- dplyr::bind_rows(data, data) %>%
        dplyr::mutate(.group = group_id_vec) %>%
        dplyr::group_by(.group)
    }

    if (is.null(metrics)) {
      res <- yardstick::metrics(
        data = data, truth = !!truth_col, estimate = !!estimate_col,
        tidyselect::all_of(prob_cols), options = options, na_rm = na_rm
      )
    } else {
      is_class <- is.factor(data[[truth_col]]) ||
        inherits(data[[truth_col]], "class_pred")
      if (is_class) {
        res <- metrics(
          data = data, truth = !!truth_col, estimate = !!estimate_col,
          tidyselect::all_of(prob_cols), na_rm = na_rm
        )
      } else {
        res <- metrics(
          data, truth = !!truth_col, estimate = !!estimate_col, na_rm = na_rm
        )
      }
    }
    if (na_rm) {
      na_counts <- data %>%
        dplyr::summarise(.estimate = sum(is.na(.data[[estimate_col]]))) %>%
        dplyr::mutate(.metric = "num_na")
      res <- res %>%
        dplyr::bind_rows(na_counts)
    }
    return(res %>% dplyr::select(-.estimator))
  }

  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  eval_tib <- fit_results %>%
    dplyr::mutate(
      .eval_res = purrr::map(
        1:nrow(fit_results),
        ~eval_pred_err_rowwise(data = fit_results[.x, ])
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), .eval_res) %>%
    tidyr::unnest(.eval_res)
  return(eval_tib)
}

#' @rdname eval_pred_err_funs
#'
#' @export
summarize_pred_err <- function(fit_results, vary_params = NULL,
                               nested_data = NULL, truth_col, estimate_col,
                               prob_cols = NULL, metrics = NULL, groups = NULL,
                               options = list(), na_rm = FALSE,
                               summary_funs = c("mean", "median", "min", "max",
                                                "sd", "raw"),
                               custom_summary_funs = NULL,
                               eval_id = "pred_err") {
  if (!is.null(groups)) {
    group_vars <- c(".dgp_name", ".method_name", vary_params, ".group", ".metric")
  } else {
    group_vars <- c(".dgp_name", ".method_name", vary_params, ".metric")
  }
  eval_tib <- eval_pred_err(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col,
    estimate_col = estimate_col, prob_cols = prob_cols,
    metrics = metrics, groups = groups, options = options, na_rm = na_rm
  ) %>%
    dplyr::group_by(dplyr::across({{group_vars}}))

  eval_summary <- summarize_eval_results(
    eval_data = eval_tib, eval_id = eval_id, value_col = ".estimate",
    summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
    na_rm = na_rm
  )
  return(eval_summary)
}

#' Evaluate and/or summarize ROC or PR curves.
#'
#' @name eval_pred_curve_funs
#' @description Evaluate the ROC or PR curves, given the true responses and the
#'   predicted probabilities for each class. \code{eval_pred_curve()} evaluates
#'   the ROC or PR curve for each experimental replicate separately.
#'   \code{summarize_pred_curve()} summarizes the ROC or PR curve across
#'   experimental replicates.
#'
#' @inheritParams shared_eval_lib_args
#' @inheritParams eval_pred_err
#'
#' @returns
#' The output of \code{eval_pred_curve()} is a \code{tibble} with the following
#' columns:
#' \describe{
#' \item{.rep}{Replicate ID.}
#' \item{.dgp_name}{Name of DGP.}
#' \item{.method_name}{Name of Method.}
#' \item{.group}{If \code{groups} is not \code{NULL}, this column specifies the
#'   name of the group under evaluation. Otherwise, this column is not returned.}
#' \item{curve_estimate}{A list of tibbles with x and y coordinate values for
#'   the ROC/PR curve for the given experimental replicate. If
#'   \code{curve = "ROC"}, the \code{tibble} has the columns \code{.threshold},
#'   \code{FPR}, and \code{TPR} for the threshold, false positive rate, and true
#'   positive rate, respectively. If \code{curve = "PR"}, the \code{tibble} has
#'   the columns \code{.threshold}, \code{recall}, and \code{precision}.}
#' }
#' as well as any columns specified by \code{vary_params}.
#'
#' The output of \code{summarize_pred_curve()} is a grouped \code{tibble}
#' containing both identifying information and the prediction curve results
#' aggregated over experimental replicates. Specifically, the identifier columns
#' include \code{.dgp_name}, \code{.method_name}, and any columns specified by
#' \code{vary_params}. In addition, there are results columns corresponding to
#' the requested statistics in \code{summary_funs} and
#' \code{custom_summary_funs}. If \code{curve = "ROC"}, these results columns
#' include \code{FPR} and others that end in the suffix "_TPR". If
#' \code{curve = "PR"}, the results columns include \code{recall} and others
#' that end in the suffix "_precision".
#'
#' @family prediction_error_funs
#'
#' @examples
#' #######################################
#' #### Binary Classification Problem ####
#' #######################################
#' # generate example fit_results data for a binary classification problem
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4,
#'              FUN = function(x) {
#'                as.factor(sample(0:1, size = 100, replace = TRUE))
#'              }),
#'   # predicted class probabilities
#'   class_probs = lapply(1:4, FUN = function(x) runif(n = 100, min = 0, max = 1))
#' )
#'
#' # evaluate ROC/PR curve for each replicate
#' roc_results <- eval_pred_curve(fit_results, curve = "ROC",
#'                                truth_col = "y", prob_cols = "class_probs")
#' pr_results <- eval_pred_curve(fit_results, curve = "PR",
#'                               truth_col = "y", prob_cols = "class_probs")
#'
#' # summarize ROC/PR curves across replicates
#' roc_summary <- summarize_pred_curve(fit_results, curve = "ROC",
#'                                     truth_col = "y", prob_cols = "class_probs")
#' pr_summary <- summarize_pred_curve(fit_results, curve = "PR",
#'                                    truth_col = "y", prob_cols = "class_probs")
#'
#' ############################################
#' #### Multi-class Classification Problem ####
#' ############################################
#' # generate example fit_results data for a multi-class classification problem
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4,
#'              FUN = function(x) {
#'                as.factor(sample(c("a", "b", "c"), size = 100, replace = TRUE))
#'              }),
#'   # predicted class probabilities
#'   class_probs = lapply(1:4,
#'                        FUN = function(x) {
#'                          tibble::tibble(a = runif(n = 100, min = 0, max = 0.5),
#'                                         b = runif(n = 100, min = 0, max = 0.5),
#'                                         c = 1 - a - b)
#'                        })
#' )
#'
#' # evaluate ROC/PR curve for each replicate
#' roc_results <- eval_pred_curve(fit_results, curve = "ROC",
#'                                nested_data = "class_probs",
#'                                truth_col = "y",
#'                                prob_cols = c("a", "b", "c"))
#' pr_results <- eval_pred_curve(fit_results, curve = "PR",
#'                               nested_data = "class_probs",
#'                               truth_col = "y",
#'                               prob_cols = c("a", "b", "c"))
#'
#' # summarize ROC/PR curves across replicates
#' roc_summary <- summarize_pred_curve(fit_results, curve = "ROC",
#'                                     nested_data = "class_probs",
#'                                     truth_col = "y",
#'                                     prob_cols = c("a", "b", "c"))
#' pr_summary <- summarize_pred_curve(fit_results, curve = "PR",
#'                                    nested_data = "class_probs",
#'                                    truth_col = "y",
#'                                    prob_cols = c("a", "b", "c"))
#'
NULL

#' @rdname eval_pred_curve_funs
#'
#' @export
eval_pred_curve <- function(fit_results, vary_params = NULL, nested_data = NULL,
                            truth_col, prob_cols, curve = c("ROC", "PR"),
                            groups = NULL, options = list(), na_rm = FALSE) {
  specificity <- NULL  # to fix no visible binding for global variable error
  sensitivity <- NULL
  FPR <- NULL
  curve_estimate <- NULL
  curve <- match.arg(curve)

  eval_pred_curve_rowwise <- function(data) {
    if (!is.null(nested_data)) {
      data <- data %>% tidyr::unnest(tidyselect::all_of(nested_data))
    }
    cols <- colnames(data)
    truth_col <- tidyselect::vars_pull(cols, tidyselect::all_of(truth_col))
    prob_cols <- intersect(cols, prob_cols)
    data <- data %>%
      tidyr::unnest(tidyselect::all_of(c(truth_col, prob_cols)))

    if (!is.null(groups)) {
      group_id_vec <- c(rep(".all", nrow(data)), groups)
      data <- dplyr::bind_rows(data, data) %>%
        dplyr::mutate(.group = group_id_vec) %>%
        dplyr::group_by(.group)
    }

    if (identical(curve, "ROC")) {
      curve_df <- yardstick::roc_curve(
        data = data, truth = !!truth_col, tidyselect::all_of(prob_cols),
        options = options, na_rm = na_rm
      ) %>%
        dplyr::rename(FPR = specificity, TPR = sensitivity) %>%
        dplyr::mutate(FPR = 1 - FPR)
    } else if (identical(curve, "PR")) {
      curve_df <- yardstick::pr_curve(
        data = data, truth = !!truth_col, tidyselect::all_of(prob_cols),
        na_rm = na_rm
      )
    }
    return(curve_df)
  }

  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  eval_tib <- fit_results %>%
    dplyr::mutate(
      curve_estimate = purrr::map(
        1:nrow(fit_results),
        ~eval_pred_curve_rowwise(data = fit_results[.x, ])
      )
    ) %>%
    dplyr::select(tidyselect::all_of(id_vars), curve_estimate)
  return(eval_tib)
}

#' @rdname eval_pred_curve_funs
#'
#' @export
summarize_pred_curve <- function(fit_results, vary_params = NULL,
                                 nested_data = NULL, truth_col, prob_cols,
                                 curve = c("ROC", "PR"),
                                 groups = NULL, options = list(), na_rm = FALSE,
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
  if (!is.null(groups)) {
    group_vars <- c(".dgp_name", ".method_name", vary_params, ".group", xvar)
  } else {
    group_vars <- c(".dgp_name", ".method_name", vary_params, xvar)
  }

  eval_tib <- eval_pred_curve(
    fit_results = fit_results, vary_params = vary_params,
    nested_data = nested_data, truth_col = truth_col, prob_cols = prob_cols,
    curve = curve, groups = groups, options = options, na_rm = na_rm
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
