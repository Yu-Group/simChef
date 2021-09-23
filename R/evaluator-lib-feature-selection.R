#' Evaluate feature support recovery.
#' 
#' @description Evaluate various support recovery metrics, given the true
#'   feature support set \code{s} and the estimated feature support set 
#'   \code{shat}.
#'   
#' @param s Named vector of the true feature support set, where the names are
#'   the feature names and the vector values are non-zero for the true
#'   support set and zero for the true non-support.
#' @param shat Named vector of the estimated feature support set, where the 
#'   names are the feature names and the vector values are non-zero for the 
#'   estimated support with larger values indicating greater feature importance
#'   and zero for the estimated non-support.
#' @param metrics Character vector of the support recovery metrics to compute.
#'   Elements of the vector must be one of "TP", "FP", "TPR", "FPR", "FDR", 
#'   "P", "N", "AUROC", "AUPRC"
#' @param custom_metrics Named list of custom metric functions to compute. Names
#'   in the list should correspond to the name of the metric. Values in the
#'   list should be a function that takes in the arguments \code{s} and 
#'   \code{shat} only and returns the evaluated metric value.
#' 
#' @return A (long-shaped) data frame with the following columns:
#' \describe{
#' \item{metric}{Name of feature support recovery metric.}
#' \item{value}{Feature support recovery value for the given metric.}
#' }
#' 
#' @details TODO: explain different metrics
#' @examples
#' eval_out <- eval_feature_recovery(
#'   s = c(a = 1, b = 0, c = .5), 
#'   shat = c(a = 0, b = .5, c = .5)
#' )
#' eval_out <- eval_feature_recovery(
#'   s = c(a = 1, b = 0, c = .5),
#'   shat = c(a = 0, b = .5, c = .5),
#'   custom_metrics = list(equal = function(s, shat) {mean(s == shat)},
#'                         TP2 = function(s, shat) {
#'                           sum(names(shat)[shat != 0] %in% names(s)[s != 0])
#'                         })
#' )
#' @export
eval_feature_recovery <- function(s, shat, 
                                  metrics = c("TP", "FP", "TPR", "FPR", "FDR", 
                                              "P", "N", "AUROC", "AUPRC"),
                                  custom_metrics = NULL) {
  # error checking
  metrics <- match.arg(metrics, several.ok = TRUE)
  
  s_set <- names(s)[s != 0]
  shat_set <- names(shat)[shat != 0]
  
  eval_out <- tibble::tibble(metric = character(), value = numeric())
  for (m in metrics) {
    if (m == "TP") {
      err <- sum(shat_set %in% s_set)
    } else if (m == "FP") {
      err <- sum(!(shat_set %in% s_set))
    } else if (m == "TPR") {
      err <- sum(shat_set %in% s_set) / length(s_set)
    } else if (m == "FPR") {
      err <- sum(!(shat_set %in% s_set)) / sum(s == 0)
    } else if (m == "FDR") {
      err <- sum(!(shat_set %in% s_set)) / length(shat_set)
    } else if (m == "P") {
      err <- length(s_set)
    } else if (m == "N") {
      err <- sum(s == 0)
    } else if (m %in% c("AUROC", "AUPRC")) {
      err <- eval_pred_err(y = as.numeric(s != 0), yhat = shat[names(s)], 
                           metrics = m)$value[[1]]
    }
    eval_out <- eval_out %>%
      tibble::add_row(metric = m, value = err)
  }
  
  if (!is.null(custom_metrics)) {
    if (is.null(names(custom_metrics))) {
      names(custom_metrics) <- paste0("ft_recovery_metric",
                                      1:length(custom_metrics))
    }
    custom_eval_out <- purrr::map_dfr(
      custom_metrics,
      function(metric_fun) {
        tibble::tibble_row(
          value = metric_fun(s = s, shat = shat)
        )
      },
      .id = "metric"
    )
    eval_out <- dplyr::bind_rows(eval_out, custom_eval_out)
  }
  
  return(eval_out)
}

#' Show feature importance scores.
#' 
#' @param fit_results A tibble, as returned by the \code{Experiment$fit()}
#'   method.
#' @param vary_params A vector of parameter names that are varied over in the 
#'   Experiment.
#' @param fi Character string or vector. Name of column(s) in \code{fit_results}
#'   with the feature importance score information.
#' 
#' @return TODO
#' 
#' @export
eval_feature_importances <- function(fit_results, vary_params = NULL,
                                     fi = "imp") {
  id_cols <- c("rep", "dgp_name", "method_name", vary_params)
  fi_out <- fit_results %>%
    dplyr::select(tidyselect::all_of(c(id_cols, fi))) %>%
    tidyr::unnest(tidyselect::all_of(fi))
  return(fi_out)
}

#' Summarize prediction error evaluation results.
#' 
#' @description Summarize prediction error evaluation results for a variety of
#'   evaluation metrics across experimental repetitions.
#' 
#' @param fit_results A tibble, as returned by the \code{Experiment$fit()} 
#'   method.
#' @param vary_params A vector of parameter names that are varied over in the 
#'   Experiment.
#' @param s Character string. Name of column in \code{fit_results} with the
#'   true feature support sets.
#' @param shat Character string. Name of column in \code{fit_results} with the
#'   predicted feature support sets.
#' @inheritParams eval_feature_recovery
#' @inheritParams summarize_eval_results
#' 
#' @return A data frame with the following columns in addition to any arguments
#'   in the \code{Experiment}'s \code{vary_params}:
#' \describe{
#' \item{dgp_name}{Name of DGP.}
#' \item{method_name}{Name of Method.}
#' \item{metric}{Name of prediction error metric.}
#' \item{...}{Other columns corresponding to the results from the provided 
#'   \code{summary_funs} and \code{custom_summary_funs} functions.}
#' }
#' 
#' @export
summarize_feature_recovery <- function(fit_results, vary_params = NULL, 
                                       s = "s", shat = "shat",
                                       metrics = c("TP", "FP", "TPR", "FPR", 
                                                   "FDR", "P", "N", "AUROC",
                                                   "AUPRC"),
                                       custom_metrics = NULL,
                                       summary_funs = c("mean", "median", "min",
                                                        "max", "sd", "raw"),
                                       custom_summary_funs = NULL, 
                                       na.rm = F) {
  metrics <- match.arg(metrics, several.ok = TRUE)
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  group_vars <- c("dgp_name", "method_name", vary_params, "metric")
  
  eval_results <- fit_results %>%
    dplyr::mutate(
      eval_out = purrr::map2(
        .data[[s]], .data[[shat]], 
        ~eval_feature_recovery(s = ..1, shat = ..2, 
                               metrics = metrics, 
                               custom_metrics = custom_metrics)
      )
    ) %>%
    tidyr::unnest(eval_out) %>%
    dplyr::group_by(dplyr::across({{group_vars}}))
  
  eval_summary <- summarize_eval_results(
    eval_results, id = "feature_recovery",
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na.rm = na.rm
  )
  return(eval_summary)
}

#' Summarize feature importance results.
#' 
#' @description Summarize feature importance results across experimental
#'   repetitions.
#' 
#' @param fit_results A tibble, as returned by the \code{Experiment$fit()} 
#'   method.
#' @param vary_params A vector of parameter names that are varied over in the 
#'   Experiment.
#' @param fi Character string or vector. Name of column(s) in \code{fit_results}
#'   with the feature importance score information.
#' @param feature_col Character string. Name of the feature column. Could be the
#'   name of a column in \code{fi} or \code{fit_results}.
#' @param imp_col Character string. Name of the importance column. Could be the 
#'   name of a column in \code{fi} or \code{fit_results}.
#' @inheritParams eval_feature_recovery
#' @inheritParams summarize_eval_results
#' 
#' @return A data frame with the following columns in addition to any arguments
#'   in the \code{Experiment}'s \code{vary_params}:
#' \describe{
#' \item{dgp_name}{Name of DGP.}
#' \item{method_name}{Name of Method.}
#' \item{metric}{Name of prediction error metric.}
#' \item{...}{Other columns corresponding to the results from the provided 
#'   \code{summary_funs} and \code{custom_summary_funs} functions.}
#' }
#' 
#' @export
summarize_feature_importances <- function(fit_results, vary_params = NULL, 
                                          fi = "fi", feature_col = "feature",
                                          imp_col = "importance",
                                          summary_funs = c("mean", "median",
                                                           "min", "max", "sd",
                                                           "raw"),
                                          custom_summary_funs = NULL, 
                                          na.rm = F) {
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  
  group_vars <- c("dgp_name", "method_name", vary_params, feature_col)
  
  eval_results <- eval_feature_importances(fit_results = fit_results,
                                           vary_params = vary_params,
                                           fi = fi) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
    eval_results, id = "feature_imp", value = imp_col,
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na.rm = na.rm
  )
  return(eval_summary)
}