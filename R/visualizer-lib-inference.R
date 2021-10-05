#' Plot the rejection probability of a test.
#' 
#' @description Plot the probability of rejecting the null hypothesis
#'   across various levels of significance.
#' 
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @inheritParams eval_reject_prob
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{eval_reject_prob()}.
#'   
#' @inherit plot_eval_summary return
#'
#' @family inference_funs
#'
#' @importFrom rlang .data
#' @export
plot_reject_prob <- function(fit_results, eval_results, evaluator_name = NULL,
                             vary_params = NULL, feature = NULL, 
                             show = c("line"), ...) {
  .alpha <- NULL  # to fix no visible binding for global variable error
  show <- match.arg(show, choices = c("point", "line", "bar"))
  if (!is.null(feature)) {
    arg_list <- get_dot_args(
      user_args = list(...), 
      default_args = list(eval_id = "",
                          eval_fun = "eval_reject_prob",
                          x_str = ".alpha",
                          y_str = "Rejection Prob",
                          facet_wrap = substitute(~ .data[[feature]],
                                                  list(feature = feature)))
    )
  } else {
    arg_list <- get_dot_args(
      user_args = list(...), 
      default_args = list(eval_id = "",
                          eval_fun = "eval_reject_prob",
                          x_str = ".alpha",
                          y_str = "Rejection Prob")
    )
  }
  
  eval_out <- get_eval_tibble(fit_results, eval_results, evaluator_name,
                              arg_list$eval_id, arg_list$eval_fun, 
                              vary_params, show, arg_list$y_str, ...)
  
  if (identical(show, "bar")) {
    eval_out <- eval_out %>%
      dplyr::mutate(.alpha = as.factor(.alpha))
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_out = eval_out,
                  vary_params = vary_params, show = show),
             arg_list)
  )
  return(plt)
}

#' Plot testing error evaluation results according to various metrics.
#' 
#' @description Plot the raw or summarized testing errors as a 
#'   boxplot, scatter plot, line plot, or bar plot with or without 1 SD error 
#'   bars. 
#' 
#' @inheritParams plot_pred_err
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_testing_err()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family inference_funs
#' 
#' @export
plot_testing_err <- function(fit_results, eval_results, evaluator_name = NULL,
                             vary_params = NULL, metrics = NULL, 
                             show = c("point", "line", "errorbar"), ...) {
  .metric <- NULL  # to fix no visible binding for global variable error
  arg_list <- get_dot_args(
    user_args = list(...), 
    default_args = list(eval_id = "testing_err",
                        eval_fun = "summarize_testing_err",
                        facet_wrap_formula = ~ .metric,
                        facet_args = list(scales = "free"))
  )
  
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  eval_out <- NULL
  if (!is.null(evaluator_name)) {
    eval_out <- eval_results[[evaluator_name]]
    if (!is.null(metrics)) {
      metric_names <- names(attr(metrics, "metrics"))
      eval_out <- eval_out %>%
        dplyr::filter(.metric %in% metric_names)
    }
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_out = eval_out,
                  vary_params = vary_params, show = show, metrics = metrics),
             arg_list)
  )
  return(plt)
}

#' Plot ROC/PR curves for feature rankings from p-values.
#' 
#' @description Plot ROC/PR curves for the feature rankings from p-values or 
#'   some summary thereof across experimental replicates.
#' 
#' @inheritParams plot_pred_curve
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_pval_ranking_curve()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family inference_funs
#' 
#' @export
plot_pval_ranking_curve <- function(fit_results, eval_results,
                                    evaluator_name = NULL, vary_params = NULL, 
                                    metric = c("ROC", "PR"),
                                    show = c("line", "ribbon"), ...) {
  metric <- match.arg(metric)
  if (metric == "ROC") {
    eval_id <- "TPR"
    x_str <- "FPR"
  } else if (metric == "PR") {
    eval_id <- "precision"
    x_str <- "recall"
  }
  arg_list <- get_dot_args(
    user_args = list(...), 
    default_args = list(eval_id = eval_id,
                        eval_fun = "summarize_pval_ranking_curve",
                        x_str = x_str,
                        ribbon_args = list(alpha = 0.2))
  )
  
  eval_out <- NULL
  if (!is.null(evaluator_name)) {
    eval_out <- eval_results[[evaluator_name]]
    if (("recall" %in% colnames(eval_out)) && (identical(metric, "ROC"))) {
      eval_out <- NULL
    } else if (("FPR" %in% colnames(eval_out)) && (identical(metric, "PR"))) {
      eval_out <- NULL
    }
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_out = eval_out,
                  vary_params = vary_params, show = show, metric = metric),
             arg_list)
  )
  return(plt)
}

