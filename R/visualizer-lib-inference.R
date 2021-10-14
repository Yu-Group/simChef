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
  arg_list <- get_dot_args(
    user_args = list(...),
    default_args = list(eval_id = "testing_err",
                        eval_fun = "summarize_testing_err")
  )
  plt <- do.call(
    plot_pred_err,
    c(list(fit_results = fit_results, eval_results = eval_results,
           evaluator_name = evaluator_name, vary_params = vary_params,
           metrics = metrics, show = show),
      arg_list)
  )
  return(plt)
}

#' Plot ROC/PR curves for feature rankings, ranked by p-values.
#' 
#' @description Plot ROC/PR curves for the feature rankings, ranked by their 
#'   p-values or some summary thereof across experimental replicates.
#' 
#' @inheritParams plot_pred_curve
#' @param ... Additional arguments to pass to \code{plot_eval_summary()}. This
#'   includes arguments for plotting and for passing into
#'   \code{summarize_testing_curve()}.
#' 
#' @inherit plot_eval_summary return
#' 
#' @family inference_funs
#' 
#' @export
plot_testing_curve <- function(fit_results, eval_results,
                               evaluator_name = NULL, vary_params = NULL, 
                               curve = c("ROC", "PR"),
                               show = c("line", "ribbon"), ...) {
  arg_list <- get_dot_args(
    user_args = list(...),
    default_args = list(eval_fun = "summarize_testing_curve")
  )
  plt <- do.call(
    plot_pred_curve,
    args = c(list(fit_results = fit_results, eval_results = eval_results,
                  evaluator_name = evaluator_name, vary_params = vary_params,
                  curve = curve, show = show),
             arg_list)
  )
  return(plt)
}

#' Plot the rejection probability of a hypothesis test.
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
                             vary_params = NULL, feature_col = NULL, 
                             show = c("line"), ...) {
  .alpha <- NULL  # to fix no visible binding for global variable error
  show <- match.arg(show, choices = c("point", "line", "bar"))
  if (!is.null(feature_col)) {
    arg_list <- get_dot_args(
      user_args = list(...), 
      default_args = list(eval_id = "",
                          eval_fun = "eval_reject_prob",
                          x_str = ".alpha",
                          y_str = "reject_prob",
                          facet_wrap = substitute(
                            ~ .data[[feature_col]],
                            list(feature_col = feature_col))
                          )
    )
  } else {
    arg_list <- get_dot_args(
      user_args = list(...), 
      default_args = list(eval_id = "",
                          eval_fun = "eval_reject_prob",
                          x_str = ".alpha",
                          y_str = "reject_prob")
    )
  }
  
  if (!is.null(evaluator_name)) {
    eval_tib <- eval_results[[evaluator_name]]
  } else {
    eval_tib <- NULL
  }
  eval_tib <- get_eval_tibble(fit_results = fit_results, eval_tib = eval_tib,
                              eval_id = arg_list$eval_id, 
                              eval_fun = arg_list$eval_fun, 
                              vary_params = vary_params, show = show, 
                              y_str = arg_list$y_str, feature_col = feature_col,
                              ...)
  
  if (identical(show, "bar")) {
    eval_tib <- eval_tib %>% dplyr::mutate(.alpha = as.factor(.alpha))
  }
  
  plt <- do.call(
    plot_eval_summary, 
    args = c(list(fit_results = fit_results, eval_tib = eval_tib,
                  vary_params = vary_params, show = show, 
                  feature_col = feature_col),
             arg_list)
  )
  return(plt)
}