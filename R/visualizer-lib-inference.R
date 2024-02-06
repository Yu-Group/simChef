#' Plot testing error evaluation results according to various metrics.
#'
#' @description Plot the raw or summarized testing errors as a
#'   boxplot, scatter plot, line plot, or bar plot with or without 1 SD error
#'   bars.
#'
#' @inheritParams plot_pred_err
#'
#' @inherit plot_eval_constructor return
#'
#' @family inference_funs
#'
#' @examples
#' # generate example fit_results data
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
#' # generate example eval_results data
#' eval_results <- list(
#'   `Testing Errors` = summarize_testing_err(
#'     fit_results,
#'     nested_cols = "feature_info",
#'     truth_col = "true_support",
#'     pval_col = "pval"
#'   )
#' )
#'
#' # create bar plot using pre-computed evaluation results
#' plt <- plot_testing_err(eval_results = eval_results,
#'                         eval_name = "Testing Errors",
#'                         show = c("bar", "errorbar"))
#' # or alternatively, create the same plot directly from fit results
#' plt <- plot_testing_err(fit_results = fit_results, show = c("bar", "errorbar"),
#'                         eval_fun_options = list(
#'                           nested_cols = "feature_info",
#'                           truth_col = "true_support",
#'                           pval_col = "pval"
#'                         ))
#'
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' plt <- plot_testing_err(eval_results = eval_results,
#'                         eval_name = "Testing Errors",
#'                         show = c("bar", "errorbar"),
#'                         plot_by = ".alpha")
#'
#' @export
plot_testing_err <- function(fit_results = NULL,
                             eval_results = NULL, eval_name = NULL,
                             eval_fun = "summarize_testing_err",
                             eval_fun_options = NULL,
                             vary_params = NULL, metrics = NULL,
                             show = c("point", "line", "errorbar"), ...) {
  arg_list <- get_dot_args(
    user_args = rlang::list2(...),
    default_args = list(eval_id = "testing_err")
  )
  plt <- do.call(
    plot_pred_err,
    args = c(
      list(fit_results = fit_results,
           eval_results = eval_results, eval_name = eval_name,
           eval_fun = eval_fun, eval_fun_options = eval_fun_options,
           vary_params = vary_params, metrics = metrics, show = show),
      arg_list
    )
  )
  return(plt)
}

#' Plot ROC/PR curves for feature rankings, ranked by p-values.
#'
#' @description Plot ROC/PR curves for the feature rankings, ranked by their
#'   p-values or some summary thereof across experimental replicates.
#'
#' @inheritParams plot_pred_curve
#'
#' @inherit plot_eval_constructor return
#'
#' @family inference_funs
#'
#' @examples
#' # generate example fit_results data
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
#' # generate example eval_results data
#' eval_results <- list(
#'   `ROC` = summarize_testing_curve(
#'     fit_results,
#'     curve = "ROC",
#'     nested_cols = "feature_info",
#'     truth_col = "true_support",
#'     pval_col = "pval"
#'   ),
#'   `PR` = summarize_testing_curve(
#'     fit_results,
#'     curve = "PR",
#'     nested_cols = "feature_info",
#'     truth_col = "true_support",
#'     pval_col = "pval"
#'   )
#' )
#'
#' # create summary ROC/PR plots using pre-computed evaluation results
#' roc_plt <- plot_testing_curve(eval_results = eval_results,
#'                               eval_name = "ROC", curve = "ROC",
#'                               show = c("line", "ribbon"))
#' pr_plt <- plot_testing_curve(eval_results = eval_results,
#'                              eval_name = "PR", curve = "PR",
#'                              show = c("line", "ribbon"))
#' # or alternatively, create the same plots directly from fit results
#' roc_plt <- plot_testing_curve(fit_results = fit_results,
#'                               show = c("line", "ribbon"),
#'                               curve = "ROC",
#'                               eval_fun_options = list(
#'                                 nested_cols = "feature_info",
#'                                 truth_col = "true_support",
#'                                 pval_col = "pval"
#'                               ))
#' pr_plt <- plot_testing_curve(fit_results = fit_results,
#'                              show = c("line", "ribbon"),
#'                              curve = "PR",
#'                              eval_fun_options = list(
#'                                nested_cols = "feature_info",
#'                                truth_col = "true_support",
#'                                pval_col = "pval"
#'                              ))
#'
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' roc_plt <- plot_testing_curve(eval_results = eval_results,
#'                               eval_name = "ROC", curve = "ROC",
#'                               show = c("line", "ribbon"),
#'                               plot_by = ".dgp_name")
#'
#' @export
plot_testing_curve <- function(fit_results = NULL,
                               eval_results = NULL, eval_name = NULL,
                               eval_fun = "summarize_testing_curve",
                               eval_fun_options = NULL,
                               vary_params = NULL, curve = c("ROC", "PR"),
                               show = c("line", "ribbon"), ...) {
  arg_list <- rlang::list2(...)
  plt <- do.call(
    plot_pred_curve,
    args = c(
      list(fit_results = fit_results,
           eval_results = eval_results, eval_name = eval_name,
           eval_fun = eval_fun, eval_fun_options = eval_fun_options,
           vary_params = vary_params, curve = curve, show = show),
      arg_list
    )
  )
  return(plt)
}

#' Plot the rejection probability of a hypothesis test.
#'
#' @description Plot the probability of rejecting the null hypothesis
#'   across various levels of significance.
#'
#' @inheritParams shared_viz_lib_args
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams eval_reject_prob
#' @param show_features Vector of feature names corresponding to features to
#'   display in the plot. If `NULL` (default), shows all features in the
#'   data.
#' @param show_identity_line Logical indicating whether or not to plot the
#'   y = x line.
#'
#' @inherit plot_eval_constructor return
#'
#' @family inference_funs
#'
#' @examples
#' # generate example fit_results data
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
#'         # estimated p-values
#'         pval = 10^(sample(-3:0, 3, replace = TRUE))
#'       )
#'     }
#'   )
#' )
#'
#' # generate example eval_results data
#' eval_results <- list(
#'   `Reject Prob.` = eval_reject_prob(
#'     fit_results,
#'     nested_cols = "feature_info",
#'     feature_col = "feature",
#'     pval_col = "pval"
#'   )
#' )
#'
#' # create bar plot using pre-computed evaluation results
#' plt <- plot_reject_prob(eval_results = eval_results,
#'                         eval_name = "Reject Prob.",
#'                         feature_col = "feature")
#' # or alternatively, create the same plot directly from fit results
#' plt <- plot_reject_prob(fit_results = fit_results,
#'                         feature_col = "feature",
#'                         eval_fun_options = list(
#'                           nested_cols = "feature_info",
#'                           pval_col = "pval"
#'                         ))
#'
#' # can customize plot (see plot_eval_constructor() for possible arguments)
#' plt <- plot_reject_prob(eval_results = eval_results,
#'                         eval_name = "Reject Prob.",
#'                         facet_formula = NULL,
#'                         plot_by = "feature")
#'
#' @importFrom rlang .data
#' @export
plot_reject_prob <- function(fit_results = NULL,
                             eval_results = NULL, eval_name = NULL,
                             eval_fun = "eval_reject_prob",
                             eval_fun_options = NULL,
                             vary_params = NULL,
                             feature_col = NULL, show_features = NULL,
                             show_identity_line = FALSE, show = c("line"),
                             ...) {
  .alpha <- NULL  # to fix no visible binding for global variable error
  show <- match.arg(show, choices = c("point", "line", "bar"))
  if (!is.null(feature_col)) {
    arg_list <- get_dot_args(
      user_args = rlang::list2(...),
      default_args = list(eval_id = "",
                          x_str = ".alpha",
                          y_str = "reject_prob",
                          facet_formula = substitute(
                            ~ .data[[feature_col]],
                            list(feature_col = feature_col)
                          ),
                          facet_type = "wrap")
    )
  } else {
    arg_list <- get_dot_args(
      user_args = rlang::list2(...),
      default_args = list(eval_id = "",
                          x_str = ".alpha",
                          y_str = "reject_prob")
    )
  }

  if (show_identity_line) {
    arg_list$add_ggplot_layers <- c(
      list(ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey")),
      arg_list$add_ggplot_layers
    )
  }

  plot_data <- get_plot_data(
    fit_results = fit_results,
    eval_results = eval_results,
    eval_name = eval_name,
    eval_fun = eval_fun,
    eval_fun_options = c(eval_fun_options, list(feature_col = feature_col))
  )

  if (identical(show, "bar")) {
    plot_data <- plot_data %>% dplyr::mutate(.alpha = as.factor(.alpha))
  }
  if (!is.null(show_features)) {
    plot_data <- plot_data %>%
      dplyr::filter(.data[[feature_col]] %in% show_features)
  }

  plt <- do.call(
    plot_eval_constructor,
    args = c(
      list(plot_data = plot_data, vary_params = vary_params, show = show),
      arg_list
    )
  )
  return(plt)
}
