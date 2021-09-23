#' Evaluate prediction error.
#' 
#' @description Evaluate various prediction error metrics, given the observed 
#'   values \code{y} and the predicted values \code{yhat}.
#'   
#' @param y Vector, matrix, or data.frame of the true/observed response values.
#' @param yhat Vector, matrix, or data.frame of the predicted response values. 
#'   Must be of the same dimension as \code{y}.
#' @param metrics Character vector of the prediction error metrics to compute.
#'   Elements of the vector must be one of "RMSE", "MSE", "R2", "MAE",
#'   "Corr", "ClassErr", "BalancedClassErr", "AUROC", "AUPRC".
#' @param custom_metrics Named list of custom metric functions to compute. Names
#'   in the list should correspond to the name of the metric. Values in the
#'   list should be a function that takes in the arguments \code{y} and 
#'   \code{yhat} only and returns the evaluated metric value.
#' @param group (Optional) vector of group ids (as a factor) to use for
#'   assessing within-group prediction errors.
#' @param na.rm Logical. Should missing values be removed?
#' 
#' @return A (long-shaped) data frame with the following columns:
#' \describe{
#' \item{group}{Name of group for within-group prediction errors. Note that the 
#'   group id "all" is used to denote the full data set without any grouping. 
#'   Column is only present if \code{group} is not \code{NULL}.}
#' \item{metric}{Name of prediction error metric.}
#' \item{value}{Prediction error value for the given group and metric.}
#' }
#' 
#' @details TODO: explain different metrics and which are used for regression 
#'   vs. binary vs. classification.
#' @examples
#' eval_out <- eval_pred_err(y = rnorm(10), yhat = rnorm(10),
#'                           metrics = c("RMSE", "MSE", "R2", "MAE", "Corr"))
#' eval_out <- eval_pred_err(y = rep(0:1, length.out = 10), yhat = runif(10),
#'                           metrics = c("AUROC", "AUPRC"))
#' eval_out <- eval_pred_err(y = rep(c("a", "b"), length.out = 10),
#'                           yhat = runif(10), metrics = c("AUROC", "AUPRC"))
#' eval_out <- eval_pred_err(y = rep(c("a", "b"), length.out = 10),
#'                           yhat = rep(c("a", "b"), length.out = 10), 
#'                           metrics = c("BalancedClassErr", "ClassErr"))
#' eval_out <- eval_pred_err(y = rnorm(10), yhat = rnorm(10),
#'                           metrics = c("RMSE", "MSE", "R2", "MAE", "Corr"),
#'                           custom_metrics = list(
#'                             MSE2 = function(y, yhat) {mean((y - yhat)^2)},
#'                             MAE2 = function(y, yhat) {mean(abs(y - yhat))}
#'                           ))
#' @export
eval_pred_err <- function(y, yhat, metrics = NULL, custom_metrics = NULL,
                          group = NULL, na.rm = F) {
  # error checking
  metrics <- match.arg(metrics, 
                       choices = c("RMSE", "MSE", "R2", "MAE", "Corr", 
                                   "ClassErr", "BalancedClassErr", 
                                   "AUROC", "AUPRC"), 
                       several.ok = TRUE)
  isvec <- is.null(dim(y))
  if ((isvec & (length(y) != length(yhat))) |
      (!isvec & any(dim(y) != dim(yhat)))) {
    stop("y and yhat must be the same size.")
  }
  if (("AUROC" %in% metrics) | ("AUPRC" %in% metrics)) {
    if (length(unique(y)) != 2) {
      stop("y must be binary to evaluate AUC and PR metrics.")
    }
    ylevels <- levels(as.factor(y))
    Y0 <- ylevels[1]
    Y1 <- ylevels[2]
  }
  if (("ClassErr" %in% metrics) | ("BalancedClassErr" %in% metrics)) {
    if (length(unique(y)) == length(y)) {
      warning("ClassErr and BalancedClassErr are metrics for classification problems, ",
              "but the response y does not look like a classification outcome vector. ",
              "Please double check that the choice of metric is correct.")
    }
    if ((length(unique(yhat)) > length(unique(y))) && 
        (is.numeric(yhat) && !is.integer(yhat))) {
      warning("The predicted responses, yhat, take on more values than the observed ",
              "response, y. ClassErr and BalancedClassErr checks for equality between ",
              "y and yhat. Please make sure that yhat contains the class responses ",
              "and not the predicted probabilities.")
    }
  }
  
  # create (long) grouped prediction data frame with groups, y, and yhat
  if (isvec) {
    pred_df <- data.frame(group = "all", y = y, yhat = yhat)
    if (!is.null(group)) {
      pred_df <- rbind(pred_df, data.frame(group = group, y = y, yhat = yhat))
    }
    pred_df <- pred_df %>%
      dplyr::group_by(group)
  } else {
    y_long <- data.frame(id = 1:nrow(y), group = "all", y) %>%
      tidyr::gather(key = "column", value = "y", -group, -id)
    yhat_long <- data.frame(id = 1:nrow(yhat), group = "all", yhat) %>%
      tidyr::gather(key = "column", value = "yhat", -group, -id)
    if (!is.null(group)) {
      y_long <- rbind(
        y_long,
        data.frame(id = 1:nrow(y), group = group, y) %>%
          tidyr::gather(key = "column", value = "y", -group, -id)
      )
      yhat_long <- rbind(
        yhat_long,
        data.frame(id = 1:nrow(yhat), group = group, yhat) %>%
          tidyr::gather(key = "column", value = "yhat", -group, -id)
      )
    }
    pred_df <- dplyr::left_join(y_long, yhat_long, 
                                by = c("id", "group", "column")) %>%
      dplyr::group_by(group, column)
  }
  
  # compute error metrics between y and yhat
  err_out <- NULL
  for (m in metrics) {
    if (m == "RMSE") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = sqrt(mean((y - yhat)^2, na.rm = na.rm)))
    } else if (m == "MSE") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = mean((y - yhat)^2, na.rm = na.rm))
    } else if (m == "R2") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = 1 - mean((y - yhat)^2, na.rm = na.rm) / 
                           mean((y - mean(y))^2, na.rm = na.rm))
    } else if (m == "MAE") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = mean(abs(y - yhat), na.rm = na.rm))
    } else if (m == "Corr") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = cor(y, yhat, use = "pairwise.complete.obs"))
    } else if (m == "ClassErr") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = mean(y == yhat, na.rm = na.rm))
    } else if (m == "BalancedClassErr") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = mean(sapply(unique(y),
                                             function(y0) {
                                               mean(y[y == y0] == yhat[y == y0], 
                                                    na.rm = na.rm)
                                             }),
                                      na.rm = na.rm))
    } else if (m == "AUROC") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = PRROC::roc.curve(yhat[(y == Y1) & !(is.na(y))],
                                                  yhat[(y == Y0) & !(is.na(y))],
                                                  curve = F)$auc)
    } else if (m == "AUPRC") {
      err <- pred_df %>%
        dplyr::summarise(metric = m,
                         value = PRROC::pr.curve(yhat[(y == Y1) & !(is.na(y))],
                                                 yhat[(y == Y0) & !(is.na(y))],
                                                 curve = F)$auc.integral)
    }
    err_out <- rbind(err_out, err)
  }
  
  if (!is.null(custom_metrics)) {
    if (is.null(names(custom_metrics))) {
      names(custom_metrics) <- paste0("pred_err_metric",
                                      1:length(custom_metrics))
    }
    custom_err_out <- purrr::map_dfr(
      custom_metrics,
      function(metric_fun) {
        pred_df %>% dplyr::summarise(value = metric_fun(y = y, yhat = yhat))
      },
      .id = "metric"
    )
    err_out <- dplyr::bind_rows(err_out, custom_err_out)
  }
  
  # clean up output formatting
  if (!isvec) {
    err_out <- err_out %>%
      tidyr::spread(key = "column", value = "value") %>%
      dplyr::select(group, metric, colnames(data.frame(y)))
  }
  if (is.null(group)) {
    err_out <- err_out %>% 
      dplyr::ungroup() %>%
      dplyr::select(-group)
  }
  
  return(err_out)
}

#' Evaluate ROC or PR curves.
#' 
#' @description Compute the ROC or PR curves and its corresponding AUC value for
#'   the given observed and predicted response values.
#'   
#' @param y Vector of the true/observed response values.
#' @param yhat Vector of the predicted response values. Must be of the same 
#'   length as \code{y}.
#' @param metric A character string. Either "ROC" or "PR".
#' 
#' @return A list of two:
#' \describe{
#' \item{AUC}{Area under the curve.}
#' \item{curve_df}{Data frame with the x and y values to plot curve. If 
#'   \code{metric = "ROC"}, the data frame columns give the true positive rate 
#'   (TPR) and the false positive rate (FPR). If \code{metric = "PR"}, the data
#'   frame columns give the Precision and Recall.}
#' }
#' 
#' @export
eval_auc_curve <- function(y, yhat, metric = c("ROC", "PR")) {
  metric <- match.arg(metric)
  if (length(y) != length(yhat)) {
    stop("Length of y must be equal to length of yhat.")
  } else if (length(unique(y)) != 2) {
    stop("y must be binary to evaluate AUC and PR metrics.")
  }
  
  ylevels <- levels(as.factor(y))
  Y0 <- ylevels[1]
  Y1 <- ylevels[2]
  
  if (all(yhat == yhat[1])) {
    warning("Predictions are all the same. Returning NULL.")
    return(NULL)
  } else {
    if (metric == "ROC") {
      out <- PRROC::roc.curve(yhat[(y == Y1) & !is.na(y)], 
                              yhat[(y == Y0) & !is.na(y)], 
                              curve = T)
      return(list(AUC = out$auc, 
                  curve_df = data.frame(FPR = out$curve[, 1],
                                        TPR = out$curve[, 2])))
    } else if (metric == "PR") {
      out <- PRROC::pr.curve(yhat[(y == Y1) & !(is.na(y))],
                             yhat[(y == Y0) & !(is.na(y))],
                             curve = T)
      return(list(AUC = out$auc.integral,
                  curve_df = data.frame(Recall = out$curve[, 1],
                                        Precision = out$curve[, 2])))
    }
  }
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
#' @param y Character string. Name of column in \code{fit_results} with the
#'   observed response values.
#' @param yhat Character string. Name of column in \code{fit_results} with the
#'   predicted response values.
#' @inheritParams eval_pred_err
#' @inheritParams summarize_eval_results
#' 
#' @return A data frame with the following columns in addition to any arguments
#'   in the \code{Experiment}'s \code{vary_params}:
#' \describe{
#' \item{dgp_name}{Name of DGP.}
#' \item{method_name}{Name of Method.}
#' \item{group}{Name of group for within-group prediction errors. Note that the 
#'   group id "all" is used to denote the full data set without any grouping. 
#'   Column is only present if \code{group} is not \code{NULL}.}
#' \item{metric}{Name of prediction error metric.}
#' \item{...}{Other columns corresponding to the results from the provided 
#'   \code{summary_funs} and \code{custom_summary_funs} functions.}
#' }
#' 
#' @export
summarize_pred_err <- function(fit_results, vary_params = NULL, 
                               y = "y", yhat = "yhat",
                               metrics = NULL, custom_metrics = NULL,
                               summary_funs = c("mean", "median", "min", "max",
                                                "sd", "raw"),
                               custom_summary_funs = NULL,
                               group = NULL, na.rm = F) {
  metrics <- match.arg(metrics, 
                       choices = c("RMSE", "MSE", "R2", "MAE", "Corr", 
                                   "ClassErr", "BalancedClassErr", 
                                   "AUROC", "AUPRC"), 
                       several.ok = TRUE)
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  
  if (!is.null(group)) {
    group_vars <- c("dgp_name", "method_name", vary_params, "group", "metric")
  } else {
    group_vars <- c("dgp_name", "method_name", vary_params, "metric")
  }
  eval_results <- fit_results %>%
    dplyr::mutate(
      eval_out = purrr::map2(.data[[y]], .data[[yhat]], 
                             ~eval_pred_err(y = ..1, yhat = ..2, 
                                            metrics = metrics, 
                                            custom_metrics = custom_metrics, 
                                            group = group, na.rm = na.rm))
    ) %>%
    tidyr::unnest(eval_out) %>%
    dplyr::group_by(dplyr::across({{group_vars}})) 
  
  eval_summary <- summarize_eval_results(
    eval_results, id = "pred_err",
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na.rm = na.rm
  )
  return(eval_summary)
}

#' Summarize ROC/PR curves.
#' 
#' @description Summarize ROC/PR curves across experimental repetitions.
#' 
#' @param fit_results A tibble, as returned by the \code{Experiment$fit()} 
#'   method.
#' @param vary_params A vector of parameter names that are varied over in the 
#'   Experiment.
#' @param y Character string. Name of column in \code{fit_results} with the
#'   observed response values.
#' @param yhat Character string. Name of column in \code{fit_results} with the
#'   predicted response values.
#' @param grid Sequence of x-values to evaluate ROC or PR curve.
#' @inheritParams eval_auc_curve
#' @inheritParams summarize_eval_results
#' 
#' @return A data frame with the following columns in addition to any arguments
#'   in the \code{Experiment}'s \code{vary_params}:
#' \describe{
#' \item{dgp_name}{Name of DGP.}
#' \item{method_name}{Name of Method.}
#' \item{...}{Other columns corresponding to the results from the provided 
#'   \code{summary_funs} and \code{custom_summary_funs} functions.}
#' }
#' 
#' @export
summarize_auc_curve <- function(fit_results, vary_params = NULL, 
                                y = "y", yhat = "yhat", metric = c("ROC", "PR"),
                                x_grid = seq(0, 1, by = 1e-2),
                                summary_funs = c("mean", "median", "min", "max",
                                                 "sd", "raw"),
                                custom_summary_funs = NULL,
                                na.rm = F) {
  # TODO: test this function if eval_auc_curve returns NULL
  metric <- match.arg(metric)
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  if (metric == "PR") {
    xvar <- "Recall"
    yvar <- "Precision"
  } else if (metric == "ROC") {
    xvar <- "FPR"
    yvar <- "TPR"
  }
  group_vars <- c("dgp_name", "method_name", vary_params, xvar)
  
  eval_results <- fit_results %>%
    dplyr::mutate(
      eval_out = purrr::map2(.data[[y]], .data[[yhat]],
                             ~eval_auc_curve(y = ..1, yhat = ..2,
                                             metric = metric))
    ) %>%
    tidyr::unnest_wider(eval_out) %>%
    # transform auc curves to have same x scale
    dplyr::mutate(
      curve_df = purrr::map(
        curve_df,
        function(df) {
          data.frame(
            .x_auc = x_grid, 
            .y_auc = purrr::map_dbl(x_grid, 
                                    ~max(df[df[[xvar]] <= .x, yvar]))
          ) %>%
            setNames(c(xvar, yvar))
        }
      )
    ) %>%
    tidyr::unnest(curve_df) %>%
    dplyr::group_by(dplyr::across({{group_vars}}))
  
  eval_summary <- summarize_eval_results(
    eval_results, id = yvar, value = yvar,
    summary_funs = summary_funs,
    custom_summary_funs = custom_summary_funs,
    na.rm = na.rm
  )
  return(eval_summary)
}
