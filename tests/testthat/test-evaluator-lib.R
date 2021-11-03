test_that("Functions in Evaluator prediction library work properly", {
  ## eval_pred_err / summarize_pred_err - regression
  # generate example fit_results data for a regression problem
  fit_results_reg <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    y = lapply(1:4, FUN = function(x) (1:100)),
    predictions = lapply(1:4, FUN = function(x) (1:100)*(x+.1))
  )

  eval_results <- eval_pred_err(fit_results_reg,
                                truth_col = "y",
                                estimate_col = "predictions")
  eval_results_summary <- summarize_pred_err(fit_results_reg,
                                             truth_col = "y",
                                             estimate_col = "predictions")
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  group_ids <- rep(c("a", "b"), length.out = 100)
  eval_results <- eval_pred_err(fit_results_reg,
                                truth_col = "y",
                                estimate_col = "predictions",
                                groups = group_ids)
  eval_results_summary <- summarize_pred_err(fit_results_reg,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             groups = group_ids)
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  metrics <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  eval_results <- eval_pred_err(fit_results_reg,
                                truth_col = "y",
                                estimate_col = "predictions",
                                metrics = metrics)
  eval_results_summary <- summarize_pred_err(fit_results_reg,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             metrics = metrics)
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  range_fun <- function(x) return(max(x) - min(x))
  eval_results_summary <- summarize_pred_err(
    fit_results_reg,
    truth_col = "y",
    estimate_col = "predictions",
    custom_summary_funs = list(range_pred_err = range_fun)
  )
  expect_snapshot_output(eval_results_summary)

  ## eval_pred_err / summarize_pred_err - binary classification
  # generate example fit_results data for a binary classification problem
  fit_results_bin <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    y = lapply(1:4, FUN = function(x) as.factor(rep(0:1, length.out = 100))),
    class_probs = lapply(1:4, FUN = function(x) 1:100 / 100),
    predictions = lapply(class_probs,
                         FUN = function(x) as.factor(ifelse(x > 0.5, 1, 0)))
  )

  eval_results <- eval_pred_err(fit_results_bin,
                                truth_col = "y",
                                estimate_col = "predictions",
                                prob_cols = "class_probs")
  eval_results_summary <- summarize_pred_err(fit_results_bin,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             prob_cols = "class_probs")
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  eval_results <- eval_pred_err(fit_results_bin,
                                truth_col = "y",
                                estimate_col = "predictions")
  eval_results_summary <- summarize_pred_err(fit_results_bin,
                                             truth_col = "y",
                                             estimate_col = "predictions")
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  ## eval_pred_err / summarize_pred_err - multi-class classification
  # generate example fit_results data for a multi-class classification problem
  fit_results_class <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    y = lapply(1:4,
               FUN = function(x) {
                 as.factor(rep(c("a", "b", "c"), length.out = 6))
               }),
    class_probs = lapply(1:4,
                         FUN = function(x) {
                           tibble::tibble(a = c(0.1, 0.5, 0.8, 0.2, 0.2, 0.3),
                                          b = c(0.4, 0.1, 0.1, 0.5, 0.5, 0.1),
                                          c = 1 - a - b)
                         }),
    predictions = lapply(class_probs,
                         FUN = function(x) {
                           yhat <- apply(x, 1,
                                         FUN = function(xi) names(which.max(xi))[1])
                           return(factor(yhat, levels = c("a", "b", "c")))
                         })
  )

  eval_results <- eval_pred_err(fit_results_class,
                                truth_col = "y",
                                estimate_col = "predictions",
                                prob_cols = c("a", "b", "c"),
                                nested_data = "class_probs")
  eval_results_summary <- summarize_pred_err(fit_results_class,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             prob_cols = c("a", "b", "c"),
                                             nested_data = "class_probs")
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  eval_results <- eval_pred_err(fit_results_class,
                                truth_col = "y",
                                estimate_col = "predictions")
  eval_results_summary <- summarize_pred_err(fit_results_class,
                                             truth_col = "y",
                                             estimate_col = "predictions")
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)
  
  ## eval_pred_curve / summarize_pred_curve - binary classification
  roc_results <- eval_pred_curve(fit_results_bin, curve = "ROC",
                                 truth_col = "y", prob_cols = "class_probs")
  pr_results <- eval_pred_curve(fit_results_bin, curve = "PR",
                                truth_col = "y", prob_cols = "class_probs")
  roc_summary <- summarize_pred_curve(fit_results_bin, curve = "ROC",
                                      truth_col = "y", prob_cols = "class_probs")
  pr_summary <- summarize_pred_curve(fit_results_bin, curve = "PR",
                                     truth_col = "y", prob_cols = "class_probs")
  expect_snapshot_output(roc_results)
  expect_snapshot_output(pr_results)
  expect_snapshot_output(roc_summary)
  expect_snapshot_output(pr_summary)

  ## eval_pred_curve / summarize_pred_curve - multi-class classification
  roc_results <- eval_pred_curve(fit_results_class, curve = "ROC",
                                 nested_data = "class_probs",
                                 truth_col = "y",
                                 prob_cols = c("a", "b", "c"))
  pr_results <- eval_pred_curve(fit_results_class, curve = "PR",
                                nested_data = "class_probs",
                                truth_col = "y",
                                prob_cols = c("a", "b", "c"))
  roc_summary <- summarize_pred_curve(fit_results_class, curve = "ROC",
                                      nested_data = "class_probs",
                                      truth_col = "y",
                                      prob_cols = c("a", "b", "c"))
  pr_summary <- summarize_pred_curve(fit_results_class, curve = "PR",
                                     nested_data = "class_probs",
                                     truth_col = "y",
                                     prob_cols = c("a", "b", "c"))
  expect_snapshot_output(roc_results)
  expect_snapshot_output(pr_results)
  expect_snapshot_output(roc_summary)
  expect_snapshot_output(pr_summary)
})

test_that("Functions in Evaluator feature selection library work properly", {
  # generate example fit_results data for a feature selection problem
  fit_results <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    feature_info = lapply(
      1:4,
      FUN = function(i) {
        tibble::tibble(
          feature = c("featureA", "featureB", "featureC"),
          true_support = c(TRUE, FALSE, TRUE),
          est_support = c(TRUE, FALSE, FALSE),
          est_importance = c(i, 1.5, 2.5)
        )
      }
    )
  )

  ## eval_feature_selection_err / summarize_feature_selection_err
  eval_results <- eval_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance"
  )
  eval_results_summary <- summarize_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance"
  )
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  metrics <- yardstick::metric_set(yardstick::sens, yardstick::spec)
  eval_results <- eval_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance",
    metrics = metrics
  )
  eval_results_summary <- summarize_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance",
    metrics = metrics
  )
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  range_fun <- function(x) return(max(x) - min(x))
  eval_results_summary <- summarize_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance",
    custom_summary_funs = list(range_feature_selection = range_fun)
  )
  expect_snapshot_output(eval_results_summary)
  
  ## eval_feature_selection_curve / summarize_feature_selection_curve
  # evaluate feature selection ROC/PR curves for each replicate
  roc_results <- eval_feature_selection_curve(
    fit_results,
    curve = "ROC",
    nested_data = "feature_info",
    truth_col = "true_support",
    imp_col = "est_importance"
  )
  pr_results <- eval_feature_selection_curve(
    fit_results,
    curve = "PR",
    nested_data = "feature_info",
    truth_col = "true_support",
    imp_col = "est_importance"
  )
  roc_summary <- summarize_feature_selection_curve(
    fit_results,
    curve = "ROC",
    nested_data = "feature_info",
    truth_col = "true_support",
    imp_col = "est_importance"
  )
  pr_summary <- summarize_feature_selection_curve(
    fit_results,
    curve = "PR",
    nested_data = "feature_info",
    truth_col = "true_support",
    imp_col = "est_importance"
  )
  expect_snapshot_output(roc_results)
  expect_snapshot_output(pr_results)
  expect_snapshot_output(roc_summary)
  expect_snapshot_output(pr_summary)
  
  ## eval_feature_importance / summarize_feature_importance
  eval_results <- eval_feature_importance(
    fit_results,
    nested_data = "feature_info",
    feature_col = "feature",
    imp_col = "est_importance"
  )
  eval_results_summary <- summarize_feature_importance(
    fit_results,
    nested_data = "feature_info",
    feature_col = "feature",
    imp_col = "est_importance"
  )
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)
})

test_that("Functions in Evaluator inference library work properly", {
  # generate example fit_results data for an inference problem
  fit_results <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    feature_info = lapply(
      1:4,
      FUN = function(i) {
        tibble::tibble(
          feature = c("featureA", "featureB", "featureC"),
          true_support = c(TRUE, FALSE, TRUE),
          pval = c(10^(-(i - 1)), 10^-1.5, 10^2.5)
        )
      }
    )
  )

  ## eval_testing_err / summarize_testing_err
  eval_results <- eval_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  metrics <- yardstick::metric_set(yardstick::sens, yardstick::spec)
  eval_results <- eval_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    metrics = metrics
  )
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    metrics = metrics
  )
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  eval_results <- eval_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    alphas = c(0.05, 0.1)
  )
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    alphas = c(0.05, 0.1)
  )
  expect_snapshot_output(eval_results)
  expect_snapshot_output(eval_results_summary)

  range_fun <- function(x) return(max(x) - min(x))
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    custom_summary_funs = list(range_testing_err = range_fun)
  )
  expect_snapshot_output(eval_results_summary)
  
  ## eval_testing_curve / summarize_testing_curve
  roc_results <- eval_testing_curve(
    fit_results,
    curve = "ROC",
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  pr_results <- eval_testing_curve(
    fit_results,
    curve = "PR",
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  roc_summary <- summarize_testing_curve(
    fit_results,
    curve = "ROC",
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  pr_summary <- summarize_testing_curve(
    fit_results,
    curve = "PR",
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  expect_snapshot_output(roc_results)
  expect_snapshot_output(pr_results)
  expect_snapshot_output(roc_summary)
  expect_snapshot_output(pr_summary)
  
  ## eval_reject_prob / summarize_reject_prob
  eval_results <- eval_reject_prob(
    fit_results,
    nested_data = "feature_info",
    feature_col = "feature",
    pval_col = "pval"
  )
  expect_snapshot_output(eval_results)

  eval_results <- eval_reject_prob(
    fit_results,
    nested_data = "feature_info",
    feature_col = "feature",
    pval_col = "pval",
    alphas = c(0.05, 0.1)
  )
  expect_snapshot_output(eval_results)
})

test_that("Functions in Evaluator utilities library work properly", {
  # create example eval_data to summarize
  eval_data <- tibble::tibble(.rep = rep(1:2, times = 2),
                              .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
                              .method_name = "Method",
                              result = 1:4) %>%
    dplyr::group_by(.dgp_name, .method_name)

  ## summarize_eval_results
  results <- summarize_eval_results(eval_data = eval_data, eval_id = "res",
                                    value_col = "result")
  expect_equal(dplyr::group_keys(results), 
               tibble::tibble(.dgp_name = c("DGP1", "DGP2"),
                              .method_name = "Method"))
  expect_snapshot_output(results)
  
  results <- summarize_eval_results(eval_data = eval_data, eval_id = "res",
                                    value_col = "result",
                                    summary_funs = c("mean", "sd"))
  expect_equal(dplyr::group_keys(results), 
               tibble::tibble(.dgp_name = c("DGP1", "DGP2"),
                              .method_name = "Method"))
  expect_snapshot_output(results)

  range_fun <- function(x) return(max(x) - min(x))
  results <- summarize_eval_results(eval_data = eval_data, value_col = "result",
                                    custom_summary_funs = list(range = range_fun))
  expect_equal(dplyr::group_keys(results), 
               tibble::tibble(.dgp_name = c("DGP1", "DGP2"),
                              .method_name = "Method"))
  expect_snapshot_output(results)
})
