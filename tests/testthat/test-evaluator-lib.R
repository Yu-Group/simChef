test_that("Functions in Evaluator prediction library work properly", {
  ## eval_pred_err / summarize_pred_err - regression
  # generate example fit_results data for a regression problem
  reps <- 1:2
  dgps <- c("DGP1", "DGP2")
  methods <- "Method"
  metrics <- c("rmse", "rsq", "mae")
  metrics_sorted <- sort(metrics)
  groups <- c("a", "b")

  fit_results_reg <- tibble::tibble(
    .rep = rep(reps, times = 2),
    .dgp_name = rep(dgps, each = 2),
    .method_name = methods,
    y = lapply(1:4, FUN = function(x) (1:100)),
    predictions = lapply(1:4, FUN = function(x) (1:100)*(x+.1)),
    .group = lapply(1:4, FUN = function(x) rep(groups, length.out = 100))
  )

  # test eval_pred_err and summarize_pred_err
  eval_results <- eval_pred_err(fit_results_reg,
                                truth_col = "y",
                                estimate_col = "predictions")
  eval_results_orig <- eval_results
  eval_results_summary <- summarize_pred_err(fit_results_reg,
                                             truth_col = "y",
                                             estimate_col = "predictions")
  eval_results_summary_orig <- eval_results_summary

  expect_true(tibble::is_tibble(eval_results))
  expect_equal(dim(eval_results), c(12, 5))
  expect_equal(unique(eval_results$.rep), reps)
  expect_equal(unique(eval_results$.dgp_name), dgps)
  expect_equal(unique(eval_results$.method_name), methods)
  expect_equal(unique(eval_results$.metric), metrics)
  expect_equal(
    eval_results %>% dplyr::filter(.metric == "rsq"),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods,
      .metric = "rsq",
      .estimate = 1
    )
  )

  expect_true(tibble::is_tibble(eval_results_summary))
  expect_equal(dim(eval_results_summary), c(6, 9))
  expect_equal(
    colnames(eval_results_summary),
    c(".dgp_name", ".method_name", ".metric",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_pred_err"))
  )
  expect_equal(unique(eval_results_summary$.dgp_name), dgps)
  expect_equal(unique(eval_results_summary$.method_name), methods)
  expect_equal(unique(eval_results_summary$.metric), metrics_sorted)
  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 3),
      .method_name = methods,
      .metric = rep(metrics_sorted, times = 2)
    )
  )
  expect_equal(
    eval_results_summary %>% dplyr::filter(.metric == "rsq") %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      .metric = "rsq",
      mean_pred_err = 1,
      median_pred_err = 1,
      min_pred_err = 1,
      max_pred_err = 1,
      sd_pred_err = 0,
      raw_pred_err = list(c(1, 1), c(1, 1))
    )
  )

  # test eval_pred_err and summarize_pred_err with group argument
  eval_results <- eval_pred_err(fit_results_reg,
                                truth_col = "y",
                                estimate_col = "predictions",
                                group_cols = ".group")
  eval_results_summary <- summarize_pred_err(fit_results_reg,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             group_cols = ".group")

  expect_true(tibble::is_tibble(eval_results))
  expect_equal(dim(eval_results), c(24, 6))
  expect_equal(unique(eval_results$.rep), reps)
  expect_equal(unique(eval_results$.dgp_name), dgps)
  expect_equal(unique(eval_results$.method_name), methods)
  expect_equal(unique(eval_results$.metric), metrics)
  expect_equal(unique(eval_results$.group), groups)
  expect_equal(
    eval_results %>% dplyr::filter(.metric == "rsq"),
    tibble::tibble(
      .rep = rep(rep(reps, each = 2), times = 2),
      .dgp_name = rep(dgps, each = 4),
      .method_name = methods,
      .group = rep(groups, times = 4),
      .metric = "rsq",
      .estimate = 1
    )
  )

  expect_true(tibble::is_tibble(eval_results_summary))
  expect_equal(dim(eval_results_summary), c(12, 10))
  expect_equal(
    colnames(eval_results_summary),
    c(".dgp_name", ".method_name", ".group", ".metric",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_pred_err"))
  )
  expect_equal(unique(eval_results_summary$.dgp_name), dgps)
  expect_equal(unique(eval_results_summary$.method_name), methods)
  expect_equal(unique(eval_results_summary$.metric), metrics_sorted)
  expect_equal(unique(eval_results$.group), groups)
  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 6),
      .method_name = methods,
      .group = rep(rep(groups, each = 3), times = 2),
      .metric = rep(metrics_sorted, times = 4)
    )
  )
  expect_equal(
    eval_results_summary %>% dplyr::filter(.metric == "rsq") %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods,
      .group = rep(groups, times = 2),
      .metric = "rsq",
      mean_pred_err = 1,
      median_pred_err = 1,
      min_pred_err = 1,
      max_pred_err = 1,
      sd_pred_err = 0,
      raw_pred_err = rep(list(c(1, 1)), 4)
    )
  )

  # test eval_pred_err and summarize_pred_err with metrics argument
  metric_funs <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  metrics <- c("rmse", "rsq")
  eval_results <- eval_pred_err(fit_results_reg,
                                truth_col = "y",
                                estimate_col = "predictions",
                                metrics = metric_funs)
  eval_results_summary <- summarize_pred_err(fit_results_reg,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             metrics = metric_funs)

  expect_equal(
    eval_results,
    eval_results_orig %>% dplyr::filter(.metric %in% metrics)
  )

  expect_equal(
    eval_results_summary,
    eval_results_summary_orig %>% dplyr::filter(.metric %in% metrics)
  )

  # test summarize_pred_err with custom_summary_funs
  range_fun <- function(x) return(max(x) - min(x))
  eval_results_summary <- summarize_pred_err(
    fit_results_reg,
    truth_col = "y",
    estimate_col = "predictions",
    custom_summary_funs = list(range_pred_err = range_fun)
  )
  expect_equal(
    dim(eval_results_summary),
    c(nrow(eval_results_summary_orig), ncol(eval_results_summary_orig) + 1)
  )
  expect_equal(eval_results_summary %>% dplyr::select(-range_pred_err),
               eval_results_summary_orig)
  expect_equal(round(eval_results_summary$range_pred_err, 2),
               rep(c(50.5, 58.17, 0), times = 2))

  ## eval_pred_err / summarize_pred_err - binary classification
  # generate example fit_results data for a binary classification problem
  metrics <- c("accuracy", "kap", "mn_log_loss", "roc_auc")
  metrics_sorted <- sort(metrics)
  fit_results_bin <- tibble::tibble(
    .rep = rep(reps, times = 2),
    .dgp_name = rep(dgps, each = 2),
    .method_name = methods,
    y = lapply(1:4, FUN = function(x) as.factor(rep(0:1, length.out = 100))),
    class_probs = lapply(1:4, FUN = function(x) 1:100 / 100),
    predictions = lapply(class_probs,
                         FUN = function(x) as.factor(ifelse(x > 0.5, 1, 0)))
  )

  eval_results <- eval_pred_err(fit_results_bin,
                                truth_col = "y",
                                estimate_col = "predictions",
                                prob_cols = "class_probs")
  eval_results_orig <- eval_results
  eval_results_summary <- summarize_pred_err(fit_results_bin,
                                             truth_col = "y",
                                             estimate_col = "predictions",
                                             prob_cols = "class_probs")
  eval_results_summary_orig <- eval_results_summary

  expect_true(tibble::is_tibble(eval_results))
  expect_equal(dim(eval_results), c(16, 5))
  expect_equal(unique(eval_results$.rep), reps)
  expect_equal(unique(eval_results$.dgp_name), dgps)
  expect_equal(unique(eval_results$.method_name), methods)
  expect_equal(unique(eval_results$.metric), metrics)
  expect_equal(
    eval_results %>% dplyr::filter(.metric == "roc_auc"),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods,
      .metric = "roc_auc",
      .estimate = 0.49
    )
  )

  expect_true(tibble::is_tibble(eval_results_summary))
  expect_equal(dim(eval_results_summary), c(8, 9))
  expect_equal(
    colnames(eval_results_summary),
    c(".dgp_name", ".method_name", ".metric",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_pred_err"))
  )
  expect_equal(unique(eval_results_summary$.dgp_name), dgps)
  expect_equal(unique(eval_results_summary$.method_name), methods)
  expect_equal(unique(eval_results_summary$.metric), metrics_sorted)
  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 4),
      .method_name = methods,
      .metric = rep(metrics_sorted, times = 2)
    )
  )
  expect_equal(
    eval_results_summary %>% dplyr::filter(.metric == "roc_auc") %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      .metric = "roc_auc",
      mean_pred_err = 0.49,
      median_pred_err = 0.49,
      min_pred_err = 0.49,
      max_pred_err = 0.49,
      sd_pred_err = 0,
      raw_pred_err = rep(list(c(0.49, 0.49)), 2)
    )
  )

  eval_results <- eval_pred_err(fit_results_bin,
                                truth_col = "y",
                                estimate_col = "predictions")
  eval_results_summary <- summarize_pred_err(fit_results_bin,
                                             truth_col = "y",
                                             estimate_col = "predictions")

  expect_equal(
    eval_results,
    eval_results_orig %>% dplyr::filter(.metric %in% c("accuracy", "kap"))
  )

  expect_equal(
    eval_results_summary,
    eval_results_summary_orig %>% dplyr::filter(.metric %in% c("accuracy", "kap"))
  )

  ## eval_pred_err / summarize_pred_err - multi-class classification
  # generate example fit_results data for a multi-class classification problem
  fit_results_class <- tibble::tibble(
    .rep = rep(reps, times = 2),
    .dgp_name = rep(dgps, each = 2),
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

  expect_true(tibble::is_tibble(eval_results))
  expect_equal(dim(eval_results), dim(eval_results_orig))
  expect_equal(eval_results %>% dplyr::select(-.estimate),
               eval_results_orig %>% dplyr::select(-.estimate))
  expect_equal(
    eval_results %>% dplyr::filter(.metric == "roc_auc"),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods,
      .metric = "roc_auc",
      .estimate = 0.5
    )
  )

  expect_true(tibble::is_tibble(eval_results_summary))
  expect_equal(dim(eval_results_summary), dim(eval_results_summary_orig))
  expect_equal(
    colnames(eval_results_summary),
    colnames(eval_results_summary_orig)
  )
  expect_equal(
    eval_results_summary %>% dplyr::select(.dgp_name, .method_name, .metric),
    eval_results_summary_orig %>% dplyr::select(.dgp_name, .method_name, .metric)
  )

  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    eval_results_summary_orig %>% dplyr::group_keys()
  )
  expect_equal(
    eval_results_summary %>% dplyr::filter(.metric == "roc_auc") %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      .metric = "roc_auc",
      mean_pred_err = 0.5,
      median_pred_err = 0.5,
      min_pred_err = 0.5,
      max_pred_err = 0.5,
      sd_pred_err = 0,
      raw_pred_err = rep(list(c(0.5, 0.5)), 2)
    )
  )

  eval_results_orig <- eval_results
  eval_results_summary_orig <- eval_results_summary

  eval_results <- eval_pred_err(fit_results_class,
                                truth_col = "y",
                                estimate_col = "predictions")
  eval_results_summary <- summarize_pred_err(fit_results_class,
                                             truth_col = "y",
                                             estimate_col = "predictions")
  expect_equal(
    eval_results,
    eval_results_orig %>% dplyr::filter(.metric %in% c("accuracy", "kap"))
  )

  expect_equal(
    eval_results_summary,
    eval_results_summary_orig %>% dplyr::filter(.metric %in% c("accuracy", "kap"))
  )

  ## eval_pred_curve / summarize_pred_curve - binary classification
  roc_results <- eval_pred_curve(fit_results_bin, curve = "ROC",
                                 truth_col = "y", prob_cols = "class_probs")
  pr_results <- eval_pred_curve(fit_results_bin, curve = "PR",
                                truth_col = "y", prob_cols = "class_probs")
  roc_summary <- summarize_pred_curve(fit_results_bin, curve = "ROC",
                                      truth_col = "y", prob_cols = "class_probs")
  pr_summary <- summarize_pred_curve(fit_results_bin, curve = "PR",
                                     truth_col = "y", prob_cols = "class_probs")

  expect_true(tibble::is_tibble(roc_results))
  expect_equal(dim(roc_results), c(4, 4))
  expect_equal(
    roc_results %>% dplyr::select(-curve_estimate),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods
    )
  )
  expect_equal(dim(roc_results$curve_estimate[[1]]), c(102, 3))
  expect_equal(colnames(roc_results$curve_estimate[[1]]),
               c(".threshold", "FPR", "TPR"))

  expect_true(tibble::is_tibble(pr_results))
  expect_equal(dim(pr_results), c(4, 4))
  expect_equal(
    pr_results %>% dplyr::select(-curve_estimate),
    roc_results %>% dplyr::select(-curve_estimate)
  )
  expect_equal(dim(pr_results$curve_estimate[[1]]), c(101, 3))
  expect_equal(colnames(pr_results$curve_estimate[[1]]),
               c(".threshold", "recall", "precision"))

  expect_true(tibble::is_tibble(roc_summary))
  expect_equal(dim(roc_summary), c(202, 9))
  expect_equal(
    colnames(roc_summary),
    c(".dgp_name", ".method_name", "FPR",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_TPR"))
  )
  expect_equal(unique(roc_summary$.dgp_name), dgps)
  expect_equal(unique(roc_summary$.method_name), methods)
  expect_equal(unique(roc_summary$FPR), seq(0, 1, by = 0.01))
  expect_equal(
    roc_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 101),
      .method_name = methods,
      FPR = rep(seq(0, 1, by = 0.01), times = 2)
    )
  )
  expect_equal(
    roc_summary[1, ] %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps[1],
      .method_name = methods,
      FPR = 0,
      mean_TPR = 0,
      median_TPR = 0,
      min_TPR = 0,
      max_TPR = 0,
      sd_TPR = 0,
      raw_TPR = list(c(0, 0))
    )
  )

  expect_true(tibble::is_tibble(pr_summary))
  expect_equal(dim(roc_summary), dim(pr_summary))
  expect_equal(
    colnames(pr_summary),
    c(".dgp_name", ".method_name", "recall",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_precision"))
  )
  expect_equal(unique(pr_summary$.dgp_name), dgps)
  expect_equal(unique(pr_summary$.method_name), methods)
  expect_equal(unique(pr_summary$recall), seq(0, 1, by = 0.01))
  expect_equal(
    pr_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 101),
      .method_name = methods,
      recall = rep(seq(0, 1, by = 0.01), times = 2)
    )
  )
  expect_equal(
    pr_summary[1, ] %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps[1],
      .method_name = methods,
      recall = 0,
      mean_precision = 1,
      median_precision = 1,
      min_precision = 1,
      max_precision = 1,
      sd_precision = 0,
      raw_precision = list(c(1, 1))
    )
  )
  roc_results_orig <- roc_results
  pr_results_orig <- pr_results
  roc_summary_orig <- roc_summary
  pr_summary_orig <- pr_summary

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

  expect_true(tibble::is_tibble(roc_results))
  expect_equal(dim(roc_results), dim(roc_results_orig))
  expect_equal(
    roc_results %>% dplyr::select(-curve_estimate),
    roc_results_orig %>% dplyr::select(-curve_estimate)
  )
  expect_equal(dim(roc_results$curve_estimate[[1]]), c(19, 4))
  expect_equal(colnames(roc_results$curve_estimate[[1]]),
               c(".level", ".threshold", "FPR", "TPR"))

  expect_true(tibble::is_tibble(pr_results))
  expect_equal(dim(pr_results), dim(pr_results_orig))
  expect_equal(
    pr_results %>% dplyr::select(-curve_estimate),
    roc_results %>% dplyr::select(-curve_estimate)
  )
  expect_equal(dim(pr_results$curve_estimate[[1]]), c(16, 4))
  expect_equal(colnames(pr_results$curve_estimate[[1]]),
               c(".level", ".threshold", "recall", "precision"))

  expect_true(tibble::is_tibble(roc_summary))
  expect_equal(dim(roc_summary), c(202, 9))
  expect_equal(colnames(roc_summary), colnames(roc_summary_orig))
  expect_equal(
    roc_summary %>% dplyr::select(.dgp_name, .method_name, FPR),
    roc_summary_orig %>% dplyr::select(.dgp_name, .method_name, FPR)
  )
  expect_equal(
    roc_summary %>% dplyr::group_keys(),
    roc_summary_orig %>% dplyr::group_keys()
  )
  expect_equal(
    roc_summary[1, ] %>% dplyr::ungroup(),
    roc_summary_orig[1, ] %>% dplyr::ungroup()
  )

  expect_true(tibble::is_tibble(pr_summary))
  expect_equal(dim(roc_summary), dim(pr_summary))
  expect_equal(colnames(pr_summary), colnames(pr_summary_orig))
  expect_equal(
    pr_summary %>% dplyr::select(.dgp_name, .method_name, recall),
    pr_summary_orig %>% dplyr::select(.dgp_name, .method_name, recall)
  )
  expect_equal(
    pr_summary %>% dplyr::group_keys(),
    pr_summary_orig %>% dplyr::group_keys()
  )
  expect_equal(
    pr_summary[1, ] %>% dplyr::ungroup(),
    pr_summary_orig[1, ] %>% dplyr::ungroup()
  )
})

test_that("Functions in Evaluator feature selection library work properly", {
  reps <- 1:2
  dgps <- c("DGP1", "DGP2")
  methods <- "Method"
  features <- c("featureA", "featureB", "featureC")
  metrics <- c("tp", "fp", "sens", "spec", "ppv", "pos", "neg", "roc_auc", "pr_auc")
  metrics_sorted <- sort(metrics)

  # generate example fit_results data for a feature selection problem
  fit_results <- tibble::tibble(
    .rep = rep(reps, times = 2),
    .dgp_name = rep(dgps, each = 2),
    .method_name = methods,
    feature_info = lapply(
      1:4,
      FUN = function(i) {
        tibble::tibble(
          feature = features,
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
  eval_results_orig <- eval_results
  eval_results_summary <- summarize_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance"
  )
  eval_results_summary_orig <- eval_results_summary

  expect_true(tibble::is_tibble(eval_results))
  expect_equal(dim(eval_results), c(36, 5))
  expect_equal(unique(eval_results$.rep), reps)
  expect_equal(unique(eval_results$.dgp_name), dgps)
  expect_equal(unique(eval_results$.method_name), methods)
  expect_equal(unique(eval_results$.metric), metrics)
  expect_equal(
    eval_results %>% dplyr::filter(.metric == "ppv"),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods,
      .metric = "ppv",
      .estimate = 1
    )
  )

  expect_true(tibble::is_tibble(eval_results_summary))
  expect_equal(dim(eval_results_summary), c(18, 9))
  expect_equal(
    colnames(eval_results_summary),
    c(".dgp_name", ".method_name", ".metric",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_feature_selection"))
  )
  expect_equal(unique(eval_results_summary$.dgp_name), dgps)
  expect_equal(unique(eval_results_summary$.method_name), methods)
  expect_equal(unique(eval_results_summary$.metric), metrics_sorted)
  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = length(metrics)),
      .method_name = methods,
      .metric = rep(metrics_sorted, times = 2)
    )
  )
  expect_equal(
    eval_results_summary %>% dplyr::filter(.metric == "ppv") %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      .metric = "ppv",
      mean_feature_selection = 1,
      median_feature_selection = 1,
      min_feature_selection = 1,
      max_feature_selection = 1,
      sd_feature_selection = 0,
      raw_feature_selection = rep(list(c(1, 1)), times = 2)
    )
  )

  metric_funs <- yardstick::metric_set(yardstick::sens, yardstick::spec)
  metrics <- c("sens", "spec")
  eval_results <- eval_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance",
    metrics = metric_funs
  )
  eval_results_summary <- summarize_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance",
    metrics = metric_funs
  )

  expect_equal(
    eval_results,
    eval_results_orig %>% dplyr::filter(.metric %in% metrics)
  )
  expect_equal(
    eval_results_summary,
    eval_results_summary_orig %>% dplyr::filter(.metric %in% metrics)
  )

  range_fun <- function(x) return(max(x) - min(x))
  eval_results_summary <- summarize_feature_selection_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    estimate_col = "est_support",
    imp_col = "est_importance",
    custom_summary_funs = list(range_feature_selection = range_fun)
  )

  expect_equal(
    dim(eval_results_summary),
    c(nrow(eval_results_summary_orig), ncol(eval_results_summary_orig) + 1)
  )
  expect_equal(eval_results_summary %>% dplyr::select(-range_feature_selection),
               eval_results_summary_orig)

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

  expect_true(tibble::is_tibble(roc_results))
  expect_equal(dim(roc_results), c(4, 4))
  expect_equal(
    roc_results %>% dplyr::select(-curve_estimate),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods
    )
  )
  expect_equal(
    roc_results$curve_estimate[[1]],
    tibble::tibble(
      .threshold = c(-Inf, 1, 1.5, 2.5, Inf),
      FPR = c(1, 1, 1, 0, 0),
      TPR = c(1, 1, 0.5, 0.5, 0)
    ),
    ignore_attr = TRUE
  )

  expect_true(tibble::is_tibble(pr_results))
  expect_equal(dim(pr_results), c(4, 4))
  expect_equal(
    pr_results %>% dplyr::select(-curve_estimate),
    roc_results %>% dplyr::select(-curve_estimate)
  )
  expect_equal(
    pr_results$curve_estimate[[1]],
    tibble::tibble(
      .threshold = c(Inf, 2.5, 1.5, 1),
      recall = c(0, 0.5, 0.5, 1),
      precision = c(1, 1, 0.5, 2/3)
    ),
    ignore_attr = TRUE
  )

  expect_true(tibble::is_tibble(roc_summary))
  expect_equal(dim(roc_summary), c(202, 9))
  expect_equal(
    colnames(roc_summary),
    c(".dgp_name", ".method_name", "FPR",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_TPR"))
  )
  expect_equal(unique(roc_summary$.dgp_name), dgps)
  expect_equal(unique(roc_summary$.method_name), methods)
  expect_equal(unique(roc_summary$FPR), seq(0, 1, by = 0.01))
  expect_equal(
    roc_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 101),
      .method_name = methods,
      FPR = rep(seq(0, 1, by = 0.01), times = 2)
    )
  )

  expect_true(tibble::is_tibble(pr_summary))
  expect_equal(dim(roc_summary), dim(pr_summary))
  expect_equal(
    colnames(pr_summary),
    c(".dgp_name", ".method_name", "recall",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_precision"))
  )
  expect_equal(unique(pr_summary$.dgp_name), dgps)
  expect_equal(unique(pr_summary$.method_name), methods)
  expect_equal(unique(pr_summary$recall), seq(0, 1, by = 0.01))
  expect_equal(
    pr_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 101),
      .method_name = methods,
      recall = rep(seq(0, 1, by = 0.01), times = 2)
    )
  )
  roc_results_orig <- roc_results
  pr_results_orig <- pr_results
  roc_summary_orig <- roc_summary
  pr_summary_orig <- pr_summary

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

  expect_equal(
    eval_results,
    tibble::tibble(
      .rep = rep(rep(reps, each = 3), times = 2),
      .dgp_name = rep(dgps, each = 6),
      .method_name = methods,
      feature = rep(features, times = 4),
      est_importance = c(1, 1.5, 2.5,
                         2, 1.5, 2.5,
                         3, 1.5, 2.5,
                         4, 1.5, 2.5)
    )
  )

  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 3),
      .method_name = methods,
      feature = rep(features, times = 2)
    )
  )
  expect_equal(
    eval_results_summary %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 3),
      .method_name = methods,
      feature = rep(features, times = 2),
      mean_feature_importance = c(1.5, 1.5, 2.5, 3.5, 1.5, 2.5),
      median_feature_importance = c(1.5, 1.5, 2.5, 3.5, 1.5, 2.5),
      min_feature_importance = c(1, 1.5, 2.5, 3, 1.5, 2.5),
      max_feature_importance = c(2, 1.5, 2.5, 4, 1.5, 2.5),
      sd_feature_importance = c(sqrt(2) / 2, 0, 0, sqrt(2) / 2, 0, 0),
      raw_feature_importance = list(
        c(1, 2),
        c(1.5, 1.5),
        c(2.5, 2.5),
        c(3, 4),
        c(1.5, 1.5),
        c(2.5, 2.5)
      )
    )
  )
})

test_that("Functions in Evaluator inference library work properly", {
  reps <- 1:2
  dgps <- c("DGP1", "DGP2")
  methods <- "Method"
  features <- c("featureA", "featureB", "featureC")
  metrics <- c("tp", "fp", "sens", "spec", "ppv", "pos", "neg", "roc_auc", "pr_auc")
  metrics_sorted <- sort(metrics)

  # generate example fit_results data for an inference problem
  fit_results <- tibble::tibble(
    .rep = rep(reps, times = 2),
    .dgp_name = rep(dgps, each = 2),
    .method_name = methods,
    feature_info = lapply(
      1:4,
      FUN = function(i) {
        tibble::tibble(
          feature = features,
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
  eval_results_orig <- eval_results
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval"
  )
  eval_results_summary_orig <- eval_results_summary

  expect_true(tibble::is_tibble(eval_results))
  expect_equal(dim(eval_results), c(36, 6))
  expect_equal(unique(eval_results$.rep), reps)
  expect_equal(unique(eval_results$.dgp_name), dgps)
  expect_equal(unique(eval_results$.method_name), methods)
  expect_equal(unique(eval_results$.alpha), 0.05)
  expect_equal(unique(eval_results$.metric), metrics)
  expect_equal(
    eval_results %>% dplyr::filter(.metric == "ppv"),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods,
      .alpha = 0.05,
      .metric = "ppv",
      .estimate = c(0, 0, 0.5, 0.5)
    )
  )

  expect_true(tibble::is_tibble(eval_results_summary))
  expect_equal(dim(eval_results_summary), c(18, 10))
  expect_equal(
    colnames(eval_results_summary),
    c(".dgp_name", ".method_name", ".metric", ".alpha",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_testing_err"))
  )
  expect_equal(unique(eval_results_summary$.dgp_name), dgps)
  expect_equal(unique(eval_results_summary$.method_name), methods)
  expect_equal(unique(eval_results_summary$.metric), metrics_sorted)
  expect_equal(
    eval_results_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = length(metrics)),
      .method_name = methods,
      .metric = rep(metrics_sorted, times = 2),
      .alpha = 0.05
    )
  )
  expect_equal(
    eval_results_summary %>% dplyr::filter(.metric == "ppv") %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      .metric = "ppv",
      .alpha = 0.05,
      mean_testing_err = c(0, 0.5),
      median_testing_err = c(0, 0.5),
      min_testing_err = c(0, 0.5),
      max_testing_err = c(0, 0.5),
      sd_testing_err = 0,
      raw_testing_err = list(c(0, 0), c(0.5, 0.5))
    )
  )

  metric_funs <- yardstick::metric_set(yardstick::sens, yardstick::spec)
  metrics <- c("sens", "spec")
  eval_results <- eval_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    metrics = metric_funs
  )
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    metrics = metric_funs
  )

  expect_equal(
    eval_results,
    eval_results_orig %>% dplyr::filter(.metric %in% metrics)
  )
  expect_equal(
    eval_results_summary,
    eval_results_summary_orig %>% dplyr::filter(.metric %in% metrics)
  )

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

  expect_equal(unique(eval_results$.alpha), c(0.05, 0.1))
  expect_equal(nrow(eval_results), nrow(eval_results_orig) * 2)
  expect_equal(
    eval_results %>% dplyr::filter(.alpha == 0.05),
    eval_results_orig
  )

  expect_equal(unique(eval_results_summary$.alpha), c(0.05, 0.1))
  expect_equal(nrow(eval_results_summary), nrow(eval_results_summary_orig) * 2)
  expect_equal(
    eval_results_summary %>% dplyr::filter(.alpha == 0.05),
    eval_results_summary_orig
  )

  range_fun <- function(x) return(max(x) - min(x))
  eval_results_summary <- summarize_testing_err(
    fit_results,
    nested_data = "feature_info",
    truth_col = "true_support",
    pval_col = "pval",
    custom_summary_funs = list(range_testing_err = range_fun)
  )

  expect_equal(
    dim(eval_results_summary),
    c(nrow(eval_results_summary_orig), ncol(eval_results_summary_orig) + 1)
  )
  expect_equal(eval_results_summary %>% dplyr::select(-range_testing_err),
               eval_results_summary_orig)

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

  expect_true(tibble::is_tibble(roc_results))
  expect_equal(dim(roc_results), c(4, 4))
  expect_equal(
    roc_results %>% dplyr::select(-curve_estimate),
    tibble::tibble(
      .rep = rep(reps, times = 2),
      .dgp_name = rep(dgps, each = 2),
      .method_name = methods
    )
  )
  expect_equal(
    roc_results$curve_estimate[[1]],
    tibble::tibble(
      .threshold = c(Inf, 10^2.5, 1, 10^(-1.5), -Inf),
      FPR = c(1, 1, 1, 1, 0),
      TPR = c(1, 1, 0.5, 0, 0)
    ),
    ignore_attr = TRUE
  )

  expect_true(tibble::is_tibble(pr_results))
  expect_equal(dim(pr_results), c(4, 4))
  expect_equal(
    pr_results %>% dplyr::select(-curve_estimate),
    roc_results %>% dplyr::select(-curve_estimate)
  )
  expect_equal(
    pr_results$curve_estimate[[1]],
    tibble::tibble(
      .threshold = c(-Inf, 10^(-1.5), 1, 10^2.5),
      recall = c(0, 0, 0.5, 1),
      precision = c(1, 0, 0.5, 2/3)
    ),
    ignore_attr = TRUE
  )

  expect_true(tibble::is_tibble(roc_summary))
  expect_equal(dim(roc_summary), c(202, 9))
  expect_equal(
    colnames(roc_summary),
    c(".dgp_name", ".method_name", "FPR",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_TPR"))
  )
  expect_equal(unique(roc_summary$.dgp_name), dgps)
  expect_equal(unique(roc_summary$.method_name), methods)
  expect_equal(unique(roc_summary$FPR), seq(0, 1, by = 0.01))
  expect_equal(
    roc_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 101),
      .method_name = methods,
      FPR = rep(seq(0, 1, by = 0.01), times = 2)
    )
  )

  expect_true(tibble::is_tibble(pr_summary))
  expect_equal(dim(roc_summary), dim(pr_summary))
  expect_equal(
    colnames(pr_summary),
    c(".dgp_name", ".method_name", "recall",
      paste0(c("mean", "median", "min", "max", "sd", "raw"), "_precision"))
  )
  expect_equal(unique(pr_summary$.dgp_name), dgps)
  expect_equal(unique(pr_summary$.method_name), methods)
  expect_equal(unique(pr_summary$recall), seq(0, 1, by = 0.01))
  expect_equal(
    pr_summary %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 101),
      .method_name = methods,
      recall = rep(seq(0, 1, by = 0.01), times = 2)
    )
  )

  ## eval_reject_prob / summarize_reject_prob
  eval_results <- eval_reject_prob(
    fit_results,
    nested_data = "feature_info",
    feature_col = "feature",
    pval_col = "pval"
  )

  expect_equal(dim(eval_results), c(19, 5))
  expect_equal(unique(eval_results$.dgp_name), dgps)
  expect_equal(unique(eval_results$.method_name), methods)
  expect_equal(unique(eval_results$feature), features)
  expect_equal(unique(eval_results$reject_prob), c(0, 0.5, 1))

  eval_results <- eval_reject_prob(
    fit_results,
    nested_data = "feature_info",
    feature_col = "feature",
    pval_col = "pval",
    alphas = c(0.05, 0.1)
  )

  expect_equal(
    eval_results %>% dplyr::group_keys(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 3),
      .method_name = methods,
      feature = rep(features, times = 2)
    )
  )
  expect_equal(
    eval_results %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = rep(dgps, each = 6),
      .method_name = methods,
      feature = rep(rep(features, each = 2), times = 2),
      .alpha = rep(c(0.05, 0.1), times = 6),
      reject_prob = c(0, 0.5, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0)
    )
  )
})

test_that("Functions in Evaluator utilities library work properly", {
  reps <- 1:2
  dgps <- c("DGP1", "DGP2")
  methods <- "Method"

  # create example eval_data to summarize
  eval_data <- tibble::tibble(.rep = rep(reps, times = 2),
                              .dgp_name = rep(dgps, each = 2),
                              .method_name = methods,
                              result = 1:4) %>%
    dplyr::group_by(.dgp_name, .method_name)

  ## summarize_eval_results
  results <- summarize_eval_results(eval_data = eval_data, eval_id = "res",
                                    value_col = "result")
  results_orig <- results
  expect_equal(dplyr::group_keys(results),
               tibble::tibble(.dgp_name = c("DGP1", "DGP2"),
                              .method_name = "Method"))
  expect_equal(
    results %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      mean_res = c(1.5, 3.5),
      median_res = c(1.5, 3.5),
      min_res = c(1, 3),
      max_res = c(2, 4),
      sd_res = sqrt(2) / 2,
      raw_res = list(c(1, 2), c(3, 4))
    )
  )

  results <- summarize_eval_results(eval_data = eval_data, eval_id = "res",
                                    value_col = "result",
                                    summary_funs = c("mean", "sd"))
  expect_equal(dplyr::group_keys(results),
               tibble::tibble(.dgp_name = c("DGP1", "DGP2"),
                              .method_name = "Method"))
  expect_equal(
    results %>% dplyr::ungroup(),
    tibble::tibble(
      .dgp_name = dgps,
      .method_name = methods,
      mean_res = c(1.5, 3.5),
      sd_res = sqrt(2) / 2
    )
  )

  range_fun <- function(x) return(max(x) - min(x))
  results <- summarize_eval_results(eval_data = eval_data, value_col = "result",
                                    custom_summary_funs = list(range = range_fun))
  expect_equal(dplyr::group_keys(results),
               tibble::tibble(.dgp_name = c("DGP1", "DGP2"),
                              .method_name = "Method"))
  expect_equal(
    results %>% dplyr::select(-range),
    results_orig %>%
      setNames(c(".dgp_name", ".method_name", "mean", "median", "min", "max", "sd", "raw"))
  )
  expect_equal(results$range, c(1, 1))
})
