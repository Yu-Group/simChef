test_that("Functions in Visualizer prediction library work properly", {
  skip_on_ci()

  # generate example fit_results data - regression
  fit_results_reg <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    y = lapply(1:4, FUN = function(x) (1:100)),
    predictions = lapply(1:4, FUN = function(x) (1:100)*(x+.1))
  )

  # generate example eval_results data - regression
  eval_results_reg <- list(
    `Prediction Errors` = summarize_pred_err(
      fit_results_reg, truth_col = "y", estimate_col = "predictions"
    )
  )

  # generate example fit_results data - classification
  fit_results_bin <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    y = lapply(1:4, FUN = function(x) as.factor(rep(0:1, length.out = 100))),
    class_probs = lapply(1:4, FUN = function(x) 1:100 / 100),
    predictions = lapply(class_probs,
                         FUN = function(x) as.factor(ifelse(x > 0.5, 1, 0)))
  )

  # generate example eval_results data - classification
  eval_results_bin <- list(
    ROC = summarize_pred_curve(
      fit_results_bin, truth_col = "y", prob_cols = "class_probs", curve = "ROC"
    ),
    PR = summarize_pred_curve(
      fit_results_bin, truth_col = "y", prob_cols = "class_probs", curve = "PR"
    )
  )

  ## plot_pred_err
  plt <- plot_pred_err(eval_results = eval_results_reg,
                       eval_name = "Prediction Errors",
                       show = c("point", "errorbar"))
  vdiffr::expect_doppelganger("plot_pred_err1", plt)
  plt <- plot_pred_err(fit_results = fit_results_reg,
                       show = c("point", "errorbar"),
                       eval_fun_options = list(truth_col = "y",
                                               estimate_col = "predictions"))
  vdiffr::expect_doppelganger("plot_pred_err1", plt)
  plt <- plot_pred_err(eval_results = eval_results_reg,
                       eval_name = "Prediction Errors",
                       show = c("point", "errorbar"),
                       color_str = NULL,
                       facet_formula = .method_name ~ .metric,
                       facet_type = "grid")
  vdiffr::expect_doppelganger("plot_pred_err2", plt)
  plt <- plot_pred_err(eval_results = eval_results_reg,
                       eval_name = "Prediction Errors",
                       show = "boxplot")
  vdiffr::expect_doppelganger("plot_pred_err3", plt)
  plt <- plot_pred_err(eval_results = eval_results_reg,
                       eval_name = "Prediction Errors",
                       show = "boxplot", color_str = ".method_name")
  vdiffr::expect_doppelganger("plot_pred_err4", plt)

  ## plot_pred_curve
  roc_plt <- plot_pred_curve(eval_results = eval_results_bin,
                             eval_name = "ROC", curve = "ROC",
                             show = c("line", "ribbon"))
  pr_plt <- plot_pred_curve(eval_results = eval_results_bin,
                            eval_name = "PR", curve = "PR",
                            show = c("line", "ribbon"))
  vdiffr::expect_doppelganger("plot_pred_curve_roc1", roc_plt)
  vdiffr::expect_doppelganger("plot_pred_curve_pr1", pr_plt)
  roc_plt <- plot_pred_curve(fit_results = fit_results_bin,
                             show = c("line", "ribbon"), curve = "ROC",
                             eval_fun_options = list(truth_col = "y",
                                                     prob_cols = "class_probs"))
  pr_plt <- plot_pred_curve(fit_results = fit_results_bin,
                            show = c("line", "ribbon"), curve = "PR",
                            eval_fun_options = list(truth_col = "y",
                                                    prob_cols = "class_probs"))
  vdiffr::expect_doppelganger("plot_pred_curve_roc1", roc_plt)
  vdiffr::expect_doppelganger("plot_pred_curve_pr1", pr_plt)
  roc_plt <- plot_pred_curve(eval_results = eval_results_bin,
                             eval_name = "ROC", curve = "ROC",
                             show = c("line", "ribbon"),
                             plot_by = ".dgp_name")
  vdiffr::expect_doppelganger("plot_pred_curve_roc2", roc_plt)

})

test_that("Functions in Visualizer feature selection library work properly", {
  skip_on_ci()

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

  # generate example eval_results data
  eval_results <- list(
    `Feature Importance` = summarize_feature_importance(
      fit_results,
      nested_cols = "feature_info",
      feature_col = "feature",
      imp_col = "est_importance"
    ),
    `Feature Selection Errors` = summarize_feature_selection_err(
      fit_results,
      nested_cols = "feature_info",
      truth_col = "true_support",
      estimate_col = "est_support",
      imp_col = "est_importance"
    ),
    ROC = summarize_feature_selection_curve(
      fit_results,
      curve = "ROC",
      nested_cols = "feature_info",
      truth_col = "true_support",
      imp_col = "est_importance"
    ),
    PR = summarize_feature_selection_curve(
      fit_results,
      curve = "PR",
      nested_cols = "feature_info",
      truth_col = "true_support",
      imp_col = "est_importance"
    )
  )

  ## plot_feature_importance
  plt <- plot_feature_importance(eval_results = eval_results,
                                 eval_name = "Feature Importance",
                                 feature_col = "feature")
  vdiffr::expect_doppelganger("plot_feature_importance1", plt)
  plt <- plot_feature_importance(fit_results = fit_results,
                                 feature_col = "feature",
                                 eval_fun_options = list(
                                   nested_cols = "feature_info",
                                   imp_col = "est_importance"
                                 ))
  vdiffr::expect_doppelganger("plot_feature_importance1", plt)
  plt <- plot_feature_importance(eval_results = eval_results,
                                 eval_name = "Feature Importance",
                                 feature_col = "feature",
                                 errorbar_args = list(width = .5, position = "dodge"),
                                 bar_args = list(width = .5))
  vdiffr::expect_doppelganger("plot_feature_importance2", plt)

  ## plot_feature_selection_err
  plt <- plot_feature_selection_err(eval_results = eval_results,
                                    eval_name = "Feature Selection Errors",
                                    show = c("bar"))
  vdiffr::expect_doppelganger("plot_feature_selection_err1", plt)
  plt <- plot_feature_selection_err(fit_results = fit_results, show = c("bar"),
                                    eval_fun_options = list(
                                      nested_cols = "feature_info",
                                      truth_col = "true_support",
                                      estimate_col = "est_support",
                                      imp_col = "est_importance"
                                    ))
  vdiffr::expect_doppelganger("plot_feature_selection_err1", plt)
  plt <- plot_feature_selection_err(eval_results = eval_results,
                                    eval_name = "Feature Selection Errors",
                                    show = c("bar"),
                                    color_str = ".dgp_name",
                                    interactive = TRUE)
  expect_true("plotly" %in% class(plt))

  ## plot_feature_selection_curve
  roc_plt <- plot_feature_selection_curve(eval_results = eval_results,
                                          eval_name = "ROC", curve = "ROC",
                                          show = c("line", "ribbon"))
  pr_plt <- plot_feature_selection_curve(eval_results = eval_results,
                                         eval_name = "PR", curve = "PR",
                                         show = c("line", "ribbon"))
  vdiffr::expect_doppelganger("plot_feature_selection_curve_roc1", roc_plt)
  vdiffr::expect_doppelganger("plot_feature_selection_curve_pr1", pr_plt)
  roc_plt <- plot_feature_selection_curve(fit_results = fit_results,
                                          show = c("line", "ribbon"),
                                          curve = "ROC",
                                          eval_fun_options = list(
                                            nested_cols = "feature_info",
                                            truth_col = "true_support",
                                            imp_col = "est_importance"
                                          ))
  pr_plt <- plot_feature_selection_curve(fit_results = fit_results,
                                         show = c("line", "ribbon"),
                                         curve = "PR",
                                         eval_fun_options = list(
                                           nested_cols = "feature_info",
                                           truth_col = "true_support",
                                           imp_col = "est_importance"
                                         ))
  vdiffr::expect_doppelganger("plot_feature_selection_curve_roc1", roc_plt)
  vdiffr::expect_doppelganger("plot_feature_selection_curve_pr1", pr_plt)
  roc_plt <- plot_feature_selection_curve(eval_results = eval_results,
                                          eval_name = "ROC", curve = "ROC",
                                          show = c("line", "ribbon"),
                                          plot_by = ".dgp_name")
  vdiffr::expect_doppelganger("plot_feature_selection_curve_roc2", roc_plt)
})

test_that("Functions in Visualizer inference library work properly", {
  skip_on_ci()

  # generate example fit_results data
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
          pval = c(10^(-(i - 1)), 10^-1.5, 10^-2.5)
        )
      }
    )
  )

  # generate example eval_results data
  eval_results <- list(
    `Testing Errors` = summarize_testing_err(
      fit_results,
      nested_cols = "feature_info",
      truth_col = "true_support",
      pval_col = "pval"
    ),
    `ROC` = summarize_testing_curve(
      fit_results,
      curve = "ROC",
      nested_cols = "feature_info",
      truth_col = "true_support",
      pval_col = "pval"
    ),
    `PR` = summarize_testing_curve(
      fit_results,
      curve = "PR",
      nested_cols = "feature_info",
      truth_col = "true_support",
      pval_col = "pval"
    ),
    `Reject Prob.` = eval_reject_prob(
      fit_results,
      nested_cols = "feature_info",
      feature_col = "feature",
      pval_col = "pval"
    )
  )

  ## plot_testing_err
  plt <- plot_testing_err(eval_results = eval_results,
                          eval_name = "Testing Errors",
                          show = c("bar", "errorbar"))
  vdiffr::expect_doppelganger("plot_testing_err1", plt)
  plt <- plot_testing_err(fit_results = fit_results,
                          show = c("bar", "errorbar"),
                          eval_fun_options = list(
                            nested_cols = "feature_info",
                            truth_col = "true_support",
                            pval_col = "pval"
                          ))
  vdiffr::expect_doppelganger("plot_testing_err1", plt)
  plt <- plot_testing_err(eval_results = eval_results,
                          eval_name = "Testing Errors",
                          show = c("bar", "errorbar"),
                          plot_by = ".alpha")
  vdiffr::expect_doppelganger("plot_testing_err2", plt)

  ## plot_testing_curve
  roc_plt <- plot_testing_curve(eval_results = eval_results,
                                eval_name = "ROC", curve = "ROC",
                                show = c("line", "ribbon"))
  pr_plt <- plot_testing_curve(eval_results = eval_results,
                               eval_name = "PR", curve = "PR",
                               show = c("line", "ribbon"))
  vdiffr::expect_doppelganger("plot_testing_curve_roc1", roc_plt)
  vdiffr::expect_doppelganger("plot_testing_curve_pr1", pr_plt)
  roc_plt <- plot_testing_curve(fit_results = fit_results,
                                show = c("line", "ribbon"),
                                curve = "ROC",
                                eval_fun_options = list(
                                  nested_cols = "feature_info",
                                  truth_col = "true_support",
                                  pval_col = "pval"
                                ))
  pr_plt <- plot_testing_curve(fit_results = fit_results,
                               show = c("line", "ribbon"),
                               curve = "PR",
                               eval_fun_options = list(
                                 nested_cols = "feature_info",
                                 truth_col = "true_support",
                                 pval_col = "pval"
                               ))
  vdiffr::expect_doppelganger("plot_testing_curve_roc1", roc_plt)
  vdiffr::expect_doppelganger("plot_testing_curve_pr1", pr_plt)
  roc_plt <- plot_testing_curve(eval_results = eval_results,
                                eval_name = "ROC", curve = "ROC",
                                show = c("line", "ribbon"),
                                plot_by = ".dgp_name")
  vdiffr::expect_doppelganger("plot_testing_curve_roc2", roc_plt)

  ## plot_reject_prob
  plt <- plot_reject_prob(eval_results = eval_results,
                          eval_name = "Reject Prob.",
                          feature_col = "feature")
  vdiffr::expect_doppelganger("plot_reject_prob1", plt)
  plt <- plot_reject_prob(fit_results = fit_results,
                          feature_col = "feature",
                          eval_fun_options = list(
                            nested_cols = "feature_info",
                            pval_col = "pval"
                          ))
  vdiffr::expect_doppelganger("plot_reject_prob1", plt)
  plt <- plot_reject_prob(eval_results = eval_results,
                          eval_name = "Reject Prob.",
                          facet_formula = NULL,
                          plot_by = "feature")
  vdiffr::expect_doppelganger("plot_reject_prob2", plt)
})

test_that("Functions in Visualizer utilities library work properly", {
  skip_on_ci()

  # generate example fit results data
  fit_results <- tibble::tibble(
    .rep = rep(1:2, times = 2),
    .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
    .method_name = c("Method"),
    y = lapply(1:4, FUN = function(x) (1:100)),
    predictions = lapply(1:4, FUN = function(x) (1:100)*(x+.1))
  )

  # generate example evaluation results data
  eval_results <- list(
    `Prediction Errors` = summarize_pred_err(
      fit_results = fit_results,
      truth_col = "y",
      estimate_col = "predictions",
      eval_id = "pred_err"
    )
  )

  ## plot_eval_constructor
  plt <- plot_eval_constructor(plot_data = eval_results[["Prediction Errors"]],
                               eval_id = "pred_err",
                               show = c("point", "errorbar"),
                               facet_formula = ~ .metric)
  vdiffr::expect_doppelganger("plot_eval_constructor1", plt)
  plt <- plot_eval_constructor(plot_data = eval_results[["Prediction Errors"]],
                               eval_id = "pred_err",
                               show = c("point", "errorbar"),
                               facet_formula = ~ .metric,
                               facet_type = "wrap",
                               errorbar_args = list(width = 0.5),
                               facet_args = list(scales = "free")) +
    ggplot2::labs(y = "Mean Prediction Error")
  vdiffr::expect_doppelganger("plot_eval_constructor2", plt)
  plt <- plot_eval_constructor(plot_data = eval_results[["Prediction Errors"]],
                               eval_id = "pred_err",
                               show = c("point", "errorbar"),
                               facet_formula = ~ .metric,
                               interactive = TRUE)
  expect_true("plotly" %in% class(plt))
  plt <- plot_eval_constructor(eval_results = eval_results,
                               eval_name = "Prediction Errors",
                               eval_id = "pred_err",
                               show = c("point", "errorbar"),
                               facet_formula = ~ .metric)
  vdiffr::expect_doppelganger("plot_eval_constructor1", plt)

  ## plot_eval_constructor with vector vary_param and grouped variables
  eval_results[["Prediction Errors"]] <- eval_results[["Prediction Errors"]] %>%
    dplyr::mutate(vary_param = list(1:2)) %>%
    dplyr::group_by(.dgp_name, vary_param)
  expect_error(
    plot_eval_constructor(plot_data = eval_results[["Prediction Errors"]],
                          vary_params = "vary_param",
                          eval_id = "pred_err",
                          show = c("point", "errorbar"),
                          facet_formula = ~ .metric),
    NA
  )

  ## plot_fit_constructor
  plot_fun <- function(fit_results, vary_params = NULL) {
    plt <- fit_results %>%
      tidyr::unnest(c("y", "predictions")) %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = y, y = predictions) +
      ggplot2::geom_point() +
      ggplot2::labs(title = sprintf("DGP: %s | Method: %s | Rep: %s",
                                    fit_results$.dgp_name,
                                    fit_results$.method_name,
                                    fit_results$.rep))
    return(plt)
  }
  plt <- plot_fit_constructor(fit_results, reps = 1, plot_fun = plot_fun)
  vdiffr::expect_doppelganger("plot_fit_constructor1", plt)

  ## get_dot_args
  arg_list <- get_dot_args(user_args = list(a = 1, b = 2, c = 3),
                           default_args = list(a = "a", d = "d"))
  expect_equal(arg_list, list(a = 1, b = 2, c = 3, d = "d"))

  # check that NULL argument gets kept in get_dot_args()
  arg_list <- get_dot_args(user_args = list(a = 1, b = NULL, c = 3),
                           default_args = list(c = "d"))
  expect_equal(arg_list, list(a = 1, b = NULL, c = 3))
  arg_list <- get_dot_args(user_args = list(a = 1, c = 3),
                           default_args = list(b = NULL))
  expect_equal(arg_list, list(a = 1, c = 3, b = NULL))
})

test_that("list_col_to_chr works properly", {
  expect_error(list_col_to_chr())

  ls1 <- list(3, 2, 1)
  ls2 <- list(c(1:2), c(3:4), c(5:7), c(3:4), c(1:3))
  ls3 <- list(matrix(1:4, nrow = 2),
              matrix(1:6, nrow = 2),
              matrix(1:6, nrow = 3),
              matrix(1:6, nrow = 2))
  ls4 <- list(tibble::tibble(a = 1:3, b = c(4, 5, 6)), "abc")
  name <- "name"

  expect_equal(list_col_to_chr(ls1, name = name, verbatim = TRUE),
               paste0(name, 3:1))
  expect_equal(list_col_to_chr(ls1, name = NULL, verbatim = TRUE),
               paste0(3:1))
  expect_equal(list_col_to_chr(ls1, name = name, verbatim = FALSE),
               paste0(name, 1:3))

  expect_equal(list_col_to_chr(ls2, name = name, verbatim = TRUE),
               paste0(name, c("1_2", "3_4", "5_6_7", "3_4", "1_2_3")))
  expect_equal(list_col_to_chr(ls2, name = name, verbatim = FALSE),
               paste0(name, c(1, 2, 3, 2, 4)))

  expect_equal(list_col_to_chr(ls3, name = NULL, verbatim = TRUE),
               c("1_2_3_4", "1_2_3_4_5_6", "1_2_3_4_5_6", "1_2_3_4_5_6"))
  expect_equal(list_col_to_chr(ls3, name = name, verbatim = FALSE),
               paste0(name, c(1, 2, 3, 2)))

  expect_equal(list_col_to_chr(ls4, name = NULL, verbatim = TRUE),
               c("1:3_c(4, 5, 6)", "abc"))
  expect_equal(list_col_to_chr(ls4, name = name, verbatim = FALSE),
               paste0(name, c(1, 2)))
})
