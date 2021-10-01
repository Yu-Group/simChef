test_that("Library of prediction metric evaluators works.", {
  dgp_fun1 <- function(n, mean) {
    list(y = rnorm(n, mean = mean), yhat = rnorm(n, mean = mean))
  }
  dgp_fun2 <- function(n, mean) {
    y <- rnorm(n, mean = mean)
    list(y = y, yhat = y)
  }
  binary_dgp <- function(n) {
    y <- as.factor(rep(0:1, length.out = n))
    yhat <- runif(n)
    list(y = y, yhat = yhat)
  }
  dgp1 <- DGP$new(dgp_fun1, n = 10, mean = 0)
  dgp2 <- DGP$new(dgp_fun2, n = 10, mean = 0)
  binary_dgp <- DGP$new(binary_dgp, n = 10)
  
  method_fun <- function(y, yhat, ...) {
    list(y = as.factor(y), yhat = yhat, estimate = as.factor(y),
         fi = data.frame(feature = c("a", "b", "c"),
                         imp = c(0, 1, 2),
                         truth = c(0, 1, 1)))
  }
  method <- Method$new(method_fun)
  
  eval <- Evaluator$new(summarize_pred_err,
                        truth = "y", estimate = "yhat",
                        metrics = NULL, groups = NULL,
                        custom_summary = list("mean" = mean,
                                              "list" = list))
  binary_eval <- Evaluator$new(summarize_pred_err,
                               truth = "y", estimate = "estimate",
                               probs = "yhat", metrics = NULL, groups = NULL)
  auc_curves <- Evaluator$new(summarize_pred_curve, 
                              truth = "y", probs = "yhat", 
                              metric = "ROC")
  fi <- Evaluator$new(summarize_feature_importances, 
                      data_col = "fi", feature = "feature", estimate = "imp")
  supp_eval <- Evaluator$new(summarize_feature_recovery, 
                             data_col = "fi", truth = "truth", estimate = "imp")
  feature_curves <- Evaluator$new(summarize_feature_recovery_curve,
                                  data_col = "fi", truth = "truth", 
                                  estimate = "imp", metric = "ROC")
  
  pred_err_plot <- Visualizer$new(plot_pred_err, 
                                  evaluator_name = "Binary Evaluator")
  pred_curve_plot <- Visualizer$new(plot_pred_curve,
                                    evaluator_name = "AUC Curve")
  fi_plot <- Visualizer$new(plot_feature_imp, 
                            evaluator_name = "FI", feature = "feature")
  fi_plot2 <- Visualizer$new(plot_feature_imp, max_features = 3,
                             data_col = "fi", feature = "feature", 
                             estimate = "imp")
  supp_plot <- Visualizer$new(plot_feature_recovery,
                              evaluator_name = "Feature Recovery",
                              show = "boxplot")
  feature_curves_plot <- Visualizer$new(plot_feature_recovery_curve,
                                        metric = "ROC", data_col = "fi",
                                        truth = "truth", estimate = "imp")
  
  experiment <- create_experiment(
    name = "test-lib-evaluators-prediction-metrics"
  ) %>%
    add_dgp(dgp1, "Random DGP") %>%
    add_dgp(dgp2, "Perfect DGP") %>%
    add_method(method, "Method") %>%
    add_evaluator(eval, "Prediction Error") %>%
    add_evaluator(fi, "FI") %>%
    add_evaluator(supp_eval, "Feature Recovery") %>%
    add_evaluator(feature_curves, "Feature Recovery Curves") %>%
    add_visualizer(fi_plot, "FI Plot") %>%
    add_visualizer(fi_plot2, "FI Plot2") %>%
    add_visualizer(supp_plot, "Feature Recovery Plot") %>%
    add_visualizer(feature_curves_plot, "Feature Recovery Curves Plot") %>%
    add_vary_across(method = "Method", a = 1:2, b = 3:4)
  fit_results <- experiment$fit(n_reps = 5, verbose = 0)
  eval_results <- experiment$evaluate(fit_results, verbose = 0)
  viz_results <- experiment$visualize(fit_results, eval_results, verbose = 0)
  
  experiment <- experiment %>%
    remove_dgp() %>%
    remove_evaluator() %>%
    remove_visualizer() %>%
    add_dgp(binary_dgp, "Binary DGP") %>%
    add_evaluator(binary_eval, "Binary Evaluator") %>%
    add_evaluator(auc_curves, "AUC Curve") %>%
    add_visualizer(pred_err_plot, "Prediction Error Plot") %>%
    add_visualizer(pred_curve_plot, "AUC Curve Plot")
  fit_results <- experiment$fit(n_reps = 5, verbose = 0)
  eval_results <- experiment$evaluate(fit_results, verbose = 0)
  viz_results <- experiment$visualize(fit_results, eval_results, verbose = 0)
})
