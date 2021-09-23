test_that("Library of prediction metric evaluators works.", {
  dgp_fun1 <- function(n, mean) {
    list(y = rnorm(n, mean = mean), yhat = rnorm(n, mean = mean))
  }
  dgp_fun2 <- function(n, mean) {
    y <- rnorm(n, mean = mean)
    list(y = y, yhat = y)
  }
  binary_dgp <- function(n) {
    y <- rep(0:1, length.out = n)
    yhat <- runif(n)
    list(y = y, yhat = yhat)
  }
  dgp1 <- DGP$new(dgp_fun1, n = 10, mean = 0)
  dgp2 <- DGP$new(dgp_fun2, n = 10, mean = 0)
  binary_dgp <- DGP$new(binary_dgp, n = 10)
  
  method_fun <- function(y, yhat, ...) {
    list(y = y, yhat = yhat, 
         s = c("a" = 0, "b" = 1, "c" = 2),
         shat = c("a" = 0, "b" = 1, "c" = 2),
         fi = data.frame(feature = c("a", "b"), imp = rnorm(2)))
  }
  method <- Method$new(method_fun)
  
  eval <- Evaluator$new(summarize_pred_err,
                        metrics = c("RMSE", "MSE", "R2", "MAE", "Corr"),
                        custom_summary = list("mean" = mean,
                                              "list" = list))
  binary_eval <- Evaluator$new(summarize_pred_err,
                               metrics = c("ClassErr", "BalancedClassErr", 
                                           "AUROC", "AUPRC"))
  fi <- Evaluator$new(summarize_feature_importances, imp_col = "imp")
  supp_eval <- Evaluator$new(summarize_feature_recovery)
  auc_curves <- Evaluator$new(summarize_auc_curve, metric = "PR")
  
  experiment <- create_experiment(
    name = "test-lib-evaluators-prediction-metrics"
  ) %>%
    add_dgp(dgp1, "Random DGP") %>%
    add_dgp(dgp2, "Perfect DGP") %>%
    add_method(method, "Method") %>%
    add_evaluator(eval, "Prediction Error") %>%
    add_evaluator(fi, "FI") %>%
    add_evaluator(supp_eval, "Feature Recovery") %>%
    add_vary_across(method = "Method", a = 1:2, b = 3:4)
  fit_results <- experiment$fit(n_reps = 5, verbose = 0)
  eval_results <- experiment$evaluate(fit_results, verbose = 0)
  
  experiment <- experiment %>%
    remove_dgp() %>%
    remove_evaluator() %>%
    add_dgp(binary_dgp, "Binary DGP") %>%
    add_evaluator(binary_eval, "Binary Evaluator") %>%
    add_evaluator(auc_curves, "AUPRC")
  fit_results <- experiment$fit(n_reps = 5, verbose = 0)
  eval_results <- experiment$evaluate(fit_results, verbose = 0)

})
