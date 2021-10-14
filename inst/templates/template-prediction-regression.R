# template simulation for evaluating prediction accuracy in regression setting

dgp <- create_dgp(
  xy_dgp_constructor, name = "Uncorrelated Gaussian Linear DGP",
  x_fun = generate_X_gaussian, y_fun = generate_y_linear, err_fun = rnorm,
  n = 200, .x_p = 10, .y_betas = c(rep(1, 5), rep(0, 5)), .err_sd = 1,
  data_split = TRUE, train_prop = 0.5, return_support = TRUE
)

lasso_method <- function(X, y, Xtest, ytest, support, ...) {
  
  cv_fit <- glmnet::cv.glmnet(x = as.matrix(X), y = y, ...)
  best_fit_idx <- cv_fit$lambda == cv_fit$lambda.1se
  preds <- predict(cv_fit, as.matrix(Xtest))
  
  p <- ncol(X)
  if (is.null(colnames(X))) {
    features <- 1:p
  } else {
    features <- colnames(X)
  }
  out <- list(
    y = y,
    predictions = c(preds),
    support_df = data.frame(
      feature = features,
      true_support = 1:p %in% support,
      imp = as.matrix(cv_fit$glmnet.fit$beta)[, best_fit_idx]
    )
  )
  return(out)
}

rf_method <- function(X, y, Xtest, ytest, support, ...) {
  
  data <- as.data.frame(X) %>%
    cbind(y = y)
  
  if (is.factor(y)) {
    mtry <- round(sqrt(ncol(X)))
  } else {
    mtry <- round(ncol(X) / 3)
  }
  
  fit <- ranger::ranger(data = data,
                        dependent.variable.name = "y",
                        importance = "impurity",
                        mtry = mtry,
                        num.threads = 1,
                        ...)
  preds <- predict(fit, as.data.frame(Xtest))$predictions

  p <- ncol(X)
  if (is.null(colnames(X))) {
    features <- 1:p
  } else {
    features <- colnames(X)
  }
  out <- list(
    y = y,
    predictions = preds,
    support_df = data.frame(
      feature = features,
      true_support = 1:p %in% support,
      imp = fit$variable.importance
    )
  )
  return(out)
}

lasso <- create_method(lasso_method, name = "Lasso")
rf <- create_method(rf_method, name = "RF")

pred_err <- create_evaluator(
  summarize_pred_err, name = "Prediction Accuracy",
  truth = "y", estimate = "predictions"
)
fi <- create_evaluator(
  summarize_feature_importances, name = "Feature Importances",
  data_col = "support_df", feature = "feature", estimate = "imp"
)
feature_sel <- create_evaluator(
  summarize_feature_recovery, "Feature Selection",
  data_col = "support_df", truth = "true_support", estimate = "imp"
)

pred_err_plot <- create_visualizer(
  plot_pred_err, name = "Prediction Accuracy Plot",
  evaluator_name = "Prediction Accuracy"
)
fi_plot <- create_visualizer(
  plot_feature_imp, name = "Feature Importances Plot",
  evaluator_name = "Feature Importances", feature = "feature",
)
feature_sel_plot <- create_visualizer(
  plot_feature_recovery, name = "Feature Selection Plot",
  evaluator_name = "Feature Selection"
)
feature_roc_curve <- create_visualizer(
  plot_feature_recovery_curve, name = "Feature Selection ROC Plot",
  metric = "ROC", data_col = "support_df",
  truth = "true_support", estimate = "imp"
)
feature_pr_curve <- create_visualizer(
  plot_feature_recovery_curve, name = "Feature Selection PR Plot",
  metric = "PR", data_col = "support_df",
  truth = "true_support", estimate = "imp"
)


experiment <- create_experiment(name = "Prediction Simulation Template") %>%
  add_dgp(dgp) %>%
  add_method(lasso) %>%
  add_method(rf) %>%
  add_evaluator(pred_err) %>%
  add_evaluator(fi) %>%
  add_evaluator(feature_sel) %>%
  add_visualizer(pred_err_plot) %>%
  add_visualizer(fi_plot) %>%
  add_visualizer(feature_sel_plot) %>%
  add_visualizer(feature_roc_curve) %>%
  add_visualizer(feature_pr_curve)

results <- run_experiment(experiment, n_reps = 10)
results$fit_results
results$eval_results
results$visualize_results

feature_roc_curve <- create_visualizer(
  plot_feature_recovery_curve, name = "Feature Selection ROC Plot",
  metric = "ROC", data_col = "support_df",
  truth = "true_support", estimate = "imp", 
  linetype_str = substitute(vary_params)
)
feature_pr_curve <- create_visualizer(
  plot_feature_recovery_curve, name = "Feature Selection PR Plot",
  metric = "PR", data_col = "support_df",
  truth = "true_support", estimate = "imp", 
  linetype_str = substitute(vary_params)
)

vary_experiment <- create_experiment(clone_from = experiment) %>%
  add_vary_across(dgp = "Uncorrelated Gaussian Linear DGP", 
                  .err_sd = c(.1, .5, 1, 2)) %>%
  update_visualizer(feature_roc_curve, name = "Feature Selection ROC Plot") %>%
  update_visualizer(feature_pr_curve, name = "Feature Selection PR Plot")
vary_results <- run_experiment(vary_experiment, n_reps = 10)
vary_results$fit_results
vary_results$eval_results
vary_results$visualize_results
