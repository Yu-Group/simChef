## code to prepare example experiment data

set.seed(331)

#' Linear Data-Generating Process
#'
#' @description Generate training and test data according to the classical
#'   linear regression model: y = X*beta + noise.
#'
#' @param n_train Number of training samples.
#' @param n_test Number of training samples.
#' @param p Number of features.
#' @param beta Coefficient vector in linear regression function.
#' @param noise_sd Standard deviation of additive noise term.
linear_dgp_fun <- function(n_train, n_test, p, beta, noise_sd) {
  n <- n_train + n_test
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- X %*% beta + rnorm(n, sd = noise_sd)
  data_list <- list(
    X_train = X[1:n_train, , drop = FALSE],
    y_train = y[1:n_train],
    X_test = X[(n_train + 1):n, , drop = FALSE],
    y_test = y[(n_train + 1):n]
  )
  return(data_list)
}

#' Linear Regression Method
#'
#' @param X_train Training data matrix.
#' @param y_train Training response vector.
#' @param X_test Test data matrix.
#' @param y_test Test response vector.
linear_method_fun <- function(X_train, y_train, X_test, y_test) {
  train_df <- dplyr::bind_cols(data.frame(X_train), y = y_train)
  fit <- lm(y ~ ., data = train_df)
  predictions <- predict(fit, data.frame(X_test))
  return(list(predictions = predictions, y_test = y_test))
}

#' Random Forest Method
#'
#' @param X_train Training data matrix.
#' @param y_train Training response vector.
#' @param X_test Test data matrix.
#' @param y_test Test response vector.
#' @param ... Additional arguments to pass to `ranger::ranger()`
rf_method_fun <- function(X_train, y_train, X_test, y_test, ...) {
  train_df <- dplyr::bind_cols(data.frame(X_train), y = y_train)
  fit <- ranger::ranger(y ~ ., data = train_df, ...)
  predictions <- predict(fit, data.frame(X_test))$predictions
  return(list(predictions = predictions, y_test = y_test))
}

## DGPs
linear_dgp <- create_dgp(
  .dgp_fun = linear_dgp_fun, .name = "Linear DGP",
  # additional named parameters to pass to .dgp_fun()
  n_train = 200, n_test = 200, p = 2, beta = c(1, 0), noise_sd = 1
)

## Methods
linear_method <- create_method(
  .method_fun = linear_method_fun, .name = "Linear Regression"
  # additional named parameters to pass to .method_fun()
)
rf_method <- create_method(
  .method_fun = rf_method_fun, .name = "Random Forest",
  # additional named parameters to pass to .method_fun()
  num.threads = 1
)

## Evaluators
prediction_error_evaluator <- create_evaluator(
  .eval_fun = summarize_pred_err, .name = 'Prediction Error',
  # additional named parameters to pass to .eval_fun()
  truth_col = "y_test", estimate_col = "predictions"
)

## Visualizers
prediction_error_plot <- create_visualizer(
  .viz_fun = plot_pred_err, .name = 'Prediction Error Plot',
  # additional named parameters to pass to .viz_fun()
  eval_name = 'Prediction Error'
)

example_experiment <- create_experiment(name = "Example Experiment") |>
  add_dgp(linear_dgp) |>
  add_method(linear_method) |>
  add_method(rf_method) |>
  add_evaluator(prediction_error_evaluator) |>
  add_visualizer(prediction_error_plot)

results <- run_experiment(example_experiment, n_reps = 10)
example_fit_results <- results$fit_results
example_eval_results <- results$eval_results
example_viz_results <- results$viz_results

usethis::use_data(
  linear_dgp, linear_method, rf_method,
  prediction_error_evaluator, prediction_error_plot,
  example_experiment,
  example_fit_results, example_eval_results, example_viz_results,
  overwrite = TRUE
)
