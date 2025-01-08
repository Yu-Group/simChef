#' simChef Example Components and Results Data
#'
#' @description
#' Collection of example [Experiment] components ([DGP]s, [Method]s,
#' [Evaluator]s, and [Visualizer]s) and results datasets (`fit_results`,
#' `eval_results`, `viz_results`).
#'
#' @name simChef_example_funs
#' @docType data
#'
#' @section Example `Experiment` Components:
#'
#' `linear_dgp`:
#' An example [DGP] which generates training and test data according to the
#' classical linear regression model: y = X*beta + noise.
#'
#' `linear_method`:
#' An example [Method] which fits a linear regression model to the training data
#' and makes predictions on the test data.
#'
#' `rf_method`:
#' An example [Method] which fits a random forest model to the training data
#' and makes predictions on the test data.
#'
#' `prediction_error_evaluator`:
#' An example [Evaluator] which calculates the prediction error between the
#' predicted and true predicted values.
#'
#' `prediction_error_plot`:
#' An example [Visualizer] which plots the prediction error.
#'
#' `example_experiment`:
#' An example [Experiment] with the following components:
#' \itemize{
#'   \item DGP: `linear_dgp`
#'   \item Method: `linear_method`, `rf_method`
#'   \item Evaluator: `prediction_error_evaluator`
#'   \item Visualizer: `prediction_error_plot`
#' }
#'
#' @section Example `Experiment` Results Datasets:
#'
#' `example_fit_results`:
#' An example of the fitted results from an [Experiment], computed via
#' [run_experiment()] (or more directly, [fit_experiment()]) applied to
#' `example_experiment`.
#'
#' `example_eval_results`:
#' An example of the evaluation results from an [Experiment], computed via
#' [run_experiment()] (or more directly, [evaluate_experiment()]) applied to
#' `example_experiment`.
#'
#' `example_viz_results`:
#' An example of the visualization results from an [Experiment], computed via
#' [run_experiment()] (or more directly, [visualize_experiment()]) applied to
#' `example_experiment`.
#'
#' @examples
#' data(linear_dgp)
#' data(linear_method)
#' data(rf_method)
#' data(prediction_error_evaluator)
#' data(prediction_error_plot)
#' data(example_experiment)
#' data(example_fit_results)
#' data(example_eval_results)
#' data(example_viz_results)
#'
NULL

#' @rdname simChef_example_funs
#' @format ## `linear_dgp`
#' An object of class [DGP]
"linear_dgp"

#' @rdname simChef_example_funs
#' @format ## `linear_method`
#' An object of class [Method]
"linear_method"

#' @rdname simChef_example_funs
#' @format ## `rf_method`
#' An object of class [Method]
"rf_method"

#' @rdname simChef_example_funs
#' @format ## `prediction_error_evaluator`
#' An object of class [Evaluator]
"prediction_error_evaluator"

#' @rdname simChef_example_funs
#' @format ## `prediction_error_plot`
#' An object of class [Visualizer]
"prediction_error_plot"

#' @rdname simChef_example_funs
#' @format ## `example_experiment`
#' An object of class [Experiment]
"example_experiment"

#' @rdname simChef_example_funs
#' @format ## `example_fit_results`
#' A `tibble` with 20 rows and 5 columns containing the fitted results from
#' fitting all `Methods` across all `DGPs` in the `example_experiment` for 10
#' repetitions. The columns are:
#' \describe{
#'   \item{.rep}{Replicate ID}
#'   \item{.dgp_name}{Name of data-generating process}
#'   \item{.method_name}{Name of method}
#'   \item{predictions}{List of elements, each a 200-length vector of the
#'   test predictions from the given DGP and method combination}
#'   \item{y_test}{List of elements, each a 200-length vector of the true test
#'   responses from the given DGP}
#' }
"example_fit_results"

#' @rdname simChef_example_funs
#' @format ## `example_eval_results`
#' A named list of 1 element containing a `tibble` with the prediction error
#' results from evaluating the `Evaluators` in the `example_experiment`.
#' This `tibble` has 6 rows and 9 columns:
#' \describe{
#'   \item{.dgp_name}{Name of data-generating process}
#'   \item{.method_name}{Name of method}
#'   \item{.metric}{Name of evaluation metric, either "rsq" (R-squared), "rmse"
#'   (root mean squared error), or "mae" (mean absolute error)}
#'   \item{mean_pred_err}{Mean prediction error across all 10 simulation
#'   replicates}
#'   \item{median_pred_err}{Median prediction error across all 10 simulation
#'   replicates}
#'   \item{min_pred_err}{Minimum prediction error across all 10 simulation
#'   replicates}
#'   \item{max_pred_err}{Maximum prediction error across all 10 simulation
#'   replicates}
#'   \item{sd_pred_err}{Standard deviation of prediction error across all 10
#'   simulation replicates}
#'   \item{raw_pred_err}{List of elements, each a vector of the raw prediction
#'   errors from the 10 simulation replicates}
#' }
"example_eval_results"

#' @rdname simChef_example_funs
#' @format ## `example_viz_results`
#' A named list of 1 element containing a `ggplot` object, visualizing the
#' prediction error results from the `example_experiment`.
"example_viz_results"
