% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluator-lib-feature-selection.R
\name{eval_feature_selection_err_funs}
\alias{eval_feature_selection_err_funs}
\alias{eval_feature_selection_err}
\alias{summarize_feature_selection_err}
\title{Evaluate and/or summarize feature selection errors.}
\usage{
eval_feature_selection_err(
  fit_results,
  vary_params = NULL,
  nested_cols = NULL,
  truth_col,
  estimate_col = NULL,
  imp_col,
  group_cols = NULL,
  metrics = NULL,
  na_rm = FALSE
)

summarize_feature_selection_err(
  fit_results,
  vary_params = NULL,
  nested_cols = NULL,
  truth_col,
  estimate_col = NULL,
  imp_col,
  group_cols = NULL,
  metrics = NULL,
  na_rm = FALSE,
  summary_funs = c("mean", "median", "min", "max", "sd", "raw"),
  custom_summary_funs = NULL,
  eval_id = "feature_selection"
)
}
\arguments{
\item{fit_results}{A tibble, as returned by \code{\link[=fit_experiment]{fit_experiment()}}.}

\item{vary_params}{A vector of \code{DGP} or \code{Method} parameter names that are
varied across in the \code{Experiment}.}

\item{nested_cols}{(Optional) A character string or vector specifying the
name of the column(s) in \code{fit_results} that need to be
unnested before evaluating results. Default is \code{NULL}, meaning no
columns in \code{fit_results} need to be unnested prior to computation.}

\item{truth_col}{A character string identifying the column in
\code{fit_results} with the true feature support data. Each element in this
column should be an array of length \code{p}, where \code{p} is the number
of features. Elements in this array should be binary with \code{TRUE} or
\code{1} meaning the feature (corresponding to that slot) is in the support
and \code{FALSE} or \code{0} meaning the feature is not in the support.}

\item{estimate_col}{An (optional) character string identifying the column in
\code{fit_results} with the estimated feature support data. Each element in
this column should be an array of length \code{p}, where \code{p} is the
number of features and the feature order aligns with that of
\code{truth_col}. Elements in this array should be binary with \code{TRUE}
or \code{1} meaning the feature (corresponding to that slot) is in the
estimated support and \code{FALSE} or \code{0} meaning the feature is not
in the estimated support. If \code{NULL} (default), the non-zero elements
of \code{imp_col} are used as the estimated feature support.}

\item{imp_col}{A character string identifying the column in
\code{fit_results} with the estimated feature importance data. Each element
in this column should be an array of length \code{p}, where \code{p} is the
number of features and the feature order aligns with that of
\code{truth_col}. Elements in this array should be numeric where a higher
magnitude indicates a more important feature.}

\item{group_cols}{(Optional) A character string or vector specifying the
column(s) to group rows by before evaluating metrics.
This is useful for assessing within-group metrics.}

\item{metrics}{A \code{metric_set} object indicating the metrics to evaluate.
See \code{\link[yardstick:metric_set]{yardstick::metric_set()}} for more details. Default \code{NULL} will
evaluate the following: number of true positives (\code{tp}), number of
false positives (\code{fp}), sensitivity (\code{sens}), specificity
(\code{spec}), positive predictive value (\code{ppv}), number of features
in the estimated support (\code{pos}), number of features not in the
estimated support (\code{neg}), AUROC (\code{roc_auc}), and AUPRC
(\code{pr_auc}). If \code{na_rm = TRUE}, the number of NA values
(\code{num_na}) is also computed.}

\item{na_rm}{A \code{logical} value indicating whether \code{NA} values
should be stripped before the computation proceeds.}

\item{summary_funs}{Character vector specifying how to summarize
evaluation metrics. Must choose from a built-in library of summary
functions - elements of the vector must be one of "mean", "median",
"min", "max", "sd", "raw".}

\item{custom_summary_funs}{Named list of custom functions to summarize
results. Names in the list should correspond to the name of the summary
function. Values in the list should be a function that takes in one
argument, that being the values of the evaluated metrics.}

\item{eval_id}{Character string. ID to be used as a suffix when naming result
columns. Default \code{NULL} does not add any ID to the column names.}
}
\value{
The output of \code{eval_feature_selection_err()} is a \code{tibble} with the
following columns:
\describe{
\item{.rep}{Replicate ID.}
\item{.dgp_name}{Name of DGP.}
\item{.method_name}{Name of Method.}
\item{.metric}{Name of the evaluation metric.}
\item{.estimate}{Value of the evaluation metric.}
}
as well as any columns specified by \code{group_cols} and \code{vary_params}.

The output of \code{summarize_feature_selection_err()} is a grouped
\code{tibble} containing both identifying information and the feature
selection results aggregated over experimental replicates. Specifically, the
identifier columns include \code{.dgp_name}, \code{.method_name}, any columns
specified by \code{group_cols} and \code{vary_params}, and \code{.metric}.
In addition, there are results columns corresponding to the requested
statistics in \code{summary_funs} and \code{custom_summary_funs}. These
columns end in the suffix specified by \code{eval_id}.
}
\description{
Evaluate various feature selection metrics, given the true
feature support and the estimated feature support.
\code{eval_feature_selection_err()} evaluates the various feature selection
metrics for each experimental replicate separately.
\code{summarize_feature_selection_err()} summarizes the various feature
selection metrics across experimental replicates.
}
\examples{
# generate example fit_results data for a feature selection problem
fit_results <- tibble::tibble(
  .rep = rep(1:2, times = 2),
  .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
  .method_name = c("Method"),
  feature_info = lapply(
    1:4,
    FUN = function(i) {
      tibble::tibble(
        # feature names
        feature = c("featureA", "featureB", "featureC"),
        # true feature support
        true_support = c(TRUE, FALSE, TRUE),
        # estimated feature support
        est_support = c(TRUE, FALSE, FALSE),
        # estimated feature importance scores
        est_importance = c(10, runif(2, min = -2, max = 2))
      )
    }
  )
)

# evaluate feature selection (using all default metrics) for each replicate
eval_results <- eval_feature_selection_err(
  fit_results,
  nested_cols = "feature_info",
  truth_col = "true_support",
  estimate_col = "est_support",
  imp_col = "est_importance"
)
# summarize feature selection error (using all default metric) across replicates
eval_results_summary <- summarize_feature_selection_err(
  fit_results,
  nested_cols = "feature_info",
  truth_col = "true_support",
  estimate_col = "est_support",
  imp_col = "est_importance"
)

# evaluate/summarize feature selection errors using specific yardstick metrics
metrics <- yardstick::metric_set(yardstick::sens, yardstick::spec)
eval_results <- eval_feature_selection_err(
  fit_results,
  nested_cols = "feature_info",
  truth_col = "true_support",
  estimate_col = "est_support",
  imp_col = "est_importance",
  metrics = metrics
)
eval_results_summary <- summarize_feature_selection_err(
  fit_results,
  nested_cols = "feature_info",
  truth_col = "true_support",
  estimate_col = "est_support",
  imp_col = "est_importance",
  metrics = metrics
)

# summarize feature selection errors using specific summary metric
range_fun <- function(x) return(max(x) - min(x))
eval_results_summary <- summarize_feature_selection_err(
  fit_results,
  nested_cols = "feature_info",
  truth_col = "true_support",
  estimate_col = "est_support",
  imp_col = "est_importance",
  custom_summary_funs = list(range_feature_selection = range_fun)
)

}
\seealso{
Other feature_selection_funs: 
\code{\link{eval_feature_importance_funs}},
\code{\link{eval_feature_selection_curve_funs}},
\code{\link{plot_feature_importance}()},
\code{\link{plot_feature_selection_curve}()},
\code{\link{plot_feature_selection_err}()}
}
\concept{feature_selection_funs}
