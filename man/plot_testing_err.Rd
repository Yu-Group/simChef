% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizer-lib-inference.R
\name{plot_testing_err}
\alias{plot_testing_err}
\title{Plot testing error evaluation results according to various metrics.}
\usage{
plot_testing_err(
  fit_results = NULL,
  eval_results = NULL,
  eval_name = NULL,
  eval_fun = "summarize_testing_err",
  eval_fun_options = NULL,
  vary_params = NULL,
  metrics = NULL,
  show = c("point", "line", "errorbar"),
  ...
)
}
\arguments{
\item{fit_results}{A tibble, as returned by \code{\link[=fit_experiment]{fit_experiment()}}.}

\item{eval_results}{A list of result tibbles, as returned by
\code{\link[=evaluate_experiment]{evaluate_experiment()}}.}

\item{eval_name}{Name of \code{Evaluator} containing results to plot. If
\code{NULL}, the data used for plotting is computed from scratch via
\code{eval_fun}.}

\item{eval_fun}{Character string, specifying the function used to compute
the data used for plotting if \code{eval_name = NULL}. If \code{eval_name}
is not \code{NULL}, this argument is ignored.}

\item{eval_fun_options}{List of named arguments to pass to \code{eval_fun}.}

\item{vary_params}{A vector of \code{DGP} or \code{Method} parameter names that are
varied across in the \code{Experiment}.}

\item{metrics}{A \code{metric_set} object indicating the metrics to plot.
See \code{\link[yardstick:metric_set]{yardstick::metric_set()}} for more details. Default \code{NULL} will
use the default metrics in \code{\link[yardstick:metrics]{yardstick::metrics()}}.}

\item{show}{Character vector with elements being one of "boxplot", "point",
"line", "bar", "errorbar", "ribbon", "violin", indicating what plot
layer(s) to construct.}

\item{...}{
  Arguments passed on to \code{\link[=plot_eval_constructor]{plot_eval_constructor}}
  \describe{
    \item{\code{eval_id}}{(Optional) Character string. ID used as the suffix for naming
columns in evaluation results tibble. If \code{eval_summary_constructor()}
was used to construct the \code{Evaluator}, this should be the same as the
\code{eval_id} argument in \code{eval_summary_constructor()}. Only used to
assign default (i.e., "auto") aesthetics in ggplot.}
    \item{\code{x_str}}{(Optional) Name of column in data frame to plot on the
x-axis. Default "auto" chooses what to plot on the x-axis automatically.}
    \item{\code{y_str}}{(Optional) Name of column in data frame to plot on the
y-axis if \code{show} is anything but "boxplot". Default "auto" chooses
what to plot on the y-axis automatically.}
    \item{\code{y_boxplot_str}}{(Optional) Name of column in data frame to plot on
the y-axis if \code{show} is "boxplot". Default "auto" chooses
what to plot on the y-axis automatically.}
    \item{\code{err_sd_str}}{(Optional) Name of column in data frame containing the
standard deviations of \code{y_str}. Used for plotting the errorbar and
ribbon ggplot layers. Default "auto" chooses what column to use for the
standard deviations automatically.}
    \item{\code{color_str}}{(Optional) Name of column in data frame to use for the
color and fill aesthetics when plotting. Default "auto" chooses what to
use for the color and fill aesthetics automatically. Use \code{NULL} to
avoid adding any color and fill aesthetic.}
    \item{\code{linetype_str}}{(Optional) Name of column in data frame to use for
the linetype aesthetic when plotting. Used only when \code{show = "line"}.
Default "auto" chooses what to use for the linetype aesthetic
automatically. Use \code{NULL} to avoid adding any linetype aesthetic.}
    \item{\code{facet_formula}}{(Optional) Formula for \code{ggplot2::facet_wrap()} or
\code{ggplot2::facet_grid()} if need be.}
    \item{\code{facet_type}}{One of "grid" or "wrap" specifying whether to use
\code{ggplot2::facet_wrap()} or \code{ggplot2::facet_grid()} if need be.}
    \item{\code{plot_by}}{(Optional) Name of column in \code{eval_tib} to use for
subsetting data and creating different plots for each unique value. Default
"auto" chooses what column to use for the subsetting automatically. Use
\code{NULL} to avoid creating multiple plots.}
    \item{\code{add_ggplot_layers}}{List of additional layers to add to a ggplot object
via \code{+}.}
    \item{\code{boxplot_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_boxplot()}.}
    \item{\code{point_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_point()}.}
    \item{\code{line_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_line()}.}
    \item{\code{bar_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_bar()}.}
    \item{\code{errorbar_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_errorbar()}.}
    \item{\code{ribbon_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_ribbon()}.}
    \item{\code{violin_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::geom_violin()}.}
    \item{\code{facet_args}}{(Optional) Additional arguments to pass into
\code{ggplot2::facet_grid()} or \code{ggplot2::facet_wrap()}.}
    \item{\code{interactive}}{Logical. If \code{TRUE}, returns interactive \code{plotly}
plots. If \code{FALSE}, returns static \code{ggplot} plots.}
  }}
}
\value{
If \code{interactive = TRUE}, returns a \code{plotly} object if
\code{plot_by} is \code{NULL} and a list of \code{plotly} objects if
\code{plot_by} is not \code{NULL}. If \code{interactive = FALSE}, returns
a \code{ggplot} object if \code{plot_by} is \code{NULL} and a list of
\code{ggplot} objects if \code{plot_by} is not \code{NULL}.
}
\description{
Plot the raw or summarized testing errors as a
boxplot, scatter plot, line plot, or bar plot with or without 1 SD error
bars.
}
\examples{
# generate example fit_results data
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
        # estimated p-values
        pval = 10^(sample(-3:0, 3, replace = TRUE))
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
  )
)

# create bar plot using pre-computed evaluation results
plt <- plot_testing_err(eval_results = eval_results,
                        eval_name = "Testing Errors",
                        show = c("bar", "errorbar"))
# or alternatively, create the same plot directly from fit results
plt <- plot_testing_err(fit_results = fit_results, show = c("bar", "errorbar"),
                        eval_fun_options = list(
                          nested_cols = "feature_info",
                          truth_col = "true_support",
                          pval_col = "pval"
                        ))

# can customize plot (see plot_eval_constructor() for possible arguments)
plt <- plot_testing_err(eval_results = eval_results,
                        eval_name = "Testing Errors",
                        show = c("bar", "errorbar"),
                        plot_by = ".alpha")

}
\seealso{
Other inference_funs: 
\code{\link{eval_reject_prob}()},
\code{\link{eval_testing_curve_funs}},
\code{\link{eval_testing_err_funs}},
\code{\link{plot_reject_prob}()},
\code{\link{plot_testing_curve}()}
}
\concept{inference_funs}
