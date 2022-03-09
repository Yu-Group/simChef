#' Arguments that are shared by multiple \code{Visualizer} library functions.
#'
#' @name shared_viz_lib_args
#' 
#' @param evaluator_name Name of \code{Evaluator} containing results to plot.
#'   To compute the evaluation summary results from scratch or if the evaluation
#'   summary results have not yet been evaluated, set to \code{NULL}.
#' @param interactive Logical. If \code{TRUE}, returns interactive \code{plotly}
#'   plots. If \code{FALSE}, returns static \code{ggplot} plots.
#' @param curve Either "ROC" or "PR" indicating whether to plot the ROC or 
#'   Precision-Recall curve.
#' @param show Character vector with elements being one of "boxplot", "point",
#'   "line", "bar", "errorbar", "ribbon" indicating what plot layer(s) to
#'   construct.
#'
#' @keywords internal
NULL

#' Developer function for plotting summary of evaluation results.
#' 
#' @description A helper function for developing new \code{Visualizer} plotting
#'   functions that plot the summarized evaluation results as a boxplot, 
#'   scatter plot, line plot, or bar plot with or without 1 SD error 
#'   bars/ribbons. This function accepts either (1) a pre-computed tibble
#'   containing the summarized evaluation results or (2) the Evaluator function 
#'   and its corresponding function arguments for computing the evaluation 
#'   results within this function call.
#'   
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @param eval_tib (Optional) \code{Tibble} (typically from the output of
#'   \code{eval_summary_constructor}) containing the summarized evaluation
#'   results to plot. If not provided, the evaluation results will be 
#'   automatically computed by calling \code{eval_fun()}. If the summarized
#'   evaluation results have already been computed previously, \code{eval_tib}
#'   should be specified to avoid duplicate computations.
#' @param eval_id Character string. ID used as the suffix for naming columns in
#'   \code{eval_summary_constructor()}. Should be the same as the \code{eval_id}
#'   argument in \code{eval_summary_constructor()}.
#' @param eval_fun Function used to compute evaluation results summary. This
#'   function is only used (and required) if necessary results have not already
#'   been computed in \code{eval_tib}.
#' @param x_str (Optional) Name of column in \code{eval_tib} to plot on the 
#'   x-axis. Default "auto" chooses what to plot on the x-axis automatically.
#' @param y_str (Optional) Name of column in \code{eval_tib} to plot on the
#'   y-axis if \code{show} is anything but "boxplot". Default "auto" chooses 
#'   what to plot on the y-axis automatically.
#' @param err_sd_str (Optional) Name of column in \code{eval_tib} containing the
#'   standard deviations of \code{y_str}. Used for plotting the errorbar and
#'   ribbon ggplot layers. Default "auto" chooses what column to use for the
#'   standard deviations automatically.
#' @param color_str (Optional) Name of column in \code{eval_tib} to use for the
#'   color and fill aesthetics when plotting. Default "auto" chooses what to 
#'   use for the color and fill aesthetics automatically. Use \code{NULL} to
#'   avoid adding any color and fill aesthetic.
#' @param linetype_str (Optional) Name of column in \code{eval_tib} to use for
#'   the linetype aesthetic when plotting. Used only when \code{show = "line"}.
#'   Default "auto" chooses what to use for the linetype aesthetic 
#'   automatically. Use \code{NULL} to avoid adding any linetype aesthetic.
#' @param facet_formula (Optional) Formula for \code{ggplot2::facet_wrap()} or
#'   \code{ggplot2::facet_grid()} if need be.
#' @param facet_type One of "grid" or "wrap" specifying whether to use
#'   \code{ggplot2::facet_wrap()} or \code{ggplot2::facet_grid()} if need be.
#' @param plot_by (Optional) Name of column in \code{eval_tib} to use for
#'   subsetting data and creating different plots for each unique value. Default
#'   "auto" chooses what column to use for the subsetting automatically. Use
#'   \code{NULL} to avoid creating multiple plots.
#' @param add_ggplot_layers List of additional layers to add to a ggplot object
#'   via \code{+}.
#' @param boxplot_args (Optional) Additional arguments to pass into
#'   \code{ggplot2::geom_boxplot()}.
#' @param point_args (Optional) Additional arguments to pass into 
#'   \code{ggplot2::geom_point()}.
#' @param line_args (Optional) Additional arguments to pass into 
#'   \code{ggplot2::geom_line()}.
#' @param bar_args (Optional) Additional arguments to pass into 
#'   \code{ggplot2::geom_bar()}.
#' @param errorbar_args (Optional) Additional arguments to pass into 
#'   \code{ggplot2::geom_errorbar()}.
#' @param ribbon_args (Optional) Additional arguments to pass into
#'   \code{ggplot2::geom_ribbon()}.
#' @param facet_args (Optional) Additional arguments to pass into 
#'   \code{ggplot2::facet_grid()} or \code{ggplot2::facet_wrap()}.
#' @param ... Additional arguments to pass to \code{eval_fun()}. This is only
#'   used if necessary results have not already been computed in 
#'   \code{eval_tib}.
#' 
#' @return If \code{interactive = TRUE}, returns a \code{plotly} object if
#'   \code{plot_by} is \code{NULL} and a list of \code{plotly} objects if
#'   \code{plot_by} is not \code{NULL}. If \code{interactive = FALSE}, returns
#'   a \code{ggplot} object if \code{plot_by} is \code{NULL} and a list of
#'   \code{ggplot} objects if \code{plot_by} is not \code{NULL}.
#' 
#' @examples 
#' # generate example fit results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4, FUN = function(x) rnorm(100)),
#'   # predicted response
#'   predictions = lapply(1:4, FUN = function(x) rnorm(100))
#' )
#' 
#' # generate example evaluation results data
#' eval_results <- list(
#'   `Prediction Errors` = summarize_pred_err(
#'     fit_results = fit_results, 
#'     truth_col = "y", 
#'     estimate_col = "predictions", 
#'     eval_id = "pred_err"
#'   )
#' )
#' 
#' # create plot using pre-computed evaluation results
#' plt <- plot_eval_summary(fit_results = fit_results, 
#'                          eval_tib = eval_results[["Prediction Errors"]],
#'                          eval_id = "pred_err",
#'                          show = c("point", "errorbar"),
#'                          facet_formula = ~ .metric)
#' # can customize plots using additional arguments or ggplot2::`+`
#' plt <- plot_eval_summary(fit_results = fit_results, 
#'                          eval_tib = eval_results[["Prediction Errors"]],
#'                          eval_id = "pred_err",
#'                          show = c("point", "errorbar"),
#'                          facet_formula = ~ .metric,
#'                          facet_type = "wrap",
#'                          errorbar_args = list(width = 0.5),
#'                          facet_args = list(scales = "free")) +
#'   ggplot2::labs(y = "Mean Prediction Error")
#' # can return interactive plotly plot
#' plt <- plot_eval_summary(fit_results = fit_results, 
#'                          eval_tib = eval_results[["Prediction Errors"]],
#'                          eval_id = "pred_err",
#'                          show = c("point", "errorbar"),
#'                          facet_formula = ~ .metric,
#'                          interactive = TRUE)
#' 
#' # create plot without pre-computing evaluation results; instead, need to 
#' # pass in summarize_* function and its arguments
#' plt <- plot_eval_summary(fit_results = fit_results,
#'                          eval_id = "pred_err",
#'                          eval_fun = "summarize_pred_err",
#'                          truth_col = "y", 
#'                          estimate_col = "predictions",
#'                          show = c("point", "errorbar"),
#'                          facet_formula = ~ .metric)
#' 
#' @importFrom rlang .data
#' @export
plot_eval_summary <- function(fit_results, eval_tib = NULL, eval_id = NULL, 
                              eval_fun = paste0("summarize_", eval_id),
                              vary_params = NULL,
                              show = c("boxplot", "point", "line", "bar", 
                                       "errorbar", "ribbon"),
                              x_str = "auto", y_str = "auto", 
                              err_sd_str = "auto", color_str = "auto", 
                              linetype_str = "auto", facet_formula = NULL, 
                              facet_type = c("grid", "wrap"), plot_by = "auto", 
                              add_ggplot_layers = NULL, boxplot_args = NULL, 
                              point_args = NULL, line_args = NULL, 
                              bar_args = NULL, errorbar_args = NULL,
                              ribbon_args = NULL, facet_args = NULL, 
                              interactive = FALSE, ...) {
  show <- match.arg(show, several.ok = TRUE)
  facet_type <- match.arg(facet_type)
  
  if (identical(y_str, "auto")) {
    y_str <- NULL
  }
  
  plt_df <- get_eval_tibble(fit_results = fit_results, eval_tib = eval_tib,
                            eval_id = eval_id, eval_fun = eval_fun, 
                            vary_params = vary_params, show = show, 
                            y_str = y_str, ...)
  
  dgps <- unique(plt_df$.dgp_name)
  methods <- unique(plt_df$.method_name)
  n_dgps <- length(dgps)
  n_methods <- length(methods)
  if (!is.null(eval_id)) {
    eval_id <- paste0("_", eval_id)
  }
  
  if (!is.null(vary_params)) {
    list_vary_params <- purrr::map_lgl(plt_df[vary_params], is.list)
    # if vary_param is a list-type column, coerce to string for plotting
    if (any(list_vary_params)) {
      group_ids <- dplyr::group_vars(plt_df)
      plt_df <- plt_df %>%
        dplyr::ungroup() %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(names(list_vary_params)),
                                    ~list_col_to_chr(.x, 
                                                     name = dplyr::cur_column(),
                                                     verbatim = TRUE))) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(group_ids)))
    }
    # if varying over multiple parameters, join column strings for plotting
    if (length(vary_params) > 1) {
      plt_df <- plt_df %>%
        tidyr::unite(col = ".vary_params", tidyselect::all_of(vary_params))
    }
  }
  
  # get default plot schematic if input is "auto"
  if (is.null(y_str) | identical(y_str, "auto")) {
    y_str <- paste0("mean", eval_id)
  }
  if (is.null(err_sd_str) | identical(err_sd_str, "auto")) {
    if (("errorbar" %in% show) && (y_str != paste0("mean", eval_id))) {
      stop("Must specify 'err_sd_str' to show error bars.")
    }
    err_sd_str <- paste0("sd", eval_id)
  }
  if (is.null(x_str) | identical(x_str, "auto")) {
    if (is.null(vary_params)) {
      if ((n_methods == 1) && (n_dgps > 1)) {
        x_str <- ".dgp_name"
      } else {
        x_str <- ".method_name"
      }
    } else if (length(vary_params) == 1) {
      x_str <- vary_params
    } else {
      x_str <- ".vary_params"
    }
  }
  if (identical(color_str, "auto")) {
    if ((x_str != ".method_name") && (n_methods > 1)) {
      color_str <- ".method_name"
    } else if ((x_str != ".dgp_name") && (n_dgps > 1)) {
      color_str <- ".dgp_name"
    } else if (!identical(x_str, vary_params) &
               !identical(x_str, ".vary_params")) {
      if (is.null(vary_params) | (length(vary_params) == 1)) {
        color_str <- vary_params
      } else {
        color_str <- ".vary_params"
      }
    } else {
      color_str <- NULL
    }
  }
  if (identical(linetype_str, "auto")) {
    linetype_str <- NULL
  }
  if (identical(plot_by, "auto")) {
    plt_args <- c(x_str, y_str, color_str, linetype_str,
                  as.character(facet_formula))
    if ((n_dgps > 1) && !(".dgp_name" %in% plt_args)) {
      plot_by <- ".dgp_name"
      plot_by_id <- plot_by
    } else if ((length(vary_params) == 1) && !(vary_params %in% plt_args)) {
      plot_by <- vary_params
      plot_by_id <- plot_by
    } else if ((length(vary_params) > 1) && !(".vary_params" %in% plt_args)) {
      plot_by <- ".vary_params"
      plot_by_id <- paste(vary_params, collapse = "_")
    } else {
      plot_by <- NULL
      plot_by_id <- plot_by
    }
    plt_df <- plt_df %>% dplyr::group_by(dplyr::across({{plot_by}}))
  } else {
    plt_df <- plt_df %>% dplyr::group_by(dplyr::across({{plot_by}}))
    if (identical(plot_by, ".vary_params")) {
      plot_by_id <- paste(vary_params, collapse = "_")
    } else {
      plot_by_id <- plot_by
    }
  }
  
  # helper function to make summary plot
  construct_plot <- function(plt_df) {
    plt <- ggplot2::ggplot(plt_df)
    base_aes <- vthemes::get_aesthetics(x_str = x_str, y_str = y_str, 
                               color_str = color_str, fill_str = color_str,
                               linetype_str = linetype_str)
    # add ggplot geom layers
    if ("ribbon" %in% show) {
      if (!is.null(color_str)) {
        ribbon_aes <- ggplot2::aes(
          x = .data[[x_str]], 
          ymin = .data[[y_str]] - .data[[err_sd_str]],
          ymax = .data[[y_str]] + .data[[err_sd_str]],
          fill = .data[[color_str]]
        )
      } else {
        ribbon_aes <- ggplot2::aes(
          x = .data[[x_str]], 
          ymin = .data[[y_str]] - .data[[err_sd_str]],
          ymax = .data[[y_str]] + .data[[err_sd_str]]
        )
      }
      plt <- plt + 
        do.call(ggplot2::geom_ribbon, 
                args = c(list(mapping = ribbon_aes), ribbon_args))
    }
    if ("errorbar" %in% show) {
      if (!is.null(color_str)) {
        errorbar_aes <- ggplot2::aes(
          x = .data[[x_str]], 
          ymin = .data[[y_str]] - .data[[err_sd_str]],
          ymax = .data[[y_str]] + .data[[err_sd_str]],
          color = .data[[color_str]]
        )
      } else {
        errorbar_aes <- ggplot2::aes(
          x = .data[[x_str]], 
          ymin = .data[[y_str]] - .data[[err_sd_str]],
          ymax = .data[[y_str]] + .data[[err_sd_str]]
        )
      }
      plt <- plt + 
        do.call(ggplot2::geom_errorbar, 
                args = c(list(mapping = errorbar_aes), errorbar_args))
    }
    if ("bar" %in% show) {
      bar_aes <- base_aes[names(base_aes) %in% c("x", "y", "colour", "fill")]
      plt <- plt +
        do.call(ggplot2::geom_bar,
                args = c(list(mapping = bar_aes, 
                              stat = "identity", position = "dodge"), 
                         bar_args))
    }
    if ("boxplot" %in% show) {
      if (is.null(color_str)) {
        group_str <- x_str
      } else {
        group_str <- sprintf("interaction(%s, %s)", x_str, color_str)
      }
      boxplot_aes <- vthemes::get_aesthetics(x_str = x_str, 
                                    y_str = paste0("raw", eval_id), 
                                    color_str = color_str, 
                                    group_str = group_str)
      plt <- plt +
        do.call(ggplot2::geom_boxplot,
                args = c(list(data = plt_df %>%
                                tidyr::unnest(.data[[paste0("raw", eval_id)]]),
                              mapping = boxplot_aes), 
                         boxplot_args))
    }
    if ("line" %in% show) {
      line_aes <- base_aes[names(base_aes) %in% c("x", "y", "colour", 
                                                  "linetype")]
      plt <- plt +
        do.call(ggplot2::geom_line,
                args = c(list(mapping = line_aes), line_args))
    }
    if ("point" %in% show) {
      point_aes <- base_aes[names(base_aes) %in% c("x", "y", "colour")]
      plt <- plt +
        do.call(ggplot2::geom_point,
                args = c(list(mapping = point_aes), point_args))
    }
    if (!is.null(facet_formula)) {
      if (facet_type == "grid") {
        plt <- plt + 
          do.call(ggplot2::facet_grid,
                  args = c(list(rows = facet_formula), facet_args))
      } else if (facet_type == "wrap") {
        plt <- plt + 
          do.call(ggplot2::facet_wrap,
                  args = c(list(facets = facet_formula), facet_args))
      }
    }
    # add theme
    plt <- plt + vthemes::theme_vmodern()
    if (!is.null(color_str)) {
      discrete <- !is.numeric(plt_df[[color_str]])
      plt <- plt + 
        vthemes::scale_color_vmodern(discrete = discrete) +
        vthemes::scale_fill_vmodern(discrete = discrete)
    }
    # add labels
    labels_ls <- purrr::map(
      list(x = x_str, y = y_str, color = color_str),
      function(l) {
        if (is.null(l)) {
          return(NULL)
        } else {
          l <- dplyr::case_when(
            identical(l, ".dgp_name") ~ "DGP",
            identical(l, ".method_name") ~ "Method",
            identical(l, paste("raw", eval_id)) ~ ifelse(is.null(eval_id),
                                                         "Value", eval_id),
            identical(l, ".vary_params") ~ paste(vary_params, collapse = "_"),
            TRUE ~ l
          )
          return(l)
        }
      }
    )
    plt <- plt + 
      ggplot2::labs(x = labels_ls$x, y = labels_ls$y, 
                    color = labels_ls$color, fill = labels_ls$color)
    if (!is.null(add_ggplot_layers)) {
      for (ggplot_layer in add_ggplot_layers) {
        plt <- plt + ggplot_layer
      }
    }
    return(plt)
  }
  
  # construct plots
  plt_ls <- dplyr::group_map(plt_df, ~construct_plot(.x), .keep = TRUE) 
  if (!is.null(plot_by)) {
    names(plt_ls) <- paste(plot_by_id, dplyr::group_keys(plt_df)[[plot_by]], 
                           sep = " = ")
    plt_ls <- purrr::map2(plt_ls, names(plt_ls),
                          function(plt, plt_name) {
                            return(plt + ggplot2::labs(title = plt_name))
                          })
  }
  
  if (interactive) {
    plt_ls <- purrr::map(plt_ls, ~plotly::ggplotly(.x))
  }
  if (length(plt_ls) == 1) {
    plt <- plt_ls[[1]]
  } else {
    plt <- plt_ls
  }
  return(plt)
}

#' Developer function for plotting results from particular replicate(s) in the 
#' \code{Experiment} fit.
#' 
#' @description A helper function for developing new \code{Visualizer} plotting
#'   functions that plot results from particular replicate(s) in the 
#'   \code{Experiment} fit. This function will construct one plot for each
#'   row in the \code{Experiment}'s \code{fit_results} from the specified
#'   replicates.
#'   
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @param reps Vector of replicates from which to plot results.
#' @param plot_fun The plotting function, which takes in the arguments
#'   \code{fit_results}, \code{vary_params}, and possibly others passed from 
#'   \code{...}.
#' @param ... Additional arguments to pass to \code{plot_fun()}.
#' 
#' @return If \code{interactive = TRUE}, returns a \code{plotly} object or 
#'   list of \code{plotly} objects if there are multiple replicates, DGPs, or 
#'   Methods to plot. If \code{interactive = FALSE}, returns a \code{ggplot}
#'   object or list of \code{ggplot} objects if there are multiple replicates,
#'   DGPs, or Methods to plot.
#'   
#' @examples 
#' # generate example fit results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4, FUN = function(x) rnorm(100)),
#'   # predicted response
#'   predictions = lapply(1:4, FUN = function(x) rnorm(100))
#' )
#' 
#' # function to plot scatter plot of y vs predictions
#' plot_fun <- function(fit_results, vary_params = NULL) {
#'   plt <- fit_results %>%
#'     tidyr::unnest(c("y", "predictions")) %>%
#'     ggplot2::ggplot() +
#'     ggplot2::aes(x = y, y = predictions) +
#'     ggplot2::geom_point() +
#'     ggplot2::labs(title = sprintf("DGP: %s | Method: %s | Rep: %s", 
#'                                   fit_results$.dgp_name,
#'                                   fit_results$.method_name,
#'                                   fit_results$.rep)) +
#'     vthemes::theme_vmodern()
#'   return(plt)
#' }
#' 
#' # returns the scatter plot for each (DGP, Method) combination from rep 1
#' plt <- plot_fit_results(fit_results, reps = 1, plot_fun = plot_fun)
#'   
#' @export
plot_fit_results <- function(fit_results, vary_params = NULL, reps = 1, 
                             plot_fun, interactive = FALSE, ...) {
  .rep <- NULL  # to fix no visible binding for global variable error
  dots_list <- list(...)
  if (identical(dots_list, list())) {
    dots_list <- NULL
  }
  plt_df <- fit_results %>%
    dplyr::filter(.rep %in% reps) %>%
    dplyr::rowwise()
  plt_ls <- dplyr::group_map(plt_df,
                             function(x, key) {
                               out <- do.call(plot_fun,
                                              c(list(fit_results = x,
                                                     vary_params = vary_params),
                                                dots_list))
                               return(out)
                             })
  plt_names <- list()
  if (length(unique(plt_df$.dgp_name)) > 1) {
    plt_names[["dgp"]] <- paste("DGP = ", plt_df$.dgp_name, sep = "")
  }
  if (length(unique(plt_df$.method_name)) > 1) {
    plt_names[["method"]] <- paste("Method = ", plt_df$.method_name, sep = "")
  }
  plt_names[["rep"]] <- paste("Rep = ", plt_df$.rep, sep = "")
  if (!is.null(vary_params)) {
    plt_names[["vary_params"]] <- purrr::map(
      vary_params,
      function(param_name) {
        if (is.list(plt_df[[param_name]])) {
          plt_col <- list_col_to_chr(plt_df[[param_name]], name = param_name,
                                     verbatim = TRUE)
        } else {
          plt_col <- plt_df[[param_name]]
        }
        return(paste(param_name, " = ", plt_col))
      }
    ) %>%
      purrr::reduce(paste, sep = " // ")
  }
  names(plt_ls) <- plt_names %>%
    purrr::reduce(paste, sep = " // ")
  
  if (interactive) {
    plt_ls <- purrr::map(plt_ls, ~plotly::ggplotly(.x))
  }
  return(plt_ls)
}

#' Get the summarized evaluation results tibble for plotting.
#' 
#' @description Helper function to get the summarized evaluation results 
#'   \code{tibble} for plotting. This function will compute the summarized
#'   evaluation results if they have not been computed previously. Otherwise,
#'   it will read in the previously computed results and compute and append any
#'   new results necessary to construct the specified plot.
#'
#' @inheritParams plot_eval_summary
#' 
#' @return A \code{tibble} with the summarized evaluation results.
#'   
#' @examples 
#' # generate example fit results data
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4, FUN = function(x) rnorm(100)),
#'   # predicted response
#'   predictions = lapply(1:4, FUN = function(x) rnorm(100))
#' )
#' 
#' # compute (from scratch) the evaluation results that are necessary for plotting
#' eval_tib <- get_eval_tibble(fit_results = fit_results, 
#'                             eval_tib = NULL,
#'                             eval_id = "pred_err",
#'                             eval_fun = "summarize_pred_err", 
#'                             show = c("point", "errorbar"),
#'                             y_str = NULL,  # or equivalently, "mean_pred_err"
#'                             # arguments to pass to `eval_fun`
#'                             truth_col = "y",
#'                             estimate_col = "predictions")
#' # this is equivalent to:
#' eval_tib2 <- summarize_pred_err(
#'   fit_results = fit_results, 
#'   truth_col = "y",
#'   estimate_col = "predictions",
#'   summary_funs = c("mean", "sd")
#' )
#' all.equal(eval_tib, eval_tib2)
#' 
#' # read in pre-computed evaluation results since it has everything needed for plotting
#' eval_tib <- get_eval_tibble(fit_results = fit_results,
#'                             eval_tib = eval_tib, 
#'                             eval_id = "pred_err",
#'                             eval_fun = "summarize_pred_err", 
#'                             show = c("point", "errorbar"),
#'                             y_str = NULL)  # or equivalently, "mean_pred_err"
#' all.equal(eval_tib, eval_tib2)
#' 
#' # if columns that are needed for plotting are missing in `eval_tib`, then
#' # `get_eval_tibble` will call the `eval_fun` and compute the necessary results
#' eval_tib <- get_eval_tibble(fit_results = fit_results, 
#'                             eval_tib = eval_tib %>% dplyr::select(-mean_pred_err),
#'                             eval_id = "pred_err",
#'                             eval_fun = "summarize_pred_err", 
#'                             show = c("point", "errorbar"),
#'                             y_str = NULL,  # or equivalently, "mean_pred_err"
#'                             # arguments to pass to `eval_fun`
#'                             truth_col = "y",
#'                             estimate_col = "predictions")
#' all.equal(eval_tib, eval_tib2)
#'   
#' @export
get_eval_tibble <- function(fit_results, eval_tib = NULL, eval_id = NULL, 
                            eval_fun = paste0("summarize_", eval_id), 
                            vary_params = NULL, show, y_str = NULL, ...) {
  
  if (!is.null(eval_id)) {
    eval_id <- paste0("_", eval_id)
  }
  
  summary_funs <- NULL
  if ("boxplot" %in% show) {
    if (!(paste0("raw", eval_id) %in% colnames(eval_tib))) {
      summary_funs <- c(summary_funs, "raw") 
    }
  }
  if (is.null(y_str) | identical(y_str, paste0("mean", eval_id))) {
    if (any(c("point", "line", "bar", "errorbar") %in% show)) {
      if (!(paste0("mean", eval_id) %in% colnames(eval_tib))) {
        summary_funs <- c(summary_funs, "mean")
      }
    }
    if (any(c("errorbar", "ribbon") %in% show)) {
      if (!(paste0("sd", eval_id) %in% colnames(eval_tib))) {
        summary_funs <- c(summary_funs, "sd")
      }
    }
  }
  
  # append new results to previous results
  if (!is.null(summary_funs) | is.null(eval_tib)) {
    new_eval_tib <- R.utils::doCall(eval_fun, fit_results = fit_results,
                                    vary_params = vary_params, 
                                    summary_funs = summary_funs, ...)
    if (!is.null(eval_tib)) {
      group_ids <- dplyr::group_vars(new_eval_tib)
      eval_tib <- dplyr::left_join(new_eval_tib, eval_tib, by = group_ids)
    } else {
      eval_tib <- new_eval_tib
    }
  }
  
  return(eval_tib)
}

#' Get dot (...) arguments.
#' 
#' @description Helper function to merge default and user-specified dot (...)
#'   arguments such that default arguments are overwritten by the user-specified
#'   arguments.
#'   
#' @param user_args List of user-specified dot (...) arguments.
#' @param default_args List of default dot (...) arguments.
#' 
#' @return A named list of arguments that includes all arguments from the user
#'   and the defaults, but with the user-specified arguments overwriting the
#'   defaults.
#'   
#' @examples
#' arg_list <- get_dot_args(user_args = list(a = 1, b = 2, c = 3), 
#'                          default_args = list(a = "a", d = "d"))
#' 
#' @export
get_dot_args <- function(user_args, default_args) {
  arg_list <- list()
  for (arg_name in unique(c(names(user_args), names(default_args)))) {
    if (arg_name %in% names(user_args)) {
      arg_list[[arg_name]] <- user_args[[arg_name]]
    } else {
      arg_list[[arg_name]] <- default_args[[arg_name]]
    }
  }
  return(arg_list)
}

#' Convert a list column to a readable character column.
#'
#' @description Convert a list-type column (typically in a tibble) to a
#'   character-type column. Often useful for plotting along this column.
#'
#' @param list_col A list-type column to be converted to character.
#' @param name Name of column. Used as a prefix in the returned character
#'   strings. Default is \code{NULL}, which adds no prefix.
#' @param verbatim If \code{TRUE}, paste list contents together into a character
#'   vector. If \code{FALSE} (default), map items in list to unique identifiers
#'   (i.e., 1, 2, 3, ...).
#'
#' @return A character vector of the same length as \code{list_col}.
#' 
#' @examples
#' # create example tibble with list columns to convert
#' plot_tib <- tibble::tibble(a = 1:3,
#'                            b = list(1, 2, 3),
#'                            c = list(1:2, 3:4, 5:6),
#'                            d = list(tibble::tibble(a = 1:3, 
#'                                                    b = c(4, 5, 6)),
#'                                     "abc",
#'                                     123))
#' # verbatim = TRUE pastes lists together verbatim
#' plot_tib_chr_verbatim <- plot_tib %>%
#'   dplyr::mutate(dplyr::across(tidyselect::everything(),
#'                               ~list_col_to_chr(.x, 
#'                                                name = dplyr::cur_column(),
#'                                                verbatim = TRUE)))
#' 
#' # verbatim = FALSE maps items in list to unique ids (i.e., 1, 2, 3, ...)
#' plot_tib_chr <- plot_tib %>%
#'   dplyr::mutate(dplyr::across(tidyselect::everything(),
#'                               ~list_col_to_chr(.x, 
#'                                                name = dplyr::cur_column(),
#'                                                verbatim = FALSE)))
#' 
#' @export
list_col_to_chr <- function(list_col, name = NULL, verbatim = FALSE) {
  if (verbatim) {
    str_col <- sapply(list_col,
                      function(x) {
                        paste0(name, paste(x, collapse = "_"))
                      })
  } else {
    unique_items <- unique(list_col)
    str_col <- sapply(list_col,
                      function(x) {
                        which(sapply(unique_items, function(y) identical(x, y)))
                      })
    str_col <- paste0(name, str_col)
  }
  return(str_col)
}
