#' Arguments that are shared by multiple \code{Visualizer} library functions.
#'
#' @name shared_viz_lib_args
#' 
#' @param eval_name Name of \code{Evaluator} containing results to plot. If
#'   \code{NULL}, the data used for plotting is computed from scratch via
#'   \code{eval_fun}.
#' @param eval_fun Character string, specifying the function used to compute
#'   the data used for plotting if \code{eval_name = NULL}. If \code{eval_name}
#'   is not \code{NULL}, this argument is ignored.
#' @param eval_fun_options List of named arguments to pass to \code{eval_fun}.
#' @param interactive Logical. If \code{TRUE}, returns interactive \code{plotly}
#'   plots. If \code{FALSE}, returns static \code{ggplot} plots.
#' @param curve Either "ROC" or "PR" indicating whether to plot the ROC or 
#'   Precision-Recall curve.
#' @param show Character vector with elements being one of "boxplot", "point",
#'   "line", "bar", "errorbar", "ribbon" indicating what plot layer(s) to
#'   construct.
#' @param ... Additional arguments to pass to \code{plot_eval_constructor()}.
#'
#' @keywords internal
NULL

#' Developer function for plotting results from Evaluator.
#' 
#' @description A helper function for developing new \code{Visualizer} plotting
#'   functions that plot the summarized evaluation results as a boxplot, 
#'   scatter plot, line plot, or bar plot with or without 1 SD error 
#'   bars/ribbons. This function accepts either (1) the name(s) of the
#'   \code{Evaluator(s)} to plot (\code{eval_names}) or (2) a tibble containing
#'   the summarized evaluation results to plot (\code{plot_data}).
#'   
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#' @param eval_names (Optional) A character vector or string, specifying the
#'   name(s) of \code{Evaluator(s)} to plot. If multiple \code{Evaluators} are
#'   specified, these result \code{tibbles} are concatenated along the rows.
#'   Must provide either \code{eval_names} or \code{plot_data}.
#' @param plot_data (Optional) \code{Tibble} (typically from the output of
#'   \code{eval_summary_constructor}) containing the summarized evaluation
#'   results to plot. Must provide either \code{eval_names} or \code{plot_data}.
#'   If \code{eval_names} is provided, this argument is ignored.
#' @param eval_id (Optional) Character string. ID used as the suffix for naming
#'   columns in evaluation results tibble. If \code{eval_summary_constructor()}
#'   was used to construct the \code{Evaluator}, this should be the same as the
#'   \code{eval_id} argument in \code{eval_summary_constructor()}. Only used to
#'   assign default (i.e., "auto") aesthetics in ggplot.
#' @param x_str (Optional) Name of column in data frame to plot on the
#'   x-axis. Default "auto" chooses what to plot on the x-axis automatically.
#' @param y_str (Optional) Name of column in data frame to plot on the
#'   y-axis if \code{show} is anything but "boxplot". Default "auto" chooses 
#'   what to plot on the y-axis automatically.
#' @param y_boxplot_str (Optional) Name of column in data frame to plot on
#'   the y-axis if \code{show} is "boxplot". Default "auto" chooses
#'   what to plot on the y-axis automatically.
#' @param err_sd_str (Optional) Name of column in data frame containing the
#'   standard deviations of \code{y_str}. Used for plotting the errorbar and
#'   ribbon ggplot layers. Default "auto" chooses what column to use for the
#'   standard deviations automatically.
#' @param color_str (Optional) Name of column in data frame to use for the
#'   color and fill aesthetics when plotting. Default "auto" chooses what to 
#'   use for the color and fill aesthetics automatically. Use \code{NULL} to
#'   avoid adding any color and fill aesthetic.
#' @param linetype_str (Optional) Name of column in data frame to use for
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
#' @param ... Not used.
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
#' # create plot using name of Evaluator
#' plt <- plot_eval_constructor(eval_results = eval_results,
#'                              eval_name = "Prediction Errors",
#'                              eval_id = "pred_err",
#'                              show = c("point", "errorbar"),
#'                              facet_formula = ~ .metric)
#' # create plot using pre-computed evaluation results
#' plt <- plot_eval_constructor(plot_data = eval_results[["Prediction Errors"]],
#'                              eval_id = "pred_err",
#'                              show = c("point", "errorbar"),
#'                              facet_formula = ~ .metric)
#' # can customize plots using additional arguments or ggplot2::`+`
#' plt <- plot_eval_constructor(eval_results = eval_results,
#'                              eval_name = "Prediction Errors",
#'                              eval_id = "pred_err",
#'                              show = c("point", "errorbar"),
#'                              facet_formula = ~ .metric,
#'                              facet_type = "wrap",
#'                              errorbar_args = list(width = 0.5),
#'                              facet_args = list(scales = "free")) +
#'   ggplot2::labs(y = "Mean Prediction Error")
#' # can return interactive plotly plot
#' plt <- plot_eval_constructor(eval_results = eval_results,
#'                              eval_name = "Prediction Errors",
#'                              eval_id = "pred_err",
#'                              show = c("point", "errorbar"),
#'                              facet_formula = ~ .metric,
#'                              interactive = TRUE)
#' 
#' @importFrom rlang .data
#' @export
plot_eval_constructor <- function(eval_results = NULL, eval_names = NULL,
                                  plot_data = NULL, eval_id = NULL,
                                  vary_params = NULL,
                                  show = c("boxplot", "point", "line", "bar",
                                           "errorbar", "ribbon"),
                                  x_str = "auto",
                                  y_str = "auto", y_boxplot_str = "auto",
                                  err_sd_str = "auto",
                                  color_str = "auto", linetype_str = "auto",
                                  facet_formula = NULL,
                                  facet_type = c("grid", "wrap"),
                                  plot_by = "auto",
                                  add_ggplot_layers = NULL, boxplot_args = NULL,
                                  point_args = NULL, line_args = NULL,
                                  bar_args = NULL, errorbar_args = NULL,
                                  ribbon_args = NULL, facet_args = NULL,
                                  interactive = FALSE, ...) {
  show <- match.arg(show, several.ok = TRUE)
  facet_type <- match.arg(facet_type)
  
  if (!is.null(eval_results) && !is.null(eval_names)) {
    plt_df <- dplyr::bind_rows(eval_results[eval_names])
  } else if (!is.null(plot_data)) {
    plt_df <- plot_data
  } else {
    stop("Must specify either (1) `eval_results` and `eval_names` or (2) `plot_data`.")
  }
  
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
  if (is.null(y_str) || identical(y_str, "auto")) {
    y_str <- paste0("mean", eval_id)
  }
  if (is.null(y_boxplot_str) || identical(y_boxplot_str, "auto")) {
    y_boxplot_str <- paste0("raw", eval_id)
  }
  if (is.null(err_sd_str) || identical(err_sd_str, "auto")) {
    if (("errorbar" %in% show) && (y_str != paste0("mean", eval_id))) {
      abort("Must specify 'err_sd_str' to show error bars.")
    }
    err_sd_str <- paste0("sd", eval_id)
  }
  if (is.null(x_str) || identical(x_str, "auto")) {
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
    } else if (!identical(x_str, vary_params) &&
               !identical(x_str, ".vary_params")) {
      if (is.null(vary_params) || (length(vary_params) == 1)) {
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

  if (!is.null(x_str)) {
    x <- rlang::sym(x_str)
  } else {
    x <- x_str
  }
  if (!is.null(y_str)) {
    y <- rlang::sym(y_str)
  } else {
    y <- y_str
  }
  if (!is.null(y_boxplot_str)) {
    y_boxplot <- rlang::sym(y_boxplot_str)
  } else {
    y_boxplot <- y_boxplot_str
  }
  if (!is.null(err_sd_str)) {
    err_sd <- rlang::sym(err_sd_str)
  } else {
    err_sd <- err_sd_str
  }
  if (!is.null(linetype_str)) {
    linetype <- rlang::sym(linetype_str)
  } else {
    linetype <- linetype_str
  }
  if (!is.null(color_str)) {
    color <- rlang::sym(color_str)
  } else {
    color <- color_str
  }
  fill <- color

  # helper function to make summary plot
  construct_plot <- function(plt_df) {
    plt <- ggplot2::ggplot(plt_df)
    # add ggplot geom layers
    if ("ribbon" %in% show) {
      ribbon_aes <- ggplot2::aes(
        x = !!x, ymin = !!y - !!err_sd, ymax = !!y + !!err_sd, fill = !!color
      )
      plt <- plt +
        do.call(
          ggplot2::geom_ribbon,
          args = c(list(mapping = ribbon_aes), ribbon_args)
        )
    }
    if ("errorbar" %in% show) {
      errorbar_aes <- ggplot2::aes(
        x = !!x, ymin = !!y - !!err_sd, ymax = !!y + !!err_sd, color = !!color
      )
      plt <- plt +
        do.call(
          ggplot2::geom_errorbar,
          args = c(list(mapping = errorbar_aes), errorbar_args)
        )
    }
    if ("bar" %in% show) {
      bar_aes <- ggplot2::aes(
        x = !!x, y = !!y, color = !!color, fill = !!fill
      )
      plt <- plt +
        do.call(
          ggplot2::geom_bar,
          args = c(
            list(mapping = bar_aes, stat = "identity", position = "dodge"),
            bar_args
          )
        )
    }
    if ("boxplot" %in% show) {
      if (is.null(color_str)) {
        boxplot_aes <- ggplot2::aes(
          x = !!x, y = !!y_boxplot, color = !!color, group = !!x
        )
      } else {
        boxplot_aes <- ggplot2::aes(
          x = !!x, y = !!y_boxplot, color = !!color,
          group = interaction(!!x, !!color)
        )
      }
      plt <- plt +
        do.call(
          ggplot2::geom_boxplot,
          args = c(
            list(data = plt_df %>%
                   tidyr::unnest(tidyselect::all_of(y_boxplot_str)),
                 mapping = boxplot_aes),
            boxplot_args
          )
        )
    }
    if ("line" %in% show) {
      line_aes <- ggplot2::aes(
        x = !!x, y = !!y, color = !!color, linetype = !!linetype
      )
      plt <- plt +
        do.call(
          ggplot2::geom_line,
          args = c(list(mapping = line_aes), line_args)
        )
    }
    if ("point" %in% show) {
      point_aes <- ggplot2::aes(
        x = !!x, y = !!y, color = !!color
      )
      plt <- plt +
        do.call(
          ggplot2::geom_point,
          args = c(list(mapping = point_aes), point_args)
        )
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
      list(xlab = x_str, ylab = y_str, colorlab = color_str),
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
      ggplot2::labs(x = labels_ls$xlab, y = labels_ls$ylab,
                    color = labels_ls$colorlab, fill = labels_ls$colorlab)
    if (("boxplot" %in% show) && (y_boxplot_str == paste0("raw", eval_id))) {
      plt <- plt +
        ggplot2::labs(y = substr(eval_id, start = 2, stop = nchar(eval_id)))
    }
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
    rlang::check_installed(
      "plotly",
      reason = "to return an interactive plot (with `interactive = TRUE`)."
    )
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
#'                                   fit_results$.rep))
#'   return(plt)
#' }
#' 
#' # returns the scatter plot for each (DGP, Method) combination from rep 1
#' plt <- plot_fit_constructor(fit_results, reps = 1, plot_fun = plot_fun)
#'   
#' @export
plot_fit_constructor <- function(fit_results, vary_params = NULL, reps = 1,
                                 plot_fun, interactive = FALSE, ...) {
  .rep <- NULL  # to fix no visible binding for global variable error
  dots_list <- rlang::list2(...)
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
    rlang::check_installed(
      "plotly",
      reason = "to return an interactive plot (with `interactive = TRUE`)."
    )
    plt_ls <- purrr::map(plt_ls, ~plotly::ggplotly(.x))
  }
  return(plt_ls)
}


#' Developer function to get data for plotting in Visualizers.
#' 
#' @description A helper function to retrieve data for plotting in
#'   \code{Visualizers}. It first looks for the data in \code{eval_results} by
#'   name (\code{eval_name}). If this is not provided, then it uses the
#'   function + arguments specified by \code{eval_fun} and
#'   \code{eval_fun_options} to compute the plotting data from
#'   \code{fit_results}.
#'
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_viz_lib_args
#'
#' @returns If \code{eval_name} is not \code{NULL}, returns the list
#'   component in \code{eval_results} named \code{eval_name}. Otherwise,
#'   returns the result of \code{eval_fun()}.
#'
#' @export
get_plot_data <- function(fit_results = NULL,
                          eval_results = NULL, eval_name = NULL,
                          eval_fun = NULL, eval_fun_options = NULL) {
  if (!is.null(eval_results) && !is.null(eval_name)) {
    plot_data <- eval_results[[eval_name]]
  } else if (!is.null(eval_fun) && !is.null(fit_results)) {
    plot_data <- R.utils::doCall(
      eval_fun, args = c(list(fit_results = fit_results), eval_fun_options)
    )
  }
  return(plot_data)
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
