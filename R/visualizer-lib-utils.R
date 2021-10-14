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
#'   x-axis.
#' @param y_str (Optional) Name of column in \code{eval_tib} to plot on the
#'   y-axis if \code{show} is anything but "boxplot".
#' @param err_sd_str (Optional) Name of column in \code{eval_tib} containing the
#'   standard deviations of \code{y_str}. Used for plotting the errorbar and
#'   ribbon ggplot layers.
#' @param color_str (Optional) Name of column in \code{eval_tib} to use for the
#'   color and fill aesthetics when plotting.
#' @param linetype_str (Optional) Name of column in \code{eval_tib} to use for
#'   the linetype aesthetic when plotting. Used only when \code{show = "line"}.
#' @param facet_wrap_formula (Optional) Formula for \code{ggplot2::facet_wrap()}
#'   if need be.
#' @param facet_grid_formula (Optional) Formula for \code{ggplot2::facet_grid()}
#'   if need be.
#' @param plot_by (Optional) Name of column in \code{eval_tib} to use for
#'   subsetting data and creating different plots for each unique value.
#' @param add_ggplot_layers Additional layers to add to a ggplot object via 
#'   \code{+}.
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
#' @importFrom rlang .data
#' @export
plot_eval_summary <- function(fit_results, eval_tib = NULL, eval_id = NULL, 
                              eval_fun = paste0("summarize_", eval_id),
                              vary_params = NULL,
                              show = c("boxplot", "point", "line", "bar", 
                                       "errorbar", "ribbon"),
                              x_str = NULL, y_str = NULL, err_sd_str = NULL, 
                              color_str = NULL, linetype_str = NULL,
                              facet_wrap_formula = NULL, 
                              facet_grid_formula = NULL, plot_by = NULL, 
                              add_ggplot_layers = NULL, boxplot_args = NULL, 
                              point_args = NULL, line_args = NULL, 
                              bar_args = NULL, errorbar_args = NULL,
                              ribbon_args = NULL, facet_args = NULL, 
                              interactive = FALSE, ...) {
  show <- match.arg(show, several.ok = TRUE)
  
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
      plt_df <- plt_df %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(names(list_vary_params)),
                                    ~list_col_to_chr(.x, 
                                                     name = dplyr::cur_column(),
                                                     verbatim = TRUE)))
    }
    # if varying over multiple parameters, join column strings for plotting
    if (length(vary_params) > 1) {
      plt_df <- plt_df %>%
        tidyr::unite(col = ".vary_params", tidyselect::all_of(vary_params))
    }
  }
  
  # get default plot schematic if input is NULL
  if (is.null(y_str)) {
    y_str <- paste0("mean", eval_id)
  }
  if (is.null(err_sd_str)) {
    if (("errorbar" %in% show) && (y_str != paste0("mean", eval_id))) {
      stop("Must specify 'err_sd_str' to show error bars.")
    }
    err_sd_str <- paste0("sd", eval_id)
  }
  if (is.null(x_str)) {
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
  if (is.null(color_str)) {
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
    }
  }
  if (is.null(plot_by)) {
    plt_args <- c(x_str, y_str, color_str, linetype_str,
                  as.character(facet_wrap_formula),
                  as.character(facet_grid_formula))
    if ((n_dgps > 1) && !(".dgp_name" %in% plt_args)) {
      plot_by <- ".dgp_name"
    } else if ((length(vary_params) == 1) && !(vary_params %in% plt_args)) {
      plot_by <- vary_params
    } else if ((length(vary_params) > 1) && !(".vary_params" %in% plt_args)) {
      plot_by <- ".vary_params"
    }
    plt_df <- plt_df %>% dplyr::ungroup()
    plot_by_id <- "id"
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
    base_aes <- get_aesthetics(x_str = x_str, y_str = y_str, 
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
      boxplot_aes <- get_aesthetics(x_str = x_str, 
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
    if (!is.null(facet_wrap_formula)) {
      plt <- plt + 
        do.call(ggplot2::facet_wrap,
                args = c(list(facets = facet_wrap_formula), facet_args))
    } else if (!is.null(facet_grid_formula)) {
      plt <- plt + 
        do.call(ggplot2::facet_grid,
                args = c(list(rows = facet_grid_formula), facet_args))
    }
    # add theme
    plt <- plt + pretty_ggplot_theme()
    if (!is.null(color_str)) {
      if (is.character(plt_df[[color_str]])) {
        plt_df[[color_str]] <- as.factor(plt_df[[color_str]])
      }
      plt <- plt + 
        pretty_ggplot_color(color = plt_df[[color_str]]) +
        pretty_ggplot_fill(fill = plt_df[[color_str]])
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
      plt <- plt + add_ggplot_layers
    }
    return(plt)
  }
  
  # construct plots
  plt_ls <- dplyr::group_map(plt_df, ~construct_plot(.x), .keep = TRUE) 
  if (!is.null(plot_by)) {
    names(plt_ls) <- paste(plot_by_id, dplyr::group_keys(plt_df)[[plot_by]], 
                           sep = " = ")
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

#' Get list of aesthetics to add to ggplot.
#' 
#' @description Helper function to ignore \code{NULL} inputs when adding
#'   aesthetics to a ggplot.
#'   
#' @param x_str Character string specifying the name of the data column to plot
#'   on the x-axis.
#' @param y_str Character string specifying the name of the data column to plot
#'   on the y-axis.
#' @param color_str Character string specifying the name of the data column to
#'   use for the color aesthetic.
#' @param fill_str Character string specifying the name of the data column to 
#'   use for the fill aesthetic.
#' @param group_str Character string specifying the name of the data column to 
#'   use for the group aesthetic.
#' @param linetype_str Character string specifying the name of the data column 
#'   to use for the linetype aesthetic.
#'   
#' @return A [ggplot2::aes()] object.
#' 
#' @keywords internal
get_aesthetics <- function(x_str = NULL, y_str = NULL,
                           color_str = NULL, fill_str = NULL,
                           group_str = NULL, linetype_str = NULL) {
  aes_list <- list()
  if (!is.null(x_str)) {
    aes_list$x <- substitute(.data[[x_str]], list(x_str = x_str))
  }
  if (!is.null(y_str)) {
    aes_list$y <- substitute(.data[[y_str]], list(y_str = y_str))
  }
  if (!is.null(color_str)) {
    aes_list$color <- substitute(.data[[color_str]], 
                                 list(color_str = color_str))
  }
  if (!is.null(fill_str)) {
    aes_list$fill <- substitute(.data[[fill_str]], list(fill_str = fill_str))
  }
  if (!is.null(linetype_str)) {
    aes_list$linetype <- substitute(as.factor(.data[[linetype_str]]),
                                    list(linetype_str = linetype_str))
  }
  if (!is.null(group_str)) {
    aes_list$group <- substitute(.data[[group_str]],
                                 list(group_str = group_str))
  }
  return(do.call(ggplot2::aes, aes_list))
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
  if (!is.null(summary_funs)) {
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
