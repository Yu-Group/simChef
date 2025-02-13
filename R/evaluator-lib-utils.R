#' Arguments that are shared by multiple `Evaluator` library functions.
#'
#' @name shared_eval_lib_args
#'
#' @param custom_summary_funs Named list of custom functions to summarize
#'   results. Names in the list should correspond to the name of the summary
#'   function. Values in the list should be a function that takes in one
#'   argument, that being the values of the evaluated metrics.
#' @param curve Either "ROC" or "PR" indicating whether to evaluate the ROC or
#'   Precision-Recall curve.
#' @param eval_id Character string. ID to be used as a suffix when naming result
#'   columns. Default `NULL` does not add any ID to the column names.
#' @param feature_col A character string identifying the column in
#'   `fit_results` with the feature names or IDs.
#' @param group_cols (Optional) A character string or vector specifying the
#'   column(s) to group rows by before evaluating metrics.
#'   This is useful for assessing within-group metrics.
#' @param na_rm A `logical` value indicating whether `NA` values
#'   should be stripped before the computation proceeds.
#' @param nested_cols (Optional) A character string or vector specifying the
#'   name of the column(s) in `fit_results` that need to be
#'   unnested before evaluating results. Default is `NULL`, meaning no
#'   columns in `fit_results` need to be unnested prior to computation.
#' @param summary_funs Character vector specifying how to summarize
#'   evaluation metrics. Must choose from a built-in library of summary
#'   functions - elements of the vector must be one of "mean", "median",
#'   "min", "max", "sd", "raw".
#' @param x_grid Vector of values between 0 and 1 at which to evaluate the ROC
#'   or PR curve. If `curve = "ROC"`, the provided vector of values are
#'   the FPR values at which to evaluate the TPR, and if `curve = "PR"`,
#'   the values are the recall values at which to evaluate the precision.
#'
#' @keywords internal
NULL


#' Developer function to construct basic Evaluator.
#'
#' @description Helper function for developing a new `Evaluator`
#'   that evaluates some function (e.g., metric) for each row in
#'   `fit_results`.
#'
#' @inheritParams shared_experiment_helpers_args
#' @inheritParams shared_eval_lib_args
#' @param fun Function to compute for each row in `fit_results`. This
#'   function can take in the following optional arguments: (1) `data` =
#'   `fit_results[i, ]`; (2) na_rm; (3) arguments specified via `...`
#'   and `fun_options`.
#' @param ... Named arguments, containing names of columns to pass to
#'   `fun`.
#' @param fun_options Named list of additional arguments to pass to `fun`.
#'
#' @examples
#' # generate example fit_results data for a regression problem
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
#' # evaluate root mean squared error for each row in fit_results
#' rmse_fun <- function(data, truth_col, estimate_col, na_rm = TRUE) {
#'   yardstick::rmse_vec(
#'     data[[truth_col]], data[[estimate_col]], na_rm = na_rm
#'   )
#' }
#' eval_results <- eval_constructor(
#'   fit_results = fit_results,
#'   fun = rmse_fun,
#'   truth_col = "y",
#'   estimate_col = "predictions"
#' ) |>
#'   tidyr::unnest(.eval_result)
#'
#' @export
eval_constructor <- function(fit_results, vary_params = NULL, fun,
                             nested_cols = NULL, ..., group_cols = NULL,
                             fun_options = NULL, na_rm = FALSE) {

  # dummies to fix R CMD check note on no visible binding for global variable
  .eval_result <- NULL

  eval_rowwise <- function(data) {
    if (!is.null(nested_cols)) {
      data <- data |> tidyr::unnest(tidyselect::all_of(nested_cols))
    }

    key_cols_ls <- rlang::list2(...)
    for (col in key_cols_ls) {
      if (!all(col %in% colnames(data))) {
        missing_col <- col[which(!(col %in% colnames(data)))[1]]
        stop(sprintf("No column named %s in fit_results.", missing_col))
      }
    }
    if (length(key_cols_ls) > 0) {
      key_cols_vec <- purrr::reduce(key_cols_ls, c)
    } else {
      key_cols_vec <- NULL
    }

    if (is.null(nested_cols)) {
      data <- data |>
        tidyr::unnest(tidyselect::all_of(c(key_cols_vec, group_cols)))
    }
    if (!is.null(group_cols)) {
      data <- data |>
        dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols)))
    }

    out <- R.utils::doCall(
      fun,
      args = c(list(data = data, na_rm = na_rm), key_cols_ls, fun_options)
    )
    return(out)
  }

  id_vars <- c(".rep", ".dgp_name", ".method_name", vary_params)
  eval_tib <- fit_results |>
    dplyr::mutate(
      .eval_result = purrr::map(
        1:nrow(fit_results), ~eval_rowwise(data = fit_results[.x, ])
      )
    ) |>
    dplyr::select(tidyselect::all_of(id_vars), .eval_result)
  return(eval_tib)
}


#' Developer function to add number of NAs to evaluator results
#'
#' @description A helper function to append rows with number of NAs (per group,
#'   if applicable) to evaluator results tibble.
#'
#' @inheritParams shared_eval_lib_args
#' @param out Evaluator results tibble to append number of NA results to.
#' @param data Data used to compute number of NAs.
#' @param value_col Character string, specifying the column used to compute
#'   the number of NAs.
#' @param ... Additional name-value pairs to pass to dplyr::mutate() to append
#'   columns.
#'
#' @returns Tibble with additional rows containing the new metric "num_na" and
#'   its corresponding ".estimate"
#'
#' @examples
#' # generate example fit_results data with NA responses
#' fit_results <- tibble::tibble(
#'   .rep = rep(1:2, times = 2),
#'   .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'   .method_name = c("Method"),
#'   # true response
#'   y = lapply(1:4, FUN = function(x) c(rnorm(100 - x), rep(NA, x))),
#'   # predicted response
#'   predictions = lapply(1:4, FUN = function(x) rnorm(100))
#' )
#'
#' # evaluate root mean squared error and number of NA responses for each row in
#' # fit_results
#' rmse_na_fun <- function(data, truth_col, estimate_col, na_rm = TRUE) {
#'   out <- tibble::tibble(
#'     .metric = "rmse",
#'     .estimate = yardstick::rmse_vec(
#'       data[[truth_col]], data[[estimate_col]], na_rm = na_rm
#'     )
#'   ) |>
#'     add_na_counts(data = data, value_col = truth_col, na_rm = na_rm)
#'   return(out)
#' }
#' eval_results <- eval_constructor(
#'   fit_results = fit_results,
#'   fun = rmse_na_fun,
#'   truth_col = "y",
#'   estimate_col = "predictions",
#'   na_rm = TRUE
#' ) |>
#'   tidyr::unnest(.eval_result)
#'
#' @importFrom rlang .data
#' @export
add_na_counts <- function(out, data, value_col, na_rm, ...) {
  if (na_rm) {
    na_counts <- data |>
      dplyr::summarise(.estimate = sum(is.na(.data[[value_col]]))) |>
      dplyr::mutate(.metric = "num_na", ...)
    out <- out |>
      dplyr::bind_rows(na_counts)
  }
  return(out)
}


#' Developer function for summarizing evaluation results.
#'
#' @description A helper function for developing new `Evaluator` functions
#'   that summarize results over pre-specified groups in a grouped
#'   `data.frame` (e.g., over multiple experimental replicates). This is
#'   often used in conjunction with `eval_constructor()`.
#'
#' @inheritParams shared_eval_lib_args
#' @param eval_data A grouped `data.frame` of evaluation results to
#'   summarize.
#' @param value_col Character string. Name of column in `eval_data` with
#'   values to summarize.
#'
#' @return A `tibble` containing the summarized results aggregated
#'   over the given groups. These columns correspond to the requested
#'   statistics in `summary_funs` and `custom_summary_funs` and end
#'   with the suffix specified by `eval_id`. Note that the group IDs are
#'   also retained in the returned `tibble`.
#'
#' @importFrom rlang .data
#'
#' @examples
#' # create example eval_data to summarize
#' eval_data <- tibble::tibble(.rep = rep(1:2, times = 2),
#'                             .dgp_name = c("DGP1", "DGP1", "DGP2", "DGP2"),
#'                             .method_name = "Method",
#'                             result = 1:4) |>
#'   dplyr::group_by(.dgp_name, .method_name)
#'
#' # summarize `result` column in eval_data
#' results <- eval_summarizer(eval_data = eval_data, eval_id = "res",
#'                            value_col = "result")
#'
#' # only compute mean and sd of `result` column in eval_data over given groups
#' results <- eval_summarizer(eval_data = eval_data, eval_id = "res",
#'                            value_col = "result",
#'                            summary_funs = c("mean", "sd"))
#'
#' # summarize `results` column using custom summary function
#' range_fun <- function(x) return(max(x) - min(x))
#' results <- eval_summarizer(eval_data = eval_data, value_col = "result",
#'                            custom_summary_funs = list(range = range_fun))
#'
#' @export
eval_summarizer <- function(eval_data, eval_id = NULL, value_col,
                            summary_funs = c("mean", "median", "min",
                                             "max", "sd", "raw"),
                            custom_summary_funs = NULL,
                            na_rm = FALSE) {
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  group_ids <- dplyr::group_vars(eval_data)
  if (length(group_ids) == 0) {
    abort("eval_data must be a grouped data.frame. Use dplyr::group_by() to specify groups.")
  }
  if (!is.null(eval_id)) {
    eval_id <- paste0("_", eval_id)
  }

  # summarize results according to summary_funs
  eval_out <- purrr::map(
    summary_funs,
    function(f) {
      summary_fun <- eval(parse(text = f))
      col_name <- paste0(f, eval_id)
      if (f == "raw") {
        eval_out <- eval_data |>
          dplyr::summarise(summary = list(.data[[value_col]]),
                           .groups = "keep")
      } else {
        eval_out <- eval_data |>
          dplyr::summarise(summary = summary_fun(.data[[value_col]],
                                                 na.rm = na_rm),
                           .groups = "keep")
      }
      return(eval_out |> dplyr::rename({{col_name}} := "summary"))
    }
  ) |>
    purrr::reduce(dplyr::left_join, by = group_ids)

  # summarize results according to custom_summary_funs
  if (!is.null(custom_summary_funs)) {
    if (is.null(names(custom_summary_funs))) {
      names(custom_summary_funs) <- paste0("eval_summary", eval_id,
                                           1:length(custom_summary_funs))
    } else if (any(names(custom_summary_funs) == "")) {
      empty_names <- which(names(custom_summary_funs) == "")
      names(custom_summary_funs)[empty_names] <- paste0("eval_summary", eval_id,
                                                        empty_names)
    }
    custom_eval_out <- purrr::map(
      1:length(custom_summary_funs),
      function(i) {
        summary_name <- names(custom_summary_funs)[i]
        summary_fun <- custom_summary_funs[[i]]
        eval_data |>
          dplyr::summarise({{summary_name}} := summary_fun(.data[[value_col]]),
                           .groups = "keep")
      }
    ) |>
      purrr::reduce(dplyr::left_join, by = group_ids)
    eval_out <- dplyr::left_join(eval_out, custom_eval_out, by = group_ids)
  }

  return(tibble::tibble(eval_out) |>
           dplyr::group_by(dplyr::across(tidyselect::all_of(group_ids))))
}


#' Rescale ROC/PR curves onto the same x-axis grid
#'
#' @description A helper function to map a ROC/PR curve with unique
#'   coordinates given in a data.frame onto a new set of x-axis values (i.e.,
#'   FPR for an ROC curve and recall for a PR curve).
#'
#' @param curve_data A `data.frame` containing the x- and y-coordinates
#'   that define the ROC/PR curve.
#' @param x_grid Vector of x-coordinates at which to evaluate ROC/PR curve
#' @param xvar Name of column in `curve_data` with the FPR values for an
#'   ROC curve or the recall values for a PR curve.
#' @param yvar Name of column in `curve_data` with the TPR values for an
#'   ROC curve or the precision values for a PR curve.
#'
#' @return A `data.frame` with the coordinates of the ROC/PR curve using
#'   the new x-axis scale. This `data.frame` has two columns with names
#'   given by those specified in `xvar` and `yvar`.
#'
#' @keywords internal
rescale_curve <- function(curve_data, x_grid, xvar, yvar) {
  # map curves onto same x-axis scale
  x_vals <- sort(unique(curve_data[[xvar]]))
  if (length(x_vals) <= length(x_grid)) {
    y_vals <- rep(NA, length(x_grid))
    y_cur <- max(curve_data[curve_data[[xvar]] == x_vals[1], yvar])
    y_vals[x_grid <= x_vals[1]] <- y_cur
    for (i in 2:length(x_vals)) {
      cur_window <- (x_grid < x_vals[i]) & is.na(y_vals)
      if (any(cur_window)) {
        y_vals[cur_window] <- y_cur
      }
      y_next <- curve_data[curve_data[[xvar]] == x_vals[i], yvar]
      if (xvar == "FPR") {
        y_vals[which(is.na(y_vals))[1]] <- min(y_next)
        y_cur <- max(y_next)
      } else if (xvar == "recall") {
        y_vals[which(is.na(y_vals))[1]] <- max(y_next)
        y_cur <- min(y_next)
      }
    }
  } else {
    y_vals <- c(max(curve_data[curve_data[[xvar]] <= x_grid[1], yvar]),
                rep(NA, length(x_grid) - 1))
    y_cur <- y_vals[1]
    for (i in 2:length(x_grid)) {
      x0 <- x_grid[i - 1]
      x1 <- x_grid[i]
      cur_window <- (curve_data[[xvar]] > x0) & (curve_data[[xvar]] <= x1)
      if (any(cur_window)) {
        if (xvar == "FPR") {
          y_vals[i] <- min(curve_data[cur_window, yvar])
          y_cur <- max(curve_data[cur_window, yvar])
        } else if (xvar == "recall") {
          y_vals[i] <- max(curve_data[cur_window, yvar])
          y_cur <- min(curve_data[cur_window, yvar])
        }
      } else {
        y_vals[i] <- y_cur
      }
    }
  }
  curve_data <- data.frame(
    .x_coord = x_grid,
    .y_coord = y_vals
  )
  return(curve_data |> stats::setNames(c(xvar, yvar)))
}

#----------------------------- Yardstick Helpers -------------------------------
#' Logic for `event_level` in custom `yardstick` metrics.
#'
#' @param xtab Frequency table from `table()`
#' @inheritParams yardstick::roc_auc
#'
#' @return Name of factor level to use as the "event" when computing evaluation
#'   metrics.
#' @keywords internal
event_col <- function(xtab, event_level) {
  if (identical(event_level, "first")) {
    colnames(xtab)[[1]]
  } else {
    colnames(xtab)[[2]]
  }
}

#' Helper function for constructing `finalize_estimator_internal` for
#'   custom `yardstick` metrics that are restricted to binary estimators.
#' @keywords internal
finalize_estimator_internal_constructor <- function(metric_dispatcher, x,
                                                    estimator) {
  yardstick::validate_estimator(estimator, estimator_override = "binary")

  if(!is.null(estimator)) {
    return(estimator)
  }

  lvls <- levels(x)

  if(length(lvls) > 2) {
    abort("A multiclass `truth` input was provided, but only `binary` is supported.")
  }

  "binary"
}

#' Helper function for constructing `metric_vec` methods for
#'   custom `yardstick` metrics.
#' @keywords internal
class_metric_vec_constructor <- function(name, fun, truth, estimate, estimator,
                                         na_rm, case_weights, event_level,
                                         ...) {
  estimator <- yardstick::finalize_estimator(
    truth, estimator, metric_class = name
  )
  yardstick::check_class_metric(truth, estimate, case_weights, estimator)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  fun(truth, estimate, event_level)
}

#-------------------------- Custom Yardstick Metrics ---------------------------
#-------------------------------------------------------------------------------
#' Number of true positives
#'
#' @description These functions calculate the [tp()] (number of true positives)
#'   of a measurement system compared to the reference results (the "truth").
#'
#' @inheritParams yardstick::ppv
#'
#' @returns
#' A `tibble` with columns `.metric`, `.estimator`, and
#'   `.estimate` with 1 row of values.
#'
#' For grouped data frames, the number of rows returned will be the same as the
#'   number of groups.
#'
#' For `tp_vec()`, a single `numeric` value (or `NA`).
#'
#' @examples
#' # Two class example data
#' two_class_example <- data.frame(
#'   truth = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE)),
#'   predicted = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE))
#' )
#'
#' # Compute number of true positives
#' tp(two_class_example, truth = truth, estimate = predicted)
#' tp_vec(two_class_example$truth, two_class_example$predicted)
#'
#' @export
tp <- function(data, ...) {
  UseMethod("tp")
}
tp <- yardstick::new_class_metric(tp, direction = "maximize")

#' @keywords internal
tp_impl <- function(truth, estimate, event_level) {
  xtab <- table(estimate, truth)
  col <- event_col(xtab, event_level)
  return(xtab[col, col])
}

#' @rdname tp
#' @export
tp.data.frame <- function(data, truth, estimate, estimator = NULL,
                          na_rm = FALSE, case_weights = NULL,
                          event_level = "first", ...) {
  yardstick::class_metric_summarizer(
    name = "tp",
    fn = tp_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !! rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname tp
#' @export
tp_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE,
                   case_weights = NULL, event_level = "first", ...) {
  class_metric_vec_constructor(
    "tp", fun = tp_impl, truth = truth,
    estimate = estimate, estimator = estimator,
    na_rm = na_rm, case_weights = case_weights,
    event_level = event_level, ...
  )
}

#' @keywords internal
finalize_estimator_internal.tp <- function(metric_dispatcher, x, estimator) {
  finalize_estimator_internal_constructor(metric_dispatcher, x, estimator)
}

#------------------------------------------------------------------------------
#' Number of false positives
#'
#' @description These functions calculate the [fp()] (number of false positives)
#'   of a measurement system compared to the reference results (the "truth").
#'
#' @inheritParams yardstick::ppv
#'
#' @returns
#' A `tibble` with columns `.metric`, `.estimator`, and
#'   `.estimate` with 1 row of values.
#'
#' For grouped data frames, the number of rows returned will be the same as the
#'   number of groups.
#'
#' For `fp_vec()`, a single `numeric` value (or `NA`).
#'
#' @examples
#' # Two class example data
#' two_class_example <- data.frame(
#'   truth = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE)),
#'   predicted = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE))
#' )
#'
#' # Compute number of false positives
#' fp(two_class_example, truth = truth, estimate = predicted)
#' fp_vec(two_class_example$truth, two_class_example$predicted)
#'
#' @export
fp <- function(data, ...) {
  UseMethod("fp")
}
fp <- yardstick::new_class_metric(fp, direction = "minimize")

#' @keywords internal
fp_impl <- function(truth, estimate, event_level) {
  xtab <- table(estimate, truth)
  col <- event_col(xtab, event_level)
  col2 <- setdiff(colnames(xtab), col)
  return(xtab[col, col2])
}

#' @rdname fp
#' @export
fp.data.frame <- function(data, truth, estimate, estimator = NULL,
                          na_rm = FALSE, case_weights = NULL,
                          event_level = "first", ...) {
  yardstick::class_metric_summarizer(
    name = "fp",
    fn = fp_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !! rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname fp
#' @export
fp_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE,
                   case_weights = NULL, event_level = "first", ...) {
  class_metric_vec_constructor(
    "fp", fun = fp_impl, truth = truth,
    estimate = estimate, estimator = estimator,
    na_rm = na_rm, case_weights = case_weights,
    event_level = event_level, ...
  )
}

#' @keywords internal
finalize_estimator_internal.fp <- function(metric_dispatcher, x, estimator) {
  finalize_estimator_internal_constructor(metric_dispatcher, x, estimator)
}

#------------------------------------------------------------------------------
#' Number of estimated positive cases
#'
#' @description These functions calculate the [pos()] (number of estimated
#'   positive cases) of a measurement system compared to the reference results
#'   (the "truth").
#'
#' @inheritParams yardstick::ppv
#'
#' @returns
#' A `tibble` with columns `.metric`, `.estimator`, and
#'   `.estimate` with 1 row of values.
#'
#' For grouped data frames, the number of rows returned will be the same as the
#'   number of groups.
#'
#' For `pos_vec()`, a single `numeric` value (or `NA`).
#'
#' @examples
#' # Two class example data
#' two_class_example <- data.frame(
#'   truth = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE)),
#'   predicted = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE))
#' )
#'
#' # Compute number of estimated "positive" classes
#' pos(two_class_example, truth = truth, estimate = predicted)
#' pos_vec(two_class_example$truth, two_class_example$predicted)
#'
#' @export
pos <- function(data, ...) {
  UseMethod("pos")
}
pos <- yardstick::new_class_metric(pos, direction = "minimize")

#' @keywords internal
pos_impl <- function(truth, estimate, event_level) {
  xtab <- table(estimate, truth)
  col <- event_col(xtab, event_level)
  return(sum(xtab[col, ]))
}

#' @rdname pos
#' @export
pos.data.frame <- function(data, truth, estimate, estimator = NULL,
                           na_rm = FALSE, case_weights = NULL,
                           event_level = "first", ...) {
  yardstick::class_metric_summarizer(
    name = "pos",
    fn = pos_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !! rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname pos
#' @export
pos_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE,
                    case_weights = NULL, event_level = "first", ...) {
  class_metric_vec_constructor(
    "pos", fun = pos_impl, truth = truth,
    estimate = estimate, estimator = estimator,
    na_rm = na_rm, case_weights = case_weights,
    event_level = event_level, ...
  )
}

#' @keywords internal
finalize_estimator_internal.pos <- function(metric_dispatcher, x, estimator) {
  finalize_estimator_internal_constructor(metric_dispatcher, x, estimator)
}

#------------------------------------------------------------------------------
#' Number of estimated negative cases
#'
#' @description These functions calculate the [neg()] (number of estimated
#'   negative cases) of a measurement system compared to the reference results
#'   (the "truth").
#'
#' @inheritParams yardstick::ppv
#'
#' @returns
#' A `tibble` with columns `.metric`, `.estimator`, and
#'   `.estimate` with 1 row of values.
#'
#' For grouped data frames, the number of rows returned will be the same as the
#'   number of groups.
#'
#' For `neg_vec()`, a single `numeric` value (or `NA`).
#'
#' @examples
#' # Two class example data
#' two_class_example <- data.frame(
#'   truth = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE)),
#'   predicted = as.factor(sample(c("Class1", "Class2"), 100, replace = TRUE))
#' )
#'
#' # Compute number of estimated "negative" classes
#' neg(two_class_example, truth = truth, estimate = predicted)
#' neg_vec(two_class_example$truth, two_class_example$predicted)
#'
#' @export
neg <- function(data, ...) {
  UseMethod("neg")
}
neg <- yardstick::new_class_metric(neg, direction = "maximize")

#' @keywords internal
neg_impl <- function(truth, estimate, event_level) {
  xtab <- table(estimate, truth)
  col <- event_col(xtab, event_level)
  col2 <- setdiff(colnames(xtab), col)
  return(sum(xtab[col2, ]))
}

#' @rdname neg
#' @export
neg.data.frame <- function(data, truth, estimate, estimator = NULL,
                           na_rm = FALSE, case_weights = NULL,
                           event_level = "first", ...) {
  yardstick::class_metric_summarizer(
    name = "neg",
    fn = neg_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    case_weights = !! rlang::enquo(case_weights),
    event_level = event_level
  )
}

#' @rdname neg
#' @export
neg_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE,
                    case_weights = NULL, event_level = "first", ...) {
  class_metric_vec_constructor(
    "neg", fun = neg_impl, truth = truth,
    estimate = estimate, estimator = estimator,
    na_rm = na_rm, case_weights = case_weights,
    event_level = event_level, ...
  )
}

#' @keywords internal
finalize_estimator_internal.neg <- function(metric_dispatcher, x, estimator) {
  finalize_estimator_internal_constructor(metric_dispatcher, x, estimator)
}
