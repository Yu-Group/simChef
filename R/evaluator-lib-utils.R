#' Arguments that are shared by multiple \code{Evaluator} library functions.
#'
#' @name shared_eval_lib_args
#' 
#' @param custom_summary_funs Named list of custom functions to summarize 
#'   results. Names in the list should correspond to the name of the summary
#'   function. Values in the list should be a function that takes in one 
#'   argument, that being the values of the evaluated metrics.
#' @param curve Either "ROC" or "PR" indicating whether to evaluate the ROC or 
#'   Precision-Recall curve.
#' @param feature_col A character string identifying the column in 
#'   \code{fit_results} with the feature names or IDs.
#' @param na_rm A \code{logical} value indicating whether \code{NA} values 
#'   should be stripped before the computation proceeds.
#' @param nested_data (Optional) Character string. If specified, should be the
#'   name of the column in \code{fit_results} containing columns that must be
#'   unnested before evaluating results. Default is \code{NULL}, meaning no
#'   columns in \code{fit_results} need to be unnested prior to computation.
#' @param options A list of named options to pass to \code{pROC::roc()} such as 
#'   \code{smooth}. These options should not include \code{response}, 
#'   \code{predictor}, \code{levels}, \code{quiet}, or \code{direction}. This
#'   argument is only used when computing the ROC and is ignored otherwise.
#' @param summary_funs Character vector specifying how to summarize 
#'   evaluation metrics. Must choose from a built-in library of summary
#'   functions - elements of the vector must be one of "mean", "median",
#'   "min", "max", "sd", "raw".
#' @param x_grid Vector of values between 0 and 1 at which to evaluate the ROC 
#'   or PR curve. If \code{curve = "ROC"}, the provided vector of values are
#'   the FPR values at which to evaluate the TPR, and if \code{curve = "PR"},
#'   the values are the recall values at which to evaluate the precision.
#'   
NULL

#' Developer function for summarizing evaluation results.
#' 
#' @description A helper function for developing new \code{Evaluator} functions
#'   that summarize results over pre-specified groups in a grouped
#'   \code{data.frame} (e.g., over multiple experimental replicates).
#'
#' @inheritParams shared_eval_lib_args
#' @param eval_data A grouped \code{data.frame} of evaluation results to 
#'   summarize.
#' @param eval_id Character string. ID to be used as a suffix when naming result
#'   columns. Default \code{NULL} does not add any ID to the column names.
#' @param value_col Character string. Name of column in \code{eval_data} with
#'   values to summarize.
#' 
#' @return A \code{tibble} containing the summarized results aggregated
#'   over the given groups. These columns correspond to the requested
#'   statistics in \code{summary_funs} and \code{custom_summary_funs} and end
#'   with the suffix specified by \code{eval_id}. Note that the group IDs are 
#'   also retained in the returned \code{tibble}.
#' 
#' @importFrom rlang .data
#' @export
summarize_eval_results <- function(eval_data, eval_id = NULL, value_col,
                                   summary_funs = c("mean", "median", "min",
                                                    "max", "sd", "raw"), 
                                   custom_summary_funs = NULL,
                                   na_rm = FALSE) {
  summary_funs <- match.arg(summary_funs, several.ok = TRUE)
  group_ids <- dplyr::group_vars(eval_data)
  if (length(group_ids) == 0) {
    stop("eval_data must be a grouped data.frame. Use dplyr::group_by() to ",
         "specify groups.")
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
        eval_out <- eval_data %>%
          dplyr::summarise(summary = list(.data[[value_col]]), 
                           .groups = "keep")
      } else {
        eval_out <- eval_data %>%
          dplyr::summarise(summary = summary_fun(.data[[value_col]], 
                                                 na.rm = na_rm),
                           .groups = "keep")
      }
      return(eval_out %>% dplyr::rename({{col_name}} := "summary"))
    }
  ) %>%
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
        eval_data %>% 
          dplyr::summarise({{summary_name}} := summary_fun(.data[[value_col]]),
                           .groups = "keep")
      }
    ) %>%
      purrr::reduce(dplyr::left_join, by = group_ids)
    eval_out <- dplyr::left_join(eval_out, custom_eval_out, by = group_ids) %>%
      dplyr::group_by(dplyr::across({{group_ids}}))
  }
  
  return(tibble::tibble(eval_out))
}

#----------------------------- Yardstick Helpers -------------------------------
#' Logic for \code{event_level} in custom \code{yardstick} metrics.
#' 
#' @param xtab Frequency table from \code{table()}
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

#' Helper function for constructing \code{finalize_esitmator_internal} for
#'   custom \code{yardstick} metrics.
#' @keywords internal
finalize_estimator_internal_constructor <- function(metric_dispatcher, x,
                                                    estimator) {
  yardstick::validate_estimator(estimator, estimator_override = "binary")
  
  if(!is.null(estimator)) {
    return(estimator)
  }
  
  lvls <- levels(x)
  
  if(length(lvls) > 2) {
    stop("A multiclass `truth` input was provided, but only `binary` is supported.")
  } 
  
  "binary"
}

#' Helper function for constructing \code{metric_vec} methods for
#'   custom \code{yardstick} metrics.
#' @keywords internal
metric_vec_constructor <- function(name, fun, truth, estimate, estimator, na_rm,
                                   event_level, ...) {
  estimator <- yardstick::finalize_estimator(truth, estimator, 
                                             metric_class = name)
  
  yardstick::metric_vec_template(
    metric_impl = fun,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    estimator = estimator,
    ...
  )
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
#' A \code{tibble} with columns \code{.metric}, \code{.estimator}, and 
#'   \code{.estimate} with 1 row of values.
#' 
#' For grouped data frames, the number of rows returned will be the same as the 
#'   number of groups.
#'   
#' For \code{tp_vec()}, a single \code{numeric} value (or \code{NA}).
#' @export
tp <- function(data, ...) {
  UseMethod("tp")
}
tp <- yardstick::new_class_metric(tp, direction = "maximize")

#' @rdname tp
#' @export
tp.data.frame <- function(data, truth, estimate, estimator = NULL, 
                          na_rm = FALSE, event_level = "first", ...) {
  yardstick::metric_summarizer(
    metric_nm = "tp",
    metric_fn = tp_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate), 
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

#' @rdname tp
#' @export
tp_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE, 
                   event_level = "first", ...) {
  
  tp_impl <- function(truth, estimate) {
    xtab <- table(estimate, truth)
    col <- event_col(xtab, event_level)
    return(xtab[col, col])
  }
  
  metric_vec_constructor("tp", fun = tp_impl, truth = truth, 
                         estimate = estimate, estimator = estimator, 
                         na_rm = na_rm, event_level = event_level, ...)
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
#' A \code{tibble} with columns \code{.metric}, \code{.estimator}, and 
#'   \code{.estimate} with 1 row of values.
#' 
#' For grouped data frames, the number of rows returned will be the same as the 
#'   number of groups.
#'   
#' For \code{fp_vec()}, a single \code{numeric} value (or \code{NA}).
#' @export
fp <- function(data, ...) {
  UseMethod("fp")
}
fp <- yardstick::new_class_metric(fp, direction = "minimize")

#' @rdname fp
#' @export
fp.data.frame <- function(data, truth, estimate, estimator = NULL, 
                          na_rm = FALSE, event_level = "first", ...) {
  yardstick::metric_summarizer(
    metric_nm = "fp",
    metric_fn = fp_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate), 
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

#' @rdname fp
#' @export
fp_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE, 
                   event_level = "first", ...) {
  
  fp_impl <- function(truth, estimate) {
    xtab <- table(estimate, truth)
    col <- event_col(xtab, event_level)
    col2 <- setdiff(colnames(xtab), col)
    return(xtab[col, col2])
  }
  
  metric_vec_constructor("fp", fun = fp_impl, truth = truth, 
                         estimate = estimate, estimator = estimator, 
                         na_rm = na_rm, event_level = event_level, ...)
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
#' A \code{tibble} with columns \code{.metric}, \code{.estimator}, and 
#'   \code{.estimate} with 1 row of values.
#' 
#' For grouped data frames, the number of rows returned will be the same as the 
#'   number of groups.
#'   
#' For \code{pos_vec()}, a single \code{numeric} value (or \code{NA}).
#' @export
pos <- function(data, ...) {
  UseMethod("pos")
}
pos <- yardstick::new_class_metric(pos, direction = "minimize")

#' @rdname pos
#' @export
pos.data.frame <- function(data, truth, estimate, estimator = NULL, 
                           na_rm = FALSE, event_level = "first", ...) {
  yardstick::metric_summarizer(
    metric_nm = "pos",
    metric_fn = pos_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate), 
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

#' @rdname pos
#' @export
pos_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE, 
                    event_level = "first", ...) {
  
  pos_impl <- function(truth, estimate) {
    xtab <- table(estimate, truth)
    col <- event_col(xtab, event_level)
    return(sum(xtab[col, ]))
  }
  
  metric_vec_constructor("pos", fun = pos_impl, truth = truth, 
                         estimate = estimate, estimator = estimator, 
                         na_rm = na_rm, event_level = event_level, ...)
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
#' A \code{tibble} with columns \code{.metric}, \code{.estimator}, and 
#'   \code{.estimate} with 1 row of values.
#' 
#' For grouped data frames, the number of rows returned will be the same as the 
#'   number of groups.
#'   
#' For \code{neg_vec()}, a single \code{numeric} value (or \code{NA}).
#' @export
neg <- function(data, ...) {
  UseMethod("neg")
}
neg <- yardstick::new_class_metric(neg, direction = "maximize")

#' @rdname neg
#' @export
neg.data.frame <- function(data, truth, estimate, estimator = NULL, 
                           na_rm = FALSE, event_level = "first", ...) {
  yardstick::metric_summarizer(
    metric_nm = "neg",
    metric_fn = neg_vec,
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate), 
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

#' @rdname neg
#' @export
neg_vec <- function(truth, estimate, estimator = NULL, na_rm = FALSE, 
                    event_level = "first", ...) {
  
  neg_impl <- function(truth, estimate) {
    xtab <- table(estimate, truth)
    col <- event_col(xtab, event_level)
    col2 <- setdiff(colnames(xtab), col)
    return(sum(xtab[col2, ]))
  }
  
  metric_vec_constructor("neg", fun = neg_impl, truth = truth, 
                         estimate = estimate, estimator = estimator, 
                         na_rm = na_rm, event_level = event_level, ...)
}

#' @keywords internal
finalize_estimator_internal.neg <- function(metric_dispatcher, x, estimator) {
  finalize_estimator_internal_constructor(metric_dispatcher, x, estimator)
}

