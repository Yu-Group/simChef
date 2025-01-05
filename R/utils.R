#' Check equality of `Experiment` components.
#'
#' @description Check if any two `DGPs`, `Methods`, `Evaluators`,
#'   or `Visualizers` are the same with respect to the function and inputted
#'   arguments.
#'
#' @param obj1 An object of class `DGP`, `Method`, `Evaluator`,
#'   or `Visualizer`
#' @param obj2 An object of class `DGP`, `Method`, `Evaluator`,
#'   or `Visualizer`
#'
#' @return Logical. Returns `TRUE` if both objects have the same function
#'   and arguments and `FALSE` otherwise.
#' @keywords internal
check_equal <- function(obj1, obj2) {

  if (!inherits(obj1, c("DGP", "Method", "Evaluator", "Visualizer"))) {
    abort("obj1 must be a 'DGP', 'Method', 'Evaluator', or 'Visualizer' object.")
  }
  if (!inherits(obj2, c("DGP", "Method", "Evaluator", "Visualizer"))) {
    abort("obj2 must be a 'DGP', 'Method', 'Evaluator', or 'Visualizer' object.")
  }

  if (!identical(class(obj1), class(obj2))) {
    return(FALSE)
  } else {
    class_name <- tolower(class(obj1)[1])
    class_name <- dplyr::case_when(class_name == "evaluator" ~ "eval",
                                   class_name == "visualizer" ~ "viz",
                                   TRUE ~ class_name)
  }
  # check if function and function parameters are equal
  if (!identical(obj1[[paste0(class_name, "_fun")]],
                 obj2[[paste0(class_name, "_fun")]],
                 ignore.environment = TRUE) ||
      !identical(obj1[[paste0(class_name, "_params")]],
                 obj2[[paste0(class_name, "_params")]],
                 ignore.environment = TRUE)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Coerce list into a tibble row.
#'
#' @description Coerce a list into a single row in a tibble. Default is to
#'   coerce using [tibble::as_tibble_row()], but if this fails, then coerce list
#'   into a tibble row, where each column in the tibble is of type list.
#'
#' @param lst List to convert into a tibble row.
#'
#' @return A tibble with one row.
#' @keywords internal
list_to_tibble_row <- function(lst, simplify = FALSE) {
  out <- purrr::map(lst, ~{
    if (is.list(.x) && !is.data.frame(.x) && length(.x) == 1) {
      .x
    } else {
      list(.x)
    }
  }) |>
    tibble::as_tibble_row()
  return(out)
}

#' Coerce list into a tibble.
#'
#' @description Coerce a list into a tibble. Default is to coerce using
#'   [tibble::as_tibble()], but if this fails, then coerce list into a tibble,
#'   where each non-scalar column in the tibble is of type list.
#'
#' @param lst List to convert into a tibble.
#'
#' @return A tibble.
#' @keywords internal
list_to_tibble <- function(lst) {
  tib <- tryCatch({
    tibble::as_tibble(lst)
  }, error = function(e) {
    out <- purrr::map(lst, ~list(.x)) |>
      tibble::as_tibble()
    simplify_cols <- purrr::map_lgl(
      out,
      ~(length(unlist(.x, recursive = F)) == 1) && !is.data.frame(.x[[1]])
    ) |>
      which() |>
      names()
    out <- out |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(simplify_cols),
                                  ~unlist(.x, recursive = F)))
    return(out)
  })
  return(tib)
}

#' Simplify tibble.
#'
#' @description Simplify or unlist list columns in tibble if each element in
#'   the list is a scalar value.
#'
#' @param tbl `tibble::tibble` to simplify.
#' @param cols Character vector of column names to simplify. If NULL (default),
#'   all columns are eligible to be simplified.
#' @param empty_as_na If TRUE (default), 0-length values will be treated as NA.
#'
#' @return A tibble that has been "simplified".
#' @export
simplify_tibble <- function(tbl, cols = NULL, empty_as_na = TRUE) {

  if (is.null(cols)) {
    cols <- colnames(tbl)
  }
  tbl_list <- purrr::imap(
    tbl,
    function(col, col_name) {

      if (!is.list(col) || !(col_name %in% cols)) {
        # only list cols need simplification
        tbl_col <- tibble::tibble(col)
        colnames(tbl_col) <- col_name
        return(tbl_col)
      }

      col_vals <- vector("list", length(col))
      names_in_col <- names(col)

      if (is.null(names_in_col)) {
        names_in_col <- vector("character", length(col))
      }

      atomic_types <- c("double", "integer", "logical",
                        "character", "complex", "raw")
      atomic_type <- NULL

      # only "names" attribute is allowed when unlisting the col
      col_unlistable <- is.vector(col)

      for (i in seq_along(col)) {

        # get the entry
        x <- col[[i]]

        # check if entry is a plain singleton list
        if (all(class(x) == "list") && length(x) == 1 && is.vector(x)) {

          # get the names at each level
          entry_names <- list(
            names_in_col[i], # outer
            names(x), # inner
            names(x[[1]]) # nested
          )

          # filter to names that are valid to serve as the outer name
          is_valid <- sapply(entry_names, function(ns) {
            !is.null(ns) && length(ns) == 1 && !is.na(ns) && any(ns != "")
          })

          # x[[1]]'s name is only valid if it's a singleton vector
          is_valid[3] <- is_valid[3] && is.vector(x[[1]])

          # check for a unique valid name
          new_name <- unique(unlist(entry_names[is_valid]))

          if (length(new_name) > 1) {
            # there are multiple unique names, so preserve all
            col_unlistable <- FALSE
          } else if (length(new_name) == 1) {
            # update the name in the column vector
            names_in_col[i] <- new_name
          }

          # unnest if there is enough space for the names
          if (length(new_name) <= 1) {
            x <- x[[1]]
          }
        }

        # get the final type (hiding warning that arises for is.na(<function>))
        if (length(x) == 0 ||
            (length(x) == 1 && all(suppressWarnings(is.na(x)))))  {
          type <-  "NA"
        } else {
          type <- typeof(x)
        }

        # check col unlistability on three criteria:
        # 1. all entries are NA or atomic of length at most 1
        # 2. no atomic has attrs other than "names"
        # 3. atomic types are compatible

        col_unlistable <- col_unlistable &&
          type %in% c(atomic_types, "NA") &&
          length(x) <= 1 &&
          (is.null(x) || is.vector(x))

        if (col_unlistable && type %in% atomic_types) {
          # record or check atomic type

          if (is.null(atomic_type)) {
            # record the type the first time we see it
            atomic_type <- type

          } else {
            # check atomic type compatibility
            is_lonely_type <- any(
              c(type, atomic_type) %in% c("character", "raw")
            )

            # don't unlist if raw and character are mixed with other types
            if (is_lonely_type && type != atomic_type) {
              col_unlistable <- FALSE
            }
          }
        }

        # put non-NULL value in the column
        if (!is.null(x)) {
          col_vals[[i]] <- x
        }

      } # END for loop over col

      if (col_unlistable) {

        # handle empties

        if (empty_as_na) {
          # some entries may be still be empty
          col_vals <- lapply(col_vals, function(x) {
            if (length(x) == 0) NA else x
          })

        } else {
          # don't unlist if there are empties
          if (any(sapply(col_vals, length) == 0)) {
            col_unlistable <- FALSE
          }
        }
      }

      if (col_unlistable) {

        # unlist

        if (isTRUE(atomic_type == "raw")) {
          # avoid casting raws to logicals if any NA entry
          col_vals <- sapply(col_vals, as.raw)

        } else {
          # safe to unlist other atomic types
          col_vals <- unlist(col_vals, use.names = FALSE)
        }
      }

      if (isTRUE(any(names_in_col != ""))) {
        # add names
        names(col_vals) <- names_in_col
      }

      tbl_col <- tibble::tibble(col_vals)
      colnames(tbl_col) <- col_name
      return(tbl_col)
    }
  ) # tbl_list <- purrr::imap(

  names(tbl_list) <- NULL
  simplified_tbl <- purrr::list_cbind(tbl_list)

  if ("simChef.debug" %in% names(attributes(tbl))) {
    attr(simplified_tbl, "simChef.debug") <- attributes(tbl)[["simChef.debug"]]
  }

  return(simplified_tbl)
}

#' Fix duplicate `vary_across` parameter names.
#'
#' @description Add "_dgp" or "_method" suffixes to parameter names that are
#'   found in both the DGP and Method `vary_across` components. This is to
#'   avoid errors that occur from duplicate column names when trying to create a
#'   tibble.
#'
#' @param dgp_params A named list of the DGP parameters.
#' @param method_params A named list of the Method parameters.
#' @param duplicate_param_names A vector of parameter names that are varied
#'   across in both a DGP and a Method
#'
#' @return A named list of the DGP and Method parameters with no duplicate
#'   names.
#' @keywords internal
fix_duplicate_param_names <- function(dgp_params, method_params,
                                      duplicate_param_names = NULL) {
  dgp_param_names <- names(dgp_params)
  method_param_names <- names(method_params)
  if (length(duplicate_param_names) >= 1) {
    dgp_param_names <- ifelse(dgp_param_names %in% duplicate_param_names,
                              paste0(dgp_param_names, "_dgp"),
                              dgp_param_names)
    method_param_names <- ifelse(method_param_names %in% duplicate_param_names,
                                 paste0(method_param_names, "_method"),
                                 method_param_names)
    names(dgp_params) <- dgp_param_names
    names(method_params) <- method_param_names
  }
  return(c(dgp_params, method_params))
}

#' Compare two tibbles.
#'
#' @description Compare two tibbles for equality or containment.
#'
#' @param x A tibble with unique rows.
#' @param y A tibble with unique rows.
#' @param op Name of opertaion.
#'
#' @return If `op == "equal"`, returns a boolean indicating if `x` and
#'   `y` have the same rows, ignoring the row order. If
#'   `op == "contained_in"`, returns a boolean indicating if all rows in
#'   `x` are contained in the rows of `y`.
#' @keywords internal
compare_tibble_rows <- function(x, y, op = c("equal", "contained_in")) {
  op <- match.arg(op)
  if ((!tibble::is_tibble(x)) || (!tibble::is_tibble(y))) {
    abort("x and y must be tibbles.")
  }
  if (ncol(x) != ncol(y)) {
    return(FALSE)
  }
  if (identical(op, "equal")) {
    if (nrow(x) != nrow(y)) {
      return(FALSE)
    }
  } else if (identical(op, "contained_in")) {
    if (nrow(x) > nrow(y)) {
      return(FALSE)
    }
  }
  duplicated_rows <- rbind(x, y) |>
    duplicated(fromLast = TRUE)
  return(all(duplicated_rows[1:nrow(x)]))
}


#' Get matching rows from x based on id
#'
#' @description Get rows in `x` tibble that match the id rows specified
#'   by the `id` tibble. This function is an alternative to the usual
#'   `dplyr::inner_join` function. The difference is that this function
#'   ignores the source bytecode of functions when looking for matching rows,
#'   while `dplyr::inner_join` treats functions with different sources
#'   as different. This function also requires that the `id` tibble have
#'   distinct rows while `dplyr::inner_join` does not. This function
#'   enables caching when functions are used as parameters in DGPs and Methods.
#'
#' @param id A tibble with distinct rows.
#' @param x A tibble.
#'
#' @return A tibble, containing the subset of rows from `x` that match
#'   id rows from `id`.
#' @keywords internal
get_matching_rows <- function(id, x) {
  if ((!tibble::is_tibble(id)) || (!tibble::is_tibble(x))) {
    abort("id and x must be tibbles.")
  }
  if (nrow(id) == 0) {
    return(tibble::tibble())
  }
  id_cols <- colnames(id)
  id_coltypes <- purrr::map_chr(id, class)
  if (anyDuplicated(id)) {
    stop("id must be a tibble with unique rows.")
  }
  x_ids <- x |>
    dplyr::select(tidyselect::all_of(id_cols))
  if (!any(id_coltypes == "list")) {
    # easy case: no functions in id tibble -> use inner_join
    out <- dplyr::inner_join(id, x, by = id_cols)
  } else if (!anyDuplicated(x_ids)) {
    # no duplicate id rows in x -> use duplicated
    df <- dplyr::bind_rows(id, x)
    keep_row_idx <- df |>
      dplyr::select(tidyselect::all_of(id_cols)) |>
      duplicated()
    out <- df |>
      dplyr::filter(!!keep_row_idx)
  } else {
    # duplicate id rows in x -> brute-force matching using duplicated
    keep_row_idx <- purrr::map_lgl(
      1:nrow(x),
      function(i) {
        dplyr::bind_rows(id, x_ids[i, ]) |>
          duplicated() |>
          dplyr::last()
      }
    )
    out <- x |>
      dplyr::filter(!!keep_row_idx)
  }
  return(out)
}


#' Call a function with given parameters and capture errors, warnings, or
#' messages that occur during evaluation.
#'
#' @param name a name to give some context for the call
#' @param fun a function to call
#' @param params arguments to pass to fun
#' @param verbose Verbosity level. If greater than 1, then handle warnings and
#'   messages in addition to errors.
#'
#' @return the results of calling fun with params
#' @keywords internal
do_call_handler <- function(name,
                            fun,
                            params = list(),
                            verbose = 1,
                            call = rlang::caller_env()) {
  handler <- function(condition = "Error") {
    function(cond) {

      # TODO: add 'signal' arg which determines whether or not to signal the
      # captured condition

      if (length(params) == 0) {
        params_str <- " (params empty)."
      } else {
        params_str <- paste0(
          utils::capture.output(tibble::glimpse(params))[-1], collapse = "\n"
        )
        params_str <- paste0(" with the following params:\n", params_str)
      }

      condition <- match.arg(
        condition, choices = c("Error", "Warning", "Message")
      )

      msg_start <- if (condition == "Message") {
        paste0("The message below")
      } else {
        paste0(cond$message, "\nThe above ", tolower(condition))
      }

      msg <- paste0(
        msg_start, " occurred while processing \"", name, "\"", params_str
      )

      if (condition == "Error") {
        rlang::abort(
          msg, parent = cond, class = "simChef_error", call = call
        )

      } else if (condition == "Warning") {
        rlang::warn(
          msg, class = "simChef_warning", call = call
        )

      } else {
        rlang::inform(
          msg, class = "simChef_message", call = call
        )
      }
    }
  }
  # As opposed to tryCatch, withCallingHandlers registers a local handler in the
  # context of the call that generated the error, warning, or message and
  # continues from where the code left off when the condition was generated.
  if (verbose > 1) {
    # catch and process warnings and messages in addition to errors
    withCallingHandlers(
      do.call(fun, params),
      error = handler(),
      warning = handler("Warning"),
      message = handler("Message")
    )
  } else {
    # handle only errors
    withCallingHandlers(
      do.call(fun, params),
      error = handler()
    )
  }
}

#' Helper function to throw informative error when column names are duplicated,
#' in particular, when the same parameter is in the user-provided method
#' results output and also in vary_across.
#'
#' @param names Vector of column names to check for uniqueness
#' @return Throws an error if duplicate names are found. Returns the original
#'   names otherwise.
#' @keywords internal
check_results_names <- function(names, method_name) {
  if (any(duplicated(names))) {
    dup_names <- unique(names[duplicated(names)])
    abort(
      paste0(
        "Cannot create `fit_results` tibble with duplicate column names: `",
        paste(dup_names, collapse = "`, `"), "`.\nPlease check that the ",
        method_name, "() output does not have the same names as\n",
        "the parameters being varied across in the Experiment.\n",
        "In particular, avoid using `", paste(dup_names, collapse = "`, `"),
        "` as names in the ", method_name, "() output."
      )
    )
  }
  return(names)
}


#' Mark Characters as HTML
#'
#' @description Marks the given text as HTML, which means the tag functions will
#'   know not to perform HTML escaping on it. Copy of \code{htmltools::HTML()}.
#'
#' @param text The text value to mark with HTML
#' @param ... Any additional values to be converted to character and
#'   concatenated together
#' @param .noWS Character vector used to omit some of the whitespace that would
#'   normally be written around this HTML. Valid options include `before`,
#'   `after`, and `outside` (equivalent to `before` and `end`).
#'
#' @returns The input `text`, but marked as HTML.
#' @export
HTML <- function(text, ..., .noWS = NULL) {
  htmlText <- c(text, as.character(rlang::dots_list(...)))

  paste8 <- function (..., sep = " ", collapse = NULL) {
    args <- c(
      lapply(list(...), enc2utf8),
      list(
        sep = if (is.null(sep)) sep else enc2utf8(sep),
        collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
      )
    )
    return(do.call(paste, args))
  }

  htmlText <- paste8(htmlText, collapse = " ")
  attr(htmlText, "html") <- TRUE
  attr(htmlText, "noWS") <- .noWS
  class(htmlText) <- c("html", "character")
  return(htmlText)
}
