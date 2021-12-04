#' Check equality of \code{Experiment} components.
#'
#' @description Check if any two \code{DGPs}, \code{Methods}, \code{Evaluators},
#'   or \code{Visualizers} are the same with respect to the function and inputted
#'   arguments.
#'
#' @param obj1 An object of class \code{DGP}, \code{Method}, \code{Evaluator},
#'   or \code{Visualizer}
#' @param obj2 An object of class \code{DGP}, \code{Method}, \code{Evaluator},
#'   or \code{Visualizer}
#'
#' @return Logical. Returns \code{TRUE} if both objects have the same function
#'   and arguments and \code{FALSE} otherwise.
#' @keywords internal
check_equal <- function(obj1, obj2) {

  if (!inherits(obj1, c("DGP", "Method", "Evaluator", "Visualizer"))) {
    stop("obj1 must be a 'DGP', 'Method', 'Evaluator', or 'Visualizer' object.")
  }
  if (!inherits(obj2, c("DGP", "Method", "Evaluator", "Visualizer"))) {
    stop("obj2 must be a 'DGP', 'Method', 'Evaluator', or 'Visualizer' object.")
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
                 ignore.environment = TRUE) |
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
#' @param ls List to convert into a tibble row.
#'
#' @return A tibble with one row.
#' @keywords internal
list_to_tibble_row <- function(ls) {
  tib <- tryCatch({
    tibble::as_tibble_row(ls)
  }, error = function(e) {
    out <- purrr::map(ls, ~list(.x)) %>%
      tibble::as_tibble_row()
    return(out)
  })
  return(tib)
}

#' Coerce list into a tibble.
#'
#' @description Coerce a list into a tibble. Default is to coerce using
#'   [tibble::as_tibble()], but if this fails, then coerce list into a tibble,
#'   where each non-scalar column in the tibble is of type list.
#'
#' @param ls List to convert into a tibble.
#'
#' @return A tibble.
#' @keywords internal
list_to_tibble <- function(ls) {
  tib <- tryCatch({
    tibble::as_tibble(ls)
  }, error = function(e) {
    out <- purrr::map(ls, ~list(.x)) %>%
      tibble::as_tibble()
    simplify_cols <- purrr::map_lgl(
      out,
      ~(length(unlist(.x, recursive = F)) == 1) & !tibble::is_tibble(.x[[1]])
    ) %>%
      which() %>%
      names()
    out <- out %>%
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
#' @param tib Tibble to simplify.
#' @param omit_cols Character vector of column names to omit when simplifying.
#'
#' @return A tibble that has been "simplified".
#' @keywords internal
simplify_tibble <- function(tib, omit_cols = NULL) {
  simplify_cols <- purrr::map_lgl(
    tib,
    function(col) {
      all(purrr::map_lgl(
        1:length(col), 
        ~(length(unlist(col[.x], recursive = FALSE)) <= 1) &
          (!tibble::is_tibble(col[[.x]]))
      ))
    }
  ) %>%
    which() %>%
    names() %>%
    setdiff(omit_cols)
  tib <- tib %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(simplify_cols), 
                                function(col) {
                                  col[sapply(col, is.null)] <- NA
                                  unlist(col, recursive = FALSE)
                                }))
  return(tib)
}

#' Fix duplicate vary across parameter names.
#' 
#' @description Add "_dgp" or "_method" suffixes to parameter names that are
#'   found in both the DGP and Method vary across components. This is to avoid
#'   errors that occur from duplicate column names when trying to create a 
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
#' @return If \code{op == "equal"}, returns a boolean indicating if \code{x} and
#'   \code{y} have the same rows, ignoring the row order. If 
#'   \code{op == "contained_in"}, returns a boolean indicating if all rows in 
#'   \code{x} are contained in the rows of \code{y}.
#' @keywords internal
compare_tibble_rows <- function(x, y, op = c("equal", "contained_in")) {
  op <- match.arg(op)
  if ((!tibble::is_tibble(x)) | (!tibble::is_tibble(y))) {
    stop("x and y must be tibbles.")
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
  duplicated_rows <- dplyr::bind_rows(x, y) %>%
    duplicated(fromLast = TRUE)
  return(all(duplicated_rows[1:nrow(x)]))
}

#' Call a function with given parameters and if an error, warning, or message
#' occurs during the call print information about the call to the console.
#'
#' @param name a name to give some context for the call
#' @param fun a function to call
#' @param params arguments to pass to fun
#' @param verbose Verbosity level. If greater than 1, then handle warnings and
#'   messages in addition to errors.
#'
#' @return the results of calling fun with params
#' @keywords internal
do_call_handler <- function(name, fun, params = list(), verbose = 1) {
  handler <- function(condition = "Error") {
    function(c) {
      if (length(params) == 0) {
        params_str <- " (params empty)."
      } else {
        params_str <- paste0(
          utils::capture.output(tibble::glimpse(params))[-1], collapse="\n"
        )
        params_str <- paste0(" with the following params:\n", params_str)
      }
      condition <- match.arg(
        condition, choices = c("Error", "Warning", "Message")
      )
      msg_start <- if (condition == "Message") {
        paste0("The message below")
      } else {
        paste0(c$message, "\nThe above ", tolower(condition))
      }
      msg <- paste0(
        msg_start, " occurred while processing \"", name, "\"", params_str
      )
      metadata <- list(name = name, params = params, pid = Sys.getpid())
      if (condition == "Error") {
        rlang::abort(
          msg, parent = c, class = "simChefError",
          data = metadata
        )
      } else if (condition == "Warning") {
        rlang::warn(
          msg, class = "simChefWarning", data = metadata
        )
      } else {
        rlang::inform(
          msg, class = "simChefMessage", data = metadata
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
    stop(
      paste0(
        "Cannot create results tibble with duplicate column names: `",
        paste(dup_names, collapse = "`, `"), "`.\nPlease check that the ",
        method_name, "() output does not have the same names as the ",
        "parameters being varied across in the Experiment and avoid using `",
        paste(dup_names, collapse = "`, `"), "` as names in the ",
        method_name, "() output."
      )
    )
  }
  return(names)
}
