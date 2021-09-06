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
                      }) %>%
      paste0(name, .)
  }
  return(str_col)
}

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
                                   TRUE ~ class_name)
  }
  # check if function and function parameters are equal
  if (!identical(obj1[[paste0(class_name, "_fun")]],
                 obj2[[paste0(class_name, "_fun")]]) |
      !identical(obj1[[paste0(class_name, "_params")]],
                 obj2[[paste0(class_name, "_params")]])) {
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
    simplify_cols <- purrr::map_lgl(out,
                                    ~length(unlist(.x, recursive = F)) == 1) %>%
      which() %>%
      names()
    out <- out %>%
      dplyr::mutate(across(simplify_cols, ~unlist(.x, recursive = F)))
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
#'
#' @return A tibble that has been "simplified".
#' @keywords internal
simplify_tibble <- function(tib) {
  simplify_cols <- purrr::map_lgl(tib,
                                  ~length(unlist(.x, recursive = F)) == 1) %>%
    which() %>%
    names()
  tib <- tib %>%
    dplyr::mutate(across(simplify_cols, ~unlist(.x, recursive = F)))
  return(tib)
}
