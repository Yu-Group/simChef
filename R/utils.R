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
                      })
    str_col <- paste0(name, str_col)
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
    simplify_cols <- purrr::map_lgl(out,
                                    ~length(unlist(.x, recursive = F)) == 1) %>%
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
      all(purrr::map_lgl(1:length(col), 
                         ~length(unlist(col[.x], recursive = FALSE)) <= 1))
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

#' The point of this method is to undo partial argument matching on the formals
#' of the method that calls make_initialize_arg_list().
#'
#' @param obj_fun a required function
#' @param ... other objects needed to construct the args list
#' @param which see argument of the same name in \code{\link{sys.call}}
#'
#' @return a named list of formal args in position order followed by dots args
#' @keywords internal
make_initialize_arg_list <- function(obj_fun, ..., which=-1) {

  sys_call <- sys.call(which)
  user_argnames <- names(sys_call)[-1]

  if (is.null(user_argnames)) {
    user_argnames <- rep("", length(sys_call) - 1)
  }

  # get formals of the calling method (`$initialize` or `create_*`)
  if (length(sys_call[[1]]) == 3) {
    # called directly from an R6Class's `$initialize()` method
    obj_generator <- eval(sys_call[[1]][[2]])
    formal_args <- formals(obj_generator$public_methods$initialize)
  } else {
    # called from a `create_*` method
    formal_args <- formals(eval(sys_call[[1]]))
  }
  formal_argnames <- names(formal_args)
  dots_idx <- which(formal_argnames == "...")
  formal_argnames <- formal_argnames[-dots_idx]

  # find position of the obj_fun arg (e.g. "dgp_fun") in the user's call,
  # allowing for partial matching
  obj_fun_name <- formal_argnames[1]
  obj_fun_idx <- which(!is.na(pmatch(user_argnames, obj_fun_name)))
  if (length(obj_fun_idx) == 0) {
    # get the first of user_argnames with name ""
    obj_fun_idx <- which(user_argnames == "")[1]
  }
  user_argnames <- user_argnames[-obj_fun_idx]

  args_list <- list(...)

  is_pmatched <- sapply(formal_argnames[-1], function(formal_argname) {
    any(!is.na(
      pmatch(user_argnames[!user_argnames %in% formal_argnames], formal_argname)
    ))
  })

  # find the positions of the strict matching args in the user's call
  for (formal_argname in formal_argnames[-1]) {

    formal_idx <- which(formal_argnames[-1] == formal_argname)

    if (!formal_argname %in% user_argnames) {

      # find the user_argname that was matched to formal_argname, if any
      pmatched_idx <- which(!is.na(pmatch(user_argnames, formal_argname)))

      # case 1: formal provided by position (without name)
      if ("" %in% user_argnames) {

        # replace the "" in user_argnames with the formal_argname
        user_idx <- which(user_argnames == "")[1]
        user_argnames[user_idx] <- formal_argname

        if (is_pmatched[formal_argname]) {

          # the user's arg is incorrectly named as formal_argname
          names(args_list)[formal_idx] <- user_argnames[pmatched_idx]

          # in args_list, one of the later formal_argnames may have been given to
          # this arg (if not also pmatched), or this arg may be missing its name
          later_idx <- (formal_idx + 1):length(is_pmatched)
          later_formals <- names(is_pmatched)[later_idx][
            !is_pmatched[later_idx]
          ]

          # the formal arg is either the first with name "" or with the name of
          # some later formal
          formal_arg_idx <- which(names(args_list) %in% c("", later_formals))[1]
          names(args_list)[formal_arg_idx] <- formal_argname

          is_pmatched[formal_argname] <- FALSE
        }

      } else if (is_pmatched[formal_argname]) { # case 2: formal not provided
        names(args_list)[formal_idx] <- user_argnames[pmatched_idx]
        is_pmatched[formal_argname] <- FALSE
      }
    }
    if (!formal_argname %in% names(args_list)) {
      # add the default arg back to the args_list if needed
      default_val <- formal_args[[formal_argname]]
      if (is.null(default_val)) {
        args_list[formal_argname] <- list(NULL)
      } else {
        args_list[[formal_argname]] <- default_val
      }
    }
  }

  args_list[[obj_fun_name]] <- obj_fun
  return(args_list)
}