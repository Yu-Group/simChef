#' Find last non-comment line, add a `%>%` to the end, then add another line.
#' 
#' @description This function is adapted from the function of the same name in 
#'   the `usemodels` package.
#' 
#' @keywords internal
pipe_value <- function(base, value, expr_width = 80) {
  # value <- rlang::enexpr(value)
  value <- rlang::expr_text(value, width = expr_width)
  clean_base <- gsub("\\n", "", base)
  clean_base <- trimws(base, which = "left")
  not_comment <- seq_along(base)[!grepl("## ", clean_base)]
  n <- max(1, max(not_comment))
  base[n] <- paste(base[n], "%>%")
  c(base, paste0("\n  ", value))
}

#' Create a function call string and print to console.
#' @keywords internal
create_fun_str <- function(name, fun, args) {
  if (is.null(names(args))) {
    names(args) <- ""
  } else {
    names(args) <- dplyr::case_when(
      names(args) != "" ~ paste0(names(args), " = "),
      TRUE ~ ""
    )
  }
  arg_str <- paste(names(args), args, sep = "", collapse = ",\n  ")
  fun_str <- paste0(name, " <- ", fun, "(\n  ", arg_str, "\n)")
  return(fun_str)
}

#' Create a string with list arguments
#' @keywords internal
create_list_args_str <- function(list_args) {
  list_args <- purrr::compact(list_args)
  arg_str <- paste(names(list_args), " = ", list_args,
                   sep = "", collapse = ",\n    ")
  list_str <- paste0("list(\n  ", "  ", arg_str, "\n  )")
  return(list_str)
}

#' Create an assignment string and print to console.
#' @keywords internal
create_assign_str <- function(name, value, throw_error = FALSE) {
  if (is.null(value) || is.null(name)) {
    return(invisible(NULL))
  }
  if (throw_error) {
    value <- paste0("stop('", value, "')")
  }
  cat(paste0(name, " <- ", value, "\n"))
  return(invisible(NULL))
}
