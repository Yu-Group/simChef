#' @export
check_equal <- function(obj1, obj2) {
  if (!identical(class(obj1), class(obj2))) {
    return(FALSE)
  } else {
    class_name <- tolower(class(obj1)[1])
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

#' @export
list_to_tibble_row <- function(ls) {
  tib <- tryCatch({
    tibble::as_tibble_row(ls)
  }, error = function(e) {
    out <- purrr::map(ls, ~list(.x)) %>%
      tibble::as_tibble_row()
    simplify_cols <- purrr::map_lgl(out, ~length(unlist(.x)) == 1) %>%
      which() %>%
      names()
    out <- out %>%
      mutate(across(simplify_cols, unlist))
    return(out)
  })
  return(tib)
}

#' @export
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
