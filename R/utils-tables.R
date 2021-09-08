#' Create pretty kable tables with custom bolding options.
#' 
#' @description Make pretty kable tables and enable easy bolding of cells in 
#'   kable (across rows, columns, or the entire table).
#' 
#' @param X Data frame or data matrix to display in table
#' @param digits Number of digits to display for numeric values
#' @param sigfig Logical. If \code{TRUE}, \code{digits} refers to the number of
#'   significant figures. If \code{FALSE}, \code{digits} refers to the number of
#'   decimal places.
#' @param align A character vector indicating the column alignment, e.g., 'c'. 
#'   For further details, see [knitr::kable()].
#' @param caption The table caption.
#' @param format One of "html" or "latex", indicating the output format.
#' @param na_disp Character string to display if NA entry is found in table.
#' @param bold_function Optional function string or vector of function strings 
#'   to use for bolding entries, e.g. ". == max(.)" or ". >= 0.5".
#' @param bold_margin Specifies the margins of X that will be used to evaluate
#'   \code{bold_function} across, i.e., 0 = across entire matrix, 1 = across 
#'   rows, and 2 = across columns. Required if \code{bold_function = TRUE}.
#' @param bold_scheme Scalar or vector of logicals, indicating whether or not
#'   to apply \code{bold_function} to row if \code{bold_margin} = 1 and to
#'   column if \code{bold_margin} = 1, 2. Default is to apply bolding to all 
#'   rows/columns.
#' @param bold_color Color of bolded text.
#' @param full_width Logical. Whether or not table should have full width. See
#'   [kableExtra::kable_styling()] for details.
#' @param position Character string determining how to position table on page; 
#'   possible values inclue "left", "right", "center", "float_left", 
#'   "float_right". See [kableExtra::kable_styling()] for details.
#' @param font_size A numeric input for table font size. See 
#'   [kableExtra::kable_styling()] for details.
#' @param fixed_thead Logical. Whether or not table header should be fixed at
#'   top. See [kableExtra::kable_styling()] for details.
#' @param scroll Logical. If \code{TRUE}, add scroll box. Only used if 
#'   \code{format = "html"}.
#' @param scroll_width A character string indicating width of the scroll box, 
#'   e.g., "50px", "100%". See [kableExtra::scroll_box()] for details.
#' @param scroll_height A character string indicating height of the scroll box,
#'   e.g. "100px". See [kableExtra::scroll_box()] for details.
#' @param return_df Logical. If \code{TRUE}, return data frame that was used
#'   as input into \code{knitr::kable()} in addition to the kable output. 
#'   If \code{FALSE}, only return the kable output.
#' @param ... Additional arguments to pass to [knitr::kable()].
#' 
#' @return If \code{return_df = FALSE}, returns a kable object. Otherwise,
#'   returns a list of two:
#' \describe{
#' \item{kable}{A kable object.}
#' \item{df}{A data frame that was used as input into \code{knitr::kable()}.}
#' }
#' 
#' @examples
#' ## Show iris data table
#' prettyKable(iris, align = "c", caption = "Iris Data Table")
#' 
#' ## Bold max value of each numeric column of Iris data in red
#' prettyKable(iris, caption = "Iris Data Table", scroll = TRUE,
#'             bold_function = ". == max(.)", bold_margin = 2,
#'             bold_scheme = c(T, T, T, T, F), bold_color = "red")
#'             
#' ## Bold min value of each row in Iris data
#' prettyKable(iris %>% dplyr::select(-Species), sigfig = T, 
#'             caption = "Iris Data Table", format = "latex", 
#'             scroll = T, na_disp = "NA",
#'             bold_function = ". == min(.)", bold_margin = 1,
#'             bold_scheme = T, bold_color = "black")
#' @export
prettyKable <- function(X, digits = 3, sigfig = T, align = "c", caption = "",
                        format = c("html", "latex"), na_disp = "NA",
                        bold_function = NULL, bold_margin = NULL, 
                        bold_scheme = T, bold_color = NULL,
                        full_width = NULL, position = "center",
                        font_size = NULL, fixed_thead = F,
                        scroll = F, scroll_width = NULL, scroll_height = NULL,
                        return_df = FALSE, ...) {
  if (sigfig) {
    dig_format <- "g"
  } else {
    dig_format <- "f"
  }
  format <- match.arg(format)
  
  options(knitr.kable.NA = na_disp)
  
  # error checking
  if (!is.null(bold_function)) {
    if (is.null(bold_margin)) {
      stop("bold_margin must be specified to bold entries in kable.")
    } else if (bold_margin == 0) {
      if (length(bold_scheme) == 1) {
        bold_scheme <- rep(bold_scheme, ncol(X))
      } else if (length(bold_scheme) != ncol(X)) {
        stop("bold_scheme must be a scalar or vector of length ncol(X).")
      }
    } else if (bold_margin == 1) {
      if (length(bold_scheme) == 1) {
        bold_scheme <- rep(bold_scheme, nrow(X))
      } else if (length(bold_scheme) != nrow(X)) {
        stop("bold_scheme must be a scalar or vector of length nrow(X).")
      }
    } else if (bold_margin == 2) {
      if (length(bold_scheme) == 1) {
        bold_scheme <- rep(bold_scheme, ncol(X))
      } else if (length(bold_scheme) != ncol(X)) {
        stop("bold_scheme must be a scalar or vector of length ncol(X).")
      }
    } else {
      stop("bold_margin must be NULL, 0, 1, or 2.")
    }
    
    if (!(length(bold_function) %in% c(1, length(bold_scheme)))) {
      stop(paste0("bold_function must be a scalar or vector of length ", 
                  length(bold_scheme)))
    }
  }
  
  X <- as.data.frame(X, row.names = rownames(X))
  
  # bold entries according to bold_function if specified
  if (is.null(bold_function)) {
    kable_df <- X
    
  } else {
    
    if (bold_margin == 0) {
      bold_function <- stringr::str_replace(bold_function, "\\(.\\)", 
                                            "\\(X[, bold_scheme]\\)")
      kable_df <- X
      int_cols <- sapply(X, is.integer)
    } else if (bold_margin == 1) {
      kable_df <- as.data.frame(t(X))
      int_cols <- apply(X, 1, is.integer)
    } else if (bold_margin == 2) {
      kable_df <- X
      int_cols <- sapply(X, is.integer)
    }
    
    for (f in unique(bold_function)) {
      # for integers
      kable_df <- kable_df %>%
        dplyr::mutate_at(
          colnames(.)[bold_scheme & int_cols & (bold_function == f)],
          list(~dplyr::case_when(
            is.na(.) ~ na_disp,
            eval(parse(text = f)) ~ 
              kableExtra::cell_spec(., color = bold_color, 
                                    bold = T, format = format),
            TRUE ~ kableExtra::cell_spec(., bold = F, format = format)
          ))
        )
      
      # for non-integers
      kable_df <- kable_df %>%
        dplyr::mutate_at(
          colnames(.)[bold_scheme & !int_cols & (bold_function == f)],
          list(~dplyr::case_when(
            is.na(.) ~ na_disp,
            eval(parse(text = f)) ~ 
              kableExtra::cell_spec(formatC(., digits = digits, 
                                            format = dig_format, flag = "#"),
                                    color = bold_color, bold = T, 
                                    format = format),
            TRUE ~ 
              kableExtra::cell_spec(formatC(., digits = digits, 
                                            format = dig_format, flag = "#"),
                                    bold = F, format = format)
          ))
        )
    }
    
    if (bold_margin == 1) {
      kable_df <- as.data.frame(t(kable_df))
    }
  }
  
  # format numeric columns
  kable_df <- kable_df %>%
    dplyr::mutate_if(
      ~is.numeric(.) & !is.integer(.),
      list(~ifelse(is.na(.), na_disp,
                   kableExtra::cell_spec(formatC(., digits = digits,
                                                 format = dig_format, 
                                                 flag = "#"),
                                         format = format)))
    )
  kable_df <- kable_df %>%
    dplyr::mutate_if(is.integer,
                     list(~ifelse(is.na(.), na_disp, 
                                  kableExtra::cell_spec(., format = format))))
  rownames(kable_df) <- rownames(X)
  colnames(kable_df) <- colnames(X)
  
  # make kable
  kable_out <- knitr::kable(kable_df, align = align, booktabs = T, 
                            format = format, linesep = "", caption = caption,
                            escape = F, ...) %>%
    kableExtra::kable_styling(latex_options = c("HOLD_position", "striped"),
                              bootstrap_options = c("striped", "hover"),
                              full_width = full_width,
                              position = position,
                              font_size = font_size,
                              fixed_thead = fixed_thead)
  
  if (scroll & (format == "html")) {
    kable_out <- kable_out %>% 
      kableExtra::scroll_box(width = scroll_width, height = scroll_height)
  }

  if (return_df) {
    return(list(kable = kable_out, df = kable_df))
  } else {
    return(kable_out)
  }
}

#' Create pretty datatable with custom bolding options.
#' 
#' @param X Data frame or data matrix to display in table
#' @param digits Number of digits to display for numeric values
#' @param sigfig Logical. If \code{TRUE}, \code{digits} refers to the number of
#'   significant figures. If \code{FALSE}, \code{digits} refers to the number of
#'   decimal places.
#' @param escape Logical. Whether or not to escape HTML entities in table. See
#'   [DT::datatable()] for details.
#' @param rownames Logical. Whether or not to show rownames in table. See
#'   [DT::datatable()] for details.
#' @param caption The table caption.
#' @param na_disp Character string to display if NA entry is found in table.
#' @param bold_function Optional function string or vector of function strings 
#'   to use for bolding entries, e.g. ". == max(.)" or ". >= 0.5".
#' @param bold_margin Specifies the margins of X that will be used to evaluate
#'   \code{bold_function} across, i.e., 0 = across entire matrix, 1 = across 
#'   rows, and 2 = across columns. Required if \code{bold_function = TRUE}.
#' @param bold_scheme Scalar or vector of logicals, indicating whether or not
#'   to apply \code{bold_function} to row if \code{bold_margin} = 1 and to
#'   column if \code{bold_margin} = 1, 2. Default is to apply bolding to all 
#'   rows/columns.
#' @param bold_color Color of bolded text.
#' @param options See \code{options} argument in [DT::datatable()].
#' @param return_df Logical. If \code{TRUE}, return data frame that was used
#'   as input into \code{DT::datatable()} in addition to the datatable output. 
#'   If \code{FALSE}, only return the datatable output.
#' @param ... Additional arguments to pass to [DT::datatable()].
#' 
#' @return If \code{return_df = FALSE}, returns a datatable object. Otherwise,
#'   returns a list of two:
#' \describe{
#' \item{dt}{A datatable object.}
#' \item{df}{A data frame that was used as input into \code{DT::datatable()}.}
#' }
#' 
#' @examples
#' ## Show iris data table
#' prettyDT(iris, caption = "Iris Data Table")
#' 
#' ## Bold max value of each numeric column of Iris data in red
#' prettyDT(iris, caption = "Iris Data Table",
#'          bold_function = ". == max(.)", bold_margin = 2,
#'          bold_scheme = c(T, T, T, T, F), bold_color = "red")
#'             
#' ## Bold min value of each row in Iris data
#' prettyDT(iris %>% dplyr::select(-Species), 
#'          sigfig = T, caption = "Iris Data Table",
#'          na_disp = "NA", bold_function = ". == min(.)", bold_margin = 1,
#'          bold_scheme = T, bold_color = "black")
#' @export     
prettyDT <- function(X, digits = 3, sigfig = T,
                     escape = F, rownames = TRUE, caption = "", na_disp = "NA",
                     bold_function = NULL, bold_margin = NULL, 
                     bold_scheme = T, bold_color = NULL,
                     options = list(), return_df = FALSE, ...) {
  
  if (sigfig) {
    dig_format <- "g"
  } else {
    dig_format <- "f"
  }
  
  if (!("columnDefs" %in% names(options))) {  # make default center alignment
    if (rownames) {
      targets <- 1:ncol(X)
    } else {
      targets <- 0:(ncol(X) - 1)
    }
    options[["columnDefs"]] <- list(list(className = "dt-center",
                                         targets = targets))
  }
  
  # error checking
  if (!is.null(bold_function)) {
    if (is.null(bold_margin)) {
      stop("bold_margin must be specified to bold entries in datatable.")
    } else if (bold_margin == 0) {
      if (length(bold_scheme) == 1) {
        bold_scheme <- rep(bold_scheme, ncol(X))
      } else if (length(bold_scheme) != ncol(X)) {
        stop("bold_scheme must be a scalar or vector of length ncol(X).")
      }
    } else if (bold_margin == 1) {
      if (length(bold_scheme) == 1) {
        bold_scheme <- rep(bold_scheme, nrow(X))
      } else if (length(bold_scheme) != nrow(X)) {
        stop("bold_scheme must be a scalar or vector of length nrow(X).")
      }
    } else if (bold_margin == 2) {
      if (length(bold_scheme) == 1) {
        bold_scheme <- rep(bold_scheme, ncol(X))
      } else if (length(bold_scheme) != ncol(X)) {
        stop("bold_scheme must be a scalar or vector of length ncol(X).")
      }
    } else {
      stop("bold_margin must be NULL, 0, 1, or 2.")
    }
    
    if (!(length(bold_function) %in% c(1, length(bold_scheme)))) {
      stop(paste0("bold_function must be a scalar or vector of length ", 
                  length(bold_scheme)))
    }
  }
  
  X <- as.data.frame(X, row.names = rownames(X))
  
  # bold entries according to bold_function if specified
  if (is.null(bold_function)) {
    dt_df <- X
    
  } else {
    
    if (bold_margin == 0) {
      bold_function <- stringr::str_replace(bold_function, "\\(.\\)", 
                                            "\\(X[, bold_scheme]\\)")
      dt_df <- X
      int_cols <- sapply(X, is.integer)
    } else if (bold_margin == 1) {
      dt_df <- as.data.frame(t(X))
      int_cols <- apply(X, 1, is.integer)
    } else if (bold_margin == 2) {
      dt_df <- X
      int_cols <- sapply(X, is.integer)
    }
    
    for (f in unique(bold_function)) {
      # for integers
      dt_df <- dt_df %>%
        dplyr::mutate_at(
          colnames(.)[bold_scheme & int_cols & (bold_function == f)],
          list(~dplyr::case_when(
            is.na(.) ~ na_disp,
            eval(parse(text = f)) ~ 
              kableExtra::cell_spec(., color = bold_color, 
                                    bold = T, format = "html"),
            TRUE ~ kableExtra::cell_spec(., bold = F, format = "html")
          ))
        )
      
      # for non-integers
      dt_df <- dt_df %>%
        dplyr::mutate_at(
          colnames(.)[bold_scheme & !int_cols & (bold_function == f)],
          list(~dplyr::case_when(
            is.na(.) ~ na_disp,
            eval(parse(text = f)) ~ 
              kableExtra::cell_spec(formatC(., digits = digits,
                                            format = dig_format, flag = "#"),
                                    color = bold_color, bold = T,
                                    format = "html"),
            TRUE ~ 
              kableExtra::cell_spec(formatC(., digits = digits, 
                                            format = dig_format, flag = "#"),
                                    bold = F, format = "html")
          ))
        )
    }
    
    if (bold_margin == 1) {
      dt_df <- as.data.frame(t(dt_df))
    }
  }
  
  # format numeric columns
  dt_df <- dt_df %>%
    dplyr::mutate_if(
      ~is.numeric(.) & !is.integer(.),
      list(~ifelse(is.na(.), na_disp,
                   kableExtra::cell_spec(formatC(., digits = digits,
                                                 format = dig_format, 
                                                 flag = "#"),
                                         format = "html")))
    )
  dt_df <- dt_df %>%
    dplyr::mutate_if(is.integer,
                     list(~ifelse(is.na(.), na_disp, 
                                  kableExtra::cell_spec(., format = "html"))))
  dt_df[is.na(dt_df)] <- na_disp
  rownames(dt_df) <- rownames(X)
  colnames(dt_df) <- colnames(X)
  
  # make datatable
  dt_out <- DT::datatable(dt_df, escape = escape, caption = caption,
                          rownames = rownames, options = options, ...)
  
  if (return_df) {
    return(list(dt = dt_out, df = dt_df))
  } else {
    return(dt_out)
  }
}

