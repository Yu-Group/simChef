#' Make pretty kable tables with custom bolding options
#' 
#' @param X data frame or data matrix to display in table
#' @param digits number of digits to display for numeric values
#' @param sigfig logical; whether or not to count digits via significant 
#'   figures
#' @param align string indicating alignment of columns in table, e.g., 'c'
#' @param caption string; caption of table
#' @param format string; one of "html" or "latex" indicating output format
#' @param na_disp what to display if NA entry is found in X
#' @param bold_function optional function string or vector of function strings 
#'   to use for bolding entries, e.g. ". == max(.)" or ". >= 0.5"
#' @param bold_margin used to evaluate bold_function across margins of X 
#'   (0 = over entire matrix, 1 = over rows, 2 = over columns)
#' @param bold_scheme scalar or vector of logicals, indicating whether or not
#'   to apply bold_function to row/column if bold_margin 0, 1, 2
#' @param bold_color color of bolded text
#' @param full_width logical; whether or not table should have full width
#' @param position character string determining how to position table on page; 
#'   possible values inclue left, right, center, float_left, float_right
#' @param font_size numeric input for table font size
#' @param fixed_head logical; whether or not table header should be fixed
#' @param scroll logical; whether to add scroll box (only for html format)
#' @param scroll_width string indicating width of box, e.g. "50px", "100%"
#' @param scroll_height string indicating height of box, e.g. "100px"
#' @param return_df logical; whether or not to return data frame
#' @param ... additional arguments to pass to kable()
#' @return A kable object if return_df is FALSE; otherwise, a list of two: a
#'   kable object and a data frame
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
#' prettyKable(iris %>% select(-Species), sigfig = T, 
#'             caption = "Iris Data Table", format = "latex", 
#'             scroll = T, na_disp = "NA",
#'             bold_function = ". == min(.)", bold_margin = 1,
#'             bold_scheme = T, bold_color = "black")
#' @export
prettyKable <- function(X, digits = 3, sigfig = T, align = "c", 
                        caption = "", format = "html", na_disp = "NA",
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
  int_cols <- sapply(X, is.integer)
  
  # bold entries according to bold_function if specified
  if (is.null(bold_function)) {
    kable_df <- X
    
  } else {
    
    if (bold_margin == 0) {
      bold_function <- stringr::str_replace(bold_function, "\\(.\\)", 
                                            "\\(X[, bold_scheme]\\)")
      kable_df <- X
    } else if (bold_margin == 1) {
      kable_df <- as.data.frame(t(X))
    } else if (bold_margin == 2) {
      kable_df <- X
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

#' Make pretty DT::datatable with custom bolding options
#' 
#' @param X data frame or data matrix to display in table
#' @param digits number of digits to display for numeric values
#' @param sigfig logical; whether or not to count digits via significant figures
#' @param escape logical; whether or not to escape HTML entities in table
#' @param rownames logical; whether or not to show rownames
#' @param caption string; caption of table
#' @param na_disp what to display if NA entry is found in X
#' @param bold_function optional function string or vector of function strings 
#'   to use for bolding entries, e.g. ". == max(.)" or ". >= 0.5"
#' @param bold_margin used to evaluate bold_function across margins of X 
#'   (0 = over entire matrix, 1 = over rows, 2 = over columns)
#' @param bold_scheme scalar or vector of logicals, indicating whether or not to
#'   apply bold_function to row/column if bold_margin 0, 1, 2
#' @param bold_color color of bolded text
#' @param return_df logical; whether or not to return data frame
#' @param options options argument in DT::datatable
#' @param ... additional arguments to pass to DT::datatable()
#' @return A DT::datatable if return_df is FALSE; otherwise, a list of two: a 
#'   DT::datatable object and a data frame 
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
#' prettyDT(iris %>% select(-Species), sigfig = T, caption = "Iris Data Table",
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
  int_cols <- sapply(X, is.integer)
  
  # bold entries according to bold_function if specified
  if (is.null(bold_function)) {
    dt_df <- X
    
  } else {
    
    if (bold_margin == 0) {
      bold_function <- stringr::str_replace(bold_function, "\\(.\\)", 
                                            "\\(X[, bold_scheme]\\)")
      dt_df <- X
    } else if (bold_margin == 1) {
      dt_df <- as.data.frame(t(X))
    } else if (bold_margin == 2) {
      dt_df <- X
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

