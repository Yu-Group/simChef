#' \code{R6} class representing a method.
#'
#' @docType class
#'
#' @description A method to be **fit** and assessed in the \code{Experiment}.
#'
#' @export
Method <- R6::R6Class(
  classname = 'Method',
  public = list(
    #' @field name The name of the \code{Method}.
    name = NULL,
    #' @field method_fun The method function.
    method_fun = NULL,
    #' @field method_params (Named) list of parameters to input into the 
    #'   method function.
    method_params = NULL,
    #' @description Create a new \code{Method}.
    #'
    #' @param method_fun The method function.
    #' @param name (Optional) The name of the \code{Method}.
    #' @param ... Arguments to pass into \code{method_fun()}.
    #'
    #' @return A new \code{Method} object.
    initialize = function(method_fun, name = NULL, ...) {
      self$name <- name
      self$method_fun <- method_fun
      self$method_params <- list(...)
    },
    #' @description Fit a \code{Method} on data using the provided \code{Method}
    #'   parameters.
    #' 
    #' @param data_list List of data to pass into \code{method_fun()}. If named,
    #'   should match arguments in \code{method_fun()}.
    #' @param ... Arguments to pass into \code{method_fun()} that will overwrite 
    #'   the initialized \code{Method} parameters. If no additional arguments 
    #'   are provided, the \code{Method} will be fit using \code{method_fun()} 
    #'   and the parameters that were set when \code{Method$new()} was called.
    #' 
    #' @return Result of \code{method_fun()}, coerced into a single tibble row.
    fit = function(data_list, ...) {
      method_params <- self$method_params
      new_method_params <- list(...)
      if (length(new_method_params) > 0) {
        for (i in 1:length(new_method_params)) {
          method_params[[names(new_method_params)[i]]] <- new_method_params[[i]]
        }
      }

      if (identical(method_params, list())) {
        fit_results <- do.call(self$method_fun, data_list)
      } else {
        fit_results <- do.call(self$method_fun, c(data_list, method_params))
      }
      if (is.null(names(fit_results))) {
        names(fit_results) <- paste0("result", 1:length(fit_results))
      }
      return(list_to_tibble_row(fit_results))
    },
    #' @description Print a \code{Method} in a nice format, showing the 
    #'   \code{Method}'s name, function, and parameters.
    #'
    #' @return The original \code{Method} object.
    print = function() {
      if (is.null(self$name)) {
        cat("Method Name: NULL \n")
      } else {
        cat("Method Name:", self$name, "\n")
      }
      cat("   Function: ")
      cat(str(self$method_fun, give.attr = F))
      cat("   Parameters: ")
      cat(str(self$method_params,
              indent.str = "     ", no.list = F))
      invisible(self)
    }
  )
)

#' Create a new \code{Method}.
#'
#' @name create_method
#' 
#' @param method_fun The method function.
#' @param name (Optional) The name of the \code{Method}.
#' @param ... Arguments to pass into \code{method_fun()}.
#'
#' @return A new instance of \code{Method}.
#'
#' @export
create_method <- function(method_fun, name = NULL, ...) {
  return(Method$new(method_fun, name = name, ...))
}
