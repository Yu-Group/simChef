#' \code{R6} class representing a method.
#'
#' @docType class
#'
#' @description A method to be **fit** and assessed in the \code{Experiment}.
#'
#' @template method-template
#'
#' @export
Method <- R6::R6Class(
  classname = 'Method',
  public = list(
    name = NULL,
    method_fun = NULL,
    method_params = NULL,
    initialize = function(method_fun, name = NULL, ...) {
      dots_list <- list(...)
      if (".args_list" %in% names(dots_list)) {
        args_list <- dots_list[[".args_list"]]
      } else {
        args_list <- make_initialize_arg_list(method_fun, name = name, ...,
                                              which = -2)
      }
      self$method_fun <- args_list$method_fun
      self$name <- args_list$name
      args_list$method_fun <- NULL
      args_list$name <- NULL
      self$method_params <- args_list
    },
    # @description Fit a \code{Method} on data using the provided \code{Method}
    #   parameters.
    #
    # @param data_list List of data to pass into \code{method_fun()}. If named,
    #   should match arguments in \code{method_fun()}.
    # @param ... Arguments to pass into \code{method_fun()} that will overwrite
    #   the initialized \code{Method} parameters. If no additional arguments
    #   are provided, the \code{Method} will be fit using \code{method_fun()}
    #   and the parameters that were set when \code{Method$new()} was called.
    #
    # @return Result of \code{method_fun()}, coerced into a single tibble row.
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
    # @description Print a \code{Method} in a nice format, showing the
    #   \code{Method}'s name, function, and parameters.
    #
    # @return The original \code{Method} object.
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
#' @param name (Optional) The name of the \code{Method}. The argument must be
#'   specified by position or typed out in whole; no partial matching is allowed
#'   for this argument.
#' @param ... Arguments to pass into \code{method_fun()}.
#'
#' @return A new instance of \code{Method}.
#'
#' @examples 
#' # create an example Method function
#' lm_fun <- function(X, y, cols = c("X1", "X2")) {
#'   lm_fit <- lm(y ~ X)
#'   pvals <- summary(lm_fit)$coefficients[cols, "Pr(>|t|)"] %>%
#'     setNames(paste(names(.), "p-value"))
#'   return(pvals)
#' }
#' 
#' # create Method with default arguments
#' lm_method <- create_method(method_fun = lm_fun, name = "OLS")
#' 
#' # create Method with non-default arguments
#' lm_method_x1 <- create_method(method_fun = lm_fun, name = "OLS X1",
#'                               # additional named parameters to pass to lm_fun()
#'                               cols = "X1")
#'                   
#' @export
create_method <- function(method_fun, name = NULL, ...) {
  args_list <- make_initialize_arg_list(method_fun, name = name, ...)
  do.call(Method$new, list(.args_list = args_list))
}
