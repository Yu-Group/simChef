#' `R6` class representing a method.
#'
#' @docType class
#'
#' @description A method to be **fit** and assessed in the `Experiment`.
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
    initialize = function(.method_fun, .name = NULL, ...) {
      self$method_fun <- .method_fun
      self$name <- .name
      self$method_params <- rlang::list2(...)
    },
    fit = function(data_list, ..., .simplify = TRUE) {
      method_params <- self$method_params
      new_method_params <- rlang::list2(...)
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
      fit_results <- list_to_tibble_row(fit_results)
      if (.simplify) {
        return(simplify_tibble(fit_results))
      }
      return(fit_results)
    },
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

#' Create a new `Method`.
#'
#' @name create_method
#'
#' @param .method_fun The method function.
#' @param .name (Optional) The name of the `Method`.
#' @param ... Arguments to pass into `.method_fun()`.
#'
#' @return A new instance of `Method`.
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
#' lm_method <- create_method(.method_fun = lm_fun, .name = "OLS")
#'
#' # create Method with non-default arguments
#' lm_method_x1 <- create_method(.method_fun = lm_fun, .name = "OLS X1",
#'                               # additional named parameters to pass to lm_fun()
#'                               cols = "X1")
#'
#' @export
create_method <- function(.method_fun, .name = NULL, ...) {
  Method$new(.method_fun, .name, ...)
}

#' Fit a `Method`.
#'
#' @name fit_method
#' @description Fit a `Method` on data using the provided `Method`
#'   parameters.
#'
#' @param method A `Method` object.
#' @param data_list List of data to pass into `Method$method_fun()`.
#'   If named, should match arguments in `Method$method_fun()`.
#' @param ... Arguments to pass into `Method$method_fun()` that will
#'   overwrite the initialized `Method` parameters. If no additional
#'   arguments are provided, the `Method` will be fit using
#'   `Method$method_fun()` and the parameters that were set when
#'   `Method$new()` was called.
#' @param .simplify If TRUE, remove list wrapping from any column that has
#'   scalar values.
#'
#' @return Result of `Method$method_fun()`, coerced into a single
#'   tibble row.
#'
#' @inherit visualize_visualizer examples
#' @export
fit_method <- function(method, data_list, ..., .simplify = TRUE) {
  method$fit(data_list, ..., .simplify = .simplify)
}
