# NOTE: R6 methods can't use the `@inheritParams` tag. If you update the
# `@param` tags below then be sure to manually replace the corresponding tags
# above `Method$initialize()`.

#' Create a new `Method`
#'
#' @name create_method
#'
#' @description Create a [Method] which can `fit()` data in an [Experiment].
#'
#' @param .method_fun The user-defined method function.
#' @param .name (Optional) The name of the `Method`, helpful for later
#'   identification.
#' @param ... User-defined default arguments to pass into `.method_fun()`.
#'
#' @return A new [Method] object.
#'
#' @examples
#' # generate some data
#' dgp_fun <- function(n, beta, rho, sigma) {
#'   cov_mat <- matrix(c(1, rho, rho, 1), byrow = T, nrow = 2, ncol = 2)
#'   X <- MASS::mvrnorm(n = n, mu = rep(0, 2), Sigma = cov_mat)
#'   y <- X %*% beta + rnorm(n, sd = sigma)
#'   return(list(X = X, y = y))
#' }
#'
#' dgp <- create_dgp(.dgp_fun = dgp_fun,
#'                   .name = "Linear Gaussian DGP",
#'                   n = 50, beta = c(1, 0), rho = 0, sigma = 1)
#' data_corr <- dgp$generate(rho = 0.7)
#'
#' # create an example Method function
#' lm_fun <- function(X, y, cols) {
#'   X <- X[, cols]
#'   lm_fit <- lm(y ~ X)
#'   pvals <- summary(lm_fit)$coefficients[-1, "Pr(>|t|)"] %>%
#'     setNames(paste(paste0("X", cols), "p-value"))
#'   return(pvals)
#' }
#'
#' # create Method with default argument `cols`
#' lm_method <- create_method(
#'   .method_fun = lm_fun,
#'   .name = "OLS",
#'   cols = c(1, 2)
#' )
#'
#' print(lm_method)
#'
#' # fit the Method on data with non-default arguments
#' lm_method$fit(data_corr, cols = 2)
#'
#' # fit the Method on data with default arguments
#' lm_method$fit(data_corr)
#'
#' @export
create_method <- function(.method_fun, .name = NULL, ...) {
  Method$new(.method_fun, .name, ...)
}

#' `R6` class representing a method
#'
#' @name Method
#'
#' @docType class
#'
#' @description `Method` which can `fit()` data in an [Experiment].
#'
#'   Generally speaking, users won't directly interact with the `Method` R6
#'   class, but instead indirectly through [create_method()] and the
#'   following `Experiment` helpers:
#'
#'   - [add_method()]
#'   - [update_method()]
#'   - [remove_method()]
#'   - [get_methods()]
#'   - [fit_experiment()]
#'
#' @seealso [create_method]
#'
#' @examples
#' # generate some data
#' dgp_fun <- function(n, beta, rho, sigma) {
#'   cov_mat <- matrix(c(1, rho, rho, 1), byrow = T, nrow = 2, ncol = 2)
#'   X <- MASS::mvrnorm(n = n, mu = rep(0, 2), Sigma = cov_mat)
#'   y <- X %*% beta + rnorm(n, sd = sigma)
#'   return(list(X = X, y = y))
#' }
#'
#' dgp <- create_dgp(.dgp_fun = dgp_fun,
#'                   .name = "Linear Gaussian DGP",
#'                   n = 50, beta = c(1, 0), rho = 0, sigma = 1)
#' data_corr <- dgp$generate(rho = 0.7)
#'
#' # create an example Method function
#' lm_fun <- function(X, y, cols) {
#'   X <- X[, cols]
#'   lm_fit <- lm(y ~ X)
#'   pvals <- summary(lm_fit)$coefficients[-1, "Pr(>|t|)"] %>%
#'     setNames(paste(names(.), "p-value"))
#'   return(pvals)
#' }
#'
#' # create Method with default argument `cols`
#' lm_method <- Method$new(
#'   .method_fun = lm_fun,
#'   .name = "OLS",
#'   cols = c(1, 2)
#' )
#'
#' print(lm_method)
#'
#' # fit the Method on data with non-default arguments
#' lm_method$fit(data_corr, cols = 2)
#'
#' # fit the Method on data with default arguments
#' lm_method$fit(data_corr)
#'
#' @export
Method <- R6::R6Class(
  classname = 'Method',

  public = list(

    #' @field name The name of the `Method`.
    name = NULL,

    #' @field method_fun The user-defined method function.
    method_fun = NULL,

    #' @field method_params A (named) list of default parameters to input into
    #'   the method function.
    method_params = NULL,

    # NOTE: R6 methods can't use the `@inheritParams` tag. If you want to update
    # the `@param` tags below, do so in the `create_method()` docs above and
    # then copy-paste the corresponding `@param` tags below.

    #' @description Initialize a new `Method` object.
    #'
    #' @param .method_fun The user-defined method function.
    #' @param .name (Optional) The name of the `Method`, helpful for later
    #'   identification.
    #' @param ... User-defined default arguments to pass into `.method_fun()`.
    #'
    #' @return A new instance of `Method`.
    initialize = function(.method_fun, .name = NULL, ...) {
      self$method_fun <- .method_fun
      self$name <- .name
      self$method_params <- rlang::list2(...)
    },

    #' @description Fit a `Method` on data using the provided `Method`
    #'   parameters.
    #'
    #' @param method A `Method` object.
    #' @param data_list List of data to pass into `Method$method_fun()`.
    #'   If named, should match arguments in `Method$method_fun()`.
    #' @param ... User-defined arguments to pass into `Method$method_fun()`
    #'   that will overwrite the initialized `Method` parameters. If no
    #'   additional arguments are provided, the `Method` will be fit using
    #'   `Method$method_fun()` and the parameters that were set when
    #'   `Method$new()` was called.
    #' @param .simplify If `TRUE`, remove list wrapping from any column that has
    #'   scalar values.
    #'
    #' @return Result of `Method$method_fun()`, coerced into a single
    #'   tibble row.
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

    #' @description Print a `Method` in a nice format, showing the
    #'   `Method`'s name, function, and parameters.
    #'
    #' @return The original `Method` object, invisibly.
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
