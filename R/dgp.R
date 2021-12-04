#' \code{R6} class representing a data-generating process.
#'
#' @docType class
#'
#' @description A data-generating process which will be used in the
#'   \code{Experiment} to **generate** data.
#'
#' @template dgp-template
#'
#' @export
DGP <- R6::R6Class(
  classname = 'DGP',
  public = list(
    name = NULL,
    dgp_fun = NULL,
    dgp_params = NULL,
    initialize = function(.dgp_fun, .name = NULL, ...) {
      self$dgp_fun <- .dgp_fun
      self$name <- .name
      self$dgp_params <- rlang::list2(...)
    },
    # @description Generate data from a \code{DGP} with the provided \code{DGP}
    #   parameters.
    #
    # @param ... Arguments to pass into \code{dgp_fun()} that will overwrite
    #   the initialized \code{DGP} parameters. If no additional arguments are
    #   provided, data will be generated using \code{dgp_fun()} with the
    #   parameters that were set when \code{DGP$new()} was called.
    #
    # @return Result of \code{dgp_fun()}.
    generate = function(...) {
      dgp_params <- self$dgp_params
      new_dgp_params <- rlang::list2(...)
      if (length(new_dgp_params) > 0) {
        for (i in 1:length(new_dgp_params)) {
          dgp_params[[names(new_dgp_params)[i]]] <- new_dgp_params[[i]]
        }
      }

      if (identical(dgp_params, list())) {
        data_list <- self$dgp_fun()
      } else {
        data_list <- do.call(self$dgp_fun, dgp_params)
      }

      # check if data_list is a list; if not, coerce to list
      if (!inherits(data_list, "list")) {
        data_list <- list(data_list)
      }

      return(data_list)
    },
    # @description Print a \code{DGP} in a nice format, showing the
    #   \code{DGP}'s name, function, and parameters.
    #
    # @return The original \code{DGP} object.
    print = function() {
      if (is.null(self$name)) {
        cat("DGP Name: NULL \n")
      } else {
        cat("DGP Name:", self$name, "\n")
      }
      cat("   Function: ")
      cat(str(self$dgp_fun, give.attr = F))
      cat("   Parameters: ")
      cat(str(self$dgp_params,
              indent.str = "     ", no.list = F))
      invisible(self)
    }
  )
)

#' Create a new \code{DGP} (data-generating process).
#'
#' @name create_dgp
#'
#' @param .dgp_fun The data-generating process function.
#' @param .name (Optional) The name of the \code{DGP}.
#' @param ... Arguments to pass into \code{.dgp_fun()}.
#'
#' @return A new instance of \code{DGP}.
#'
#' @examples 
#' # create an example DGP function
#' dgp_fun <- function(n, beta, rho, sigma) {
#'   cov_mat <- matrix(c(1, rho, rho, 1), byrow = T, nrow = 2, ncol = 2)
#'   X <- MASS::mvrnorm(n = n, mu = rep(0, 2), Sigma = cov_mat)
#'   y <- X %*% beta + rnorm(n, sd = sigma)
#'   return(list(X = X, y = y))
#' }
#' 
#' # create DGP (with uncorrelated features)
#' dgp_uncorr <- create_dgp(.dgp_fun = dgp_fun, 
#'                          .name = "Uncorrelated Linear Gaussian DGP",
#'                          # additional named parameters to pass to dgp_fun()
#'                          n = 200, beta = c(1, 0), rho = 0, sigma = 1)
#' # create DGP (with correlated features)
#' dgp_corr <- create_dgp(.dgp_fun = dgp_fun, 
#'                        .name = "Correlated Linear Gaussian DGP",
#'                        # additional named parameters to pass to dgp_fun()
#'                        n = 200, beta = c(1, 0), rho = 0.7, sigma = 1)
#' 
#' # create DGP from a function in the built-in DGP library
#' dgp <- create_dgp(.dgp_fun = linear_gaussian_dgp, 
#'                   .name = "Linear Gaussian DGP",
#'                   # additional named parameters to pass to linear_gaussian_dgp()
#'                   n = 100, p_obs = 10, err = rnorm)
#'
#' @export
create_dgp <- function(.dgp_fun, .name = NULL, ...) {
  DGP$new(.dgp_fun, .name, ...)
}
