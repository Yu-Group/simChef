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
  private = list(
    .dgp_fun_formals = NULL
  ),
  public = list(
    name = NULL,
    dgp_fun = NULL,
    dgp_params = NULL,
    initialize = function(.dgp_fun, .name = NULL, ...) {
      self$dgp_fun <- .dgp_fun
      self$name <- .name
      self$dgp_params <- rlang::list2(...)
      n_params <- length(self$dgp_params)
      if (n_params > 0 && length(names(self$dgp_params)) != n_params) {
        abort("All default .dgp_fun args given in DGP creation must be named.")
      }
      private$.dgp_fun_formals <- formalArgs(self$dgp_fun)
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

      param_names <- names(new_dgp_params)
      n_params <- length(new_dgp_params)
      n_named <- length(param_names)
      if (n_params > 1 &&
            (n_named == 0 || any(param_names[2:n_params] == ""))) {
        abort(paste0("Only the first arg passed to ",
                     "DGP$generate via ... can be unnamed"))
      }

      if (n_params > 0 && n_named > 0 && param_names[1] == "") {
        names(new_dgp_params)[1] <- private$.dgp_fun_formals[1]
      }

      for (param_name in names(dgp_params)) {
        if (!param_name %in% names(new_dgp_params)) {
          new_dgp_params[[param_name]] <- dgp_params[[param_name]]
        }
      }

      data_list <- eval(rlang::call2(self$dgp_fun, !!!new_dgp_params))

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
#' @export
create_dgp <- function(.dgp_fun, .name = NULL, ...) {
  DGP$new(.dgp_fun, .name, ...)
}
