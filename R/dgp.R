# NOTE: R6 methods can't use the `@inheritParams` tag. If you update the
# `@param` tags below then be sure to manually replace the corresponding tags
# above `DGP$initialize()`.

#' Create a new `DGP` (data-generating process)
#'
#' @name create_dgp
#'
#' @description Create a [DGP] which can `generate()` data in an [Experiment].
#'
#' @param .dgp_fun The user-defined data-generating process function.
#' @param .name (Optional) An optional name for the `DGP`, helpful for later
#'   identification.
#' @param ... User-defined default arguments to pass to `.dgp_fun()` when
#'   `DGP$generate()` is called.
#'
#' @return A new [DGP] object.
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
#' dgp <- create_dgp(.dgp_fun = dgp_fun,
#'                   .name = "Linear Gaussian DGP",
#'                   # additional named parameters to pass to dgp_fun() by default
#'                   n = 50, beta = c(1, 0), rho = 0, sigma = 1)
#'
#' print(dgp)
#'
#' data_uncorr <- dgp$generate()
#' cor(data_uncorr$X)
#'
#' data_corr <- dgp$generate(rho = 0.7)
#' cor(data_corr$X)
#'
#' @export
create_dgp <- function(.dgp_fun, .name = NULL, ...) {
  DGP$new(.dgp_fun, .name, ...)
}

#' `R6` class representing a data-generating process
#'
#' @name DGP
#'
#' @docType class
#'
#' @description `DGP`, a data-generating process which can `generate()` data in
#'   an [Experiment].
#'
#'   Generally speaking, users won't directly interact with the `DGP` R6
#'   class, but instead indirectly through [create_dgp()] and the
#'   following `Experiment` helpers:
#'
#'   - [add_dgp()]
#'   - [update_dgp()]
#'   - [remove_dgp()]
#'   - [get_dgps()]
#'   - [generate_data()]
#'
#' @seealso [create_dgp]
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
#' dgp <- DGP$new(.dgp_fun = dgp_fun,
#'                .name = "Linear Gaussian DGP",
#'                # additional named parameters to pass to dgp_fun() by default
#'                n = 50, beta = c(1, 0), rho = 0, sigma = 1)
#'
#' print(dgp)
#'
#' data_uncorr <- dgp$generate()
#' cor(data_uncorr$X)
#'
#' data_corr <- dgp$generate(rho = 0.7)
#' cor(data_corr$X)
#'
#' @export
DGP <- R6::R6Class(
  classname = 'DGP',

  private = list(
    .dgp_fun_formals = NULL
  ),

  public = list(

    #' @field name The name of the `DGP`.
    name = NULL,

    #' @field dgp_fun The user-defined data-generating process function.
    dgp_fun = NULL,

    #' @field dgp_params A (named) list of user-defined default arguments
    #'   to input into the data-generating process function.
    dgp_params = NULL,

    # NOTE: R6 methods can't use the `@inheritParams` tag. If you want to update
    # the `@param` tags below, do so in the `create_dgp()` docs above and
    # then copy-paste the corresponding `@param` tags below.

    #' @description Initialize a new `DGP` object.
    #'
    #' @param .dgp_fun The user-defined data-generating process function.
    #' @param .name (Optional) An optional name for the `DGP`, helpful for later
    #'   identification.
    #' @param ... User-defined default arguments to pass to `.dgp_fun()` when
    #'   `DGP$generate()` is called.
    #'
    #' @return A new instance of `DGP`.
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

    #' @description Generate data from a `DGP`.
    #'
    #' @param ... User-defined arguments to pass into `DGP$dgp_fun()` that will
    #'   overwrite the initialized `DGP` parameters. If no additional arguments
    #'   are provided, data will be generated using `DGP$dgp_fun()` with the
    #'   parameters that were set when `DGP$new()` was called.
    #'
    #' @return Result of `DGP$dgp_fun()`. If the result is not a list,
    #'   it will be coerced to a list.
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

    #' @description Print a `DGP` in a nice format, showing the
    #'   `DGP`'s name, function, and parameters.
    #'
    #' @return The original `DGP` object, invisibly.
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
