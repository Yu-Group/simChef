#' Arguments that are shared by multiple \code{DGP} library functions.
#'
#' @name shared_dgp_lib_args
#'
#' @param data_split Logical; if \code{TRUE}, splits data into training and test
#'   sets according to \code{train_prop}.
#' @param err Function from which to generate simulate error vector. Default
#'   is \code{NULL} which adds no error to the DGP.
#' @param intercept Scalar intercept term.
#' @param n Number of samples.
#' @param p Number of features.
#' @param return_support Logical specifying whether or not to return a vector
#'   of the support column names. If \code{X} has no column names, then the
#'   indices of the support are used.
#' @param return_values Character vector indicating what objects to return in 
#'   list. Elements in vector must be one of "X", "y", "support".
#' @param support Vector of feature indices in the true support of the DGP.
#' @param train_prop Proportion of data in training set if 
#'   \code{data_split = TRUE}.
#' @param X Data matrix or data frame.
#' @param y Response vector.
#' @param ... Other arguments to pass to err() to generate the error vector.
#'   
#' @returns A list of the named objects that were requested in
#'   \code{return_values}. See brief descriptions below.
#' \describe{
#' \item{X}{A \code{data.frame}.}
#' \item{y}{A response vector of length \code{nrow(X)}.}
#' \item{support}{A vector of feature indices indicating all features used in
#'   the true support of the DGP.}
#' }
#' Note that if \code{data_split = TRUE} and "X", "y" 
#'   are in \code{return_values}, then the returned list also contains slots for
#'   "Xtest" and "ytest".
#'
#' @keywords internal
NULL

#' Helper function to generate a coefficient vector.
#' 
#' @description Generate a coefficient vector with the specified dimensions and
#'   sparsity level.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param betas Coefficient vector or function to generate the coefficients. If
#'   a scalar is provided, the coefficient vector is a constant vector. If
#'   \code{NULL} (default), entries in the coefficient vector are drawn iid from
#'   N(0, \code{sd}^2). If a function, must take the integer arguments \code{p}
#'   and \code{s} (and optionally \code{sd}, \code{betas_name}, and other
#'   user-defined args from \code{...}) and return a numeric vector.
#' @param s Sparsity level. Coefficients corresponding to features after the
#'   \code{s}th position (i.e., positions i = \code{s} + 1, ..., \code{p}) are
#'   set to 0.
#' @param sd (Optional) SD of normal distribution from which to draw
#'   \code{betas}. Only used if \code{betas} argument is \code{NULL}.
#' @param betas_name Name of coefficient variable to use in error message.
#' @param ... Additional user arguments to pass to \code{betas} when
#'   \code{betas} is a function.
#' 
#' @returns A vector of length \code{p}.
#' 
#' @examples
#' # generate beta ~ N(0, 1) of dimension 10
#' beta <- generate_coef(p = 10)
#' 
#' # generate beta = [1, 1, 0, 0, 0]
#' beta <- generate_coef(betas = 1, p = 5, s = 2)
#' 
#' # generate beta = [1, 2, 3]
#' beta <- generate_coef(betas = 1:3, p = 3)
#' 
#' @export
generate_coef <- function(betas = NULL, p = 1, s = p, sd = 1, 
                          betas_name = "betas", ...) {
  if (s > p) {
    stop(sprintf("Got s=%s, but should be less than or equal to p=%s.", s, p))
  }
  if (is.function(betas)) {
    betas <- R.utils::doCall(betas, ...,
                             args = list(sd=sd, betas_name=betas_name),
                             alwaysArgs = list(p=p, s=s))
    if (!is.numeric(betas)) {
      msg <- sprintf(
        paste("%s is a function but didn't return a numeric vector.",
              "Instead, returned object with class(es): %s."),
        betas_name, paste0(class(betas), collapse=", ")
      )
      stop(msg)
    }
  }
  if (is.numeric(betas)) {
    if (length(betas) == 1 && p > 1) {
      betas <- rep(betas, length.out = min(s, p))
    } else if (length(betas) != p) {
      stop(sprintf("%s must have length 1 or %s.", betas_name, p))
    }
  } else if (!is.null(betas)) {
    msg <- sprintf(
      paste("%s must be NULL, a function that returns a numeric vector,",
            "or a fixed numeric vector, but instead had class: %s."),
      betas_name, paste0(class(betas), collapse=", ")
    )
    stop(msg)
  } else {
    # simulate betas from gaussian by default
    betas <- stats::rnorm(min(s, p), mean = 0, sd = sd)
  }
  if (length(betas) != p) {
      betas <- c(betas, rep(0, p - length(betas)))
  }
  return(betas)
}

#' Developer function to return consistent outputs in DGP.
#' 
#' @description Helper function that returns a consistent output for any DGP
#'   function.
#'   
#' @inheritParams shared_dgp_lib_args
#' 
#' @inherit shared_dgp_lib_args return
#' 
#' @examples 
#' # Return training/test splits using iris data and completely dense support 
#' dgp_out <- return_DGP_output(X = iris %>% dplyr::select(-Species),
#'                              y = iris$Species,
#'                              support = 1:4,
#'                              data_split = TRUE,
#'                              train_prop = 0.5,
#'                              return_values = c("X", "y", "support"))
#' 
#' @export
return_DGP_output <- function(X, y, support, data_split, train_prop,
                              return_values) {
  out <- NULL
  X <- as.data.frame(X)
  if (any(c("X", "y") %in% return_values)) {
    if (data_split) {
      data_out <- split_data(X = X, y = y, train_prop = train_prop)
    } else {
      data_out <- list(X = X, y = y)
    }
    if ("X" %in% return_values) {
      out <- c(out, data_out[intersect(names(data_out), c("X", "Xtest"))])
    }
    if ("y" %in% return_values) {
      out <- c(out, data_out[intersect(names(data_out), c("y", "ytest"))])
    }
  }
  if ("support" %in% return_values) {
    out <- c(out, list(support = support))
  }
  return(out)
}

#' Helper function to split data into training and test sets
#' 
#' @inheritParams shared_dgp_lib_args
#' @param train_prop Proportion of data in training set.
#' 
#' @returns A list of four: "X", "y", "Xtest", and "ytest" containing the
#'   training data, training response, test data, and test response,
#'   respectively.
#' 
#' @keywords internal
split_data <- function(X, y, train_prop = 0.5) {
  n <- nrow(X)
  train_ids <- sample(1:n, size = round(n * train_prop), replace = F)
  out <- list(X = X[train_ids, , drop = FALSE], y = y[train_ids],
              Xtest = X[-train_ids, , drop = FALSE], ytest = y[-train_ids])
  return(out)
}

#' Helper function to compute indicator function in LSS model.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param thresh thresholds
#' @param sgn sign of the interaction
#' 
#' @keywords internal
indicator <- function(X, thresh, sgn) {
  if (any(sgn == -1)) {
    X[, sgn == -1] <- X[, sgn == -1] * -1
    thresh[sgn == -1] <- thresh[sgn == -1] * -1
  }
  thresh_mat <- matrix(thresh, byrow = T, nrow = nrow(X), ncol = ncol(X))
  indic <- apply(X > thresh_mat, 1, all)
  return(as.integer(indic))
}

