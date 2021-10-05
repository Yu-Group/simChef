#' Helper function to generate simulated errors.
#' 
#' @description Generate simulated errors from the specified error function and
#'   passed arguments.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param err Function from which to generate simulate error vector. Default
#'   is \code{NULL} which returns an error vector of all zeros.
#' @param X Data matrix or data frame. Used to determine n if n is missing.
#' 
#' @return A vector of length n with simulated errors.
#' 
#' @details The function argument err must accept n argument.
#' 
#' @export
generate_errors <- function(err = NULL, n, X, ...) {
  if (missing(n) && missing(X)) {
    stop("Must specify either n or X to generate errors.")
  } else if (missing(n)) {
    n <- nrow(X)
  }
  if (is.null(err)) {
    eps <- matrix(0, nrow = n, ncol = 1)
  } else if (is.function(err)) {
    eps <- R.utils::doCall(err, n = n, ...)
  } else {
    stop("The argument err must either be NULL or a function.", 
         call. = FALSE)
  }
  return(eps)
}

#' Generate autoregressive Gaussian errors.
#' 
#' @description Generate correlated Gaussian errors based on an 
#'   autoregressive(1) covariance structure.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param rho Correlation.
#' 
#' @return A vector of simulated errors of length n.
#' 
#' @export
ar1_errors <- function(n, rho) {
  row1 <- rho^(0:(n - 1))
  Sigma <- stats::toeplitz(row1)
  return(MASS::mvrnorm(1, mu = rep(0, n), Sigma = Sigma))
}

#' Generate block-correlated Gaussian errors.
#' 
#' @description Generate correlated Gaussian errors based on a block-dependence
#'   covariance structure (with 3 blocks).
#'
#' @inheritParams shared_dgp_lib_args
#' @param rho Correlation.
#' 
#' @return A vector of simulated errors of length n.
#' 
#' @export
block_errors <- function(n, rho = 0.8) {
  Sigma_block <- matrix(0, nrow = n, ncol = n)
  k <- n %/% 3
  Sigma_block[1:k, 1:k] <-
    Sigma_block[(k+1):(2*k), (k+1):(2*k)] <-
    Sigma_block[(2*k+1):n, (2*k+1):n] <- rho
  diag(Sigma_block) <- 1
  return(MASS::mvrnorm(1, mu = rep(0, n), Sigma = Sigma_block))
}

#' Generate heteroskedastic Gaussian errors based on the norm of X.
#' 
#' @description Generate independent Gaussian errors with variance based on norm
#'   of row observations in the data matrix X.
#'
#' @inheritParams shared_dgp_lib_args
#' 
#' @return A vector of simulated errors of length n.
#' 
#' @export
norm_errors <- function(X) {
  norm_obs <- apply(X, 1, function(x) sum(x^2))
  Sigma <- diag(norm_obs)
  return(MASS::mvrnorm(1, mu = rep(0, nrow(X)), Sigma = Sigma))
}