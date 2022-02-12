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
#' @return A vector of simulated errors with length \code{n}.
#' 
#' @details The arguments \code{n} and \code{X} (if provided) are automatically
#'   passed to the function \code{err} under arguments of the same name.
#'   Note however that they may be unused arguments if \code{err} does not take 
#'   in arguments named \code{n} and/or \code{X} as input.
#' 
#' @examples
#' # generate standard Gaussian error vector of length 150
#' errs <- generate_errors(err = rnorm, n = 150)
#' # or alternatively,
#' errs <- generate_errors(err = rnorm, X = iris)
#' 
#' # generate Gaussian error vector with mean 0 and sd 2
#' errs <- generate_errors(err = rnorm, n = 150, sd = 2)
#' 
#' # generate error vector of all 0s
#' errs <- generate_errors(err = NULL, n = 150)
#' 
#' # generate error vector from custom error function
#' err_fun <- function(n, rho) {
#'   # simulate correlated errors from a autoregressive-1 Gaussian process
#'   row1 <- rho^(0:(n - 1))
#'   Sigma <- stats::toeplitz(row1)
#'   return(MASS::mvrnorm(1, mu = rep(0, n), Sigma = Sigma))
#' } 
#' errs <- generate_errors(err = err_fun, n = 100, rho = 0.75)
#' 
#' @export
generate_errors <- function(err = NULL, n, X, ...) {
  if (missing(n) && missing(X)) {
    stop("Must specify either n or X to generate errors.")
  } else if (missing(n)) {
    n <- nrow(X)
  }
  if (is.null(err)) {
    eps <- rep(0, n)
  } else if (is.function(err)) {
    if (!missing(X)) {
      eps <- R.utils::doCall(err, n = n, X = X, ...)
    } else {
      eps <- R.utils::doCall(err, n = n, ...)
    }
  } else if (is.numeric(err)) {
    if (length(err) == 1) {
      eps <- rep(err, n)
    } else if (length(err) == n) {
      eps <- err
    } else {
      stop("When err is numeric it must have the same length as the data.",
           call. = FALSE)
    }
  } else {
    stop("The argument err must either be NULL, a function, or numeric.",
         call. = FALSE)
  }
  return(eps)
}

#---------------------------- Common Error Types -------------------------------
#' Generate autoregressive Gaussian errors.
#' 
#' @description Generate correlated Gaussian errors based on an 
#'   autoregressive(1) covariance structure.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param rho Correlation.
#' 
#' @inherit generate_errors return
#' 
#' @examples
#' errs <- ar1_errors(n = 100, rho = 0.7)
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
#'   covariance structure with \code{n_blocks} (approximately) equally-sized 
#'   blocks.
#'
#' @inheritParams shared_dgp_lib_args
#' @param n_blocks Number of blocks.
#' @param rho Correlation. Must be a scalar or vector of length \code{k}.
#' 
#' @inherit generate_errors return
#' 
#' @examples
#' errs <- block_errors(n = 100, n_blocks = 3, rho = 0.7)
#' 
#' @export
block_errors <- function(n, n_blocks = 3, rho = 0.8) {
  Sigma_block <- matrix(0, nrow = n, ncol = n)
  block_size <- n %/% n_blocks
  
  if (length(rho) == 1) {
    rho <- rep(rho, n_blocks)
  } else if (length(rho) != n_blocks) {
    stop(sprintf("rho must have length 1 or n_blocks = %s.", n_blocks))
  }
  
  for (i in 1:n_blocks) {
    start <- (i - 1) * block_size + 1
    if (i * block_size < n && i == n_blocks) {
      end <- n
    } else {
      end <- min(i * block_size, n)
    }
    Sigma_block[start:end, start:end] <- rho[i]
  }
  diag(Sigma_block) <- 1
  return(MASS::mvrnorm(1, mu = rep(0, n), Sigma = Sigma_block))
}

#' Generate heteroskedastic Gaussian errors based on the norm of X.
#' 
#' @description Generate independent Gaussian errors with variance proportional 
#'   to norm of row observations in the data matrix X. That is,
#'   epsilon_i ~ N(0, scale * ||x_i||_2^2).
#'
#' @inheritParams shared_dgp_lib_args
#' @param scale Multiplicative scale factor.
#' 
#' @inherit generate_errors return
#' 
#' @examples
#' errs <- norm_errors(X = iris %>% dplyr::select(-Species))
#' 
#' @export
norm_errors <- function(X, scale = 1) {
  norm_obs <- apply(X, 1, function(x) sum(x^2))
  Sigma <- diag(norm_obs) * scale
  return(MASS::mvrnorm(1, mu = rep(0, nrow(X)), Sigma = Sigma))
}
