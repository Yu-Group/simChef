#' Generate a normal random matrix of covariates/features.
#'
#' @description Generate an \code{.n} x \code{.p} normal random matrix with the specified mean
#'   and covariance structure.
#'
#' @inheritParams shared_dgp_lib_args
#' @param .mean Mean of normal distribution from which to generate data. Can be
#'   either a scalar value or vector of length \code{.p}. Default is 0.
#' @param .sd Standard deviation of normal distribution from which to generate
#'   data. Default is 1.
#' @param .corr Correlation between all pairs of features. Default is 0 for no
#'   correlation.
#' @param .Sigma (Optional) \code{.p} x \code{.p} covariance matrix of the
#'   normal distribution from which to generate data. Default is NULL (not
#'   used). If provided, the arguments \code{corr} and \code{sd} are ignored.
#'
#' @return A normal random matrix of size \code{n.} x \code{.p}.
#'
#' @examples
#'
#' # Returns 100 x 10 random Gaussian matrix with X_ij ~ N(0, 1)
#' X <- generate_X_gaussian(.n = 100, .p = 10)
#'
#' # Returns 100 x 10 random Gaussian matrix with E(X_i) = 0 for all i,
#' # Var(X_i) = 4 for all i, and Cor(X_i, X_j) = 0.7 for all i != j
#' X <- generate_X_gaussian(.n = 100, .p = 10, .sd = 2, .corr = 0.7)
#'
#' # Returns 100 x 2 random Gaussian matrix: X ~ N(0, Sigma), where
#' # Sigma = [3, .5; .5, 1]
#' X <- generate_X_gaussian(
#'   .n = 100, .p = 2, .Sigma = matrix(c(3, .5, .5, 1), nrow = 2, byrow = TRUE)
#' )
#'
#' @export
generate_X_gaussian <- function(.n, .p, .mean = 0, .sd = 1, .corr = 0,
                                .Sigma = NULL) {
  if (length(.mean) == 1) {
    mean_vec <- rep(.mean, .p)
  } else if (length(.mean) != .p) {
    stop("The argument .mean must be a scalar or a vector of length .p.",
         call. = FALSE)
  } else {
    mean_vec <- .mean
  }

  if (is.null(.Sigma)) {
    if ((.corr == 0) && (length(.mean) == 1)) {
      X <- matrix(stats::rnorm(.n * .p, mean = .mean, sd = .sd), nrow = .n, ncol = .p)
    } else {
      .Sigma <- matrix(.corr, nrow = .p, ncol = .p)
      diag(.Sigma) <- 1
      D <- diag(.sd, nrow = .p, ncol = .p)
      X <- MASS::mvrnorm(n = .n, mu = mean_vec, Sigma = D %*% .Sigma %*% D)
    }
  } else {
    X <- MASS::mvrnorm(n = .n, mu = mean_vec, Sigma = .Sigma)
  }
  return(X)
}

#' Generate a design matrix X by sampling from a real-world data matrix.
#' 
#' @description Generate a design matrix X by sampling from a real-world data 
#'   matrix under the specified sampling scheme.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param n Number of samples if \code{clusters} is not \code{NULL}. If
#'   \code{clusters = NULL}, this is the number of clusters.
#' @param p Number of features. If \code{p < ncol(X)}, the \code{p} features
#'   are sampled uniformly at random from the full feature set.
#' @param clusters (Optional) Vector of cluster IDs. If provided, block or
#'   clustered sampling will be performed according to these clusters so that
#'   each cluster will be entirely in or entirely out of the retrieved sample.
#' @param replace Logical. If TRUE, sample observations with replacement; if
#'   FALSE, sample observations without replacement
#'   
#' @return A matrix of size n x p.
#' 
#' @examples
#' # get bootstrap samples from iris data set
#' X <- generate_X_rwd(X = iris, replace = TRUE)
#' 
#' # leave one batch out from iris data set
#' batch_ids <- rep(1:3, length.out = nrow(iris))
#' X <- generate_X_rwd(X = iris, n = 2, clusters = batch_ids)
#' 
#' @export
generate_X_rwd <- function(X, n = nrow(X), p = ncol(X), 
                           clusters = NULL, replace = FALSE) {
  if (is.null(clusters)) {
    if ((n != nrow(X) | replace)) {
      X <- X %>%
        as.data.frame() %>%
        dplyr::slice_sample(n = n, replace = replace)
    }
  } else if (!is.null(clusters)) {
    n_clusters <- length(unique(clusters))
    if (n > n_clusters) {
      stop("Number of clusters, n, requested is larger than the number of ",
           "available clusters from which to sample. Choose a smaller n.")
    }
    if ((n != n_clusters | replace)) {
      keep_clusters <- sample(unique(clusters), n, replace = replace)
      X <- X %>%
        dplyr::filter(tidyselect::all_of(clusters) %in% keep_clusters)
    }
  }
  if (p != ncol(X)) {
    keep_feat <- sample(1:ncol(X), p, replace = FALSE)
    X <- X[, keep_feat, drop = F]
  }
  return(X)
}
