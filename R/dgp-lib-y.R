#' Simulate linear response data.
#' 
#' @description Generate linear response data with a specified error 
#'   distribution given the observed and unobserved design matrices.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param X Design data matrix of observed variables.
#' @param U Design data matrix of unobserved (omitted) variables.
#' @param betas Coefficient vector for observed design matrix.
#' @param betas_unobs Coefficient vector for unobserved design matrix.
#' 
#' @returns If \code{return_support = TRUE}, returns a list of two:
#' \describe{
#' \item{y}{A response vector of length \code{nrow(X)}.}
#' \item{support}{A vector of feature indices in the true support of the DGP.}
#' }
#' 
#' If \code{return_support = FALSE}, returns only the response vector \code{y}.
#' 
#' @examples
#' X <- generate_X_gaussian(n = 100, p = 2)
#' U <- generate_X_gaussian(n = 100, p = 2)
#' 
#' # generate the response from: y = 3*x_1 - x_2 + N(0, 1) errors
#' y <- generate_y_linear(X = X, betas = c(3, -1), err = rnorm)
#' 
#' # generate the response from: y = 3*x_1 - x_2 + u_1 + 2*u_2
#' y <- generate_y_linear(X = X, U = U, betas = c(3, -1), betas_unobs = c(1, 2))
#' 
#' @export
generate_y_linear <- function(X, U, betas, betas_unobs, err = NULL,
                              return_support = FALSE, ...) {
  n <- nrow(X)
  p <- ncol(X)
  
  if (missing(U)) {
    U <- matrix(0, nrow = n, ncol = 1)
  }
  if (missing(betas_unobs)) {
    betas_unobs <- 0
  }
  
  eps <- generate_errors(err = err, n = n, ...)
  
  y <- c(as.matrix(U) %*% betas_unobs + as.matrix(X) %*% betas + eps)
  
  if (return_support) {
    support <- which(betas != 0)
    return(list(y = y, support = support))
  } else {
    return(y)
  }
}

#' Simulate (binary) logistic response data.
#' 
#' @description Generate (binary) logistic response data given the observed
#'   design matrices.
#' 
#' @inheritParams shared_dgp_lib_args
#' @param X Design data matrix of observed variables.
#' @param betas Coefficient vector for observed design matrix.
#' @param ... Not used.
#' 
#' @returns If \code{return_support = TRUE}, returns a list of two:
#' \describe{
#' \item{y}{A response vector of length \code{nrow(X)}.}
#' \item{support}{A vector of feature indices in the true support of the DGP.}
#' }
#' 
#' If \code{return_support = FALSE}, returns only the response vector \code{y}.
#' 
#' @examples
#' X <- generate_X_gaussian(n = 100, p = 2)
#' 
#' # generate the response from: log(p / (1 - p)) = 3*x_1 - x_2
#' # where p = P(y  = 1 | x)
#' y <- generate_y_logistic(X = X, betas = c(3, -1))
#' 
#' @export
generate_y_logistic <- function(X, betas, return_support = FALSE, ...) {
  n <- nrow(X)
  p <- ncol(X)
  
  probs <- 1 / (1 + exp(-(as.matrix(X) %*% betas)))
  y <- as.factor(
    ifelse(stats::runif(n = n, min = 0, max = 1) > probs, "0", "1")
  )
  
  if (return_support) {
    support <- which(betas != 0)
    return(list(y = y, support = support))
  } else {
    return(y)
  }
}

#' Generate locally spiky smooth (LSS) response data.
#' 
#' @description Generate LSS response data with a specified error
#'   distribution given the observed data matrices.
#'
#' @inheritParams shared_dgp_lib_args
#' @param k Order of the interactions.
#' @param s Number of interactions in the LSS model or a matrix of the support
#'   indices with each interaction taking a row in this matrix and ncol = k.
#' @param thresholds A scalar or a s x k matrix of the thresholds for each term 
#'   in the LSS model.
#' @param signs A scalar or a s x k matrix of the sign of each interaction 
#'   (1 means > while -1 means <).
#' @param betas Scalar or parameter vector for interaction terms.
#' @param intercept Scalar intercept term.
#' @param overlap If TRUE, simulate support indices with replacement; if FALSE,
#'   simulate support indices without replacement (so no overlap)
#' 
#' @returns If \code{return_support = TRUE}, returns a list of two:
#' \describe{
#' \item{y}{A response vector of length \code{nrow(X)}.}
#' \item{support}{A vector of signed feature indices in the true (interaction)
#'   support of the DGP. For example, "1+_2-" means that the interaction
#'   between high values of feature 1 and low values of feature 2 appears in the
#'   underlying DGP.}
#' }
#' 
#' If \code{return_support = FALSE}, returns only the response vector \code{y}.
#' 
#' @details Here, data is generated from the following LSS model: 
#' \deqn{E(Y|X) = intercept + sum_{i = 1}^{s} beta_i prod_{j = 1}^{k}1(X_{S_j} 	lessgtr thresholds_ij)}
#' 
#' For more details on the LSS model, see Behr, Merle, et al. "Provable Boolean Interaction Recovery from Tree Ensemble obtained via Random Forests." arXiv preprint arXiv:2102.11800 (2021).
#' 
#' @examples
#' X <- generate_X_gaussian(n = 100, p = 10)
#' 
#' # generate data from: y = 1(X_1 > 0, X_2 > 0) + 1(X_3 > 0, X_4 > 0)
#' y <- generate_y_lss(X = X, k = 2, s = matrix(1:4, nrow = 2, byrow = TRUE),
#'                     thresholds = 0, signs = 1, betas = 1)
#' 
#' # generate data from: y = 3 * 1(X_1 < 0) - 1(X_2 > 1) + N(0, 1)
#' y <- generate_y_lss(X = X, k = 1, 
#'                     s = matrix(1:2, nrow = 2),
#'                     thresholds = matrix(0:1, nrow = 2), 
#'                     signs = matrix(c(-1, 1), nrow = 2),
#'                     betas = c(3, -1),
#'                     err = rnorm)
#' 
#' @export
generate_y_lss <- function(X, k, s, thresholds = 1, signs = 1,
                           betas = 1, intercept = 0, overlap = FALSE,
                           err = NULL, return_support = FALSE, ...) {
  
  if (!is.matrix(s)) {
    support_idx <- sample(1:ncol(X), k * s, replace = overlap) %>%
      matrix(nrow = s, ncol = k)
  } else {
    support_idx <- s
    s <- nrow(support_idx)
    if (ncol(support_idx) != k) {
      stop("k does not match up with the order of interactions.")
    }
  }
  
  if (!is.matrix(thresholds)) {
    thresholds <- matrix(thresholds, nrow = s, ncol = k)
  }
  if (!is.matrix(signs)) {
    signs <- matrix(signs, nrow = s, ncol = k)
  }
  if (length(betas) == 1) {
    betas <- rep(betas, s)
  }
  
  eps <- generate_errors(err = err, n = nrow(X), ...)
  add_terms <- purrr::map(1:s,
                          function(i) {
                            indicator(X[, support_idx[i, ], drop = F], 
                                      thresholds[i, ], 
                                      signs[i, ]) * 
                              betas[i]
                          }) %>%
    purrr::reduce(`+`)
  y <- intercept + add_terms + eps
  
  if (return_support) {
    support <- purrr::map_chr(
      1:s,
      function(i) {
        paste(support_idx[i, ], ifelse(signs[i, ] == 1, "+", "-"),
              sep = "", collapse = "_")
      }
    )
    return(list(y = y, support = support))
  } else {
    return(y)
  }
}
