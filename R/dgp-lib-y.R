#' Simulate linear response data.
#' 
#' @description Generate linear response data with a specified error 
#'   distribution given the observed and unobserved design matrices.
#'
#' @param X Design data matrix of observed variables.
#' @param U Design data matrix of unobserved (omitted) variables.
#' @param betas Coefficient vector for observed design matrix.
#' @param betas_unobs Coefficient vector for unobserved design matrix.
#' @param err Function from which to generate simulate error vector. Default
#'   is \code{NULL} which adds no error to the response y.
#' @param return_support Logical specifying whether or not to return a vector
#'   of the support column names. If \code{X} has no column names, then the
#'   indices of the support are used.
#' @param ... Other arguments to pass to err() to generate the error vector.
#' 
#' @return A response vector of length nrow(X).
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
  
  y <- U %*% betas_unobs + X %*% betas + eps
  
  if (return_support) {
    if (is.null(colnames(X))) {
      support <- which(betas != 0)
    } else {
      support <- colnames(X)[betas != 0]
    }
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
#' @param X Design data matrix of observed variables.
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
#' @param err Function from which to generate simulate error vector. Default
#'   is \code{NULL} which adds no error to the response y.
#' @param return_support Logical specifying whether or not to return a vector
#'   of the support column names. If \code{X} has no column names, then the
#'   indices of the support are used.
#' @param ... Other arguments to pass to err() to generate the error vector.
#' 
#' @return A response vector of length nrow(X).
#' 
#' @export
generate_y_lss <- function(X, k, s, thresholds = 1, signs = 1,
                           betas = 1, intercept = 0, overlap = FALSE,
                           err = NULL, return_support = FALSE, ...) {
  
  if (!is.matrix(s)) {
    support_idx <- sample(1:ncol(X), k * s, replace = overlap) %>%
      matrix(., nrow = s, ncol = k)
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
    if (!is.null(colnames(X))) {
      support_idx <- apply(support_idx, 1:2, function(i) colnames(X)[i])
    }
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
