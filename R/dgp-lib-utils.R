#' Helper function to split data into training and test sets
#' 
#' @param X Data matrix.
#' @param y Response vector.
#' @param train_prop Proportion of data in training set.
#' 
#' @export
dataSplit <- function(X, y, train_prop = 0.5) {
  n <- nrow(X)
  train_ids <- sample(1:n, size = round(n * train_prop), replace = F)
  out <- list(X = X[train_ids, , drop = FALSE], y = y[train_ids],
              Xtest = X[-train_ids, , drop = FALSE], ytest = y[-train_ids])
  return(out)
}

#' Helper function to compute indicator function in LSS model.
#' 
#' @param X data matrix
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

