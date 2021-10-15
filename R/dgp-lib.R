#' General DGP constructor function to generate X and y data.
#' 
#' @description A general DGP constructor function that generates X and y data 
#'   for any supervised learning DGP, provided the functions for simulating X, 
#'   y, and the additive error term.
#'
#' @inheritParams shared_dgp_lib_args
#' @param x_fun Function to generate X data.
#' @param y_fun Function to generate y data.
#' @param err_fun Function to generate error/noise data.
#' @param ... Additional arguments to pass to \code{x_fun}, \code{y_fun},
#'   and \code{err_fun}. If argument does not exist in \code{x_fun}, 
#'   \code{y_fun}, or \code{err_fun}, argument is ignored.
#' 
#' @inherit shared_dgp_lib_args return
#'   
#' @details TODO: explain how to write x_fun, y_fun, err_fun. Required args +
#'   optional args + other caveats.
#'   Model: y = y_fun(X) + err_fun(X, y_fun(X)), where X = x_fun(...)
#'
#' @examples 
#' data_out <- xy_dgp_constructor(x_fun = MASS::mvrnorm, 
#'                                y_fun = generate_y_linear,
#'                                err_fun = rnorm, data_split = TRUE,
#'                                # dgp arguments
#'                                n = 100,
#'                                # arguments specifically for x_fun
#'                                .x_mu = rep(0, 10), .x_Sigma = diag(10),
#'                                # arguments specifically for y_fun
#'                                .y_betas = rnorm(10), .y_return_support = TRUE,
#'                                # arguments specifically for err_fun
#'                                .err_sd = 1)
#' data_out <- xy_dgp_constructor(x_fun = MASS::mvrnorm, 
#'                                y_fun = generate_y_linear,
#'                                err_fun = rnorm, data_split = TRUE,
#'                                # dgp arguments
#'                                n = 100,
#'                                # arguments specifically for x_fun
#'                                mu = rep(0, 10), Sigma = diag(10),
#'                                # arguments specifically for y_fun
#'                                betas = rnorm(10), return_support = TRUE,
#'                                # arguments specifically for err_fun
#'                                sd = 1)
#'
#' @export
xy_dgp_constructor <- function(x_fun, y_fun, err_fun, 
                               data_split = FALSE, train_prop = 0.5,
                               return_values = c("X", "y", "support"),
                               ...) {
  return_values <- match.arg(return_values, several.ok = TRUE)
  
  args_list <- list(...)
  if (identical(args_list, list())) {
    args_list <- NULL
    X_args_list <- NULL
    y_args_list <- NULL
    err_args_list <- NULL
  } else {
    if (is.null(names(args_list)) | any(names(args_list) == "")) {
      stop("Additional arguments passed to xy_dgp_wrapper() must be named.")
    }
    X_args_list <- list()
    y_args_list <- list()
    err_args_list <- list()
    for (arg_name in names(args_list)) {
      if (startsWith(arg_name, ".x_")) {
        root_arg_name <- substr(arg_name, 4, nchar(arg_name))
        X_args_list[[root_arg_name]] <- args_list[[arg_name]]
        args_list[[arg_name]] <- NULL
      } else if (startsWith(arg_name, ".y_")) {
        root_arg_name <- substr(arg_name, 4, nchar(arg_name))
        y_args_list[[root_arg_name]] <- args_list[[arg_name]]
        args_list[[arg_name]] <- NULL
      } else if (startsWith(arg_name, ".err_")) {
        root_arg_name <- substr(arg_name, 6, nchar(arg_name))
        err_args_list[[root_arg_name]] <- args_list[[arg_name]]
        args_list[[arg_name]] <- NULL
      }
    }
    if (identical(X_args_list, list())) {
      X_args_list <- NULL
    }
    if (identical(y_args_list, list())) {
      y_args_list <- NULL
    }
    if (identical(err_args_list, list())) {
      err_args_list <- NULL
    }
    if (length(args_list) == 0) {
      args_list <- NULL
    }
  }
  
  X_out <- R.utils::doCall(x_fun, args = args_list, alwaysArgs = X_args_list)
  if (!is.list(X_out)) {
    X_out <- list(X = X_out)
  }
  y_true_out <- R.utils::doCall(y_fun, args = c(X_out, args_list),
                                alwaysArgs = y_args_list)
  if (!is.list(y_true_out)) {
    y_true_out <- list(y = y_true_out)
  }
  err_out <- R.utils::doCall(err_fun, args = c(X_out, y_true_out, args_list),
                             alwaysArgs = err_args_list)
  y <- y_true_out$y + err_out
  
  out <- return_DGP_output(X = X_out$X, y = y, support = y_true_out$support, 
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  return(out)
}

#' Generate independent Gaussian covariates and linear response data.
#' 
#' @description Generate independent normally-distributed covariates (including
#'   potentially omitted variables) and linear response data with a specified
#'   error distribution.
#'
#' @inheritParams shared_dgp_lib_args
#' @param p_obs Number of observed features.
#' @param p_unobs Number of unobserved (omitted) features.
#' @param s_obs Number of observed features with non-zero coefficients.
#' @param s_unobs Number of unobserved (omitted) features with non-zero
#'   coefficients.
#' @param betas Coefficient vector for observed design matrix.
#' @param betas_unobs Coefficient vector for unobserved design matrix.
#' @param betas_sd (Optional) SD of normal distribution from which to draw 
#'   \code{betas}. Only used if \code{betas} argument is \code{NULL}.
#' @param betas_unobs_sd (Optional) SD of normal distribution from which to draw 
#'   \code{betas_unobs}. Only used if \code{betas_unobs} argument is 
#'   \code{NULL}.
#' 
#' @inherit shared_dgp_lib_args return
#' 
#' @details TODO: Support is the first s_obs and s_unobs features in the
#'   generated X and U data.
#'   
#' @export
linear_gaussian_dgp <- function(n, p_obs = 0, p_unobs = 0, 
                                s_obs = p_obs, s_unobs = p_unobs,
                                betas = NULL, betas_unobs = NULL,
                                betas_sd = 1, betas_unobs_sd = 1, err = NULL, 
                                data_split = FALSE, train_prop = 0.5,
                                return_values = c("X", "y", "support"),
                                ...) {
  return_values <- match.arg(return_values, several.ok = TRUE)
  
  # simulate observed covariates
  if (p_obs != 0) {
    X <- generate_X_gaussian(n = n, p = p_obs)
  } else {
    X <- matrix(0, nrow = n, ncol = 1)
    betas <- 0
  }
  
  # simulate unobserved covariates
  if (p_unobs != 0) {
    U <- generate_X_gaussian(n = n, p = p_unobs)
  } else {
    U <- matrix(0, nrow = n, ncol = 1)
    betas_unobs <- 0
  }
  
  # simulate betas from gaussian
  if (is.null(betas)) {
    betas <- stats::rnorm(p_obs, mean = 0, sd = betas_sd)
    if (s_obs != p_obs) {
      betas[(s_obs + 1):length(betas)] <- 0
    }
  }
  
  # simulate unobserved betas from gaussian
  if (is.null(betas_unobs)) {
    betas_unobs <- stats::rnorm(p_unobs, mean = 0, sd = betas_unobs_sd)
    if (s_unobs != p_unobs) {
      betas_unobs[(s_unobs + 1):length(betas_unobs)] <- 0
    }
  }
  
  # simulate linear y
  y <- generate_y_linear(
    X = X, U = U, betas = betas, betas_unobs = betas_unobs, err = err,
    return_support = "support" %in% return_values, ...
  )
  
  if ("support" %in% return_values) {
    support <- y$support
    y <- y$y
  }
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  
  return(out)
}

#' Generate correlated Gaussian covariates and linear response data.
#' 
#' @description Generate normally-distributed covariates that are potentially
#'   correlated and linear response data with a specified error distribution.
#'
#' @inheritParams shared_dgp_lib_args
#' @param p_uncorr Number of uncorrelated features.
#' @param p_corr Number of features in correlated group.
#' @param s_uncorr Number of features in uncorrelated group with non-zero coef.
#' @param s_corr Number of features in correlated group with non-zero coef.
#' @param corr Correlation between features in correlated group.
#' @param betas_uncorr Coefficient vector for uncorrelated features.
#' @param betas_corr Coefficient vector for correlated features.
#' @param betas_uncorr_sd (Optional) SD of normal distribution from which to 
#'   draw \code{betas_uncorr}. Only used if \code{betas_uncorr} argument is 
#'   \code{NULL}.
#' @param betas_corr_sd (Optional) SD of normal distribution from which to draw 
#'   \code{betas_corr}. Only used if \code{betas_corr} argument is 
#'   \code{NULL}.
#'   
#' @inherit shared_dgp_lib_args return
#' 
#' @export
correlated_linear_gaussian_dgp <- function(n, p_uncorr, p_corr, 
                                           s_uncorr = p_uncorr, s_corr = p_corr,
                                           corr, 
                                           betas_uncorr = NULL, 
                                           betas_corr = NULL,
                                           betas_uncorr_sd = 1,
                                           betas_corr_sd = 1,
                                           err = NULL,
                                           data_split = FALSE, train_prop = 0.5,
                                           return_values = c("X", "y", 
                                                             "support"), 
                                           ...) {
  # simulate correlated covariates
  X_corr <- NULL
  if (p_corr != 0) {
    X_corr <- generate_X_gaussian(n = n, p = p_corr, corr = corr)
  }
  
  # simulate uncorrelated covariates
  X_uncorr <- NULL
  if (p_uncorr != 0) {
    X_uncorr <- generate_X_gaussian(n = n, p = p_uncorr, corr = 0)
  }
  
  # simulate betas_corr from gaussian
  if (is.null(betas_corr)) {
    betas_corr <- stats::rnorm(p_corr, mean = 0, sd = betas_corr_sd)
    if ((s_corr != p_corr) && (p_corr > 0)) {
      betas_corr[(s_corr + 1):length(betas_corr)] <- 0
    }
  }
  
  # simulate betas_uncorr from gaussian
  if (is.null(betas_uncorr)) {
    betas_uncorr <- stats::rnorm(p_uncorr, mean = 0, sd = betas_uncorr_sd)
    if ((s_uncorr != p_uncorr) && (p_uncorr > 0)) {
      betas_uncorr[(s_uncorr + 1):length(betas_uncorr)] <- 0
    }
  }
  
  X <- cbind(X_uncorr, X_corr)
  betas <- c(betas_uncorr, betas_corr)
  
  # simulate linear y
  y <- generate_y_linear(X = X, betas = betas, err = err, 
                         return_support = "support" %in% return_values, ...)
  
  if ("support" %in% return_values) {
    support <- y$support
    y <- y$y
  }
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  return(out)
}

#' Generate independent Gaussian covariates and LSS response data.
#'
#' @description Generate independent normally-distributed covariates and LSS
#'   response data with a specified error distribution.
#'   
#' @inheritParams shared_dgp_lib_args
#' @param k Order of the interactions.
#' @param s Number of interactions in the LSS model.
#' @param thresholds A scalar or a s x k matrix of the thresholds for each term 
#'   in the LSS model.
#' @param signs A scalar or a s x k matrix of the sign of each interaction 
#'   (1 means > while -1 means <).
#' @param betas Scalar or parameter vector for interaction terms.
#' @param intercept Scalar intercept term.
#' @param overlap If TRUE, simulate support indices with replacement; if FALSE,
#'   simualte support indices without replacement (so no overlap).
#'   
#' @inherit shared_dgp_lib_args return
#' 
#' @export
lss_gaussian_dgp <- function(n, p, k, s, thresholds = 0, signs = 1, betas = 1,
                             intercept = 0, overlap = FALSE, err = NULL,
                             data_split = FALSE, train_prop = 0.5,
                             return_values = c("X", "y", "support"), ...) {
  X <- generate_X_gaussian(n = n, p = p)
  y <- generate_y_lss(X = X, k = k, s = matrix(1:(s * k), nrow = s, ncol = k),
                      thresholds = thresholds, 
                      signs = signs, betas = betas, intercept = intercept, 
                      overlap = overlap, err = err, 
                      return_support = "support" %in% return_values, ...)
  
  if ("support" %in% return_values) {
    support <- y$support
    y <- y$y
  }
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  return(out)
}

#' Generate correlated Gaussian covariates and LSS response data.
#'
#' @description Generate normally-distributed covariates that are potentially
#'   correlated and LSS response data with a specified error distribution.
#'
#' @inheritParams shared_dgp_lib_args
#' @param p_uncorr Number of uncorrelated features.
#' @param p_corr Number of features in correlated group.
#' @param s_uncorr Number of interactions from features in uncorrelated group.
#' @param s_corr Number of interactions from features in correlated group.
#' @param corr Correlation between features in correlated group.
#' @param k Order of the interactions.
#' @param thresholds A scalar or a s x k matrix of the thresholds for each term 
#'   in the LSS model.
#' @param signs A scalar or a s x k matrix of the sign of each interaction 
#'   (1 means > while -1 means <).
#' @param betas Scalar or parameter vector for interaction terms.
#' @param intercept Scalar intercept term.
#' @param overlap If \code{TRUE}, simulate support indices with replacement; if
#'   \code{FALSE}, simualte support indices without replacement (so no overlap).
#' @param mixed_int If \code{TRUE}, correlated and uncorrelated variables are
#'   mixed together when constructing an interaction of order-k. If 
#'   \code{FALSE}, each interaction of order-k is composed of only correlated
#'   variables or only uncorrelated variables.
#'   
#' @inherit shared_dgp_lib_args return
#' 
#' @export
correlated_lss_gaussian_dgp <- function(n, p_uncorr, p_corr, 
                                        s_uncorr = p_uncorr, s_corr = p_corr,
                                        corr, k, thresholds = 0, signs = 1, 
                                        betas = 1,  intercept = 0, 
                                        overlap = FALSE, mixed_int = FALSE,
                                        err = NULL, data_split = FALSE,
                                        train_prop = 0.5,
                                        return_values = c("X", "y", "support"),
                                        ...) {
  
  X_corr <- generate_X_gaussian(n = n, p = p_corr, corr = corr)
  X_uncorr <- generate_X_gaussian(n = n, p = p_uncorr, corr = 0)
  X <- cbind(X_uncorr, X_corr)
  
  support <- NULL
  if (s_uncorr > 0) {
    support <- c(support, 1:(s_uncorr * k))
  }
  if (s_corr > 0) {
    support <- c(support, (p_uncorr + 1):(p_uncorr + (s_corr * k)))
  }
  if (mixed_int) {
    s <- matrix(sample(support, size = length(support), replace = F), 
                nrow = s_uncorr + s_corr, ncol = k)
  } else {
    s <- matrix(support, nrow = s_uncorr + s_corr, ncol = k)
  }
  y <- generate_y_lss(X = X, k = k, s = s, 
                      thresholds = thresholds, signs = signs, betas = betas, 
                      intercept = intercept, overlap = overlap, err = err, 
                      return_support = "support" %in% return_values, ...)
  
  if ("support" %in% return_values) {
    support <- y$support
    y <- y$y
  }
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  return(out)
}

#' Read in real world data from X and y.
#' 
#' @inheritParams shared_dgp_lib_args
#' @inherit shared_dgp_lib_args return
#' 
#' @export
rwd_dgp <- function(X, y, data_split, train_prop = 0.5, 
                    return_values = c("X", "y")) {
  return_values <- match.arg(return_values, choices = c("X", "y", "support"),
                             several.ok = TRUE)
  out <- return_DGP_output(X = X, y = y, support = NULL,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  return(out)
}