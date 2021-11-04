#' General DGP constructor function to generate X and y data.
#' 
#' @description A general DGP constructor function that generates X and y data 
#'   for any supervised learning DGP, provided the functions for simulating X, 
#'   y, and the additive error term.
#'
#' @inheritParams shared_dgp_lib_args
#' @param x_fun Function to generate X data.
#' @param y_fun Function to generate y data.
#' @param err_fun Function to generate error/noise data. Default \code{NULL} 
#'   adds no error to the output of \code{y_fun()}.
#' @param add_err Logical. If \code{TRUE} (default), add result of 
#'   \code{err_fun()} to result of \code{y_fun()} to obtain the simulated 
#'   response vector. If \code{FALSE}, return \code{err_fun(y_fun(...), ...)} as 
#'   the simulated response vector. Note that \code{add_err = TRUE} will return 
#'   an error for categorical responses \code{y}.
#' @param ... Additional arguments to pass to \code{x_fun()}, \code{y_fun()},
#'   and \code{err_fun()}. If argument does not exist in \code{x_fun()}, 
#'   \code{y_fun()}, or \code{err_fun()}, argument is ignored.
#' 
#' @inherit shared_dgp_lib_args return
#'   
#' @details If \code{add_err = TRUE}, data is generated from the following 
#' additive model: 
#' \deqn{y = y_fun(X, ...) + err_fun(X, y_fun(X), ...), where X = x_fun(...).}
#' 
#' If \code{add_err = FALSE}, data is generated via:
#' \deqn{y = err_fun(X, y_fun(X, ...), ...), where X = x_fun(...).}
#' 
#' Note that while \code{err_fun()} is allowed to depend on both X and y, it is
#' not necessary that \code{err_fun()} depend on X or y.
#' 
#' If x_fun, y_fun, and err_fun have arguments of the same name, then use the
#' prefix ".x_", ".y_", ".err_" in front of the argument name (passed via ...) 
#' to differentiate it for use in x_fun, y_fun, or err_fun, respectively. 
#' See the examples.
#'
#' @examples 
#' # generate X = 100 x 10 standard Gaussian, y = linear regression model
#' sim_data <- xy_dgp_constructor(x_fun = MASS::mvrnorm, 
#'                                y_fun = generate_y_linear,
#'                                err_fun = rnorm, data_split = TRUE,
#'                                # shared dgp arguments
#'                                n = 100,
#'                                # arguments specifically for x_fun
#'                                .x_mu = rep(0, 10), .x_Sigma = diag(10),
#'                                # arguments specifically for y_fun
#'                                .y_betas = rnorm(10), .y_return_support = TRUE,
#'                                # arguments specifically for err_fun
#'                                .err_sd = 1)
#' # or alternatively, (since arguments of x_fun, y_fun, err_fun are unique, 
#' # with the exception of `n`)
#' sim_data <- xy_dgp_constructor(x_fun = MASS::mvrnorm, 
#'                                y_fun = generate_y_linear,
#'                                err_fun = rnorm, data_split = TRUE,
#'                                # shared dgp arguments
#'                                n = 100,
#'                                # arguments specifically for x_fun
#'                                mu = rep(0, 10), Sigma = diag(10),
#'                                # arguments specifically for y_fun
#'                                betas = rnorm(10), return_support = TRUE,
#'                                # arguments specifically for err_fun
#'                                sd = 1)
#'
#' @export
xy_dgp_constructor <- function(x_fun, y_fun, err_fun = NULL, add_err = TRUE,
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
  if (!is.null(err_fun)) {
    if (add_err) {
      if (is.factor(y_true_out$y)) {
        stop("Cannot add error term to factor response y: ",
             "'+' not meaningful for factors. Try add_err = FALSE instead.")
      }
      err_out <- R.utils::doCall(err_fun,
                                 args = c(X_out, y_true_out, args_list),
                                 alwaysArgs = err_args_list)
      y <- y_true_out$y + err_out
    } else {
      y <- R.utils::doCall(err_fun, 
                           args = c(X_out, y_true_out, args_list),
                           alwaysArgs = err_args_list)
    }
  } else {
    y <- y_true_out$y
  }
  
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
#' @details Data is generated via: \deqn{y = betas %*% X + 
#' betas_unobs %*% U + err(...),} where X, U are standard Gaussian random 
#' matrices and the true underlying support of this data is the first s_obs and 
#' s_unobs features in X and U respectively.
#' 
#' @examples
#' # generate data from: y = betas_1 * x_1 + betas_2 * x_2 + N(0, 0.5), where
#' # betas_1, betas_2 ~ N(0, 1) and X ~ N(0, I_10)
#' sim_data <- linear_gaussian_dgp(n = 100, p_obs = 10, s_obs = 2, betas_sd = 1,
#'                                 err = rnorm, sd = .5)
#' 
#' # generate data from y = betas %*% X - u_1 + t(df = 1), where
#' # betas ~ N(0, .5), betas_unobs = [-1, 0], X ~ N(0, I_10), U ~ N(0, I_2)
#' sim_data <- linear_gaussian_dgp(n = 100, p_obs = 10, p_unobs = 2,
#'                                 betas_sd = .5, betas_unobs = c(-1, 0),
#'                                 err = rt, df = 1)
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
#' @details Data is generated via: \deqn{y = betas_uncorr %*% X_uncorr + 
#' betas_corr %*% X_corr + err(...),} where X_uncorr is an (uncorrelated)
#' standard Gaussian random matrix and X_corr is a correlated Gaussian random
#' matrix with variance 1 and Cor(X_corr_i, X_corr_j) = corr for all i, j. The 
#' true underlying support of this data is the first s_uncorr and 
#' s_corr features in X_uncorr and X_corr respectively.
#' 
#' @examples
#' # generate data from: y = betas_corr_1 * x_corr_1 + betas_corr_2 * x_corr_2 + N(0, 0.5), 
#' # where betas_corr_1, betas_corr_2 ~ N(0, 1), 
#' # Var(X_corr_i) = 1, Cor(X_corr_i, X_corr_j) = 0.7 for all i, j = 1, ..., 10
#' sim_data <- correlated_linear_gaussian_dgp(n = 100, p_uncorr = 0, p_corr = 10,
#'                                            s_corr = 2, corr = 0.7, 
#'                                            err = rnorm, sd = .5)
#' 
#' # generate data from y = betas_uncorr %*% X_uncorr - X_corr_1 + t(df = 1), where
#' # betas_uncorr ~ N(0, .5), betas_corr = [-1, 0], X_uncorr ~ N(0, I_10), 
#' # X_corr ~ N(0, Sigma), Sigma has 1s on diagonals and 0.7 elsewhere.
#' sim_data <- correlated_linear_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 2,
#'                                            corr = 0.7, betas_uncorr_sd = 1,
#'                                            betas_corr = c(-1, 0), 
#'                                            err = rt, df = 1)
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

#' Generate independent Gaussian covariates and (binary) logistic response data.
#' 
#' @description Generate independent normally-distributed covariates and 
#'   logistic response data.
#'
#' @inheritParams shared_dgp_lib_args
#' @param s Number of features with non-zero coefficients.
#' @param betas Coefficient vector for observed design matrix.
#' @param betas_sd (Optional) SD of normal distribution from which to draw 
#'   \code{betas}. Only used if \code{betas} argument is \code{NULL}.
#' @param ... Not used.
#' 
#' @inherit shared_dgp_lib_args return
#'   
#' @details Data is generated via: \deqn{log(p / (1 - p)) = betas %*% X,} where 
#' p = P(y = 1 | X), X is a standard Gaussian random matrix, and the true 
#' underlying support of this data is the first s features in X (unless 
#' specified otherwise by `betas`).
#' 
#' @examples
#' # generate data from: log(p / (1 - p)) = betas_1 * x_1 + betas_2 * x_2, where
#' # betas_1, betas_2 ~ N(0, 1) and X ~ N(0, I_10)
#' sim_data <- logistic_gaussian_dgp(n = 100, p = 10, s = 2, betas_sd = 1)
#'   
#' @export
logistic_gaussian_dgp <- function(n, p, s = p, betas = NULL, betas_sd = 1,
                                  data_split = FALSE, train_prop = 0.5,
                                  return_values = c("X", "y", "support"),
                                  ...) {
  return_values <- match.arg(return_values, several.ok = TRUE)
  
  # simulate observed covariates
  X <- generate_X_gaussian(n = n, p = p)
  
  # simulate betas from gaussian
  if (is.null(betas)) {
    betas <- stats::rnorm(p, mean = 0, sd = betas_sd)
    if (s != p) {
      betas[(s + 1):length(betas)] <- 0
    }
  }
  
  # simulate linear y
  y <- generate_y_logistic(
    X = X, betas = betas, return_support = "support" %in% return_values, ...
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

#' Generate correlated Gaussian covariates and (binary) logistic response data.
#' 
#' @description Generate normally-distributed covariates that are potentially
#'   correlated and (binary) logistic response data.
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
#' @param ... Not used.
#'   
#' @inherit shared_dgp_lib_args return
#' 
#' @details Data is generated via: 
#' \deqn{log(p / (1 - p)) = betas_uncorr %*% X_uncorr + 
#' betas_corr %*% X_corr,} where p = P(y = 1 | X), X_uncorr is an 
#' (uncorrelated) standard Gaussian random matrix, and X_corr is a correlated 
#' Gaussian random matrix with variance 1 and Cor(X_corr_i, X_corr_j) = corr for 
#' all i, j. The true underlying support of this data is the first s_uncorr and 
#' s_corr features in X_uncorr and X_corr respectively.
#' 
#' @examples
#' # generate data from: log(p / (1 - p)) = betas_corr_1 * x_corr_1 + betas_corr_2 * x_corr_2, 
#' # where betas_corr_1, betas_corr_2 ~ N(0, 1), 
#' # Var(X_corr_i) = 1, Cor(X_corr_i, X_corr_j) = 0.7 for all i, j = 1, ..., 10
#' sim_data <- correlated_logistic_gaussian_dgp(n = 100, p_uncorr = 0, p_corr = 10,
#'                                              s_corr = 2, corr = 0.7)
#' 
#' # generate data from: log(p / (1 - p)) = betas_uncorr %*% X_uncorr - X_corr_1,
#' # where betas_uncorr ~ N(0, .5), betas_corr = [-1, 0], X_uncorr ~ N(0, I_10), 
#' # X_corr ~ N(0, Sigma), Sigma has 1s on diagonals and 0.7 elsewhere.
#' sim_data <- correlated_logistic_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 2,
#'                                              corr = 0.7, betas_uncorr_sd = 1,
#'                                              betas_corr = c(-1, 0))
#' 
#' @export
correlated_logistic_gaussian_dgp <- function(n, p_uncorr, p_corr, 
                                             s_uncorr = p_uncorr, 
                                             s_corr = p_corr,
                                             corr, 
                                             betas_uncorr = NULL, 
                                             betas_corr = NULL,
                                             betas_uncorr_sd = 1,
                                             betas_corr_sd = 1,
                                             data_split = FALSE, 
                                             train_prop = 0.5,
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
  y <- generate_y_logistic(X = X, betas = betas,
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
#'   simulate support indices without replacement (so no overlap).
#'   
#' @inherit shared_dgp_lib_args return
#' 
#' @details Data is generated via: \deqn{y = intercept + sum_{i = 1}^{s} beta_i prod_{j = 1}^{k}1(X_{S_j} lessgtr thresholds_ij) + err(...),} where X is a 
#' standard Gaussian random matrix. If \code{overlap = TRUE}, then the true 
#' interaction support is randomly chosen from the p features in \code{X}. If
#' \code{overlap = FALSE}, then the true interaction support is sequentially 
#' taken from the first \code{s*k} features in \code{X}. 
#' 
#' For more details on the LSS model, see Behr, Merle, et al. "Provable Boolean Interaction Recovery from Tree Ensemble obtained via Random Forests." arXiv preprint arXiv:2102.11800 (2021).
#' 
#' @examples
#' # generate data from: y = 1(X_1 > 0, X_2 > 0) + 1(X_3 > 0, X_4 > 0), where
#' # X is a 100 x 10 standard Gaussian random matrix
#' sim_data <- lss_gaussian_dgp(n = 100, p = 10, k = 2, s = 2,
#'                              thresholds = 0, signs = 1, betas = 1)
#' 
#' # generate data from: y = 3 * 1(X_1 < 0) - 1(X_2 > 1) + N(0, 1), where
#' # X is a 100 x 10 standard Gaussian random matrix
#' sim_data <- lss_gaussian_dgp(n = 100, p = 10, k = 1, s = 2, 
#'                              thresholds = matrix(0:1, nrow = 2),
#'                              signs = matrix(c(-1, 1), nrow = 2),
#'                              betas = c(3, -1),
#'                              err = rnorm)
#' 
#' @export
lss_gaussian_dgp <- function(n, p, k, s, thresholds = 0, signs = 1, betas = 1,
                             intercept = 0, overlap = FALSE, err = NULL,
                             data_split = FALSE, train_prop = 0.5,
                             return_values = c("X", "y", "support"), ...) {
  X <- generate_X_gaussian(n = n, p = p)
  if (overlap) {
    s_mat <- matrix(sample(1:p, s * k, replace = TRUE),
                    nrow = s, ncol = k, byrow = TRUE)
  } else {
    s_mat <- matrix(1:(s * k), nrow = s, ncol = k, byrow = TRUE)
  }
  
  y <- generate_y_lss(X = X, k = k, s = s_mat, thresholds = thresholds, 
                      signs = signs, betas = betas, intercept = intercept, 
                      err = err, return_support = "support" %in% return_values,
                      ...)
  
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
#' @details Data is generated via: \deqn{y = intercept + sum_{i = 1}^{s} beta_i prod_{j = 1}^{k}1(X_{S_j} lessgtr thresholds_ij) + err(...),} where 
#' X = \[X_uncorr, X_corr\], X_uncorr is an (uncorrelated) standard Gaussian 
#' random matrix, and X_corr is a correlated Gaussian random matrix with 
#' variance 1 and Cor(X_corr_i, X_corr_j) = corr for all i, j. If 
#' \code{overlap = TRUE}, then the true interaction support is randomly chosen 
#' from the (p_uncorr + p_corr) features in \code{X}. If \code{overlap = FALSE}, 
#' then the true interaction support is sequentially taken from the first 
#' \code{s_uncorr*k} features in X_uncorr and the first 
#' \code{s_corr*k} features in X_corr.
#' 
#' For more details on the LSS model, see Behr, Merle, et al. "Provable Boolean Interaction Recovery from Tree Ensemble obtained via Random Forests." arXiv preprint arXiv:2102.11800 (2021).
#' 
#' @examples
#' # generate data from: y = 1(X_1 > 0, X_2 > 0) + 1(X_3 > 0, X_4 > 0), where
#' # X is a 100 x 10 correlated Gaussian random matrix with 
#' # Var(X_i) = 1 for all i and Cor(X_i, X_j) = 0.7 for all i != j
#' sim_data <- correlated_lss_gaussian_dgp(n = 100, p_uncorr = 0, p_corr = 10, 
#'                                         k = 2, s_corr = 2, corr = 0.7,
#'                                         thresholds = 0, signs = 1, betas = 1)
#' 
#' # generate data from: y = 3 * 1(X_1 > 0, X_2 > 0) - 1(X_11 > 0, X_12 > 0) + N(0, 1), 
#' # where X = [Z, U], Z is a 100 x 10 standard Gaussian random matrix, 
#' # U is a 100 x 10 Gaussian random matrix with Var(U_i) = 1 and Cor(U_i, U_j) = 0.7
#' sim_data <- correlated_lss_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 10,
#'                                         s_uncorr = 1, s_corr = 1, corr = 0.7,
#'                                         k = 2, betas = c(3, -1), err = rnorm)
#' 
#' # generate data from: y = \sum_{i = 1}^{4} \prod_{j = 1}^{2} 1(X_{s_j} > 0), 
#' # where s_j \in {1:4, 11:14} are randomly selected indiceds, X = [Z, U], 
#' # Z is a 100 x 10 standard Gaussian random matrix, U is a 100 x 10 Gaussian 
#' # random matrix with Var(U_i) = 1 and Cor(U_i, U_j) = 0.7
#' # i.e., interactions may consist of both correlated and uncorrelated features
#' sim_data <- correlated_lss_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 10, 
#'                                         s_uncorr = 2, s_corr = 2, k = 2, 
#'                                         corr = 0.7, mixed_int = TRUE)
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
    if (overlap) {
      support <- c(support, sample(1:p_uncorr, (s_uncorr * k), replace = TRUE))
    } else {
      support <- c(support, 1:(s_uncorr * k))
    }
  }
  if (s_corr > 0) {
    if (overlap) {
      support <- c(support, sample((p_uncorr + 1):(p_corr + p_uncorr),
                                   (s_corr * k), replace = TRUE))
    } else {
      support <- c(support, (p_uncorr + 1):(p_uncorr + (s_corr * k)))
    }
  }
  if (mixed_int) {
    s <- matrix(sample(support, size = length(support), replace = F), 
                nrow = s_uncorr + s_corr, ncol = k)
  } else {
    s <- matrix(support, nrow = s_uncorr + s_corr, ncol = k, byrow = TRUE)
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
#' @examples
#' # read in iris data
#' iris_data <- rwd_dgp(X = iris %>% dplyr::select(-Species), y = iris$Species)
#' 
#' @export
rwd_dgp <- function(X, y, support = NULL, data_split = FALSE, train_prop = 0.5, 
                    return_values = c("X", "y")) {
  return_values <- match.arg(return_values, choices = c("X", "y", "support"),
                             several.ok = TRUE)
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  return(out)
}