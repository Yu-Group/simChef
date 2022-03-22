#' General DGP constructor function to generate X and y data.
#'
#' @description A general DGP constructor function that generates X and y data
#'   for any supervised learning DGP, provided the functions for simulating X,
#'   y, and the additive error term.
#'
#' @inheritParams shared_dgp_lib_args
#' @param X_fun Function to generate X data. Must take an argument \code{n} or
#'   \code{.n} which determines the number of observations to generate.
#' @param y_fun Function to generate y data. Must take an argument \code{X}
#'   which accepts the result from \code{X_fun}.
#' @param err_fun Function to generate error/noise data. Default \code{NULL}
#'   adds no error to the output of \code{y_fun()}.
#' @param add_err Logical. If \code{TRUE} (default), add result of
#'   \code{err_fun()} to result of \code{y_fun()} to obtain the simulated
#'   response vector. If \code{FALSE}, return \code{err_fun(y_fun(...), ...)} as
#'   the simulated response vector. Note that \code{add_err = TRUE} will return
#'   an error for categorical responses \code{y}.
#' @eval dots_doc(prefix = c("X", "y", "err"))
#'
#' @inherit shared_dgp_lib_args return
#'
#' @details If \code{add_err = TRUE}, data is generated from the following
#' additive model:
#' \deqn{y = y_fun(X, ...) + err_fun(X, y_fun(X), ...), where X = X_fun(...).}
#'
#' If \code{add_err = FALSE}, data is generated via:
#' \deqn{y = err_fun(X, y_fun(X, ...), ...), where X = X_fun(...).}
#'
#' Note that while \code{err_fun()} is allowed to depend on both X and y, it is
#' not necessary that \code{err_fun()} depend on X or y.
#'
#' @examples
#' # generate X = 100 x 10 standard Gaussian, y = linear regression model
#' sim_data <- xy_dgp_constructor(X_fun = MASS::mvrnorm,
#'                                y_fun = generate_y_linear,
#'                                err_fun = rnorm, data_split = TRUE,
#'                                # shared dgp arguments
#'                                n = 100,
#'                                # arguments specifically for X_fun
#'                                .X_mu = rep(0, 10), .X_Sigma = diag(10),
#'                                # arguments specifically for y_fun
#'                                .y_betas = rnorm(10), .y_return_support = TRUE,
#'                                # arguments specifically for err_fun
#'                                .err_sd = 1)
#' # or alternatively, (since arguments of X_fun, y_fun, err_fun are unique,
#' # with the exception of `n`)
#' sim_data <- xy_dgp_constructor(X_fun = MASS::mvrnorm,
#'                                y_fun = generate_y_linear,
#'                                err_fun = rnorm, data_split = TRUE,
#'                                # shared dgp arguments
#'                                n = 100,
#'                                # arguments specifically for X_fun
#'                                mu = rep(0, 10), Sigma = diag(10),
#'                                # arguments specifically for y_fun
#'                                betas = rnorm(10), return_support = TRUE,
#'                                # arguments specifically for err_fun
#'                                sd = 1)
#'
#' @export
xy_dgp_constructor <- function(n, X_fun, y_fun, err_fun = NULL, add_err = TRUE,
                               data_split = FALSE, train_prop = 0.5,
                               return_values = c("X", "y", "support"), ...) {
  return_values <- match.arg(return_values, several.ok = TRUE)

  fun_args <- dots_to_fun_args(prefix = c("X", "y", "err"), ...)
  X_args_list <- fun_args$.X_args
  y_args_list <- fun_args$.y_args
  err_args_list <- fun_args$.err_args
  optional_args_list <- fun_args$.optional_args

  X <- do_call(X_fun, n = n, .n = n,
               args = optional_args_list,
               always_args = X_args_list)
  y_out <- do_call(y_fun, X = X,
                   args = optional_args_list,
                   always_args = y_args_list)
  if (is.list(y_out)) {
    y <- y_out$y
    support <- y_out$support
  } else {
    y <- y_out
    support <- NULL
  }
  if (!is.null(err_fun)) {
    if (add_err) {
      if (is.factor(y)) {
        stop("Cannot add error term to factor response y: ",
             "'+' not meaningful for factors. Try add_err = FALSE instead.")
      }
      err_out <- do_call(
        err_fun, n = n, .n = n, X = X, y = y,
        args = optional_args_list,
        always_args = err_args_list
      )
      y <- y + err_out
    } else {
      y <- do_call(
        err_fun, n = n, .n = n, X = X, y = y,
        args = optional_args_list,
        always_args = err_args_list
      )
    }
  }
  out <- return_DGP_output(X = X, y = y, support = support,
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
#' @param s_obs Sparsity level of observed features. Coefficients corresponding
#'   to features after the \code{s_obs} position (i.e., positions i =
#'   \code{s_obs} + 1, ..., \code{p_obs}) are set to 0.
#' @param s_unobs Sparsity level of unobserved (omitted) features. Coefficients
#'   corresponding to features after the \code{s_unobs} position (i.e.,
#'   positions i = \code{s_unobs} + 1, ..., \code{p_unobs}) are set to 0.
#' @eval dots_doc(c("X", "U", "y", "betas", "betas_unobs", "err"), see_also =
#'   c("generate_X_gaussian", "generate_y_linear", "generate_coef",
#'   "generate_errors"))
#'
#' @inherit shared_dgp_lib_args return
#'
#' @details Data is generated via: \deqn{y = intercept + betas %*% X +
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
                                intercept = 0, err = NULL,
                                data_split = FALSE, train_prop = 0.5,
                                return_values = c("X", "y", "support"),
                                ...) {
  return_values <- match.arg(return_values, several.ok = TRUE)

  fun_args <- dots_to_fun_args(
    prefix = c("X", "U", "y", "err", "betas", "betas_unobs"), ...
  )
  X_args_list <- fun_args$.X_args
  U_args_list <- fun_args$.U_args
  y_args_list <- fun_args$.y_args
  err_args_list <- fun_args$.err_args
  betas_args_list <- fun_args$.betas_args
  betas_unobs_args_list <- fun_args$.betas_unobs_args
  optional_args_list <- fun_args$.optional_args

  # simulate observed covariates
  if (p_obs != 0) {
    X <- do_call(
      generate_X_gaussian, .n = n, .p = p_obs,
      args = optional_args_list,
      always_args = X_args_list
    )
    betas <- do_call(
      generate_coef, .betas = betas, .p = p_obs, .s = s_obs,
      args = optional_args_list,
      always_args = betas_args_list
    )
  } else {
    X <- matrix(0, nrow = n, ncol = 1)
    betas <- 0
  }

  # simulate unobserved covariates
  if (p_unobs != 0) {
    U <- do_call(
      generate_X_gaussian, .n = n, .p = p_unobs,
      args = optional_args_list,
      always_args = U_args_list
    )
    betas_unobs <- do_call(
      generate_coef, .betas = betas_unobs, .p = p_unobs, .s = s_unobs,
      args = optional_args_list,
      always_args = betas_unobs_args_list
    )
  } else {
    U <- matrix(0, nrow = n, ncol = 1)
    betas_unobs <- 0
  }

  eps <- do_call(
    generate_errors, err = err, n = n, X = X,
    args = optional_args_list,
    always_args = err_args_list
  )

  # simulate linear y
  y <- R.utils::doCall(
    generate_y_linear, X = X, U = U, betas = betas, betas_unobs = betas_unobs,
    intercept = intercept, err = eps,
    return_support = "support" %in% return_values,
    args = optional_args_list,
    always_args = y_args_list
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
#' @param s_uncorr Sparsity level of features in uncorrelated group.
#'   Coefficients corresponding to features after the \code{s_uncorr} position
#'   (i.e., positions i = \code{s_uncorr} + 1, ..., \code{p_uncorr}) are set to
#'   0.
#' @param s_corr Sparsity level of features in correlated group. Coefficients
#'   corresponding to features after the \code{s_corr} position (i.e.,
#'   positions i = \code{s_corr} + 1, ..., \code{p_corr}) are set to 0.
#' @param corr Correlation between features in correlated group.
#' @param betas_uncorr Coefficient vector for uncorrelated features. If a
#'   scalar is provided, the coefficient vector is a constant vector. If
#'   \code{NULL} (default), entries in the coefficient vector are drawn iid from
#'   N(0, \code{betas_uncorr_sd}^2).
#' @param betas_corr Coefficient vector for correlated features. If a
#'   scalar is provided, the coefficient vector is a constant vector. If
#'   \code{NULL} (default), entries in the coefficient vector are drawn iid from
#'   N(0, \code{betas_corr_sd}^2).
#' @param betas_uncorr_sd (Optional) SD of normal distribution from which to
#'   draw \code{betas_uncorr}. Only used if \code{betas_uncorr} argument is
#'   \code{NULL}.
#' @param betas_corr_sd (Optional) SD of normal distribution from which to draw
#'   \code{betas_corr}. Only used if \code{betas_corr} argument is
#'   \code{NULL}.
#'
#' @inherit shared_dgp_lib_args return
#'
#' @details Data is generated via: \deqn{y = intercept +
#' betas_uncorr %*% X_uncorr +
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
                                           intercept = 0,
                                           err = NULL,
                                           data_split = FALSE, train_prop = 0.5,
                                           return_values = c("X", "y",
                                                             "support"),
                                           ...) {
  # simulate correlated covariates
  X_corr <- NULL
  if (p_corr != 0) {
    X_corr <- generate_X_gaussian(.n = n, .p = p_corr, .corr = corr)
  }

  # simulate uncorrelated covariates
  X_uncorr <- NULL
  if (p_uncorr != 0) {
    X_uncorr <- generate_X_gaussian(.n = n, .p = p_uncorr, .corr = 0)
  }

  # simulate betas_corr and betas_uncorr
  betas_corr <- generate_coef(.betas = betas_corr, .p = p_corr, .s = s_corr,
                              .betas_name = "betas_corr", sd = betas_corr_sd)
  betas_uncorr <- generate_coef(.betas = betas_uncorr, .p = p_uncorr,
                                .s = s_uncorr, betas_name = "betas_uncorr",
                                sd = betas_uncorr_sd)

  X <- cbind(X_uncorr, X_corr)
  betas <- c(betas_uncorr, betas_corr)

  # simulate linear y
  y <- generate_y_linear(X = X, betas = betas, intercept = intercept, err = err,
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
#' @param s Sparsity level of features. Coefficients corresponding to features
#'   after the \code{s} position (i.e., positions i = \code{s} + 1, ...,
#'   \code{p}) are set to 0.
#' @param ... Not used.
#'
#' @inherit shared_dgp_lib_args return
#'
#' @details Data is generated via: \deqn{log(p / (1 - p)) = intercept +
#' betas %*% X,} where p = P(y = 1 | X), X is a standard Gaussian random matrix,
#' and the true underlying support of this data is the first s features in X
#' (unless specified otherwise by `betas`).
#'
#' @examples
#' # generate data from: log(p / (1 - p)) = betas_1 * x_1 + betas_2 * x_2, where
#' # betas_1, betas_2 ~ N(0, 1) and X ~ N(0, I_10)
#' sim_data <- logistic_gaussian_dgp(n = 100, p = 10, s = 2, betas_sd = 1)
#'
#' @export
logistic_gaussian_dgp <- function(n, p, s = p, betas = NULL, betas_sd = 1,
                                  intercept = 0,
                                  data_split = FALSE, train_prop = 0.5,
                                  return_values = c("X", "y", "support"),
                                  ...) {
  return_values <- match.arg(return_values, several.ok = TRUE)

  # simulate observed covariates
  X <- generate_X_gaussian(.n = n, .p = p)

  # simulate betas
  betas <- generate_coef(.betas = betas, .p = p, .s = s, sd = betas_sd)

  # simulate linear y
  y <- generate_y_logistic(
    X = X, betas = betas, intercept = intercept,
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

#' Generate correlated Gaussian covariates and (binary) logistic response data.
#'
#' @description Generate normally-distributed covariates that are potentially
#'   correlated and (binary) logistic response data.
#'
#' @inheritParams shared_dgp_lib_args
#' @inheritParams correlated_linear_gaussian_dgp
#' @param ... Not used.
#'
#' @inherit shared_dgp_lib_args return
#'
#' @details Data is generated via:
#' \deqn{log(p / (1 - p)) = intercept + betas_uncorr %*% X_uncorr +
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
                                             intercept = 0,
                                             data_split = FALSE,
                                             train_prop = 0.5,
                                             return_values = c("X", "y",
                                                               "support"),
                                             ...) {
  # simulate correlated covariates
  X_corr <- NULL
  if (p_corr != 0) {
    X_corr <- generate_X_gaussian(.n = n, .p = p_corr, .corr = corr)
  }

  # simulate uncorrelated covariates
  X_uncorr <- NULL
  if (p_uncorr != 0) {
    X_uncorr <- generate_X_gaussian(.n = n, .p = p_uncorr, .corr = 0)
  }

  # simulate betas_corr and betas_uncorr
  betas_corr <- generate_coef(.betas = betas_corr, .p = p_corr, .s = s_corr,
                              .betas_name = "betas_corr", sd = betas_corr_sd)
  betas_uncorr <- generate_coef(.betas = betas_uncorr, .p = p_uncorr,
                                .s = s_uncorr, betas_name = "betas_uncorr",
                                sd = betas_uncorr_sd)

  X <- cbind(X_uncorr, X_corr)
  betas <- c(betas_uncorr, betas_corr)

  # simulate linear y
  y <- generate_y_logistic(X = X, betas = betas, intercept = intercept,
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
#' @inheritParams correlated_linear_gaussian_dgp
#' @inheritParams generate_y_lss
#' @param p Number of features.
#' @param return_values Character vector indicating what objects to return in
#'   list. Elements in vector must be one of "X", "y", "support", "int_support".
#'
#' @returns A list of the named objects that were requested in
#'   \code{return_values}. See brief descriptions below.
#' \describe{
#' \item{X}{A \code{data.frame}.}
#' \item{y}{A response vector of length \code{nrow(X)}.}
#' \item{support}{A vector of feature indices indicating all features used in
#'   the true support of the DGP.}
#' \item{int_support}{A vector of signed feature indices in the true
#'   (interaction) support of the DGP. For example, "1+_2-" means that the
#'   interaction between high values of feature 1 and low values of feature 2
#'   appears in the underlying DGP.}
#' }
#' Note that if \code{data_split = TRUE} and "X", "y"
#'   are in \code{return_values}, then the returned list also contains slots for
#'   "Xtest" and "ytest".
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
                             return_values = c("X", "y", "support"),
                             ...) {
  return_values <- match.arg(return_values,
                             choices = c("X", "y", "support", "int_support"),
                             several.ok = TRUE)

  X <- generate_X_gaussian(.n = n, .p = p)
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
    int_support <- y$int_support
    y <- y$y
  }
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  if ("int_support" %in% return_values) {
    out$int_support <- int_support
  }
  return(out)
}

#' Generate correlated Gaussian covariates and LSS response data.
#'
#' @description Generate normally-distributed covariates that are potentially
#'   correlated and LSS response data with a specified error distribution.
#'
#' @inheritParams correlated_linear_gaussian_dgp
#' @inheritParams generate_y_lss
#' @param p_uncorr Number of uncorrelated features.
#' @param p_corr Number of features in correlated group.
#' @param s_uncorr Number of interactions from features in uncorrelated group.
#' @param s_corr Number of interactions from features in correlated group.
#' @param corr Correlation between features in correlated group.
#' @param mixed_int If \code{TRUE}, correlated and uncorrelated variables are
#'   mixed together when constructing an interaction of order-k. If
#'   \code{FALSE}, each interaction of order-k is composed of only correlated
#'   variables or only uncorrelated variables.
#' @param train_prop Proportion of data in training set if \code{data_split =
#'   TRUE}.
#' @param return_values Character vector indicating what objects to return in
#'   list. Elements in vector must be one of "X", "y", "support", "int_support".
#'
#' @inherit lss_gaussian_dgp return
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
  return_values <- match.arg(return_values,
                             choices = c("X", "y", "support", "int_support"),
                             several.ok = TRUE)

  X_corr <- generate_X_gaussian(.n = n, .p = p_corr, .corr = corr)
  X_uncorr <- generate_X_gaussian(.n = n, .p = p_uncorr, .corr = 0)
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
    int_support <- y$int_support
    y <- y$y
  }
  out <- return_DGP_output(X = X, y = y, support = support,
                           data_split = data_split, train_prop = train_prop,
                           return_values = return_values)
  if ("int_support" %in% return_values) {
    out$int_support <- int_support
  }
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

#' Generate data from a model with omitted variable bias.
#'
#' @description Takes in a data-generating process (DGP), and induces some bias
#'   due to omitted variable(s). In other words, this function will generate
#'   a design matrix `X` and response vector `y` according to the inputted
#'   DGP function, but will return a partially missing design matrix, where
#'   some variable/feature columns have been omitted.
#'
#' @param dgp_fun A function that generates data according to some known
#'   data-generating process. This function should return an object of the same
#'   format as the output of `return_DGP_output()`.
#' @param omitted_vars A vector of indices or column names corresponding to
#'   columns in X that should be omitted.
#' @param ... Additional arguments to pass to `dgp_fun()`.
#'
#' @returns The returned object has the same format as the output of
#'   `dgp_fun()`, except that specified variables, given by `omitted_vars`, have
#'   been omitted from the `X` component and the `support` (if applicable).
#'
#' @examples
#' # generate data from a linear gaussian DGP with the first variable missing
#' dgp_out <- omitted_var_dgp(dgp_fun = linear_gaussian_dgp,
#'                            n = 100, p_obs = 10, s_obs = 2,
#'                            omitted_vars = 1)
#' # or equivalently, (minus the difference in column names)
#' dgp_out <- linear_gaussian_dgp(n = 10, p_obs = 9, p_unobs = 1,
#'                                s_obs = 1, s_unobs = 1)
#'
#' @export
omitted_var_dgp <- function(dgp_fun, omitted_vars = 1, ...) {
  dgp_out <- dgp_fun(...)
  X_orig <- dgp_out$X

  if (is.numeric(omitted_vars)) {
    if (max(omitted_vars) > ncol(X_orig)) {
      stop("Omitted variable indices exceed the number of columns in X.")
    } else if (length(setdiff(1:ncol(X_orig), omitted_vars)) == 0) {
      stop("Cannot omit all variables in X. ",
           "Must leave at least one observed variable in X.")
    }
  } else {
    if (any(!(omitted_vars %in% colnames(X_orig)))) {
      stop("Some omitted variable names cannot be found in X.")
    }
  }

  dgp_out$X <- X_orig %>%
    dplyr::select(-tidyselect::all_of(unique(omitted_vars)))

  if ("support" %in% names(dgp_out)) {
    col_support <- colnames(X_orig)[dgp_out$support]
    dgp_out$support <- which(colnames(dgp_out$X) %in% col_support)
  }
  return(dgp_out)
}
