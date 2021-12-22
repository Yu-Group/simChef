test_that("Functions in the DGP library work properly", {
  ## xy_dgp_constructor
  sim_data <- xy_dgp_constructor(x_fun = MASS::mvrnorm,
                                 y_fun = generate_y_linear,
                                 err_fun = rnorm, data_split = TRUE,
                                 n = 100,
                                 .x_mu = rep(0, 10), .x_Sigma = diag(10),
                                 .y_betas = rnorm(10), .y_return_support = TRUE,
                                 .err_sd = 1)
  expect_equal(names(sim_data), c("X", "Xtest", "y", "ytest", "support"))
  expect_equal(sim_data$support, 1:10)
  expect_equal(dim(sim_data$X), c(50, 10))
  expect_equal(length(sim_data$y), 50)
  expect_equal(dim(sim_data$Xtest), c(50, 10))
  expect_equal(length(sim_data$ytest), 50)
  
  sim_data <- xy_dgp_constructor(x_fun = MASS::mvrnorm,
                                 y_fun = generate_y_linear,
                                 err_fun = rnorm, data_split = TRUE,
                                 n = 100,
                                 mu = rep(0, 10), Sigma = diag(10),
                                 betas = rnorm(10), return_support = TRUE,
                                 sd = 1)
  expect_equal(names(sim_data), c("X", "Xtest", "y", "ytest", "support"))
  expect_equal(sim_data$support, 1:10)
  expect_equal(dim(sim_data$X), c(50, 10))
  expect_equal(length(sim_data$y), 50)
  expect_equal(dim(sim_data$Xtest), c(50, 10))
  expect_equal(length(sim_data$ytest), 50)

  expect_error(xy_dgp_constructor(
    x_fun = MASS::mvrnorm, y_fun = generate_y_linear, .y_ = "bad"
  ))
  
  ## linear_gaussian_dgp
  sim_data <- linear_gaussian_dgp(n = 100, p_obs = 10, s_obs = 2, betas_sd = 1,
                                  err = rnorm, sd = .5)
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:2)
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)

  sim_data <- linear_gaussian_dgp(n = 100, p_obs = 10, p_unobs = 2,
                                  betas_sd = .5, betas_unobs = c(-1, 0),
                                  err = rt, df = 1)
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:10)
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)
  
  ## correlated_linear_gaussian_dgp
  sim_data <- correlated_linear_gaussian_dgp(n = 100, p_uncorr = 0, p_corr = 10,
                                             s_corr = 2, corr = 0.7,
                                             err = rnorm, sd = .5)
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:2)
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)

  sim_data <- correlated_linear_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 2,
                                             corr = 0.7, betas_uncorr_sd = 1,
                                             betas_corr = c(-1, 0),
                                             err = rt, df = 1)
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:11)
  expect_equal(dim(sim_data$X), c(100, 12))
  expect_equal(length(sim_data$y), 100)

  ## logistic_gaussian_dgp
  sim_data <- logistic_gaussian_dgp(n = 100, p = 10, s = 2, betas_sd = 1)
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:2)
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)
  
  ## correlated_logistic_gaussian_dgp
  sim_data <- correlated_logistic_gaussian_dgp(n = 100, p_uncorr = 0, p_corr = 10,
                                               s_corr = 2, corr = 0.7)
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:2)
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)
  
  sim_data <- correlated_logistic_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 2,
                                               corr = 0.7, betas_uncorr_sd = 1,
                                               betas_corr = c(-1, 0))
  expect_equal(names(sim_data), c("X", "y", "support"))
  expect_equal(sim_data$support, 1:11)
  expect_equal(dim(sim_data$X), c(100, 12))
  expect_equal(length(sim_data$y), 100)

  ## lss_gaussian_dgp
  return_values <- c("X", "y", "support", "int_support")
  sim_data <- lss_gaussian_dgp(n = 100, p = 10, k = 2, s = 2,
                               thresholds = 0, signs = 1, betas = 1,
                               return_values = return_values)
  expect_equal(names(sim_data), return_values)
  expect_true(setequal(sim_data$support, 1:4))
  expect_equal(sim_data$int_support, c("1+_2+", "3+_4+"))
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)

  sim_data <- lss_gaussian_dgp(n = 100, p = 10, k = 1, s = 2,
                               thresholds = matrix(0:1, nrow = 2),
                               signs = matrix(c(-1, 1), nrow = 2),
                               betas = c(3, -1),
                               err = rnorm,
                               return_values = return_values)
  expect_equal(names(sim_data), return_values)
  expect_true(setequal(sim_data$support, 1:2))
  expect_equal(sim_data$int_support, c("1-", "2+"))
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)
  
  ## correlated_lss_gaussian_dgp
  sim_data <- correlated_lss_gaussian_dgp(n = 100, p_uncorr = 0, p_corr = 10,
                                          k = 2, s_corr = 2, corr = 0.7,
                                          thresholds = 0, signs = 1, betas = 1,
                                          return_values = return_values)
  expect_equal(names(sim_data), return_values)
  expect_true(setequal(sim_data$support, 1:4))
  expect_equal(sim_data$int_support, c("1+_2+", "3+_4+"))
  expect_equal(dim(sim_data$X), c(100, 10))
  expect_equal(length(sim_data$y), 100)

  sim_data <- correlated_lss_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 10,
                                          s_uncorr = 1, s_corr = 1, corr = 0.7,
                                          k = 2, betas = c(3, -1), err = rnorm,
                                          return_values = return_values)
  expect_equal(names(sim_data), return_values)
  expect_true(setequal(sim_data$support, c(1:2, 11:12)))
  expect_equal(sim_data$int_support, c("1+_2+", "11+_12+"))
  expect_equal(dim(sim_data$X), c(100, 20))
  expect_equal(length(sim_data$y), 100)

  sim_data <- correlated_lss_gaussian_dgp(n = 100, p_uncorr = 10, p_corr = 10,
                                          s_uncorr = 2, s_corr = 2, k = 2,
                                          corr = 0.7, mixed_int = TRUE,
                                          return_values = return_values)
  expect_equal(names(sim_data), return_values)
  expect_equal(length(sim_data$support), 8)
  expect_equal(length(sim_data$int_support), 4)
  expect_equal(dim(sim_data$X), c(100, 20))
  expect_equal(length(sim_data$y), 100)
})

test_that("Functions in the DGP X library work properly", {
  # generate_X_gaussian
  X <- generate_X_gaussian(n = 100, p = 10)
  expect_equal(dim(X), c(100, 10))
  
  X <- generate_X_gaussian(n = 100, p = 10, sd = 2, corr = 0.7)
  expect_equal(dim(X), c(100, 10))

  X <- generate_X_gaussian(
    n = 100, p = 2, Sigma = matrix(c(3, .5, .5, 1), nrow = 2, byrow = TRUE)
  )
  expect_equal(dim(X), c(100, 2))
  
  # generate_X_rwd
  X <- generate_X_rwd(X = iris, replace = TRUE)
  expect_equal(dim(X), c(nrow(iris), ncol(iris)))

  batch_ids <- rep(1:3, length.out = nrow(iris))
  X <- generate_X_rwd(X = iris, n = 2, clusters = batch_ids)
  expect_equal(dim(X), c(100, ncol(iris)))
  
  expect_error(generate_X_rwd(iris, n = 4, clusters = batch_ids))
})

test_that("Functions in the DGP y library work properly", {
  X <- generate_X_gaussian(n = 100, p = 2)
  U <- generate_X_gaussian(n = 100, p = 2)
  
  ## generate_y_linear
  y <- generate_y_linear(X = X, betas = c(3, -1), err = rnorm)
  expect_equal(length(y), nrow(X))

  y <- generate_y_linear(X = X, U = U, betas = c(3, -1), betas_unobs = c(1, 2))
  expect_equal(length(y), nrow(X))
  expect_equal(y, 3 * X[, 1] - X[, 2] + U[, 1] + 2 * U[, 2])
  
  ## generate_y_logistic
  y <- generate_y_logistic(X = X, betas = c(3, -1))
  expect_equal(length(y), nrow(X))
  
  ## generate_y_lss
  X <- generate_X_gaussian(n = 100, p = 10)
  y <- generate_y_lss(X = X, k = 2, s = matrix(1:4, nrow = 2, byrow = TRUE),
                      thresholds = 0, signs = 1, betas = 1)
  expect_equal(length(y), nrow(X))
  expect_equal(y, as.numeric((X[, 1] > 0) * (X[, 2] > 0) + 
                               (X[, 3] > 0) * (X[, 4] > 0)))

  y <- generate_y_lss(X = X, k = 1,
                      s = matrix(1:2, nrow = 2),
                      thresholds = matrix(0:1, nrow = 2),
                      signs = matrix(c(-1, 1), nrow = 2),
                      betas = c(3, -1),
                      err = rnorm)
  expect_equal(length(y), nrow(X))
})

test_that("Functions in the DGP error library work properly", {
  ## generate_errors
  errs <- generate_errors(err = rnorm, n = 150)
  expect_equal(length(errs), 150)
  errs <- generate_errors(err = rnorm, X = iris)
  expect_equal(length(errs), 150)
  errs <- generate_errors(err = rnorm, n = 150, sd = 2)
  expect_equal(length(errs), 150)
  errs <- generate_errors(err = NULL, n = 150)
  expect_equal(errs, rep(0, 150))

  err_fun <- function(n, rho) {
    row1 <- rho^(0:(n - 1))
    Sigma <- stats::toeplitz(row1)
    return(MASS::mvrnorm(1, mu = rep(0, n), Sigma = Sigma))
  }
  errs <- generate_errors(err = err_fun, n = 100, rho = 0.75)
  expect_equal(length(errs), 100)
  
  # check other common error functions
  errs <- ar1_errors(n = 100, rho = 0.7)
  expect_equal(length(errs), 100)
  errs <- block_errors(n = 100, rho = 0.7)
  expect_equal(length(errs), 100)
  errs <- norm_errors(X = iris %>% dplyr::select(-Species))
  expect_equal(length(errs), 150)
})

test_that("Functions in the DGP utilities library work properly", {
  ## return_DGP_output
  dgp_out <- return_DGP_output(X = iris %>% dplyr::select(-Species),
                               y = iris$Species,
                               support = 1:4,
                               data_split = TRUE,
                               train_prop = 0.5,
                               return_values = c("X", "y", "support"))
  expect_equal(names(dgp_out), c("X", "Xtest", "y", "ytest", "support"))
  
  dgp_out <- return_DGP_output(X = iris %>% dplyr::select(-Species),
                               y = iris$Species,
                               support = 1:4,
                               data_split = TRUE,
                               train_prop = 0.5,
                               return_values = c("X", "y"))
  expect_equal(names(dgp_out), c("X", "Xtest", "y", "ytest"))
  
  dgp_out <- return_DGP_output(X = iris %>% dplyr::select(-Species),
                               y = iris$Species,
                               support = NULL,
                               data_split = FALSE,
                               return_values = c("X", "y", "support"))
  expect_equal(names(dgp_out), c("X", "y", "support"))
  expect_equal(dgp_out$support, NULL)
})

test_that("generate_coef works as expected", {

  expect_equal(generate_coef(1, p = 5, s = 2),
               c(1, 1, 0, 0, 0))
  expect_equal(generate_coef(1:3, p = 3),
               c(1, 2, 3))
  expect_equal(generate_coef(function(p) { p }), 1)
  expect_equal(generate_coef(function(p) { p }, p=2), c(2, 2))
  expect_equal(generate_coef(function(p) { p }, p=2, s=1), c(2, 0))

  set.seed(123)
  betas1 <- stats::rnorm(n=10)
  set.seed(123)
  betas2 <- stats::rnorm(n=10, sd=2)
  set.seed(123)
  expect_equal(generate_coef(p=10), betas1)
  set.seed(123)
  expect_equal(generate_coef(p=10, sd=2), betas2)
  set.seed(123)
  expect_equal(generate_coef(p=10, s=5), c(betas1[1:5], rep(0, 5)))
  set.seed(123)
  expect_equal(generate_coef(p=10, s=5, sd=2), c(betas2[1:5], rep(0, 5)))

  betas_fun1 <- function(p, s) {
    return(c(stats::rnorm(s), rep(0, p - s)))
  }
  betas_fun2 <- function(p, s, sd) {
    return(c(stats::rnorm(s, sd=sd), rep(0, p - s)))
  }
  betas_fun3 <- function(p, s, shift) {
    return(c(shift + stats::rnorm(s), rep(0, p - s)))
  }

  set.seed(123)
  expect_equal(generate_coef(betas_fun1, p=10), betas1)
  set.seed(123)
  expect_equal(generate_coef(betas_fun1, p=10, s=5),
               c(betas1[1:5], rep(0, 5)))
  set.seed(123)
  expect_equal(generate_coef(betas_fun2, p=10, s=5),
               c(betas1[1:5], rep(0, 5)))
  set.seed(123)
  expect_equal(generate_coef(betas_fun2, p=10, s=5, sd=2),
               c(betas2[1:5], rep(0, 5)))
  set.seed(123)
  expect_equal(generate_coef(betas_fun3, p=10, s=5, shift=3),
               c(3 + betas1[1:5], rep(0, 5)))

  expect_error(generate_coef(p = 1, s = 2))
  expect_error(generate_coef(function(p, s) return("nope")))
  expect_error(generate_coef(function(p, s) return(rep(0, s)), p=3, s=2))
  expect_error(generate_coef(betas=TRUE))
})

test_that("dots_to_fun_args works as expected", {
  expect_equal(dots_to_fun_args(),
               list(
                 .x_args = NULL,
                 .y_args = NULL,
                 .err_args = NULL,
                 .betas_args = NULL,
                 .optional_args = NULL
               ))

  expect_equal(dots_to_fun_args(c("x", "y")),
               list(
                 .x_args = NULL,
                 .y_args = NULL,
                 .optional_args = NULL
               ))

  expect_equal(dots_to_fun_args(c("x", "y"), arg1 = "arg1"),
               list(
                 .x_args = NULL,
                 .y_args = NULL,
                 .optional_args = list(arg1 = "arg1")
               ))

  expect_equal(dots_to_fun_args(c("x", "y"),
                                .y_arg1 = "y1",
                                .betas_ = "betas1"),
               list(
                 .x_args = NULL,
                 .y_args = list(arg1 = "y1"),
                 .optional_args = list(.betas_ = "betas1")
               ))

  expect_equal(
    dots_to_fun_args(
      .x_arg1 = "x1", .x_arg2 = "x2", .err_arg1 = c("err11", "err12"),
      .betas_arg1 = list(betas11 = c("betas", "11"))
    ),
    list(
      .x_args = list(arg1 = "x1", arg2 = "x2"),
      .y_args = NULL,
      .err_args = list(arg1 = c("err11", "err12")),
      .betas_args = list(arg1 = list(betas11 = c("betas", "11"))),
      .optional_args = NULL
    )
  )

  expect_equal(
    dots_to_fun_args(
      .x_arg1 = "x1", .x_arg2 = "x2", .err_arg1 = c("err11", "err12"),
      .betas_arg1 = list(betas11 = c("betas", "11")), arg1 = "arg1"
    ),
    list(
      .x_args = list(arg1 = "x1", arg2 = "x2"),
      .y_args = NULL,
      .err_args = list(arg1 = c("err11", "err12")),
      .betas_args = list(arg1 = list(betas11 = c("betas", "11"))),
      .optional_args = list(arg1 = "arg1")
    )
  )

  expect_error(dots_to_fun_args(c("x", "y"), "arg1"))
  expect_error(dots_to_fun_args(c("x", "y"), .y_ = "y1"))
})
