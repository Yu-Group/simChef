test_that("DGP initialization works properly", {
  expect_error(DGP$new())

  fun <- function(x) x^2
  dgp_fun1 <- function(x, ...) x + 1
  dgp1 <- DGP$new(dgp_fun = dgp_fun1)
  dgp1b <- DGP$new(dgp_fun = dgp_fun1, name = "DGP")
  dgp1c <- DGP$new(dgp_fun = dgp_fun1,
                   a = 5, b = 1:5, c = data.frame(d = 1:2))
  dgp1d <- DGP$new(dgp_fun1, n = 100)
  dgp1e <- DGP$new(n = 100, fun = fun, dgp_fun1)
  dgp1f <- DGP$new(n = 100, dgp_fun1, "DGP")
  # TODO: add expect_* statements for dgp1g
  dgp1g <- DGP$new(dgp_fun1, "DGP", n = 100) # *

  named_list <- list()
  names(named_list) <- character(0)

  # print statements
  expect_snapshot_output(dgp1)
  expect_snapshot_output(dgp1b)
  expect_snapshot_output(dgp1c)

  # basic initialization
  expect_equal(dgp1$name, NULL)
  expect_equal(dgp1$dgp_fun, dgp_fun1)
  expect_equal(dgp1$dgp_params, named_list)

  # basic initialization with name
  expect_equal(dgp1b$name, "DGP")
  expect_equal(dgp1b$dgp_params, named_list)

  # dgp_fun arguments
  expect_equal(dgp1c$dgp_params,
               list(a = 5, b = 1:5, c = data.frame(d = 1:2)))
  expect_equal(dgp1c$dgp_fun, dgp1$dgp_fun)

  # preventing dgp_fun arg partial matching
  expect_equal(dgp1d$dgp_params, list(n = 100))
  expect_equal(dgp1d$dgp_fun, dgp_fun1)
  expect_null(dgp1d$name)

  # scrambled init args
  expect_equal(dgp1e$dgp_params, list(n = 100, fun = fun))
  expect_equal(dgp1e$dgp_fun, dgp_fun1)
  expect_null(dgp1e$name)

  # scrambled init args with name
  expect_equal(dgp1f$dgp_params, list(n = 100))
  expect_equal(dgp1f$dgp_fun, dgp_fun1)
  expect_equal(dgp1f$name, "DGP")

  # initialize with create_dgp
  dgp2 <- create_dgp(dgp_fun = dgp_fun1)
  dgp2b <- create_dgp(dgp_fun = dgp_fun1, name = "DGP")
  dgp2c <- create_dgp(dgp_fun = dgp_fun1,
                      a = 5, b = 1:5, c = data.frame(d = 1:2))
  dgp2d <- create_dgp(dgp_fun1, n = 100)
  dgp2e <- create_dgp(n = 100, fun = fun, dgp_fun1)
  dgp2f <- create_dgp(n = 100, dgp_fun1, "DGP")

  expect_error(create_dgp())
  expect_error(create_dgp(name = "DGP"))
  expect_equal(dgp1, dgp2)
  expect_equal(dgp1b, dgp2b)
  expect_equal(dgp1c, dgp2c)
  expect_equal(dgp1d, dgp2d)
  expect_equal(dgp1e, dgp2e)
  expect_equal(dgp1f, dgp2f)
})

test_that("DGP$generate() works properly", {

  dgp_fun1 <- function(x) x + 1
  dgp1 <- create_dgp(dgp_fun = dgp_fun1)
  dgp1b <- create_dgp(dgp_fun = dgp_fun1, x = 2)
  dgp1c <- create_dgp(dgp_fun = dgp_fun1, y = 1)

  dgp_fun2 <- function(...) list(...)
  dgp2 <- create_dgp(dgp_fun = dgp_fun2)
  dgp2b <- create_dgp(dgp_fun = dgp_fun2, a = 2)

  dgp_fun3 <- function(x) tibble::tibble(a = x, b = x)
  dgp3 <- create_dgp(dgp_fun = dgp_fun3)
  dgp_fun4 <- function(x, y) list(a = x, b = rep(y, 3))
  dgp4 <- create_dgp(dgp_fun = dgp_fun4)

  # basic generate dgp
  expect_error(dgp1$generate())
  expect_equal(dgp1$generate(x = 2), list(3))
  expect_error(dgp1$generate(x = 2, y = 3))
  expect_equal(dgp1b$generate(), list(3))
  expect_equal(dgp1b$generate(x = 3), list(4))
  expect_error(dgp1c$generate())

  # ... in dgp_fun works as expected
  expect_equal(dgp2$generate(), list())
  expect_equal(dgp2$generate(x = 1:3, y = 1), list(x = 1:3, y = 1))
  expect_equal(dgp2b$generate(), list(a = 2))
  expect_equal(dgp2b$generate(a = 3, b = 1), list(a = 3, b = 1))

  # returns list
  expect_equal(dgp3$generate(x = 1), list(tibble::tibble(a = 1, b = 1)))
  expect_equal(dgp4$generate(x = "x", y = "y"), list(a = "x", b = rep("y", 3)))

})
