test_that("Evaluator initialization works properly", {
  expect_error(Evaluator$new())

  func <- function(x) x^2
  eval_fun1 <- function(x, ...) x + 1
  evaluator1 <- Evaluator$new(eval_fun = eval_fun1)
  evaluator1b <- Evaluator$new(eval_fun = eval_fun1, name = "Evaluator")
  evaluator1c <- Evaluator$new(eval_fun = eval_fun1,
                               rmd_options = list(digits = 3))
  evaluator1d <- Evaluator$new(eval_fun = eval_fun1,
                               rmd_options = list(caption = "caption"))
  evaluator1e <- Evaluator$new(eval_fun = eval_fun1,
                               rmd_options = list(digits = 3,
                                                  caption = "caption"))
  evaluator1f <- Evaluator$new(eval_fun = eval_fun1,
                               a = 5, b = 1:5, c = data.frame(d = 1:2))
  evaluator1g <- Evaluator$new(eval_fun1, n = 100)
  evaluator1h <- Evaluator$new(n = 100, func = func, eval_fun1)
  evaluator1i <- Evaluator$new(eval_fun1, n = 100, "Evaluator")
  evaluator1j <- Evaluator$new(eval_fun1, "Evaluator",
                               list(caption = "caption"), rm = TRUE, n = 100)
  evaluator1k <- Evaluator$new(eval_fun1, "Evaluator", n = 100, rm = TRUE)
  evaluator1l <- Evaluator$new(eval_fun1, a = 5, "Evaluator",
                               list(caption = "caption"), rm = TRUE)

  named_list <- list()
  names(named_list) <- character(0)

  # basic initialization
  expect_equal(evaluator1$name, NULL)
  expect_equal(evaluator1$eval_fun, eval_fun1)
  expect_equal(evaluator1$eval_params, named_list)
  expect_equal(evaluator1$rmd_options,
               list(digits = 2, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE)))
  expect_equal(evaluator1$show, TRUE)

  # basic initialization with name
  expect_equal(evaluator1b$name, "Evaluator")
  expect_equal(evaluator1b$eval_params, named_list)

  # rmd options input
  expect_equal(evaluator1c$rmd_options,
               list(digits = 3, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE)))
  expect_equal(evaluator1c$eval_params, named_list)
  expect_equal(evaluator1d$rmd_options,
               list(digits = 2, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE),
                    caption = "caption"))
  expect_equal(evaluator1d$eval_params, named_list)
  expect_equal(evaluator1e$rmd_options,
               list(digits = 3, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE),
                    caption = "caption"))
  expect_equal(evaluator1e$eval_params, named_list)

  # eval_fun arguments
  expect_equal(evaluator1f$eval_params,
               list(a = 5, b = 1:5, c = data.frame(d = 1:2)))
  expect_equal(evaluator1f$eval_fun, evaluator1$eval_fun)

  # preventing eval_fun arg partial matching
  expect_equal(evaluator1g$eval_params, list(n = 100))
  expect_equal(evaluator1g$eval_fun, eval_fun1)
  expect_null(evaluator1g$name)

  # scrambled init args
  expect_equal(evaluator1h$eval_params, list(n = 100, func = func))
  expect_equal(evaluator1h$eval_fun, eval_fun1)
  expect_null(evaluator1h$name)

  # scrambled init args with name
  expect_equal(evaluator1i$eval_params, list(n = 100))
  expect_equal(evaluator1i$eval_fun, eval_fun1)
  expect_equal(evaluator1i$name, "Evaluator")

  # both formals by position, two user args partial matching
  expect_equal(evaluator1j$eval_params, list(n = 100, rm = TRUE))
  expect_equal(evaluator1j$eval_fun, eval_fun1)
  expect_equal(evaluator1j$name, "Evaluator")
  expect_equal(evaluator1j$rmd_options,
               list(digits = 2, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE),
                    caption = "caption"))

  # one formal by position, two user args partial matching
  expect_equal(evaluator1k$eval_params, list(n = 100, rm = TRUE))
  expect_equal(evaluator1k$eval_fun, eval_fun1)
  expect_equal(evaluator1k$name, "Evaluator")
  expect_equal(evaluator1k$rmd_options,
               list(digits = 2, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE)))

  # both formals by shifted position, one user arg partial matching
  expect_equal(evaluator1l$eval_params, list(rm = TRUE, a = 5))
  expect_equal(evaluator1l$eval_fun, eval_fun1)
  expect_equal(evaluator1l$name, "Evaluator")
  expect_equal(evaluator1l$rmd_options,
               list(digits = 2, sigfig = FALSE,
                    options = list(scrollX = TRUE, scrollCollapse = TRUE),
                                   caption = "caption"))

  # initialize with create_evaluator
  evaluator2 <- create_evaluator(eval_fun = eval_fun1)
  evaluator2b <- create_evaluator(eval_fun = eval_fun1, name = "Evaluator")
  evaluator2c <- create_evaluator(eval_fun = eval_fun1,
                                  rmd_options = list(digits = 3))
  evaluator2d <- create_evaluator(eval_fun = eval_fun1,
                                  rmd_options = list(caption = "caption"))
  evaluator2e <- create_evaluator(eval_fun = eval_fun1,
                                  rmd_options = list(digits = 3,
                                                     caption = "caption"))
  evaluator2f <- create_evaluator(eval_fun = eval_fun1,
                                  a = 5, b = 1:5, c = data.frame(d = 1:2))
  evaluator2g <- create_evaluator(eval_fun1, n = 100)
  evaluator2h <- create_evaluator(n = 100, func = func, eval_fun1)
  evaluator2i <- create_evaluator(eval_fun1, n = 100, "Evaluator")
  evaluator2j <- create_evaluator(eval_fun1, "Evaluator",
                                  list(caption = "caption"), rm = TRUE, n = 100)
  evaluator2k <- create_evaluator(eval_fun1, "Evaluator", n = 100, rm = TRUE)
  evaluator2l <- create_evaluator(eval_fun1, a = 5, "Evaluator",
                                  list(caption = "caption"), rm = TRUE)

  expect_error(create_evaluator())
  expect_error(create_evaluator(name = "Evaluator"))
  expect_equal(evaluator1, evaluator2)
  expect_equal(evaluator1b, evaluator2b)
  expect_equal(evaluator1c, evaluator2c)
  expect_equal(evaluator1d, evaluator2d)
  expect_equal(evaluator1e, evaluator2e)
  expect_equal(evaluator1f, evaluator2f)
  expect_equal(evaluator1g, evaluator2g)
  expect_equal(evaluator1h, evaluator2h)
  expect_equal(evaluator1i, evaluator2i)
  expect_equal(evaluator1j, evaluator2j)
  expect_equal(evaluator1k, evaluator2k)
  expect_equal(evaluator1l, evaluator2l)
})

test_that("Evaluator$evaluate() works properly", {

  eval_fun1 <- function(x) x + 1
  evaluator1 <- create_evaluator(eval_fun = eval_fun1)
  evaluator1b <- create_evaluator(eval_fun = eval_fun1, x = 2)
  evaluator1c <- create_evaluator(eval_fun = eval_fun1, x = 2, y = 1)

  eval_fun2 <- function(...) list(...)
  evaluator2 <- create_evaluator(eval_fun = eval_fun2)
  evaluator2b <- create_evaluator(eval_fun = eval_fun2, a = 2)

  eval_fun3 <- function(fit_results = NULL, vary_params = NULL) {
    res <- list(fit_results = fit_results, vary_params = vary_params)
    return(res)
  }
  evaluator3 <- create_evaluator(eval_fun = eval_fun3)
  evaluator3b <- create_evaluator(eval_fun = eval_fun3, fit_results = "fit")

  eval_fun4 <- function() tibble::tibble(a = 1, b = 2)
  evaluator4 <- create_evaluator(eval_fun = eval_fun4)
  eval_fun5 <- function() list(a = 1, b = 2)
  evaluator5 <- create_evaluator(eval_fun = eval_fun5)

  # basic evaluate method
  expect_error(evaluator1$evaluate())
  expect_error(evaluator1$evaluate(x = 2))
  expect_error(evaluator1b$evaluate())
  expect_equal(evaluator1b$evaluate(fit_results = NULL),
               tibble::tibble(value = 3))

  # check that vary_params are optional args while others must match
  expect_equal(evaluator1b$evaluate(fit_results = NULL, vary_params = "a"),
               tibble::tibble(value = 3))
  expect_error(evaluator1c$evaluate(fit_results = NULL))

  # ... in eval_fun works as expected
  expect_equal(evaluator2$evaluate(fit_results = NULL), tibble::tibble())
  expect_equal(evaluator2b$evaluate(fit_results = NULL), tibble::tibble(a = 2))
  expect_equal(evaluator2b$evaluate(fit_results = "fit", vary_params = "params",
                                    a = 1),
               tibble::tibble(a = 2))

  # fit_results, vary_params args work if provided
  expect_equal(
    evaluator3$evaluate(fit_results = "fit", vary_params = "params"),
    tibble::tibble(fit_results = "fit", vary_params = "params")
  )

  # returns tibble
  expect_equal(
    evaluator4$evaluate(fit_results = NULL),
    tibble::tibble(a = 1, b = 2)
  )
  expect_equal(
    evaluator5$evaluate(fit_results = NULL),
    tibble::tibble(a = 1, b = 2)
  )
})
