test_that("Visualizer initialization works properly", {
  expect_error(Visualizer$new())

  func <- function(x) x^2
  visualizer_fun1 <- function(x, ...) x + 1
  visualizer1 <- Visualizer$new(visualizer_fun = visualizer_fun1)
  visualizer1b <- Visualizer$new(visualizer_fun = visualizer_fun1, name = "Visualizer")
  visualizer1c <- Visualizer$new(visualizer_fun = visualizer_fun1,
                                 rmd_options = list(height = 8))
  visualizer1d <- Visualizer$new(visualizer_fun = visualizer_fun1,
                                 rmd_options = list(width = 8))
  visualizer1e <- Visualizer$new(visualizer_fun = visualizer_fun1,
                                 rmd_options = list(height = 8, width = 8))
  visualizer1f <- Visualizer$new(visualizer_fun = visualizer_fun1,
                                 a = 5, b = 1:5, c = data.frame(d = 1:2))
  visualizer1g <- Visualizer$new(visualizer_fun1, n = 100)
  visualizer1h <- Visualizer$new(n = 100, func = func, visualizer_fun1)
  visualizer1i <- Visualizer$new(n = 100, visualizer_fun1, "Visualizer")
  visualizer1j <- Visualizer$new(visualizer_fun1, "Visualizer",
                                 list(width = 8), rm = TRUE, n = 100)
  visualizer1k <- Visualizer$new(visualizer_fun1, "Visualizer", n = 100, rm = TRUE)
  visualizer1l <- Visualizer$new(visualizer_fun1, a = 5, "Visualizer",
                                 list(width = 8), rm = TRUE)

  named_list <- list()
  names(named_list) <- character(0)

  # basic initialization
  expect_equal(visualizer1$name, NULL)
  expect_equal(visualizer1$visualizer_fun, visualizer_fun1)
  expect_equal(visualizer1$visualizer_params, named_list)
  expect_equal(visualizer1$rmd_options, list(height = 6, width = 10))
  expect_equal(visualizer1$show, TRUE)

  # basic initialization with name
  expect_equal(visualizer1b$name, "Visualizer")
  expect_equal(visualizer1b$visualizer_params, named_list)

  # rmd options input
  expect_equal(visualizer1c$rmd_options, list(height = 8, width = 10))
  expect_equal(visualizer1c$visualizer_params, named_list)
  expect_equal(visualizer1d$rmd_options, list(height = 6, width = 8))
  expect_equal(visualizer1d$visualizer_params, named_list)
  expect_equal(visualizer1e$rmd_options, list(height = 8, width = 8))
  expect_equal(visualizer1e$visualizer_params, named_list)

  # visualizer_fun arguments
  expect_equal(visualizer1f$visualizer_params,
               list(a = 5, b = 1:5, c = data.frame(d = 1:2)))
  expect_equal(visualizer1f$visualizer_fun, visualizer1$visualizer_fun)
  expect_equal(visualizer1f$rmd_options, list(height = 6, width = 10))

  # preventing visualizer_fun arg partial matching
  expect_equal(visualizer1g$visualizer_params, list(n = 100))
  expect_equal(visualizer1g$visualizer_fun, visualizer_fun1)
  expect_null(visualizer1g$name)
  expect_equal(visualizer1g$rmd_options, list(height = 6, width = 10))

  # scrambled init args
  expect_equal(visualizer1h$visualizer_params, list(n = 100, func = func))
  expect_equal(visualizer1h$visualizer_fun, visualizer_fun1)
  expect_null(visualizer1h$name)
  expect_equal(visualizer1h$rmd_options, list(height = 6, width = 10))

  # scrambled init args with name
  expect_equal(visualizer1i$visualizer_params, list(n = 100))
  expect_equal(visualizer1i$visualizer_fun, visualizer_fun1)
  expect_equal(visualizer1i$name, "Visualizer")
  expect_equal(visualizer1i$rmd_options, list(height = 6, width = 10))

  # both formals by position, two user args partial matching
  expect_equal(visualizer1j$visualizer_params, list(n = 100, rm = TRUE))
  expect_equal(visualizer1j$visualizer_fun, visualizer_fun1)
  expect_equal(visualizer1j$name, "Visualizer")
  expect_equal(visualizer1j$rmd_options, list(height = 6, width = 8))

  # one formal by position, two user args partial matching
  expect_equal(visualizer1k$visualizer_params, list(n = 100, rm = TRUE))
  expect_equal(visualizer1k$visualizer_fun, visualizer_fun1)
  expect_equal(visualizer1k$name, "Visualizer")
  expect_equal(visualizer1k$rmd_options, list(height = 6, width = 10))

  # both formals by shifted position, one user arg partial matching
  expect_equal(visualizer1l$visualizer_params, list(rm = TRUE, a = 5))
  expect_equal(visualizer1l$visualizer_fun, visualizer_fun1)
  expect_equal(visualizer1l$name, "Visualizer")
  expect_equal(visualizer1l$rmd_options, list(height = 6, width = 8))

  # initialize with create_visualizer
  visualizer2 <- create_visualizer(visualizer_fun = visualizer_fun1)
  visualizer2b <- create_visualizer(visualizer_fun = visualizer_fun1, name = "Visualizer")
  visualizer2c <- create_visualizer(visualizer_fun = visualizer_fun1,
                              rmd_options = list(height = 8))
  visualizer2d <- create_visualizer(visualizer_fun = visualizer_fun1,
                              rmd_options = list(width = 8))
  visualizer2e <- create_visualizer(visualizer_fun = visualizer_fun1,
                              rmd_options = list(height = 8, width = 8))
  visualizer2f <- create_visualizer(visualizer_fun = visualizer_fun1,
                                    a = 5, b = 1:5, c = data.frame(d = 1:2))
  visualizer2g <- create_visualizer(visualizer_fun1, n = 100)
  visualizer2h <- create_visualizer(n = 100, func = func, visualizer_fun1)
  visualizer2i <- create_visualizer(n = 100, visualizer_fun1, "Visualizer")
  visualizer2j <- create_visualizer(visualizer_fun1, "Visualizer",
                                    list(width = 8), rm = TRUE, n = 100)
  visualizer2k <- create_visualizer(visualizer_fun1, "Visualizer", n = 100, rm = TRUE)
  visualizer2l <- create_visualizer(visualizer_fun1, a = 5, "Visualizer",
                                    list(width = 8), rm = TRUE)

  expect_error(create_visualizer())
  expect_error(create_visualizer(name = "Visualizer"))
  expect_equal(visualizer1, visualizer2)
  expect_equal(visualizer1b, visualizer2b)
  expect_equal(visualizer1c, visualizer2c)
  expect_equal(visualizer1d, visualizer2d)
  expect_equal(visualizer1e, visualizer2e)
  expect_equal(visualizer1f, visualizer2f)
  expect_equal(visualizer1g, visualizer2g)
  expect_equal(visualizer1h, visualizer2h)
  expect_equal(visualizer1i, visualizer2i)
  expect_equal(visualizer1j, visualizer2j)
  expect_equal(visualizer1k, visualizer2k)
  expect_equal(visualizer1l, visualizer2l)
})

test_that("Visualizer$visualize() works properly", {

  visualizer_fun1 <- function(x) x + 1
  visualizer1 <- create_visualizer(visualizer_fun = visualizer_fun1)
  visualizer1b <- create_visualizer(visualizer_fun = visualizer_fun1, x = 2)
  visualizer1c <- create_visualizer(visualizer_fun = visualizer_fun1, x = 2, y = 1)

  visualizer_fun2 <- function(...) list(...)
  visualizer2 <- create_visualizer(visualizer_fun = visualizer_fun2)
  visualizer2b <- create_visualizer(visualizer_fun = visualizer_fun2, a = 2)

  visualizer_fun3 <- function(fit_results = NULL, eval_results = NULL,
                        vary_params = NULL) {
    res <- list(fit_results = fit_results,
                eval_results = eval_results,
                vary_params = vary_params)
    return(res)
  }
  visualizer3 <- create_visualizer(visualizer_fun = visualizer_fun3)
  visualizer3b <- create_visualizer(visualizer_fun = visualizer_fun3, fit_results = "fit")

  # basic visualizer method
  expect_error(visualizer1$visualize())
  expect_error(visualizer1$visualize(x = 2))
  expect_equal(visualizer1b$visualize(), 3)

  # check that fit_results, eval_results, vary_params are optional args while
  # others must match
  expect_equal(visualizer1b$visualize(fit_results = "a"), 3)
  expect_equal(visualizer1b$visualize(eval_results = "a"), 3)
  expect_equal(visualizer1b$visualize(vary_params = "a"), 3)
  expect_error(visualizer1c$visualize())

  # ... in visualizer_fun works as expected
  expect_equal(visualizer2$visualize(), list())
  expect_equal(visualizer2b$visualize(), list(a = 2))
  expect_equal(visualizer2b$visualize(fit_results = "fit",
                              eval_results = "eval",
                              vary_params = "params",
                              a = 1),
               list(a = 2))

  # fit_results, eval_results, vary_params args work if provided
  expect_equal(
    visualizer3$visualize(),
    list(fit_results = NULL, eval_results = NULL, vary_params = NULL)
  )
  expect_equal(
    visualizer3$visualize(fit_results = "fit",
                  eval_results = "eval",
                  vary_params = "params"),
    list(fit_results = "fit", eval_results = "eval", vary_params = "params")
  )
  expect_error(visualizer3b$visualize())

})
