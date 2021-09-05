test_that("Plotter initialization works properly", {
  expect_error(Plotter$new())
  
  plot_fun1 <- function(x, ...) x + 1
  plotter1 <- Plotter$new(plot_fun = plot_fun1)
  plotter1b <- Plotter$new(plot_fun = plot_fun1, name = "Plotter")
  plotter1c <- Plotter$new(plot_fun = plot_fun1,
                           rmd_options = list(height = 8))
  plotter1d <- Plotter$new(plot_fun = plot_fun1,
                           rmd_options = list(width = 8))
  plotter1e <- Plotter$new(plot_fun = plot_fun1,
                           rmd_options = list(height = 8, width = 8))
  plotter1f <- Plotter$new(plot_fun = plot_fun1, 
                           a = 5, b = 1:5, c = data.frame(d = 1:2))
  
  # basic initialization
  expect_equal(plotter1$name, NULL)
  expect_equal(plotter1$plot_fun, plot_fun1)
  expect_equal(plotter1$plot_params, list())
  expect_equal(plotter1$rmd_options, list(height = 6, width = 10))
  expect_equal(plotter1$show, TRUE)
  
  # basic initialization with name
  expect_equal(plotter1b$name, "Plotter")
  expect_equal(plotter1b$plot_params, list())

  # rmd options input
  expect_equal(plotter1c$rmd_options, list(height = 8, width = 10))
  expect_equal(plotter1c$plot_params, list())
  expect_equal(plotter1d$rmd_options, list(height = 6, width = 8))
  expect_equal(plotter1d$plot_params, list())
  expect_equal(plotter1e$rmd_options, list(height = 8, width = 8))
  expect_equal(plotter1e$plot_params, list())
  
  # plot_fun arguments
  expect_equal(plotter1f$plot_params, 
               list(a = 5, b = 1:5, c = data.frame(d = 1:2)))
  expect_equal(plotter1f$plot_fun, plotter1$plot_fun)
  
  # initialize with create_plotter
  plotter2 <- create_plotter(plot_fun = plot_fun1)
  plotter2b <- create_plotter(plot_fun = plot_fun1, name = "Plotter")
  plotter2c <- create_plotter(plot_fun = plot_fun1,
                              rmd_options = list(height = 8))
  plotter2d <- create_plotter(plot_fun = plot_fun1, 
                              rmd_options = list(width = 8))
  plotter2e <- create_plotter(plot_fun = plot_fun1,
                              rmd_options = list(height = 8, width = 8))
  plotter2f <- create_plotter(plot_fun = plot_fun1, 
                              a = 5, b = 1:5, c = data.frame(d = 1:2))
  expect_error(create_plotter())
  expect_error(create_plotter(name = "Plotter"))
  expect_equal(plotter1, plotter2)
  expect_equal(plotter1b, plotter2b)
  expect_equal(plotter1c, plotter2c)
  expect_equal(plotter1d, plotter2d)
  expect_equal(plotter1e, plotter2e)
  expect_equal(plotter1f, plotter2f)
})

test_that("Plotter$plot() works properly", {
  
  plot_fun1 <- function(x) x + 1
  plotter1 <- create_plotter(plot_fun = plot_fun1)
  plotter1b <- create_plotter(plot_fun = plot_fun1, x = 2)
  plotter1c <- create_plotter(plot_fun = plot_fun1, x = 2, y = 1)
  
  plot_fun2 <- function(...) list(...)
  plotter2 <- create_plotter(plot_fun = plot_fun2)
  plotter2b <- create_plotter(plot_fun = plot_fun2, a = 2)
  
  plot_fun3 <- function(fit_results = NULL, eval_results = NULL, 
                        vary_params = NULL) {
    res <- list(fit_results = fit_results,
                eval_results = eval_results,
                vary_params = vary_params)
    return(res)
  }
  plotter3 <- create_plotter(plot_fun = plot_fun3)
  plotter3b <- create_plotter(plot_fun = plot_fun3, fit_results = "fit")
  
  # basic plot method
  expect_error(plotter1$plot())
  expect_error(plotter1$plot(x = 2))
  expect_equal(plotter1b$plot(), 3)
  
  # check that fit_results, eval_results, vary_params are optional args while
  # others must match
  expect_equal(plotter1b$plot(fit_results = "a"), 3)
  expect_equal(plotter1b$plot(eval_results = "a"), 3)
  expect_equal(plotter1b$plot(vary_params = "a"), 3)
  expect_error(plotter1c$plot())
  
  # ... in plot_fun works as expected
  expect_equal(plotter2$plot(), list())
  expect_equal(plotter2b$plot(), list(a = 2))
  expect_equal(plotter2b$plot(fit_results = "fit",
                              eval_results = "eval",
                              vary_params = "params",
                              a = 1), 
               list(a = 2))
  
  # fit_results, eval_results, vary_params args work if provided
  expect_equal(
    plotter3$plot(), 
    list(fit_results = NULL, eval_results = NULL, vary_params = NULL)
  )
  expect_equal(
    plotter3$plot(fit_results = "fit", 
                  eval_results = "eval", 
                  vary_params = "params"),
    list(fit_results = "fit", eval_results = "eval", vary_params = "params")
  )
  expect_error(plotter3b$plot())
  
})