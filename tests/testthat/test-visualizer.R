test_that("Visualizer initialization works properly", {
  expect_error(Visualizer$new())
  
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
  visualizer1g <- Visualizer$new(visualizer_fun = visualizer_fun1, show = FALSE)
  
  # print statements
  expect_snapshot_output(visualizer1)
  expect_snapshot_output(visualizer1b)
  expect_snapshot_output(visualizer1c)
  expect_snapshot_output(visualizer1f)
  
  # basic initialization
  expect_equal(visualizer1$name, NULL)
  expect_equal(visualizer1$visualizer_fun, visualizer_fun1)
  expect_equal(visualizer1$visualizer_params, list())
  expect_equal(visualizer1$rmd_options, list(height = 6, width = 10))
  expect_equal(visualizer1$show, TRUE)
  
  # basic initialization with name
  expect_equal(visualizer1b$name, "Visualizer")
  expect_equal(visualizer1b$visualizer_params, list())

  # rmd options input
  expect_equal(visualizer1c$rmd_options, list(height = 8, width = 10))
  expect_equal(visualizer1c$visualizer_params, list())
  expect_equal(visualizer1d$rmd_options, list(height = 6, width = 8))
  expect_equal(visualizer1d$visualizer_params, list())
  expect_equal(visualizer1e$rmd_options, list(height = 8, width = 8))
  expect_equal(visualizer1e$visualizer_params, list())
  
  # visualizer_fun arguments
  expect_equal(visualizer1f$visualizer_params, 
               list(a = 5, b = 1:5, c = data.frame(d = 1:2)))
  expect_equal(visualizer1f$visualizer_fun, visualizer1$visualizer_fun)
  
  # show rmd input
  expect_equal(visualizer1g$show, FALSE)
  
  # initialize with create_visualizer
  visualizer2 <- create_visualizer(visualizer_fun = visualizer_fun1)
  visualizer2b <- create_visualizer(visualizer_fun = visualizer_fun1, 
                                    name = "Visualizer")
  visualizer2c <- create_visualizer(visualizer_fun = visualizer_fun1,
                                    rmd_options = list(height = 8))
  visualizer2d <- create_visualizer(visualizer_fun = visualizer_fun1, 
                                    rmd_options = list(width = 8))
  visualizer2e <- create_visualizer(visualizer_fun = visualizer_fun1,
                                    rmd_options = list(height = 8, width = 8))
  visualizer2f <- create_visualizer(visualizer_fun = visualizer_fun1, 
                                    a = 5, b = 1:5, c = data.frame(d = 1:2))
  visualizer2g <- create_visualizer(visualizer_fun = visualizer_fun1, show = F)
  
  expect_error(create_visualizer())
  expect_error(create_visualizer(name = "Visualizer"))
  expect_equal(visualizer1, visualizer2)
  expect_equal(visualizer1b, visualizer2b)
  expect_equal(visualizer1c, visualizer2c)
  expect_equal(visualizer1d, visualizer2d)
  expect_equal(visualizer1e, visualizer2e)
  expect_equal(visualizer1f, visualizer2f)
  expect_equal(visualizer1g, visualizer2g)
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
