test_that("Method initialization works properly", {
  expect_error(Method$new())
  
  method_fun1 <- function(x, ...) x + 1
  method1 <- Method$new(method_fun = method_fun1)
  method1b <- Method$new(method_fun = method_fun1, name = "Method")
  method1c <- Method$new(method_fun = method_fun1, 
                         a = 5, b = 1:5, c = data.frame(d = 1:2))
  
  # print statements
  expect_snapshot_output(method1)
  expect_snapshot_output(method1b)
  expect_snapshot_output(method1c)
  
  # basic initialization
  expect_equal(method1$name, NULL)
  expect_equal(method1$method_fun, method_fun1)
  expect_equal(method1$method_params, list())
  
  # basic initialization with name
  expect_equal(method1b$name, "Method")
  expect_equal(method1b$method_params, list())
  
  # method_fun arguments
  expect_equal(method1c$method_params, 
               list(a = 5, b = 1:5, c = data.frame(d = 1:2)))
  expect_equal(method1c$method_fun, method1$method_fun)
  
  # initialize with create_method
  method2 <- create_method(method_fun = method_fun1)
  method2b <- create_method(method_fun = method_fun1, name = "Method")
  method2c <- create_method(method_fun = method_fun1, 
                            a = 5, b = 1:5, c = data.frame(d = 1:2))
  
  expect_error(create_method())
  expect_error(create_method(name = "Method"))
  expect_equal(method1, method2)
  expect_equal(method1b, method2b)
  expect_equal(method1c, method2c)
})

test_that("Method$fit() works properly", {
  
  method_fun1 <- function(x) x + 1
  method1 <- create_method(method_fun = method_fun1)
  method1b <- create_method(method_fun = method_fun1, x = 2)
  method1c <- create_method(method_fun = method_fun1, y = 1)
  
  method_fun2 <- function(...) list(...)
  method2 <- create_method(method_fun = method_fun2)
  method2b <- create_method(method_fun = method_fun2, a = 2)
  
  method_fun3 <- function(x) tibble::tibble(a = x, b = x)
  method3 <- create_method(method_fun = method_fun3)
  method_fun4 <- function(x, y) list(a = x, b = rep(y, 3))
  method4 <- create_method(method_fun = method_fun4)
  
  # basic fit method
  expect_error(method1$fit())
  expect_error(method1$fit(x = 2))
  expect_equal(method1$fit(data_list = NULL, x = 2),
               tibble::tibble(result1 = 3))
  expect_equal(method1$fit(data_list = list(x = 2)),
               tibble::tibble(result1 = 3))
  expect_error(method1$fit(data_list = list(x = 2, y = 3)))
  expect_error(method1$fit(data_list = list(x = 2), y = 3))
  expect_equal(method1b$fit(data_list = NULL),
               tibble::tibble(result1 = 3))
  expect_equal(method1b$fit(data_list = NULL, x = 3),
               tibble::tibble(result1 = 4))
  expect_error(method1b$fit(data_list = list(x = 3)))
  expect_error(method1c$fit(data_list = NULL))
  
  # ... in method_fun works as expected
  expect_error(method2$fit(data_list = NULL))
  expect_equal(method2$fit(data_list = list(a = 1, b = 2:3)),
               tibble::tibble(a = list(1), b = list(2:3)))
  expect_equal(method2$fit(data_list = NULL, a = 1, b = 2:3),
               tibble::tibble(a = list(1), b = list(2:3)))
  expect_equal(method2b$fit(data_list = NULL),
               tibble::tibble(a = 2))
  expect_equal(method2b$fit(data_list = NULL, a = 3, b = 4),
               tibble::tibble(a = 3, b = 4))
  
  # returns tibble row
  expect_equal(method3$fit(data_list = list(x = 1)),
               tibble::tibble(a = list(1), b = list(1)))
  expect_equal(method3$fit(data_list = NULL, x = 1),
               tibble::tibble(a = list(1), b = list(1)))
  expect_equal(method4$fit(data_list = list(x = "x", y = "y")),
               tibble::tibble(a = list("x"), b = list(rep("y", 3))))

})