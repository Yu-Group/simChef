test_that("list_col_to_chr works properly", {
  expect_error(list_col_to_chr())
  
  ls1 <- list(3, 2, 1)
  ls2 <- list(c(1:2), c(3:4), c(5:7), c(3:4), c(1:3))
  ls3 <- list(matrix(1:4, nrow = 2),
              matrix(1:6, nrow = 2),
              matrix(1:6, nrow = 3),
              matrix(1:6, nrow = 2))
  ls4 <- list(tibble::tibble(a = 1:3, b = c(4, 5, 6)), "abc")
  name <- "name"
  
  expect_equal(list_col_to_chr(ls1, name = name, verbatim = TRUE),
               paste0(name, 3:1))
  expect_equal(list_col_to_chr(ls1, name = NULL, verbatim = TRUE),
               paste0(3:1))
  expect_equal(list_col_to_chr(ls1, name = name, verbatim = FALSE),
               paste0(name, 1:3))
  
  expect_equal(list_col_to_chr(ls2, name = name, verbatim = TRUE),
               paste0(name, c("1_2", "3_4", "5_6_7", "3_4", "1_2_3")))
  expect_equal(list_col_to_chr(ls2, name = name, verbatim = FALSE),
               paste0(name, c(1, 2, 3, 2, 4)))
  
  expect_equal(list_col_to_chr(ls3, name = NULL, verbatim = TRUE),
               c("1_2_3_4", "1_2_3_4_5_6", "1_2_3_4_5_6", "1_2_3_4_5_6"))
  expect_equal(list_col_to_chr(ls3, name = name, verbatim = FALSE),
               paste0(name, c(1, 2, 3, 2)))
  
  expect_equal(list_col_to_chr(ls4, name = NULL, verbatim = TRUE),
               c("1:3_c(4, 5, 6)", "abc"))
  expect_equal(list_col_to_chr(ls4, name = name, verbatim = FALSE),
               paste0(name, c(1, 2)))
})

test_that("check_equal works properly", {
  expect_error(check_equal())
  
  dgp1 <- create_dgp(dgp_fun = function(x) x + 1)
  dgp1_copy <- create_dgp(dgp_fun = function(x) x + 1)
  dgp1b <- create_dgp(dgp_fun = function(x) x + 1, y = 2)
  dgp2 <- create_dgp(dgp_fun = function(x) x + 2)
  method1 <- create_method(method_fun = function(x) x - 1)
  method1_copy <- create_method(method_fun = function(x) x - 1)
  method2 <- create_method(method_fun = function(x) x - 2)
  eval1 <- create_evaluator(eval_fun = function(x) x * 1)
  eval1b <- create_evaluator(eval_fun = function(x) x * 1,
                             rmd_options = list(digits = 3),
                             name = "Eval")
  eval2 <- create_evaluator(eval_fun = function(x) x * 2)
  visualizer1 <- create_visualizer(visualizer_fun = function(x) x)
  visualizer1b <- create_visualizer(visualizer_fun = function(x) x,
                           rmd_options = list(height = 8))
  visualizer2 <- create_visualizer(visualizer_fun = function(x) x / 2)
  
  expect_true(check_equal(dgp1, dgp1))
  expect_true(check_equal(dgp1, dgp1_copy))
  expect_true(check_equal(method1, method1))
  expect_true(check_equal(method1, method1_copy))
  expect_true(check_equal(eval1, eval1))
  expect_true(check_equal(eval1, eval1b))
  expect_true(check_equal(visualizer1, visualizer1))
  expect_true(check_equal(visualizer1, visualizer1b))
  
  expect_false(check_equal(dgp1, dgp1b))
  expect_false(check_equal(dgp1, dgp2))
  expect_false(check_equal(dgp1, method1))
  expect_false(check_equal(dgp1, eval1))
  expect_false(check_equal(dgp1, visualizer1))
  expect_false(check_equal(method1, method2))
  expect_false(check_equal(eval1, eval2))
  expect_false(check_equal(visualizer1, visualizer2))
  
  expect_error(check_equal(dgp1, "dgp2"))
  expect_error(check_equal("dgp1", dgp2))
})

test_that("list_to_tibble_row works properly", {
  expect_error(list_to_tibble_row())
  
  ls0 <- list(1, 2, 3)
  ls1 <- list(a = 1, b = 2, c = 3)
  ls2 <- list(a = 1:3, b = 2:4, c = 3:5)
  ls3 <- list(a = 1, b = 1:2, c = 1:3,
              d = data.frame(e = 1:4, f = 1:4),
              g = tibble::tibble(h = 1:2, i = 1:2, j = 1:2))
  ls4 <- list(a = list(list(1)), b = list(2), c = list(list(1:3)))
  
  expect_error(list_to_tibble_row(ls0))
  expect_equal(list_to_tibble_row(ls1), 
               tibble::tibble(a = 1, b = 2, c = 3))
  expect_equal(list_to_tibble_row(ls2), 
               tibble::tibble(a = list(1:3), b = list(2:4), c = list(3:5)))
  expect_equal(list_to_tibble_row(ls3), 
               tibble::tibble(a = list(1), b = list(1:2), c = list(1:3),
                              d = list(data.frame(e = 1:4, f = 1:4)),
                              g = list(tibble::tibble(h = 1:2, i = 1:2, 
                                                      j = 1:2))))
  expect_equal(list_to_tibble_row(ls4),
               tibble::as_tibble(ls4))
})

test_that("list_to_tibble works properly", {
  expect_error(list_to_tibble())
  
  ls0 <- list(1, 2, 3)
  ls1 <- list(a = 1, b = 2, c = 3)
  ls2 <- list(a = 1:3, b = 2:4, c = 3:5)
  ls3 <- list(a = 1, b = 1:2, c = 1:3,
              d = data.frame(e = 1:4, f = 1:4),
              g = tibble::tibble(h = 1:2, i = 1:2, j = 1:2))
  ls4 <- list(a = list(list(1)), b = list(2), c = list(list(1:3)))
  
  expect_error(list_to_tibble(ls0))
  expect_equal(list_to_tibble(ls1), tibble::tibble(a = 1, b = 2, c = 3))
  expect_equal(list_to_tibble(ls2), tibble::tibble(a = 1:3, b = 2:4, c = 3:5))
  expect_equal(list_to_tibble(ls3), 
               tibble::tibble(a = 1, b = list(1:2), c = list(1:3),
                              d = list(data.frame(e = 1:4, f = 1:4)),
                              g = list(tibble::tibble(h = 1:2, i = 1:2, 
                                                      j = 1:2))))
  expect_equal(list_to_tibble(ls4),
               tibble::as_tibble(ls4))
})

test_that("simplify_tibble works properly", {
  expect_error(simplify_tibble())
  
  tib1 <- tibble::tibble(a = 1, b = 2, c = 3)
  tib2 <- tibble::tibble(a = 1:3, b = 2:4, c = 3:5)
  tib3 <- tibble::tibble(a = list(1), b = list(1:2), c = list(1:3),
                         d = list(data.frame(e = 1:4, f = 1:4)),
                         g = list(tibble::tibble(h = 1:2, i = 1:2, j = 1:2)))
  tib4 <- tibble::tibble(a = list(list(1)), b = list(2), c = list(list(1:3)))
  
  expect_equal(simplify_tibble(tib1), tib1)
  expect_equal(simplify_tibble(tib2), tib2)
  expect_equal(simplify_tibble(tib3), tib3 %>% dplyr::mutate(a = 1))
  expect_equal(simplify_tibble(tib4), 
               tibble::tibble(a = list(1), b = 2, c = list(1:3)))
})
