test_that("Add/update/remove vary across works properly", {
  dgp_fun <- function(x, y = NULL) list(x = x)
  dgp <- create_dgp(dgp_fun = dgp_fun, x = 1:10)
  
  method_fun <- function(x, idx = 1) list(x_idx = x[idx])
  method <- create_method(method_fun = method_fun)
  
  experiment <- create_experiment(name = "test-vary-across-dgp") %>%
    add_dgp(dgp, name = "DGP") %>%
    add_method(method, name = "Method")
  
  no_vary_list <- list(dgp = list(), method = list())
  
  expect_equal(experiment$get_vary_across(), no_vary_list)
  
  expect_error(experiment %>% add_vary_across(dgp = "DGP", z = 1:3))
  expect_error(experiment %>% add_vary_across(method = "Method", z = 1:3))
  expect_error(experiment %>% add_vary_across(dgp = "Method", idx = 1:3))
  expect_error(experiment %>% add_vary_across(method = "DGP", x = 1:3))
  
  # adding/updating DGP vary across params
  experiment %>% add_vary_across(dgp = "DGP", x = 1:3)
  expect_equal(experiment$get_vary_across(),
               list(dgp = list(DGP = list(x = 1:3)), method = list()))
  experiment %>% update_vary_across(dgp = "DGP", x = list(1:3, 2:4))
  expect_equal(experiment$get_vary_across(),
               list(dgp = list(DGP = list(x = list(1:3, 2:4))),
                    method = list()))
  expect_error(experiment %>% add_vary_across(dgp = "DGP", x = 1:3))
  experiment %>% add_vary_across(dgp = "DGP", y = c("a", "b"))
  expect_equal(experiment$get_vary_across(),
               list(dgp = list(DGP = list(x = list(1:3, 2:4),
                                          y = c("a", "b"))),
                    method = list()))
  
  # removing DGP vary across params
  expect_error(experiment %>% remove_vary_across(dgp = "DGP", param_names = "z"))
  experiment %>% remove_vary_across(dgp = "DGP", param_names = "x")
  expect_false("x" %in% names(experiment$get_vary_across()$dgp$DGP))
  expect_true("y" %in% names(experiment$get_vary_across()$dgp$DGP))
  experiment %>% remove_vary_across(dgp = "DGP", param_names = "y")
  expect_equal(experiment$get_vary_across()$dgp, list())
  
  # adding/updating/removing Method vary across params
  experiment %>% add_vary_across(method = "Method", idx = 1:3)
  expect_equal(experiment$get_vary_across(), 
               list(dgp = list(), method = list(Method = list(idx = 1:3))))
  experiment %>% update_vary_across(method = "Method", idx = list(1:2, 3:4))
  expect_equal(experiment$get_vary_across(),
               list(dgp = list(), 
                    method = list(Method = list(idx = list(1:2, 3:4)))))
  experiment %>% remove_vary_across(method = "Method")
  expect_equal(experiment$get_vary_across(), no_vary_list)
  
  # adding/removing multiple vary across params in single DGP/Method
  experiment %>% add_vary_across(dgp = "DGP", x = 1:3, y = c("a", "b"))
  expect_true(all(c("x", "y") %in% names(experiment$get_vary_across()$dgp$DGP)))
  experiment %>% remove_vary_across(dgp = "DGP")
  expect_equal(experiment$get_vary_across(), no_vary_list)
})

test_that("Vary across in Experiment runs properly", {
  dgp_fun <- function(x, y = NULL) list(x = x)
  dgp <- create_dgp(dgp_fun = dgp_fun, x = 1:10)
  
  method_fun <- function(x, idx = 1) list(x_idx = x[idx])
  method <- create_method(method_fun = method_fun)
  
  fit_results_fun <- function(fit_results) fit_results
  fit_results_eval <- create_evaluator(eval_fun = fit_results_fun)
  vary_params_fun <- function(vary_params = NULL) vary_params
  vary_params_eval <- create_evaluator(eval_fun = vary_params_fun)
  
  experiment <- create_experiment(name = "test-vary-across-dgp") %>%
    add_dgp(dgp, name = "DGP") %>%
    add_method(method, name = "Method") %>%
    add_evaluator(fit_results_eval, name = "Fit Results") %>%
    add_evaluator(vary_params_eval, name = "Vary Params")
  
  # test scalar dgp vary across case
  x <- 1:3
  experiment <- experiment %>%
    add_vary_across(dgp = "DGP", x = x)
  fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
  expect_equal(
    fit_results, 
    tibble::tibble(rep = "1", dgp_name = "DGP", method_name = "Method", 
                   x = x, x_idx = x)
  )
  eval_results <- evaluate_experiment(experiment, fit_results = fit_results,
                                      save = FALSE, verbose = 0)
  expect_equal(
    eval_results,
    list(`Fit Results` = fit_results, 
         `Vary Params` = tibble::tibble(value = "x"))
  )
  
  # test list-type dgp vary across case
  x <- list(1:2, 3:5, 8:11)
  experiment <- experiment %>%
    update_vary_across(dgp = "DGP", x = x)
  fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
  expect_equal(
    fit_results, 
    tibble::tibble(rep = "1", dgp_name = "DGP", method_name = "Method", 
                   x = x, x_idx = purrr::map_dbl(x, ~.x[1]))
  )
  eval_results <- evaluate_experiment(experiment, fit_results = fit_results,
                                      save = FALSE, verbose = 0)
  expect_equal(
    eval_results,
    list(`Fit Results` = fit_results, 
         `Vary Params` = tibble::tibble(value = "x"))
  )
  
  # test scalar method vary across case
  idx <- 1:3
  experiment <- experiment %>%
    remove_vary_across(dgp = "DGP") %>%
    add_vary_across(method = "Method", idx = idx)
  fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
  expect_equal(
    fit_results, 
    tibble::tibble(rep = "1", dgp_name = "DGP", method_name = "Method", 
                   idx = idx, x_idx = idx)
  )
  eval_results <- evaluate_experiment(experiment, fit_results = fit_results,
                                      save = FALSE, verbose = 0)
  expect_equal(
    eval_results,
    list(`Fit Results` = fit_results, 
         `Vary Params` = tibble::tibble(value = "idx"))
  )
  
  # test list-type method vary across case
  idx <- list(1:2, 3:5, 7:10)
  experiment <- experiment %>%
    update_vary_across(method = "Method", idx = idx)
  fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
  expect_equal(
    fit_results, 
    tibble::tibble(rep = "1", dgp_name = "DGP", method_name = "Method", 
                   idx = idx, x_idx = idx)
  )
  eval_results <- evaluate_experiment(experiment, fit_results = fit_results,
                                      save = FALSE, verbose = 0)
  expect_equal(
    eval_results,
    list(`Fit Results` = fit_results, 
         `Vary Params` = tibble::tibble(value = "idx"))
  ) 
  
  # test multi vary across scheme
  n_reps <- 5
  experiment <- experiment %>%
    remove_vary_across(method = "Method") %>%
    add_vary_across(dgp = "DGP", x = list(1:4, 1:5, 1:6), y = 1:2) %>%
    add_vary_across(method = "Method", idx = 1:3)
  fit_results <- fit_experiment(experiment, n_reps = n_reps, 
                                save = FALSE, verbose = 0)
  expect_equal(nrow(fit_results), 3 * 3 * 2 * n_reps)
  eval_results <- evaluate_experiment(experiment, fit_results = fit_results,
                                      save = FALSE, verbose = 0)
  expect_equal(
    eval_results,
    list(`Fit Results` = fit_results, 
         `Vary Params` = tibble::tibble(value = c("x", "y", "idx")))
  )
})