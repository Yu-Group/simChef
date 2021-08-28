test_that("Experiment initialization works properly", {

  exper <- Experiment$new()
  expect_equal(exper$name, "experiment")
  expect_equal(exper$get_save_dir(), file.path(getwd(), "results", "experiment"))

  exper <- Experiment$new(name = "exper_name", save_dir = "exper_dir")
  expect_equal(exper$name, "exper_name")
  expect_equal(exper$get_save_dir(), file.path(getwd(), "exper_dir"))

  exper <- create_experiment()
  expect_equal(exper$name, "experiment")
  expect_equal(exper$get_save_dir(), file.path(getwd(), "results", "experiment"))

  exper <- create_experiment(name = "exper_name", save_dir = "exper_dir")
  expect_equal(exper$name, "exper_name")
  expect_equal(exper$get_save_dir(), file.path(getwd(), "exper_dir"))

  dgp_fun1 <- function(x) x + 1
  dgp_fun2 <- function(x) x + 2
  dgp1 <- DGP$new(dgp_fun1)
  dgp2 <- DGP$new(dgp_fun2)
  eval_fun1 <- function(x) x - 1
  eval_fun2 <- function(x) x - 2
  eval_fun3 <- function(x) x - 3
  eval1 <- Evaluator$new(eval_fun1)
  eval2 <- Evaluator$new(eval_fun2)
  eval3 <- Evaluator$new(eval_fun3)
  exper <- create_experiment(
    dgp_list = list(dgp1, dgp2),
    evaluator_list = list("a" = eval1, eval2, eval3)
  )
  dgp_list <- exper$get_dgps()
  method_list <- exper$get_methods()
  evaluator_list <- exper$get_evaluators()
  plotter_list <- exper$get_plotters()

  expect_equal(method_list, list())
  expect_equal(plotter_list, list())
  expect_equal(length(dgp_list), 2)
  expect_equal(dgp_list[['dgp1']], dgp1)
  expect_equal(dgp_list[['dgp2']], dgp2)
  expect_equal(length(evaluator_list), 3)
  expect_equal(evaluator_list[['a']], eval1)
  expect_equal(evaluator_list[['evaluator1']], eval2)
  expect_equal(evaluator_list[['evaluator2']], eval3)

  expect_error(create_experiment(dgp_list = c(1, 2, 3)))

})
