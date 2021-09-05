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

test_that("Generate data from Experiment works properly", {
  dgp_fun1 <- function(x, y = NULL) x + 1
  dgp_fun2 <- function(x, y = NULL) x + 2
  dgp1 <- DGP$new(dgp_fun1, x = 1)
  dgp2 <- DGP$new(dgp_fun2, x = 1)
  
  method_fun1 <- function(x, y = NULL) x * 1
  method1 <- Method$new(method_fun1)
  
  # with one dgp
  experiment <- create_experiment(name = "test-generate-data") %>%
    add_dgp(dgp1, "DGP1")
  expect_equal(generate_data(experiment), list(DGP1 = list(list(2))))
  expect_equal(generate_data(experiment, n_reps = 2), 
               list(DGP1 = list(list(2), list(2))))
  
  # with two dgps
  experiment %>% add_dgp(dgp2, "DGP2")
  expect_equal(generate_data(experiment), list(DGP1 = list(list(2)),
                                               DGP2 = list(list(3))))
  expect_equal(generate_data(experiment, n_reps = 2), 
               list(DGP1 = list(list(2), list(2)),
                    DGP2 = list(list(3), list(3))))
  
  # varying across one dgp
  experiment %>% add_vary_across(dgp = "DGP1", x = c(1, 2))
  expect_equal(length(generate_data(experiment)), 2)
  expect_snapshot_output(generate_data(experiment))
  expect_equal(length(unlist(generate_data(experiment, n_reps = 3))), 9)
  expect_equal(length(unlist(generate_data(experiment, n_reps = 3)$DGP1)), 6)
  expect_snapshot_output(generate_data(experiment, n_reps = 3))
  
  # varying across two dgp
  experiment %>% add_vary_across(dgp = "DGP2", x = c(1, 2, 3))
  expect_equal(length(generate_data(experiment)), 2)
  expect_snapshot_output(generate_data(experiment))
  data_out <- generate_data(experiment, n_reps = 3)
  expect_equal(length(unlist(data_out)), 15)
  expect_equal(length(unlist(data_out$DGP2)), 9)
  expect_snapshot_output(data_out)
  
  # adding method does not affect data output
  experiment %>% 
    add_method(method1, "Method1") %>%
    add_vary_across(method = "Method1", y = c(1, 2))
  expect_equal(experiment$generate_data(n_reps = 3), data_out)
  
  # varying across two parameters in dgp
  experiment %>%
    add_vary_across(dgp = "DGP1", y = c(1, 2))
  expect_snapshot_output(generate_data(experiment))
  expect_equal(length(unlist(generate_data(experiment))), 7)
  expect_equal(length(unlist(generate_data(experiment)$DGP1)), 4)
  expect_equal(length(unlist(generate_data(experiment, n_reps = 2)$DGP1)), 8)
  expect_snapshot_output(generate_data(experiment, n_reps = 2))
  expect_equal(generate_data(experiment, n_reps = 2),
               experiment$generate_data(n_reps = 2))
  
})

test_that("Saving methods in Experiment works properly", {
  
  # check default path directory
  experiment <- create_experiment(name = "test-saving")
  old_path <- R.utils::getAbsolutePath(file.path("results", "test-saving"))
  expect_equal(experiment$get_save_dir(), old_path)
  
  # check set_save_dir()
  experiment %>%
    set_save_dir(file.path("results", "test-saving-new"))
  new_path <- R.utils::getAbsolutePath(file.path("results", "test-saving-new"))
  expect_equal(experiment$get_save_dir(), new_path)
  
  # check save_experiment
  expect_error(save_experiment())
  if (file.exists(file.path(new_path, "experiment.rds"))) {
    file.remove(file.path(new_path, "experiment.rds"))
  }
  expect_error(save_experiment(experiment), NA)
  expect_true(file.exists(file.path(new_path, "experiment.rds")))
  expect_false(file.exists(file.path(old_path, "experiment.rds")))
})

test_that("Printing Experiment works properly", {
  
  dgp_fun1 <- function(x) x + 1
  dgp_fun2 <- function(x) x + 2
  dgp1 <- DGP$new(dgp_fun1)
  dgp2 <- DGP$new(dgp_fun2)
  method_fun1 <- function(x) x * 1
  method1 <- Method$new(method_fun1)
  eval_fun1 <- function(x) x - 1
  eval_fun2 <- function(x) x - 2
  eval_fun3 <- function(x) x - 3
  eval1 <- Evaluator$new(eval_fun1)
  eval2 <- Evaluator$new(eval_fun2)
  eval3 <- Evaluator$new(eval_fun3)
  plot_fun1 <- function(x) x / 3
  plot1 <- Plotter$new(plot_fun1)
  
  experiment <- create_experiment(name = "test-print")
  expect_snapshot_output(print(experiment))
  
  experiment %>%
    add_dgp(dgp1, "DGP1") %>%
    add_dgp(dgp2, "DGP2") %>%
    add_method(method1, "Method1") %>%
    add_evaluator(eval1, "Evaluator1") %>%
    add_evaluator(eval2, "Evaluator2") %>%
    add_evaluator(eval3, "Evaluator3")
  expect_snapshot_output(print(experiment))
  
  experiment %>%
    add_plotter(plot1, "Plotter1")
  expect_snapshot_output(print(experiment))
  
  # check vary across prints properly
  experiment %>%
    add_vary_across(dgp = "DGP1", x = 1:3)
  expect_snapshot_output(print(experiment))
  
  experiment %>%
    remove_vary_across(dgp = "DGP1") %>%
    add_vary_across(method = "Method1", x = 1:3)
  expect_snapshot_output(print(experiment))
  
  experiment %>%
    add_vary_across(dgp = "DGP1", x = 1:3) %>%
    add_vary_across(dgp = "DGP2", x = 2:4)
  expect_snapshot_output(print(experiment))
  
  experiment %>%
    update_vary_across(dgp = "DGP1", x = list(1:3)) %>%
    update_vary_across(dgp = "DGP2", x = list(2:4))
  expect_snapshot_output(print(experiment))
})