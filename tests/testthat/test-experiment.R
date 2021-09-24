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
  visualizer_list <- exper$get_visualizers()

  expect_equal(method_list, list())
  expect_equal(visualizer_list, list())
  expect_equal(length(dgp_list), 2)
  expect_equal(dgp_list[['dgp1']], dgp1)
  expect_equal(dgp_list[['dgp2']], dgp2)
  expect_equal(length(evaluator_list), 3)
  expect_equal(evaluator_list[['a']], eval1)
  expect_equal(evaluator_list[['evaluator1']], eval2)
  expect_equal(evaluator_list[['evaluator2']], eval3)

  expect_error(create_experiment(dgp_list = c(1, 2, 3)))

})

test_that("Modifying DGPs/Methods/Evaluators/Visualizers works properly", {
  dgp_fun1 <- function(x) x + 1
  dgp_fun2 <- function(x) x + 2
  dgp1 <- DGP$new(dgp_fun1)
  dgp2 <- DGP$new(dgp_fun2)
  method_fun1 <- function(x) x * 1
  method_fun2 <- function(x) x * 2
  method1 <- Method$new(method_fun1)
  method2 <- Method$new(method_fun2)
  eval_fun1 <- function(x) x - 1
  eval_fun2 <- function(x) x - 2
  eval1 <- Evaluator$new(eval_fun1)
  eval2 <- Evaluator$new(eval_fun2)
  plot_fun1 <- function(x) x / 1
  plot_fun2 <- function(x) x / 2
  plot1 <- Visualizer$new(plot_fun1)
  plot2 <- Visualizer$new(plot_fun2)

  experiment1 <- create_experiment(name = "test-modify-experiment")
  experiment2 <- create_experiment(name = "test-modify-experiment")

  # adding dgp
  experiment1 %>% add_dgp(dgp1, "DGP1")
  experiment2$add_dgp(dgp1, "DGP1")
  expect_equal(names(experiment1$get_dgps()), "DGP1")
  expect_equal(experiment1, experiment2)
  expect_equal(experiment1$get_methods(), list())
  # adding method
  experiment1 %>% add_method(method1, "Method1")
  experiment2$add_method(method1, "Method1")
  expect_equal(names(experiment1$get_methods()), "Method1")
  expect_equal(experiment1, experiment2)
  # adding evaluator
  experiment1 %>% add_evaluator(eval1, "Evaluator1")
  experiment2$add_evaluator(eval1, "Evaluator1")
  expect_equal(names(experiment1$get_evaluators()), "Evaluator1")
  expect_equal(experiment1, experiment2)
  # adding visualizer
  experiment1 %>%
    add_visualizer(plot1, "Visualizer1") %>%
    add_visualizer(plot2, "Visualizer2")
  experiment2$add_visualizer(plot1, "Visualizer1")$
    add_visualizer(plot2, "Visualizer2")
  expect_equal(names(experiment1$get_visualizers()),
               c("Visualizer1", "Visualizer2"))
  expect_equal(experiment1, experiment2)

  # error checking for add_*
  expect_error(experiment1 %>% add_dgp("DGP"))
  expect_error(experiment1 %>% add_dgp(dgp1, "DGP1"))

  # updating dgp
  experiment1 %>% update_dgp(dgp2, "DGP1")
  experiment2$update_dgp(dgp2, "DGP1")
  expect_equal(experiment1$get_dgps()[["DGP1"]]$dgp_fun, dgp_fun2)
  expect_equal(experiment1, experiment2)
  # updating method
  experiment1 %>% update_method(method2, "Method1")
  experiment2$update_method(method2, "Method1")
  expect_equal(experiment1$get_methods()[["Method1"]]$method_fun, method_fun2)
  expect_equal(experiment1, experiment2)
  # updating evaluator
  experiment1 %>% update_evaluator(eval2, "Evaluator1")
  experiment2$update_evaluator(eval2, "Evaluator1")
  expect_equal(experiment1$get_evaluators()[["Evaluator1"]]$eval_fun, eval_fun2)
  expect_equal(experiment1, experiment2)
  # updating visualizer
  experiment1 %>% update_visualizer(plot2, "Visualizer1")
  experiment2$update_visualizer(plot2, "Visualizer1")
  expect_equal(experiment1$get_visualizers()[["Visualizer1"]]$visualizer_fun,
               plot_fun2)
  expect_equal(experiment1, experiment2)

  # error checking for update_*
  expect_error(experiment1 %>% update_evaluator("Evaluator2"))
  expect_error(experiment1 %>% update_evaluator(eval2, "Evaluator2"))

  # get_* methods
  expect_error(get_dgps())
  expect_equal(experiment1$get_dgps(), get_dgps(experiment1))
  expect_equal(experiment1$get_methods(), get_methods(experiment1))
  expect_equal(experiment1$get_evaluators(), get_evaluators(experiment1))
  expect_equal(experiment1$get_visualizers(), get_visualizers(experiment1))

  # make copy of experiments for later
  experiment1_copy <- create_experiment(name = experiment1$name,
                                        clone_from = experiment1)
  experiment2_copy <- create_experiment(name = experiment2$name,
                                        clone_from = experiment2)

  # removing dgp
  experiment1 %>% remove_dgp("DGP1")
  experiment2$remove_dgp("DGP1")
  expect_equal(length(experiment1$get_dgps()), 0)
  # removing method
  experiment1 %>% remove_method("Method1")
  experiment2$remove_method("Method1")
  expect_equal(length(experiment1$get_methods()), 0)
  # removing evaluator
  experiment1 %>% remove_evaluator("Evaluator1")
  experiment2$remove_evaluator("Evaluator1")
  expect_equal(length(experiment1$get_evaluators()), 0)
  # removing visualizer
  experiment1 %>% remove_visualizer("Visualizer1")
  experiment2$remove_visualizer("Visualizer1")
  expect_equal(names(experiment1$get_visualizers()), "Visualizer2")
  expect_equal(experiment1, experiment2)

  # error checking for remove_*
  expect_error(experiment1 %>% remove_dgp("Evaluator3"))

  # remove all
  experiment1_copy %>% remove_dgp()
  experiment2_copy$remove_dgp()
  expect_equal(experiment1_copy$get_dgps(), list())
  expect_equal(names(experiment1_copy$get_methods()), "Method1")
  expect_equal(experiment1_copy, experiment2_copy)
  experiment1_copy %>% remove_method()
  experiment2_copy$remove_method()
  expect_equal(experiment1_copy$get_methods(), list())
  expect_equal(experiment1_copy, experiment2_copy)
  experiment1_copy %>% remove_evaluator()
  experiment2_copy$remove_evaluator()
  expect_equal(experiment1_copy$get_evaluators(), list())
  expect_equal(experiment1_copy, experiment2_copy)
  experiment1_copy %>% remove_visualizer()
  experiment2_copy$remove_visualizer()
  expect_equal(experiment1_copy$get_visualizers(), list())
  expect_equal(experiment1_copy, experiment2_copy)
})

test_that("Running experiment works properly", {
  dgp_fun1 <- function(x, y = NULL) x + 1
  dgp_fun2 <- function(x, y = NULL) x + 2
  dgp1 <- DGP$new(dgp_fun1, x = 1)
  dgp2 <- DGP$new(dgp_fun2, x = 1)

  method_fun1 <- function(x, y = NULL) x * 1
  method1 <- Method$new(method_fun1)
  method_fun2 <- function(x, idx = 1) list(x_idx = x[idx])
  method2 <- create_method(method_fun = method_fun2)

  fit_results_fun <- function(fit_results) fit_results
  fit_results_eval <- create_evaluator(eval_fun = fit_results_fun)
  vary_params_fun <- function(vary_params = NULL) vary_params
  vary_params_eval <- create_evaluator(eval_fun = vary_params_fun)

  fit_plot_fun <- function(fit_results) fit_results
  fit_plot <- create_visualizer(visualizer_fun = fit_plot_fun)
  eval_plot_fun <- function(eval_results) eval_results
  eval_plot <- create_visualizer(visualizer_fun = eval_plot_fun)

  experiment <- create_experiment(name = "test-run")
  expect_error(experiment$run(verbose = 0))
  experiment %>% add_dgp(dgp1, name = "DGP1")
  expect_error(experiment$run(verbose = 0))
  experiment %>% add_method(method1, name = "Method1")
  expect_error(experiment$run(verbose = 0))
  experiment %>% add_evaluator(fit_results_eval, name = "Evaluator1")
  expect_error(experiment$run(verbose = 0))
  experiment %>% add_visualizer(fit_plot, name = "Visualizer1")

  # remove cache
  if (dir.exists(file.path("results", "test-run"))) {
    for (fname in list.files(file.path("results", "test-run"),
                             recursive = T, full.names = TRUE)) {
      file.remove(fname)
    }
  }

  # experiment with 1 dgp, method, evaluator, visualizer
  results <- experiment$run(verbose = 0)
  expect_equal(results$fit_results,
               experiment$fit(verbose = 0))
  expect_equal(results$eval_results,
               experiment$evaluate(results$fit_results, verbose = 0))
  expect_equal(results$visualize_results,
               experiment$visualize(fit_results = results$fit_results,
                                    eval_results = results$eval_results,
                                    verbose = 0))
  expect_equal(results, run_experiment(experiment, verbose = 0))

  # check if saving works
  expect_warning(experiment$run(save = c(F, F, F), verbose = 0))
  results <- experiment$run(save = c("test"), verbose = 0)
  expect_false(file.exists(file.path("results", "test-run", "experiment.rds")))
  results <- experiment$run(save = "fit", verbose = 0)
  expect_true(file.exists(file.path("results", "test-run",
                                    "fit_results.rds")))
  expect_false(file.exists(file.path("results", "test-run",
                                     "eval_results.rds")))
  expect_false(file.exists(file.path("results", "test-run",
                                     "visualize_results.rds")))
  results <- experiment$run(save = c("eval", "visualize"), verbose = 0)
  expect_true(file.exists(file.path("results", "test-run",
                                    "eval_results.rds")))
  expect_true(file.exists(file.path("results", "test-run",
                                    "visualize_results.rds")))

  # experiment with 2 dgps, methods, evaluators, visualizers
  experiment %>%
    add_dgp(dgp2, "DGP2") %>%
    add_method(method2, "Method2") %>%
    add_evaluator(vary_params_eval, "Evaluator2") %>%
    add_visualizer(eval_plot, "Visualizer2")
  results <- experiment$run(verbose = 0)
  expect_equal(results$fit_results,
               experiment$fit(verbose = 0))
  expect_equal(results$eval_results,
               experiment$evaluate(results$fit_results, verbose = 0))
  expect_equal(results$visualize_results,
               experiment$visualize(fit_results = results$fit_results,
                                    eval_results = results$eval_results,
                                    verbose = 0))
  expect_equal(results, run_experiment(experiment, verbose = 0))

  # check n_reps works
  results <- experiment$run(n_reps = 3, verbose = 0)
  expect_equal(results$fit_results,
               experiment$fit(n_reps = 3, verbose = 0))
  expect_equal(results, run_experiment(experiment, n_reps = 3, verbose = 0))
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

test_that("Fitting experiment works properly", {
  dgp_fun1 <- function(x, y = NULL) x + 1
  dgp_fun2 <- function(x, y = NULL) list(x = x + 2, y = y)
  dgp1 <- DGP$new(dgp_fun1, x = 1)
  dgp2 <- DGP$new(dgp_fun2, x = 1, y = 2)

  method_fun1 <- function(x, y = NULL) x * 1
  method1 <- Method$new(method_fun1)
  method_fun2 <- function(x, y = NULL) {
    list(scalar = x + 2,
         vector = rep(y, 3),
         matrix = matrix(1:8, nrow = 4),
         tibble = tibble::tibble(a = 1:3, b = 2:4))
  }
  method2 <- create_method(method_fun = method_fun2)

  experiment <- create_experiment(name = "test-fit")
  expect_error(experiment$fit(verbose = 0))
  experiment %>% add_dgp(dgp1, name = "DGP1")
  expect_error(experiment$fit(verbose = 0))

  # with one dgp and one method
  experiment %>% add_method(method1, name = "Method1")
  expect_equal(experiment$fit(verbose = 0),
               tibble::tibble(rep = "1", dgp_name = "DGP1",
                              method_name = "Method1", result1 = 2))
  fit_results <- experiment$fit(n_reps = 3, save = TRUE, verbose = 0)
  expect_equal(nrow(fit_results), 3)
  expect_equal(fit_results, fit_experiment(experiment, n_reps = 3, verbose = 0))

  # with one dgp and one method, both of which return lists
  fit_results <- create_experiment(name = "test-fit") %>%
    add_dgp(dgp2, "DGP2") %>%
    add_method(method2, "Method2") %>%
    fit_experiment(n_reps = 3, verbose = 0)
  expect_true(tibble::is_tibble(fit_results))
  expect_equal(nrow(fit_results), 3)
  expect_snapshot_output(fit_results)

  # with two dgps and two methods
  experiment %>%
    add_dgp(dgp2, "DGP2") %>%
    add_method(method2, "Method2")
  fit_results <- experiment$fit(n_reps = 3, verbose = 0)
  expect_true(tibble::is_tibble(fit_results))
  expect_equal(nrow(fit_results), 12)
  expect_snapshot_output(fit_results)

})

test_that("Evaluating experiment works properly", {
  fit_results_fun <- function(fit_results) fit_results
  fit_results_eval <- create_evaluator(eval_fun = fit_results_fun)
  vary_params_fun <- function(vary_params = NULL) vary_params
  vary_params_eval <- create_evaluator(eval_fun = vary_params_fun)
  fit_results <- tibble::tibble(a = 1:3)

  # with no evaluators
  experiment <- create_experiment(name = "test-evaluate")
  expect_error(experiment$evaluate(fit_results = fit_results, verbose = 0))

  # with one evaluator
  experiment %>% add_evaluator(fit_results_eval, name = "Fit Results")
  expect_error(experiment$evaluate(verbose = 0))
  eval_results <- experiment$evaluate(fit_results = fit_results,
                                      save = TRUE, verbose = 0)
  expect_equal(length(eval_results), 1)
  expect_equal(
    purrr::map_lgl(eval_results, tibble::is_tibble),
    rep(TRUE, length(eval_results)) %>% setNames(names(eval_results))
  )
  expect_equal(
    eval_results,
    evaluate_experiment(experiment, fit_results = fit_results, verbose = 0)
  )
  expect_snapshot_output(eval_results)

  # check that multiple evaluators works
  experiment %>% add_evaluator(vary_params_eval, name = "Vary Params")
  eval_results <- experiment$evaluate(fit_results, verbose = 0)
  expect_equal(length(eval_results), 2)
  expect_equal(
    purrr::map_lgl(eval_results, tibble::is_tibble),
    rep(TRUE, length(eval_results)) %>% setNames(names(eval_results))
  )
  expect_equal(
    eval_results,
    evaluate_experiment(experiment, fit_results = fit_results, verbose = 0)
  )
  expect_snapshot_output(eval_results)
})

test_that("Plotting experiment works properly", {

  fit_plot_fun <- function(fit_results) fit_results
  fit_plot <- create_visualizer(visualizer_fun = fit_plot_fun)
  eval_plot_fun <- function(eval_results) eval_results
  eval_plot <- create_visualizer(visualizer_fun = eval_plot_fun)

  fit_results <- list(tibble::tibble(a = "fit"))
  eval_results <- list(tibble::tibble(a = "eval"))

  # with no visualizers
  experiment <- create_experiment(name = "test-visualize")
  expect_error(experiment$visualize(fit_results = fit_results,
                                    eval_results = eval_results,
                                    verbose = 0))

  # with one visualizer
  experiment %>% add_visualizer(fit_plot, name = "Fit Results")
  visualize_results <- experiment$visualize(fit_results, eval_results,
                                            save = TRUE, verbose = 0)
  expect_equal(length(visualize_results), 1)
  expect_equal(
    visualize_results,
    visualize_experiment(experiment, fit_results = fit_results,
                         eval_results = eval_results, verbose = 0)
  )
  expect_snapshot_output(visualize_results)
  
  # check that multiple visualizers works
  experiment %>% add_visualizer(eval_plot, name = "Vary Params")
  visualize_results <- experiment$visualize(fit_results, eval_results,
                                            verbose = 0)
  expect_equal(length(visualize_results), 2)
  expect_equal(
    visualize_results,
    visualize_experiment(experiment, fit_results = fit_results,
                         eval_results = eval_results, verbose = 0)
  )
  expect_snapshot_output(visualize_results)
})

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

  # check get_vary_across
  expect_equal(experiment$get_vary_across(), get_vary_across(experiment))

  # adding/removing multiple vary across params in single DGP/Method
  experiment %>% add_vary_across(dgp = "DGP", x = 1:3, y = c("a", "b"))
  expect_true(all(c("x", "y") %in% names(experiment$get_vary_across()$dgp$DGP)))
  experiment %>% remove_vary_across(dgp = "DGP")
  expect_equal(experiment$get_vary_across(), no_vary_list)

  # removing all vary across params in experiment
  expect_error(experiment %>% remove_vary_across())
  experiment %>%
    add_vary_across(dgp = "DGP", x = 1:3, y = c("a", "b")) %>%
    add_vary_across(metho = "Method", idx = 1:3)
  experiment %>% remove_vary_across()
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

  # test multi-type dgp vary across case
  x <- list(1, 3:5)
  experiment <- experiment %>%
    remove_vary_across() %>%
    add_vary_across(dgp = "DGP", x = x)
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
  
  # test list-type method vary across case
  idx <- list(1:2, 3:5, 7:10)
  experiment <- experiment %>%
    remove_vary_across() %>%
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
  
  # test multi-type (dgp, method) vary across case
  x <- list(1, 3:5)
  experiment <- experiment %>%
    remove_vary_across() %>%
    add_vary_across(dgp = "DGP", x = x) %>%
    add_vary_across(method = "Method", idx = list(1, 1:2))
  fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
  expect_equal(
    fit_results, 
    tibble::tibble(rep = "1", dgp_name = "DGP", method_name = "Method", 
                   x = list(1, 1, 3:5, 3:5), 
                   idx = list(1, 1:2, 1, 1:2),
                   x_idx = list(1, c(1, NA), 3, 3:4))
  )
  eval_results <- evaluate_experiment(experiment, fit_results = fit_results,
                                      save = FALSE, verbose = 0)
  expect_equal(
    eval_results,
    list(`Fit Results` = fit_results, 
         `Vary Params` = tibble::tibble(value = c("x", "idx")))
  )
  
  parallel_strategies <- list(
    "reps", "dgps", "methods", c("reps", "dgps"), c("reps", "methods"),
    c("dgps", "methods"), c("reps", "dgps", "methods")
  )
  for (strat in parallel_strategies) {
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0,
                                  parallel_strategy = strat)
    expect_equal(nrow(fit_results), 4)
    expect_true(all(fit_results$x_idx %in% list(1, c(1, NA), 3, 3:4)))
    expect_true(all(fit_results$idx %in% list(1, 1:2, 1, 1:2)))
    expect_true(all(fit_results$x %in% list(1, 1, 3:5, 3:5)))
  }
})

test_that("Caching in Experiment runs properly", {
  
  # create experiment
  dgp_fun1 <- function(x, y = NULL) rnorm(1, mean = x)
  dgp_fun2 <- function(x, y = NULL) rnorm(1, mean = x, sd = 2)
  dgp1 <- DGP$new(dgp_fun1, x = 0)
  dgp2 <- DGP$new(dgp_fun2, x = 0)
  
  method_fun1 <- function(x, y = NULL) x * 1
  method1 <- Method$new(method_fun1)
  method_fun2 <- function(x, idx = 1) list(x_idx = x[idx])
  method2 <- create_method(method_fun = method_fun2)
  
  fit_results_fun <- function(fit_results) fit_results
  fit_results_eval <- create_evaluator(eval_fun = fit_results_fun)
  vary_params_fun <- function(vary_params = NULL) vary_params
  vary_params_eval <- create_evaluator(eval_fun = vary_params_fun)
  
  fit_plot_fun <- function(fit_results) fit_results
  fit_plot <- create_visualizer(visualizer_fun = fit_plot_fun)
  eval_plot_fun <- function(eval_results) eval_results
  eval_plot <- create_visualizer(visualizer_fun = eval_plot_fun)
  
  experiment <- create_experiment(name = "test-cache") %>%
    add_dgp(dgp1, name = "DGP1") %>%
    add_method(method1, name = "Method1") %>%
    add_evaluator(fit_results_eval, name = "Evaluator1") %>%
    add_visualizer(fit_plot, name = "Visualizer1")
  
  # basic cache usage
  verbose <- 0
  results0 <- experiment$run(n_reps = 10, use_cached = TRUE, save = FALSE,
                             verbose = verbose)
  results1 <- experiment$run(n_reps = 10, save = TRUE, verbose = verbose)
  expect_false(identical(results0$fit_results, results1$fit_results))
  results2 <- experiment$run(n_reps = 10, use_cached = TRUE, verbose = verbose)
  expect_equal(results1, results2)
  
  # caching when adding objects
  experiment %>% add_dgp(dgp2, "DGP2")
  # runs error - need save = TRUE
  # results3 <- experiment$run(n_reps = 10, use_cached = TRUE, save = FALSE)
  results3 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results3$fit_results), 20)
  expect_equal(results2$fit_results, 
               results3$fit_results %>% dplyr::filter(dgp_name == "DGP1"))
  experiment %>% add_method(method2, "Method2")
  results4 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE, 
                             verbose = verbose)
  expect_equal(nrow(results4$fit_results), 40)
  experiment %>% add_evaluator(vary_params_eval, "Eval2")
  results5 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(results4$fit_results, results5$fit_results)
  expect_equal(results4$eval_results$Evaluator1,
               results5$eval_results$Evaluator1)
  experiment %>% add_visualizer(eval_plot, "Plot2")
  results6 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(results4$fit_results, results6$fit_results)
  expect_equal(results5$eval_results, results6$eval_results)
  expect_equal(results5$visualize_results$Visualizer1,
               results6$visualize_results$Visualizer1)
  results7 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(results6, results7)
  
  # caching when update objects does not change original object
  experiment %>% update_dgp(dgp2, "DGP2")
  results8 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  
  # caching when updating objects that actually change
  experiment %>% update_dgp(dgp1, "DGP2")
  fit_cols <- colnames(results7$fit_results)
  results8 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  results8$fit_results <- results8$fit_results %>% dplyr::select({{fit_cols}})
  expect_equal(nrow(results7$fit_results), nrow(results8$fit_results))
  expect_false(identical(results7$fit_results, results8$fit_results))
  expect_equal(results8$fit_results %>% dplyr::filter(dgp_name == "DGP1"),
               results7$fit_results %>% dplyr::filter(dgp_name == "DGP1"))
  expect_false(identical(results8$eval_results, results7$eval_results))
  expect_false(identical(results8$visualize_results, results7$visualize_results))
  experiment %>% update_method(method1, "Method2")
  results9 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results7$fit_results), nrow(results9$fit_results))
  expect_false(identical(results8$eval_results, results7$eval_results))
  expect_false(identical(results8$visualize_results, results7$visualize_results))
  experiment %>% update_evaluator(fit_results_eval, "Eval2")
  results10 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                              verbose = verbose)
  expect_equal(results10$fit_results, results9$fit_results)
  expect_equal(length(results10$eval_results), 2)
  experiment %>% update_visualizer(fit_plot, "Plot2")
  results11 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                              verbose = verbose)
  expect_equal(results11$fit_results, results10$fit_results)
  expect_equal(results11$eval_results, results10$eval_results)
  expect_equal(length(results11$visualize_results), 2)
  
  # caching when removing objects
  experiment %>% remove_dgp("DGP2")
  results12 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                              verbose = verbose)
  expect_equal(results12$fit_results,
               results11$fit_results %>% dplyr::filter(dgp_name == "DGP1"))
  expect_false(identical(results12$eval_results, results11$eval_results))
  expect_false(identical(results12$visualize_results, results11$visualize_results))
  experiment %>% remove_method("Method2")
  results13 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                              verbose = verbose)
  expect_equal(results13$fit_results,
               results12$fit_results %>% dplyr::filter(method_name == "Method1"))
  expect_false(identical(results13$eval_results, results12$eval_results))
  expect_false(identical(results13$visualize_results, results12$visualize_results))
  experiment %>% remove_evaluator("Eval2")
  results14 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                              verbose = verbose)
  expect_equal(results14$fit_results, results13$fit_results)
  expect_equal(names(results14$eval_results), "Evaluator1")
  expect_equal(results14$eval_results, results13$eval_results[1])
  experiment %>% remove_visualizer("Plot2")
  results15 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                              verbose = verbose)
  expect_equal(results15$fit_results, results14$fit_results)
  expect_equal(results15$eval_results, results14$eval_results)
  expect_equal(names(results15$visualize_results), "Visualizer1")
  expect_equal(results15$visualize_results, results14$visualize_results[1])
  
  # caching when vary across
  experiment %>% add_vary_across(dgp = "DGP1", x = c(0, 1))
  results1 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results1$fit_results), 10 * 2)
  experiment %>% add_vary_across(method = "Method1", y = c(0, 1))
  results2 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results2$fit_results), 10 * 2 * 2)
  experiment %>% remove_vary_across(method = "Method1")
  results3 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results3$fit_results), 10 * 2)
  expect_true(identical(results1$fit_results, results3$fit_results))
  experiment %>% update_vary_across(dgp = "DGP1", x = c(0, 2))
  results4 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(results3$fit_results %>% dplyr::filter(x == 0),
               results4$fit_results %>% dplyr::filter(x == 0))
  experiment %>% update_vary_across(dgp = "DGP1", x = list(0, 2, 4))
  results5 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(results4$fit_results %>% dplyr::filter(x %in% c(0, 2)),
               results5$fit_results %>% dplyr::filter(x %in% c(0, 2)))
  experiment %>% add_vary_across(method = "Method1", y = list("a", "b"))
  results6 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results6$fit_results), 10 * 3 * 2)
  
  # check caching when n changes
  results7 <- experiment$run(n_reps = 4, use_cached = TRUE, save = TRUE, 
                             verbose = verbose)
  expect_equal(nrow(results7$fit_results), 4 * 3 * 2)
  expect_equal(results7$fit_results, 
               results6$fit_results %>% dplyr::filter(as.numeric(rep) <= 4))
  results8 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                             verbose = verbose)
  expect_equal(nrow(results8$fit_results), 10 * 3 * 2)
  # expect_equal(results8$fit_results %>% dplyr::filter(as.numeric(rep) <= 4),
  #              results7$fit_results)
  expect_false(identical(results7$fit_results,
                         results8$fit_results %>% 
                           dplyr::filter(as.numeric(rep) <= 4)))
  
  # check when add multiple new objects to experiment
  experiment %>% add_dgp(dgp2, "DGP2")
  parallel_strategies <- list(
    "reps", "dgps", "methods", c("reps", "dgps"), c("reps", "methods"),
    c("dgps", "methods"), c("reps", "dgps", "methods")
  )
  for (i in 1:length(parallel_strategies)) {
    parallel_strategy <- parallel_strategies[[i]]
    experiment %>%
      update_dgp(dgp2, "DGP2") %>%
      update_vary_across(method = method1, y = c("a", letters[i+2]))
    results9 <- experiment$run(
      n_reps = 10, use_cached = TRUE, save = TRUE, verbose = verbose, 
      parallel_strategy = parallel_strategy
    )
    expect_equal(nrow(results9$fit_results), 10 * 4 * 2)
  }
  
  # check clear cache
  results10 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE, 
                              verbose = verbose)
  expect_equal(results9, results10)
  experiment %>% clear_cache()
  results11 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE, 
                              verbose = verbose)
  expect_false(identical(results11$fit_results, results10$fit_results))
  
  # check running fit, evaluate, and visualize separately
  
})

test_that("Saving methods in Experiment works properly", {

  # check default path directory
  experiment <- create_experiment(name = "test-saving")
  old_path <- R.utils::getAbsolutePath(file.path("results", "test-saving"))
  expect_equal(experiment$get_save_dir(), old_path)
  expect_equal(experiment$get_save_dir(), get_save_dir(experiment))

  # check set_save_dir()
  experiment %>%
    set_save_dir(file.path("results", "test-saving-new"))
  new_path <- R.utils::getAbsolutePath(file.path("results", "test-saving-new"))
  expect_equal(experiment$get_save_dir(), new_path)
  expect_equal(experiment$get_save_dir(), get_save_dir(experiment))

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
  visualizer_fun1 <- function(x) x / 3
  visualizer1 <- Visualizer$new(visualizer_fun1)

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
    add_visualizer(visualizer1, "Visualizer1")
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

test_that("Various parallel strategies in experiment work properly", {
  dgp_fun1 <- function(y = "") return(x = list(paste0("data1", y)))
  dgp_fun2 <- function() return(x = list("data2"))
  dgp1 <- create_dgp(dgp_fun1)
  dgp2 <- create_dgp(dgp_fun2)
  method_fun1 <- function(x, y = "") return(list(result = paste0(x, "+method1", y)))
  method_fun2 <- function(x) return(list(result = paste0(x, "+method2")))
  method1 <- create_method(method_fun1)
  method2 <- create_method(method_fun2)
  experiment <- create_experiment(
    dgp_list = list(dgp1, dgp2),
    method_list = list(method1, method2)
  )

  expected_results <- c("data1+method1", "data1+method2",
                        "data2+method1", "data2+method2")

  strategies <- list(c("reps"), c("dgps"), c("methods"),
                     c("reps", "dgps"), c("reps", "methods"),
                     c("dgps", "methods"), c("reps", "dgps", "methods"))

  for (strat in strategies) {

    results <- experiment$fit(n_reps=2, parallel_strategy=strat, verbose=0)

    expect_true(
      all(c("rep", "dgp_name", "method_name", "result") %in% names(results))
    )

    results_tally <- results %>%
      dplyr::group_by(result) %>%
      dplyr::tally()

    expect_true(length(results_tally$result) == 4)
    expect_true(all(expected_results %in% results_tally$result))
    expect_equal(results_tally$n, rep(2, 4))
  }

  # add vary_across
  experiment %>%
    add_vary_across(dgp = dgp1, y = c("a", "b", "c")) %>%
    add_vary_across(method = method1, y = c("a", "b", "c"))

  expected_results <- c("data1a+method1a", "data1a+method1b", "data1a+method1c",
                        "data1b+method1a", "data1b+method1b", "data1b+method1c",
                        "data1c+method1a", "data1c+method1b", "data1c+method1c",
                        "data1a+method2", "data1b+method2", "data1c+method2",
                        "data2+method1a", "data2+method1b", "data2+method1c",
                        "data2+method2")

  for (strat in strategies) {
    # TODO: this shouldn't produce an error
    # see https://github.com/Yu-Group/simChef/issues/75
    expect_error(results <- experiment$fit(n_reps=2, parallel_strategy=strat,
                                           verbose=0))
  }

})
