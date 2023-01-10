withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {

  # put all tests inside of this block to avoid leaving temporary files laying
  # around when tests fail (for some reason withr::local_tempdir() isn't
  # working)

  withr::local_options(list(simChef.debug = FALSE))

  test_that("Experiment initialization works properly", {

    exper <- Experiment$new()
    expect_equal(exper$name, "experiment")
    expect_equal(exper$get_save_dir(),
                 file.path(getwd(), "results", "experiment"))

    exper <- Experiment$new(name = "exper_name", save_dir = "exper_dir")
    expect_equal(exper$name, "exper_name")
    expect_equal(exper$get_save_dir(), file.path(getwd(), "exper_dir"))

    exper <- create_experiment()
    expect_equal(exper$name, "experiment")
    expect_equal(exper$get_save_dir(),
                 file.path(getwd(), "results", "experiment"))

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
    expect_equal(dgp_list[["dgp1"]], dgp1)
    expect_equal(dgp_list[["dgp2"]], dgp2)
    expect_equal(length(evaluator_list), 3)
    expect_equal(evaluator_list[["a"]], eval1)
    expect_equal(evaluator_list[["evaluator1"]], eval2)
    expect_equal(evaluator_list[["evaluator2"]], eval3)

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
    expect_equal(experiment1$get_visualizers()[["Visualizer1"]]$viz_fun,
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
    method2 <- create_method(.method_fun = method_fun2)

    fit_results_fun <- function(fit_results) fit_results
    fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)
    vary_params_fun <- function(vary_params = NULL) vary_params
    vary_params_eval <- create_evaluator(.eval_fun = vary_params_fun)

    fit_plot_fun <- function(fit_results) fit_results
    fit_plot <- create_visualizer(.viz_fun = fit_plot_fun)
    eval_plot_fun <- function(eval_results) eval_results
    eval_plot <- create_visualizer(.viz_fun = eval_plot_fun)

    experiment <- create_experiment(name = "test-run")
    expect_error(experiment$run(verbose = 0))
    experiment %>% add_dgp(dgp1, name = "DGP1")
    expect_error(experiment$run(verbose = 0))
    experiment %>% add_method(method1, name = "Method1")
    expect_error(experiment$run(verbose = 0), NA)
    experiment %>% add_evaluator(fit_results_eval, name = "Evaluator1")
    expect_error(experiment$run(verbose = 0), NA)
    experiment %>% add_visualizer(fit_plot, name = "Visualizer1")

    # remove cache
    ## if (dir.exists(file.path("results", "test-run"))) {
    ##   for (fname in list.files(file.path("results", "test-run"),
    ##                            recursive = T, full.names = TRUE)) {
    ##     file.remove(fname)
    ##   }
    ## }

    # experiment with 1 dgp, method, evaluator, visualizer
    results <- experiment$run(verbose = 0)
    expect_equal(results$fit_results,
                 experiment$fit(verbose = 0))
    expect_equal(results$eval_results,
                 experiment$evaluate(results$fit_results, verbose = 0))
    expect_equal(results$viz_results,
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
                                       "viz_results.rds")))
    results <- experiment$run(save = c("eval", "viz"), verbose = 0)
    expect_true(file.exists(file.path("results", "test-run",
                                      "eval_results.rds")))
    expect_true(file.exists(file.path("results", "test-run",
                                      "viz_results.rds")))

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
    expect_equal(results$viz_results,
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
    dgp_fun3 <- function(x, y = NULL) data.frame(a = x)
    dgp1 <- DGP$new(dgp_fun1, x = 1)
    dgp2 <- DGP$new(dgp_fun2, x = 1)
    dgp3 <- DGP$new(dgp_fun3, x = 1)

    method_fun1 <- function(x, y = NULL) x * 1
    method1 <- Method$new(method_fun1)

    # with one dgp
    experiment <- create_experiment(name = "test-generate-data") %>%
      add_dgp(dgp3, "DGP3")
    expect_equal(generate_data(experiment),
                 tibble::tibble(
                   .rep = "1",
                   .dgp_name = "DGP3",
                   .data_output = list(data.frame(a = 1))
                 ))
    experiment <- create_experiment(name = "test-generate-data") %>%
      add_dgp(dgp1, "DGP1")
    expect_equal(generate_data(experiment),
                 tibble::tibble(
                   .rep = "1",
                   .dgp_name = "DGP1",
                   .data_output = list(2)
                 ))
    expect_equal(generate_data(experiment, n_reps = 2),
                 tibble::tibble(
                   .rep = as.character(1:2),
                   .dgp_name = "DGP1",
                   .data_output = list(2, 2)
                 ))

    # with two dgps
    experiment %>% add_dgp(dgp2, "DGP2")
    expect_equal(generate_data(experiment),
                 tibble::tibble(
                   .rep = "1",
                   .dgp_name = c("DGP1", "DGP2"),
                   .data_output = list(2, 3)
                 ))
    expect_equal(generate_data(experiment, n_reps = 2),
                 tibble::tibble(
                   .rep = rep(as.character(1:2), times = 2),
                   .dgp_name = rep(c("DGP1", "DGP2"), each = 2),
                   .data_output = list(2, 2, 3, 3)
                 ))

    # varying across one dgp
    experiment %>% add_vary_across(.dgp = "DGP1", x = c(1, 2))
    expect_equal(nrow(generate_data(experiment)), 3)
    expect_equal(
      generate_data(experiment),
      tibble::tibble(
        .rep = "1",
        .dgp_name = c("DGP1", "DGP1", "DGP2"),
        x = c(1, 2, NA),
        .data_output = list(2, 3, 3)
      )
    )
    expect_equal(nrow(generate_data(experiment, n_reps = 3)), 9)
    expect_equal(
      generate_data(experiment, n_reps = 3),
      tibble::tibble(
        .rep = as.character(rep(1:3, times = 3)),
        .dgp_name = rep(c("DGP1", "DGP1", "DGP2"), each = 3),
        x = rep(c(1, 2, NA), each = 3),
        .data_output = list(2, 2, 2, 3, 3, 3, 3, 3, 3)
      )
    )

    # varying across two dgp
    experiment %>% add_vary_across(.dgp = "DGP2", x = c(1, 2, 3))
    expect_equal(nrow(generate_data(experiment)), 5)
    expect_equal(
      generate_data(experiment),
      tibble::tibble(
        .rep = "1",
        .dgp_name = c(rep("DGP1", 2), rep("DGP2", 3)),
        x = c(1:2, 1:3),
        .data_output = list(2, 3, 3, 4, 5)
      )
    )
    data_out <- generate_data(experiment, n_reps = 3)
    expect_equal(nrow(data_out), 15)
    expect_equal(
      data_out,
      tibble::tibble(
        .rep = as.character(rep(1:3, 5)),
        .dgp_name = c(rep("DGP1", 6), rep("DGP2", 9)),
        x = c(rep(1:2, each = 3), rep(1:3, each = 3)),
        .data_output = list(2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5)
      )
    )

    # adding method does not affect data output
    experiment %>%
      add_method(method1, "Method1") %>%
      add_vary_across(.method = "Method1", y = c(1, 2))
    expect_equal(experiment$generate_data(n_reps = 3), data_out)

    # varying across two parameters in dgp
    experiment %>%
      add_vary_across(.dgp = "DGP2", y = c(1, 2))
    expect_equal(
      generate_data(experiment),
      tibble::tibble(
        .rep = "1",
        .dgp_name = c(rep("DGP1", 2), rep("DGP2", 6)),
        x = c(1:2, rep(1:3, times = 2)),
        y = c(NA, NA, rep(1:2, each = 3)),
        .data_output = list(2, 3, 3, 4, 5, 3, 4, 5)
      )
    )
    expect_equal(
      generate_data(experiment, n_reps = 2),
      tibble::tibble(
        .rep = as.character(rep(1:2, 8)),
        .dgp_name = c(rep("DGP1", 4), rep("DGP2", 12)),
        x = rep(c(1:2, rep(1:3, times = 2)), each = 2),
        y = rep(c(NA, NA, rep(1:2, each = 3)), each = 2),
        .data_output = list(2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 3, 3, 4, 4, 5, 5)
      )
    )
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
    method2 <- create_method(.method_fun = method_fun2)

    experiment <- create_experiment(name = "test-fit")
    expect_error(experiment$fit(verbose = 0))
    experiment %>% add_dgp(dgp1, name = "DGP1")
    expect_error(experiment$fit(verbose = 0))

    # with one dgp and one method
    experiment %>% add_method(method1, name = "Method1")
    expect_equal(experiment$fit(verbose = 0),
                 tibble::tibble(.rep = "1", .dgp_name = "DGP1",
                                .method_name = "Method1", result1 = 2))
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

    dgp_fun <- function(x, y = NULL) x + 1
    dgp <- DGP$new(dgp_fun, x = 1)
    method_fun <- function(x, y = NULL) x * 1
    method <- Method$new(method_fun)
    fit_results_fun <- function(fit_results) fit_results
    fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)
    vary_params_fun <- function(vary_params = NULL) vary_params
    vary_params_eval <- create_evaluator(.eval_fun = vary_params_fun)

    experiment <- create_experiment(name = "test-evaluate") %>%
      add_dgp(dgp, "DGP") %>%
      add_method(method, "Method")
    fit_results <- experiment$fit(verbose = 0)

    # with no evaluators
    expect_error(experiment$evaluate(fit_results = fit_results, verbose = 0), NA)

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
    expect_equal(
      eval_results,
      list(`Fit Results` = tibble::tibble(.rep = as.character(1), .dgp_name = "DGP",
                                          .method_name = "Method", result1 = 2))
    )

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
    expect_equal(
      eval_results,
      list(`Fit Results` = tibble::tibble(.rep = as.character(1), .dgp_name = "DGP",
                                          .method_name = "Method", result1 = 2),
           `Vary Params` = tibble::tibble())
    )
  })

  test_that("Plotting experiment works properly", {

    dgp_fun <- function(x, y = NULL) x + 1
    dgp <- DGP$new(dgp_fun, x = 1)
    method_fun <- function(x, y = NULL) x * 1
    method <- Method$new(method_fun)
    fit_results_fun <- function(fit_results) fit_results
    fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)
    fit_plot_fun <- function(fit_results) fit_results
    fit_plot <- create_visualizer(.viz_fun = fit_plot_fun)
    eval_plot_fun <- function(eval_results) eval_results
    eval_plot <- create_visualizer(.viz_fun = eval_plot_fun)

    experiment <- create_experiment(name = "test-evaluate") %>%
      add_dgp(dgp, "DGP") %>%
      add_method(method, "Method") %>%
      add_evaluator(fit_results_eval, "Evaluator")
    fit_results <- experiment$fit(verbose = 0)
    eval_results <- experiment$evaluate(fit_results, verbose = 0)

    # with no visualizers
    expect_error(experiment$visualize(fit_results = fit_results,
                                      eval_results = eval_results,
                                      verbose = 0),
                 NA)

    # with one visualizer
    experiment %>% add_visualizer(fit_plot, name = "Fit Results")
    viz_results <- experiment$visualize(fit_results, eval_results,
                                        save = TRUE, verbose = 0)
    expect_equal(length(viz_results), 1)
    expect_equal(
      viz_results,
      visualize_experiment(experiment, fit_results = fit_results,
                           eval_results = eval_results, verbose = 0)
    )
    expect_equal(
      viz_results,
      list(`Fit Results` = fit_results)
    )

    # check that multiple visualizers works
    experiment %>% add_visualizer(eval_plot, name = "Vary Params")
    viz_results <- experiment$visualize(fit_results, eval_results,
                                        verbose = 0)
    expect_equal(length(viz_results), 2)
    expect_equal(
      viz_results,
      visualize_experiment(experiment, fit_results = fit_results,
                           eval_results = eval_results, verbose = 0)
    )
    expect_equal(
      viz_results,
      list(`Fit Results` = fit_results, `Vary Params` = eval_results)
    )
  })

  test_that("Add/update/remove vary across works properly", {

    dgp_fun <- function(x, y = NULL) list(x = x)
    dgp <- create_dgp(.dgp_fun = dgp_fun, x = 1:10)

    method_fun <- function(x, idx = 1) list(x_idx = x[idx])
    method <- create_method(.method_fun = method_fun)

    experiment <- create_experiment(name = "test-vary-across-dgp") %>%
      add_dgp(dgp, name = "DGP") %>%
      add_method(method, name = "Method")

    no_vary_list <- list(dgp = list(), method = list())

    expect_equal(experiment$get_vary_across(), no_vary_list)

    expect_error(experiment %>% add_vary_across(.dgp = "DGP", z = 1:3))
    expect_error(experiment %>% add_vary_across(.method = "Method", z = 1:3))
    expect_error(experiment %>% add_vary_across(.dgp = "Method", idx = 1:3))
    expect_error(experiment %>% add_vary_across(.method = "DGP", x = 1:3))

    # adding/updating DGP vary across params
    experiment %>% add_vary_across(.dgp = "DGP", x = 1:3)
    expect_equal(experiment$get_vary_across(),
                 list(dgp = list(DGP = list(x = 1:3)), method = list()))
    experiment %>% update_vary_across(.dgp = "DGP", x = list(1:3, 2:4))
    expect_equal(experiment$get_vary_across(),
                 list(dgp = list(DGP = list(x = list(1:3, 2:4))),
                      method = list()))
    expect_error(experiment %>% add_vary_across(.dgp = "DGP", x = 1:3))
    experiment %>% add_vary_across(.dgp = "DGP", y = c("a", "b"))
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
    experiment %>% add_vary_across(.method = "Method", idx = 1:3)
    expect_equal(experiment$get_vary_across(),
                 list(dgp = list(), method = list(Method = list(idx = 1:3))))
    experiment %>% update_vary_across(.method = "Method", idx = list(1:2, 3:4))
    expect_equal(experiment$get_vary_across(),
                 list(dgp = list(),
                      method = list(Method = list(idx = list(1:2, 3:4)))))
    experiment %>% remove_vary_across(method = "Method")
    expect_equal(experiment$get_vary_across(), no_vary_list)

    # check get_vary_across
    expect_equal(experiment$get_vary_across(), get_vary_across(experiment))

    # adding/removing multiple vary across params in single DGP/Method
    experiment %>% add_vary_across(.dgp = "DGP", x = 1:3, y = c("a", "b"))
    expect_true(all(c("x", "y") %in% names(experiment$get_vary_across()$dgp$DGP)))
    experiment %>% remove_vary_across(dgp = "DGP")
    expect_equal(experiment$get_vary_across(), no_vary_list)

    # removing all vary across params in experiment
    expect_error(experiment %>% remove_vary_across())
    experiment %>%
      add_vary_across(.dgp = "DGP", x = 1:3, y = c("a", "b")) %>%
      add_vary_across(.metho = "Method", idx = 1:3)
    experiment %>% remove_vary_across()
    expect_equal(experiment$get_vary_across(), no_vary_list)
  })

  test_that("Vary across in Experiment runs properly", {

    dgp_fun <- function(x, y = NULL) list(x = x)
    dgp <- create_dgp(.dgp_fun = dgp_fun, x = 1:10)

    method_fun <- function(x, idx = 1) list(x_idx = x[idx])
    method <- create_method(.method_fun = method_fun)

    fit_results_fun <- function(fit_results) fit_results
    fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)
    vary_params_fun <- function(vary_params = NULL) vary_params
    vary_params_eval <- create_evaluator(.eval_fun = vary_params_fun)

    experiment <- create_experiment(name = "test-vary-across-dgp") %>%
      add_dgp(dgp, name = "DGP") %>%
      add_method(method, name = "Method") %>%
      add_evaluator(fit_results_eval, name = "Fit Results") %>%
      add_evaluator(vary_params_eval, name = "Vary Params")

    # test scalar dgp vary across case
    x <- 1:3
    experiment <- experiment %>%
      add_vary_across(.dgp = "DGP", x = x)
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(
      fit_results,
      tibble::tibble(.rep = "1", .dgp_name = "DGP", .method_name = "Method",
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
      update_vary_across(.dgp = "DGP", x = x)
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(
      fit_results,
      tibble::tibble(.rep = "1", .dgp_name = "DGP", .method_name = "Method",
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
      add_vary_across(.method = "Method", idx = idx)
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(
      fit_results,
      tibble::tibble(.rep = "1", .dgp_name = "DGP", .method_name = "Method",
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
      add_vary_across(.dgp = "DGP", x = x)
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(
      fit_results,
      tibble::tibble(.rep = "1", .dgp_name = "DGP", .method_name = "Method",
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
      add_vary_across(.method = "Method", idx = idx)
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(
      fit_results,
      tibble::tibble(.rep = "1", .dgp_name = "DGP", .method_name = "Method",
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
      add_vary_across(.dgp = "DGP", x = list(1:4, 1:5, 1:6), y = 1:2) %>%
      add_vary_across(.method = "Method", idx = 1:3)
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
      add_vary_across(.dgp = "DGP", x = x) %>%
      add_vary_across(.method = "Method", idx = list(1, 1:2))
    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(
      fit_results,
      tibble::tibble(.rep = "1", .dgp_name = "DGP", .method_name = "Method",
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

  })

  test_that("Vary across with multiple DGPs/Methods works properly", {

    dgp_fun1 <- function(x, y = NULL) list(x = x)
    dgp1 <- create_dgp(.dgp_fun = dgp_fun1, x = 1:10)
    dgp_fun2 <- function(x, y = NULL, z = 2) list(x = x)
    dgp2 <- create_dgp(.dgp_fun = dgp_fun2, x = 1:10)

    method_fun1 <- function(x, idx = 1) list(x_idx = x[idx])
    method1 <- create_method(.method_fun = method_fun1)
    method_fun2 <- function(x, idx = 1) list(x_idx = rev(x)[idx])
    method2 <- create_method(.method_fun = method_fun2)

    experiment <- create_experiment(name = "test-multi-vary-across") %>%
      add_dgp(dgp1, name = "DGP1") %>%
      add_dgp(dgp2, name = "DGP2") %>%
      add_method(method1, name = "Method1") %>%
      add_method(method2, name = "Method2")

    no_vary_list <- list(dgp = list(), method = list())

    expect_error(
      experiment %>% add_vary_across(.dgp = c("DGP2", "DGP1"), z = 1:3)
    )
    expect_equal(experiment %>% get_vary_across(), no_vary_list)
    expect_error(
      experiment %>% update_vary_across(.dgp = c("DGP2", "DGP1"), x = 1:3)
    )

    # adding/updating multiple DGP vary across params
    experiment %>%
      add_vary_across(.dgp = c("DGP1", "DGP2"), x = 1:3)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 1:3), DGP2 = list(x = 1:3)),
           method = list())
    )
    experiment %>%
      update_vary_across(.dgp = c("DGP2", "DGP1"), x = list(1:3, 2:4))
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = list(1:3, 2:4)),
                      DGP2 = list(x = list(1:3, 2:4))),
           method = list())
    )
    expect_error(
      experiment %>% add_vary_across(.dgp = c("DGP1", "DGP2"), x = 1:3)
    )
    experiment %>%
      add_vary_across(.dgp = c("DGP1", "DGP2"), y = c("a", "b"))
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = list(1:3, 2:4), y = c("a", "b")),
                      DGP2 = list(x = list(1:3, 2:4), y = c("a", "b"))),
           method = list())
    )
    experiment %>%
      update_vary_across(.dgp = "DGP2", y = c("a", "b", "c"))
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = list(1:3, 2:4), y = c("a", "b")),
                      DGP2 = list(x = list(1:3, 2:4), y = c("a", "b", "c"))),
           method = list())
    )

    # removing DGP vary across params
    expect_error(
      experiment %>%
        remove_vary_across(dgp = c("DGP1", "DGP2"), param_names = "z")
    )
    experiment %>%
      remove_vary_across(dgp = c("DGP1", "DGP2"), param_names = "x")
    expect_false("x" %in% names(experiment$get_vary_across()$dgp$DGP1))
    expect_false("x" %in% names(experiment$get_vary_across()$dgp$DGP2))
    expect_true("y" %in% names(experiment$get_vary_across()$dgp$DGP1))
    expect_true("y" %in% names(experiment$get_vary_across()$dgp$DGP2))
    experiment %>% remove_vary_across()
    expect_equal(experiment$get_vary_across()$dgp, list())

    # adding/updating/removing Method vary across params
    experiment %>%
      add_vary_across(.method = c("Method1", "Method2"), idx = 1:3)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(),
           method = list(Method1 = list(idx = 1:3), Method2 = list(idx = 1:3)))
    )
    experiment %>%
      update_vary_across(.method = c("Method1", "Method2"),
                         idx = list(1:2, 3:4))
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(),
           method = list(Method1 = list(idx = list(1:2, 3:4)),
                         Method2 = list(idx = list(1:2, 3:4))))
    )
    experiment %>% remove_vary_across(method = c("Method1", "Method2"))
    expect_equal(experiment$get_vary_across(), no_vary_list)

    # adding/removing multiple vary across params in multiple DGP/Method
    expect_error(
      experiment %>%
        add_vary_across(.dgp = c("DGP2", "DGP1"), x = 1:3, z = c("a", "b"))
    )
    expect_equal(experiment %>% get_vary_across(), no_vary_list)
    experiment %>%
      add_vary_across(.dgp = c("DGP1", "DGP2"), x = 1:3, y = c("a", "b"))
    expect_true(
      all(c("x", "y") %in% names(experiment$get_vary_across()$dgp$DGP1))
    )
    expect_true(
      all(c("x", "y") %in% names(experiment$get_vary_across()$dgp$DGP2))
    )
    experiment %>% remove_vary_across(dgp = c("DGP1", "DGP2"))
    expect_equal(experiment$get_vary_across(), no_vary_list)

    # add DGPs and Methods using various syntax
    experiment %>%
      add_vary_across(.dgp = c("DGP1", "DGP2"), x = 1:3, y = c("a", "b")) %>%
      add_vary_across(.method = c("Method1", "Method2"), idx = 1:3)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 1:3, y = c("a", "b")),
                      DGP2 = list(x = 1:3, y = c("a", "b"))),
           method = list(Method1 = list(idx = 1:3),
                         Method2 = list(idx = 1:3)))
    )
    experiment %>% remove_vary_across()
    expect_equal(experiment$get_vary_across(), no_vary_list)

    experiment %>%
      add_vary_across(.dgp = c("DGP1"), x = 1:3)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 1:3)),
           method = list())
    )
    expect_error(
      experiment %>%
        add_vary_across(.dgp = c("DGP2", "DGP1"), x = 1:3)
    )
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 1:3),
                      DGP2 = list(x = 1:3)),
           method = list())
    )
    experiment %>%
      update_vary_across(.dgp = list("DGP1", "DGP2"), x = 2:4)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 2:4),
                      DGP2 = list(x = 2:4)),
           method = list())
    )
    experiment %>%
      update_vary_across(.dgp = list(dgp1, dgp2), x = 3:5)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 3:5),
                      DGP2 = list(x = 3:5)),
           method = list())
    )
    experiment %>%
      update_vary_across(.dgp = c(dgp1, dgp2), x = 4:6)
    expect_equal(
      experiment$get_vary_across(),
      list(dgp = list(DGP1 = list(x = 4:6),
                      DGP2 = list(x = 4:6)),
           method = list())
    )
    experiment %>%
      remove_vary_across()

    # test running Experiment
    fit_results_fun <- function(fit_results) fit_results
    fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)
    vary_params_fun <- function(vary_params = NULL) vary_params
    vary_params_eval <- create_evaluator(.eval_fun = vary_params_fun)
    experiment %>%
      add_vary_across(.dgp = c("DGP1", "DGP2"), x = 1:3, y = c("a", "b")) %>%
      add_vary_across(.method = c("Method1", "Method2"), idx = 1:3) %>%
      add_evaluator(fit_results_eval, name = "Fit Results") %>%
      add_evaluator(vary_params_eval, name = "Vary Params")

    experiment_copy <- create_experiment(
      name = "test-multi-vary-across-copy", clone_from = experiment
    ) %>%
      add_vary_across(.dgp = "DGP1", x = 1:3, y = c("a", "b")) %>%
      add_vary_across(.dgp = "DGP2", x = 1:3, y = c("a", "b")) %>%
      add_vary_across(.method = "Method1", idx = 1:3) %>%
      add_vary_across(.method = "Method2", idx = 1:3)

    fit_results <- fit_experiment(experiment, save = FALSE, verbose = 0)
    expect_equal(nrow(fit_results), 3 * 2 * 2 * 2 * 3)
    fit_results_copy <- fit_experiment(experiment_copy, save = FALSE, verbose = 0)
    expect_equal(fit_results, fit_results_copy)

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

  test_that("Exporting visualizers in Experiment works properly", {

    dgp_fun <- function(x, y = NULL) x + 1
    dgp <- DGP$new(dgp_fun, x = 1)
    method_fun <- function(x, y = NULL) x * 1
    method <- Method$new(method_fun)
    ggplot_plot <- create_visualizer(
      .viz_fun = function() ggplot2::ggplot()
    )
    plotly_plot <- create_visualizer(
      .viz_fun = function() plotly::ggplotly(ggplot2::ggplot())
    )
    error_plot <- create_visualizer(
      .viz_fun = function() 5
    )

    experiment <- create_experiment(name = "test-evaluate") %>%
      add_dgp(dgp, "DGP") %>%
      add_method(method, "Method")
    fit_results <- experiment$fit(verbose = 0)
    eval_results <- experiment$evaluate(fit_results, verbose = 0)
    fpath <- file.path(experiment$get_save_dir(), "viz_results")

    # with no visualizers
    expect_error(experiment$export_visualizers(), NA)
    expect_equal(length(list.files(path = fpath)), 0)

    # warning if can't export visualizer
    experiment %>% add_visualizer(plotly_plot, name = "plotly")
    viz_results <- experiment$visualize(fit_results, eval_results,
                                        save = TRUE, verbose = 0)
    expect_warning(experiment %>% export_visualizers())
    expect_equal(length(list.files(path = fpath)), 0)

    # with one working visualizer
    experiment %>%
      add_visualizer(ggplot_plot, name = "ggplot") %>%
      add_visualizer(error_plot, name = "error")
    viz_results <- experiment$visualize(fit_results, eval_results,
                                        save = TRUE, verbose = 0)
    expect_warning(expect_warning(experiment$export_visualizers()))
    expect_equal(list.files(path = fpath), "ggplot.pdf")

    # using vary across
    experiment %>%
      add_vary_across(.dgp = "DGP", y = 1:3)
    results <- run_experiment(experiment, save = TRUE, verbose = 0)
    expect_warning(expect_warning(experiment$export_visualizers()))
    expect_equal(
      list.files(
        path = file.path(experiment$get_save_dir(), "DGP", "Varying y", "viz_results")
      ),
      "ggplot.pdf"
    )
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
    viz_fun1 <- function(x) x / 3
    visualizer1 <- Visualizer$new(viz_fun1)

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
      add_vary_across(.dgp = "DGP1", x = 1:3)
    expect_snapshot_output(print(experiment))

    experiment %>%
      remove_vary_across(dgp = "DGP1") %>%
      add_vary_across(.method = "Method1", x = 1:3)
    expect_snapshot_output(print(experiment))

    experiment %>%
      add_vary_across(.dgp = "DGP1", x = 1:3) %>%
      add_vary_across(.dgp = "DGP2", x = 2:4)
    expect_snapshot_output(print(experiment))

    experiment %>%
      update_vary_across(.dgp = "DGP1", x = list(1:3)) %>%
      update_vary_across(.dgp = "DGP2", x = list(2:4))
    expect_snapshot_output(print(experiment))
  })

  test_that("Capturing errors, warnings, and messages from user-defined functions works as expected", {

    skip_on_ci()
    skip_on_cran()

    dgp_fun <- function(n=100, rho=0.5, noise_level=1) {
      X <- data.frame(.n = n, .rho = rho, .noise_level = noise_level)
      if (rho < 0.5)
        warning("rho must be greater than 0.5")
      return(list(X = X))
    }

    method_fun <- function(X, param1=1, param2=2, vec=c(1,2,3)) {
      if (param2 == 3)
        stop(sprintf("invalid param2 value: %s", param2))
      if (!3 %in% vec) {
        message("3 isn't in vec")
      }
      return(X)
    }

    eval_fun <- function(fit_results) {
      if (nrow(fit_results) > 1) {
        warning("that's a lot of rows")
      }
      fit_results[1, ]
    }

    viz_fun <- function(fit_results, eval_results, error=FALSE) {
      if (error) {
        stop("Oh no!")
      } else {
        warning("False alarm!")
      }
      "plot"
    }

    experiment <- create_experiment(name = "error-tracking") %>%
      add_dgp(create_dgp(dgp_fun, n = 10)) %>%
      add_dgp(create_dgp(dgp_fun, .name = "dgp_test", n = 10)) %>%
      add_method(create_method(method_fun), name = "method_test") %>%
      add_vary_across(
        .dgp = "dgp1",
        rho = c(0.2),
        noise_level = c(1, 2)
      ) %>%
      add_vary_across(
        .dgp = "dgp_test",
        rho = c(0.2, 0.9),
        noise_level = c(1, 2)
      ) %>%
      add_vary_across(
        .method = "method_test",
        param2 = c(2, 4),
        vec = list(c(2,3,4), 4:7)
      ) %>%
      add_evaluator(create_evaluator(eval_fun)) %>%
      add_visualizer(create_visualizer(viz_fun))

    expect_snapshot(
      invisible(experiment$fit(n_reps = 2)), transform = transform_fun
    )

    expect_snapshot(
      invisible(fit_results <- experiment$fit(n_reps = 2, verbose = 2)),
      transform = transform_fun
    )

    expect_snapshot(
      invisible(experiment$evaluate(fit_results)),
      transform = transform_fun
    )
    expect_snapshot(
      invisible(eval_results <- experiment$evaluate(fit_results, verbose = 2)),
      transform = transform_fun
    )

    expect_snapshot(
      invisible(experiment$visualize(fit_results, eval_results)),
      transform = transform_fun
    )
    expect_snapshot(
      invisible(viz_results <- experiment$visualize(
        fit_results, eval_results, verbose = 2
      )),
      transform = transform_fun
    )

    experiment %>%
      add_visualizer(create_visualizer(viz_fun, error = TRUE))

    expect_snapshot(
      invisible(experiment$visualize(fit_results, eval_results, verbose = 2)),
      error = TRUE, transform = transform_fun
    )

  })

  test_that("Signal simChef_error when output from Method has same name as a vary_across param", {
      dgp_fun1 <- function(y = "") return(x = list(paste0("data1", y)))
      dgp1 <- create_dgp(dgp_fun1)

      method_fun1 <- function(x) return(list(y = paste0(x, "+method1")))
      method1 <- create_method(method_fun1)

      method_fun2 <- function(x, z = "a") return(list(z = paste0(x, "+method2", z)))
      method2 <- create_method(method_fun2)

      # varying dgp param
      experiment <- create_experiment(dgp_list = list(dgp1),
                                      method_list = list(method1)) %>%
        add_vary_across(.dgp = dgp1, y = c("a", "b", "c"))

      expected_err_msg <- "Cannot create `fit_results` tibble with duplicate column names: "

      err <- expect_error(experiment$fit(n_reps = 2, verbose = 0))
      expect_partial_results_and_errors(err)
      expect_true(grepl(paste0(expected_err_msg, "`y`."), err$message))
      expect_s3_class(err$errors$.method_output[[1]], "tbl_df")
      expect_named(err$errors$.method_output[[1]], "y")
      expect_equal(err$errors$.method_output[[1]]$y[[1]], "data1a+method1")

      experiment <- create_experiment(dgp_list = list(dgp1),
                                      method_list = list(method2 = method2)) %>%
        add_vary_across(.method = method2, z = c("a", "b", "c"))

      # varying method param
      err <- expect_error(experiment$fit(n_reps = 2, verbose = 0))
      expect_partial_results_and_errors(err)
      expect_true(grepl(paste0(expected_err_msg, "`z`."), err$message))
      expect_s3_class(err$errors$.method_output[[1]], "tbl_df")
      expect_length(err$errors$.method_output, 3)
      for (i in seq(3)) {
        expected_output <- paste0(
          "data1+method2", unlist(experiment$get_vary_across())[i]
        )
        expect_named(err$errors$.method_output[[i]], "z")
        expect_equal(err$errors$.method_output[[i]]$z[[1]], expected_output)
      }
  })

}) # withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {
