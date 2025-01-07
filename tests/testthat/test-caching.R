withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {

  # put all tests inside of this block to avoid leaving temporary files laying
  # around when tests fail (for some reason withr::local_tempdir() isn't
  # working)

  test_that("Caching in Experiment runs properly", {

    withr::local_options(
      list(
        simChef.debug = FALSE,
        future.globals.maxSize = 850 * 1024^2
      )
    )

    # create experiment
    dgp_fun1 <- function(x, y = NULL) rnorm(1, mean = x)
    dgp_fun2 <- function(x, y = NULL) rnorm(1, mean = x, sd = 2)
    dgp1 <- DGP$new(dgp_fun1, x = 0)
    dgp2 <- DGP$new(dgp_fun2, x = 0)

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

    experiment <- create_experiment(name = "test-cache") |>
      add_dgp(dgp1, name = "DGP1") |>
      add_method(method1, name = "Method1") |>
      add_evaluator(fit_results_eval, name = "Evaluator1") |>
      add_visualizer(fit_plot, name = "Visualizer1")
    exp <- create_experiment(
      name = "test-cache-save-per-rep", save_in_bulk = FALSE
    ) |>
      add_dgp(dgp1, name = "DGP1") |>
      add_method(method1, name = "Method1") |>
      add_evaluator(fit_results_eval, name = "Evaluator1") |>
      add_visualizer(fit_plot, name = "Visualizer1")

    # remove cache
    if (dir.exists(file.path("results", "test-cache"))) {
      for (fname in list.files(file.path("results", "test-cache"),
                               recursive = TRUE, full.names = TRUE)) {
        file.remove(fname)
      }
    }
    if (dir.exists(file.path("results", "test-cache-save-per-rep"))) {
      for (fname in list.files(file.path("results", "test-cache-save-per-rep"),
                               recursive = TRUE, full.names = TRUE)) {
        file.remove(fname)
      }
    }

    verbose <- 0
    n_reps <- 2

    # basic cache usage
    results0 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                               verbose = verbose)
    results1 <- experiment$run(n_reps = n_reps, save = TRUE, verbose = verbose)
    expect_false(identical(results0$fit_results, results1$fit_results))
    results2 <- experiment$run(n_reps = n_reps, use_cached = TRUE, verbose = verbose)
    expect_equal(results1, results2)

    res0 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                    verbose = verbose)
    res1 <- exp$run(n_reps = n_reps, save = TRUE, verbose = verbose)
    expect_false(identical(res0$fit_results, res1$fit_results))
    res2 <- exp$run(n_reps = n_reps, use_cached = TRUE, verbose = verbose)
    expect_equal(res1, res2)

    # caching when adding objects
    experiment |> add_dgp(dgp2, "DGP2")
    results3 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results3$fit_results), 2 * n_reps)
    expect_equal(results2$fit_results,
                 results3$fit_results |> dplyr::filter(.dgp_name == "DGP1"))
    experiment |> add_method(method2, "Method2")
    results4 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results4$fit_results), 2 * 2 * n_reps)
    experiment |> add_evaluator(vary_params_eval, "Eval2")
    results5 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(results4$fit_results, results5$fit_results)
    expect_equal(results4$eval_results$Evaluator1,
                 results5$eval_results$Evaluator1)
    experiment |> add_visualizer(eval_plot, "Plot2")
    results6 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(results4$fit_results, results6$fit_results)
    expect_equal(results5$eval_results, results6$eval_results)
    expect_equal(results5$viz_results$Visualizer1,
                 results6$viz_results$Visualizer1)
    results7 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(results6, results7)

    exp |> add_dgp(dgp2, "DGP2")
    res3 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    expect_equal(nrow(res3$fit_results), 2 * n_reps)
    expect_equal(res2$fit_results,
                 res3$fit_results |> dplyr::filter(.dgp_name == "DGP1"))
    exp |> add_method(method2, "Method2")
    res4 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    expect_equal(nrow(res4$fit_results), 2 * 2 * n_reps)
    exp |> add_evaluator(vary_params_eval, "Eval2")
    res5 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    expect_equal(res4$fit_results, res5$fit_results)
    expect_equal(res4$eval_results$Evaluator1,
                 res5$eval_results$Evaluator1)
    exp |> add_visualizer(eval_plot, "Plot2")
    res6 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    expect_equal(res4$fit_results, res6$fit_results)
    expect_equal(res5$eval_results, res6$eval_results)
    expect_equal(res5$viz_results$Visualizer1,
                 res6$viz_results$Visualizer1)
    res7 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    expect_equal(res6, res7)

    # caching when update objects does not change original object
    experiment |> update_dgp(dgp2, "DGP2")
    results8 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(results8, results7)

    exp |> update_dgp(dgp2, "DGP2")
    res8 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                        verbose = verbose)
    expect_equal(res8, res7)

    # caching when updating objects that actually change
    experiment |> update_dgp(dgp1, "DGP2")
    fit_cols <- colnames(results7$fit_results)
    results8 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    results8$fit_results <- results8$fit_results |> dplyr::select({{fit_cols}})
    expect_equal(nrow(results7$fit_results), nrow(results8$fit_results))
    expect_false(identical(results7$fit_results, results8$fit_results))
    expect_equal(results8$fit_results |> dplyr::filter(.dgp_name == "DGP1"),
                 results7$fit_results |> dplyr::filter(.dgp_name == "DGP1"))
    expect_false(identical(results8$eval_results, results7$eval_results))
    expect_false(identical(results8$viz_results, results7$viz_results))
    experiment |> update_method(method1, "Method2")
    results9 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results7$fit_results), nrow(results9$fit_results))
    expect_false(identical(results8$eval_results, results7$eval_results))
    expect_false(identical(results8$viz_results, results7$viz_results))
    experiment |> update_evaluator(fit_results_eval, "Eval2")
    results10 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(results10$fit_results, results9$fit_results)
    expect_equal(length(results10$eval_results), 2)
    experiment |> update_visualizer(fit_plot, "Plot2")
    results11 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(results11$fit_results, results10$fit_results)
    expect_equal(results11$eval_results, results10$eval_results)
    expect_equal(length(results11$viz_results), 2)

    exp |> update_dgp(dgp1, "DGP2")
    fit_cols <- colnames(res7$fit_results)
    res8 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    res8$fit_results <- res8$fit_results |> dplyr::select({{fit_cols}})
    expect_equal(nrow(res7$fit_results), nrow(res8$fit_results))
    expect_false(identical(res7$fit_results, res8$fit_results))
    expect_equal(res8$fit_results |> dplyr::filter(.dgp_name == "DGP1"),
                 res7$fit_results |> dplyr::filter(.dgp_name == "DGP1"))
    expect_false(identical(res8$eval_results, res7$eval_results))
    expect_false(identical(res8$viz_results, res7$viz_results))
    exp |> update_method(method1, "Method2")
    res9 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                    verbose = verbose)
    expect_equal(nrow(res7$fit_results), nrow(res9$fit_results))
    expect_false(identical(res8$eval_results, res7$eval_results))
    expect_false(identical(res8$viz_results, res7$viz_results))
    exp |> update_evaluator(fit_results_eval, "Eval2")
    res10 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                     verbose = verbose)
    expect_equal(res10$fit_results, res9$fit_results)
    expect_equal(length(res10$eval_results), 2)
    exp |> update_visualizer(fit_plot, "Plot2")
    res11 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                     verbose = verbose)
    expect_equal(res11$fit_results, res10$fit_results)
    expect_equal(res11$eval_results, res10$eval_results)
    expect_equal(length(res11$viz_results), 2)

    # caching when removing objects
    experiment |> remove_dgp("DGP2")
    results12 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                 verbose = verbose)
    expect_equal(results12$fit_results,
                 results11$fit_results |> dplyr::filter(.dgp_name == "DGP1"))
    expect_false(identical(results12$eval_results, results11$eval_results))
    expect_false(identical(results12$viz_results, results11$viz_results))
    experiment |> remove_method("Method2")
    results13 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                 verbose = verbose)
    expect_equal(results13$fit_results,
                 results12$fit_results |> dplyr::filter(.method_name == "Method1"))
    expect_false(identical(results13$eval_results, results12$eval_results))
    expect_false(identical(results13$viz_results, results12$viz_results))
    experiment |> remove_evaluator("Eval2")
    results14 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                 verbose = verbose)
    expect_equal(results14$fit_results, results13$fit_results)
    expect_equal(names(results14$eval_results), "Evaluator1")
    expect_equal(results14$eval_results, results13$eval_results[1])
    experiment |> remove_visualizer("Plot2")
    results15 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                 verbose = verbose)
    expect_equal(results15$fit_results, results14$fit_results)
    expect_equal(results15$eval_results, results14$eval_results)
    expect_equal(names(results15$viz_results), "Visualizer1")
    expect_equal(results15$viz_results, results14$viz_results[1])

    exp |> remove_dgp("DGP2")
    res12 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(res12$fit_results,
                 res11$fit_results |> dplyr::filter(.dgp_name == "DGP1"))
    expect_false(identical(res12$eval_results, res11$eval_results))
    expect_false(identical(res12$viz_results, res11$viz_results))
    exp |> remove_method("Method2")
    res13 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(res13$fit_results,
                 res12$fit_results |> dplyr::filter(.method_name == "Method1"))
    expect_false(identical(res13$eval_results, res12$eval_results))
    expect_false(identical(res13$viz_results, res12$viz_results))
    exp |> remove_evaluator("Eval2")
    res14 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(res14$fit_results, res13$fit_results)
    expect_equal(names(res14$eval_results), "Evaluator1")
    expect_equal(res14$eval_results, res13$eval_results[1])
    exp |> remove_visualizer("Plot2")
    res15 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(res15$fit_results, res14$fit_results)
    expect_equal(res15$eval_results, res14$eval_results)
    expect_equal(names(res15$viz_results), "Visualizer1")
    expect_equal(res15$viz_results, res14$viz_results[1])

    # caching when vary across
    experiment |> add_vary_across(.dgp = "DGP1", x = c(0, 1))
    results1 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results1$fit_results), 2 * n_reps)
    experiment |> add_vary_across(.method = "Method1", y = c(0, 1))
    results2 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results2$fit_results), 2 * 2 * n_reps)
    experiment |> remove_vary_across(method = "Method1")
    results3 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results3$fit_results), 2 * n_reps)
    expect_true(identical(results1$fit_results, results3$fit_results))
    experiment |> update_vary_across(.dgp = "DGP1", x = c(0, 2))
    results4 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(results3$fit_results |> dplyr::filter(x == 0),
                 results4$fit_results |> dplyr::filter(x == 0))
    experiment |> update_vary_across(.dgp = "DGP1", x = list(0, 2, 4))
    results5 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(results4$fit_results |> dplyr::filter(x %in% c(0, 2)),
                 results5$fit_results |> dplyr::filter(x %in% c(0, 2)))
    experiment |> add_vary_across(.method = "Method1", y = list("a", "b"))
    results6 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(results6$fit_results), 3 * 2 * n_reps)
    experiment |> update_vary_across(.method = "Method1", y = list("a", 1))
    results6b <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(
      results6$fit_results |>
        dplyr::filter(y %in% c("a")),
      results6b$fit_results |>
        dplyr::mutate(
          y = purrr::map_chr(y, as.character)
        ) |>
        dplyr::filter(y %in% c("a"))
    )
    expect_equal(nrow(results6b$fit_results), 3 * 2 * n_reps)

    exp |> add_vary_across(.dgp = "DGP1", x = c(0, 1))
    res1 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(res1$fit_results), 2 * n_reps)
    exp |> add_vary_across(.method = "Method1", y = c(0, 1))
    res2 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(res2$fit_results), 2 * 2 * n_reps)
    exp |> remove_vary_across(method = "Method1")
    res3 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(res3$fit_results), 2 * n_reps)
    expect_true(identical(res1$fit_results, res3$fit_results))
    exp |> update_vary_across(.dgp = "DGP1", x = c(0, 2))
    res4 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(res3$fit_results |> dplyr::filter(x == 0),
                 res4$fit_results |> dplyr::filter(x == 0))
    exp |> update_vary_across(.dgp = "DGP1", x = list(0, 2, 4))
    res5 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(res4$fit_results |> dplyr::filter(x %in% c(0, 2)),
                 res5$fit_results |> dplyr::filter(x %in% c(0, 2)))
    exp |> add_vary_across(.method = "Method1", y = list("a", "b"))
    res6 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(res6$fit_results), 3 * 2 * n_reps)
    exp |> update_vary_across(.method = "Method1", y = list("a", 1))
    res6b <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                     verbose = verbose)
    expect_equal(
      res6$fit_results |>
        dplyr::filter(y %in% c("a")),
      res6b$fit_results |>
        dplyr::mutate(
          y = purrr::map_chr(y, as.character)
        ) |>
        dplyr::filter(y %in% c("a"))
    )
    expect_equal(nrow(res6b$fit_results), 3 * 2 * n_reps)

    # check caching when n changes
    results6b <- experiment$run(n_reps = 10, use_cached = FALSE, save = TRUE,
                                verbose = verbose)
    results7 <- experiment$run(n_reps = 4, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    extra_reps_fpath <- file.path(
      "results", "test-cache", "DGP1-Method1", "Varying x-y",
      "fit_results_extra_cached_reps.rds"
    )
    extra_fit_results7 <- readRDS(extra_reps_fpath)
    expect_equal(nrow(results7$fit_results), 4 * 3 * 2)
    expect_equal(nrow(extra_fit_results7), 6 * 3 * 2)
    expect_equal(results7$fit_results,
                 results6b$fit_results |> dplyr::filter(as.numeric(.rep) <= 4))
    expect_equal(simplify_tibble(extra_fit_results7),
                 results6b$fit_results |> dplyr::filter(as.numeric(.rep) > 4))
    results8 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    fit_results8 <- experiment$get_cached_results("fit")
    expect_equal(results8$fit_results, fit_results8)
    expect_equal(nrow(results8$fit_results), 10 * 3 * 2)
    expect_false(file.exists(extra_reps_fpath))
    expect_true(identical(results7$fit_results,
                          results8$fit_results |>
                            dplyr::filter(as.numeric(.rep) <= 4)))
    expect_true(identical(results6b$fit_results, results8$fit_results))

    res6b <- exp$run(n_reps = 10, use_cached = FALSE, save = TRUE,
                     verbose = verbose)
    extra_rep_fpath <- file.path(
      "results", "test-cache-save-per-rep", "DGP1-Method1", "Varying x-y",
      "fit_results",
      "fit_result6.rds"
    )
    extra_fit_res7a <- readRDS(extra_rep_fpath)
    res7 <- exp$run(n_reps = 4, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    extra_fit_res7b <- readRDS(extra_rep_fpath)
    expect_equal(extra_fit_res7a, extra_fit_res7b)
    expect_equal(nrow(res7$fit_results), 4 * 3 * 2)
    res8 <- exp$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_equal(nrow(res8$fit_results), 10 * 3 * 2)
    expect_true(identical(res6b$fit_results, res8$fit_results))

    # check caching when n changes and experiment changes
    experiment |> add_dgp(dgp2, "DGP2")
    results9 <- experiment$run(n_reps = 5, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    expect_true(all(results9$fit_results$.rep %in% as.character(1:5)))
    expect_equal(results9$fit_results |> dplyr::filter(.dgp_name != "DGP2"),
                 results8$fit_results |> dplyr::filter(as.numeric(.rep) <= 5))
    expect_equal(simplify_tibble(readRDS(extra_reps_fpath)),
                 results8$fit_results |> dplyr::filter(as.numeric(.rep) > 5))
    expect_equal(nrow(results9$fit_results), 5 * 3 * 2 + 5 * 2)

    exp |> add_dgp(dgp2, "DGP2")
    extra_fit_res10a <- readRDS(extra_rep_fpath)
    res9 <- exp$run(n_reps = 5, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    extra_fit_res10b <- readRDS(extra_rep_fpath)
    expect_true(all(res9$fit_results$.rep %in% as.character(1:5)))
    expect_equal(res9$fit_results |> dplyr::filter(.dgp_name != "DGP2"),
                 res8$fit_results |> dplyr::filter(as.numeric(.rep) <= 5))
    expect_equal(extra_fit_res10a, extra_fit_res10b)
    expect_equal(nrow(res9$fit_results), 5 * 3 * 2 + 5 * 2)

    # check return_all_cached_reps works properly
    results10 <- experiment$run(n_reps = 5, use_cached = FALSE,
                                return_all_cached_reps = TRUE, verbose = verbose)
    expect_equal(nrow(results9$fit_results), nrow(results10$fit_results))
    results11 <- experiment$run(n_reps = 5, use_cached = TRUE,
                                return_all_cached_reps = TRUE, verbose = verbose)
    expect_equal(nrow(results11$fit_results), 10 * 3 * 2 + 5 * 2)
    expect_equal(results11$eval_results, results9$eval_results)
    expect_equal(results11$viz_results, results9$viz_results)
    experiment |> remove_dgp("DGP2")

    res10 <- exp$run(n_reps = 5, use_cached = FALSE,
                                return_all_cached_reps = TRUE, verbose = verbose)
    expect_equal(nrow(res9$fit_results), nrow(res10$fit_results))
    res11 <- exp$run(n_reps = 5, use_cached = TRUE,
                                return_all_cached_reps = TRUE, verbose = verbose)
    expect_equal(nrow(res11$fit_results), 10 * 3 * 2 + 5 * 2)
    expect_equal(res11$eval_results, res9$eval_results)
    expect_equal(res11$viz_results, res9$viz_results)
    exp |> remove_dgp("DGP2")

    # check when add multiple new objects to experiment
    experiment |> add_dgp(dgp2, "DGP3")
    experiment |>
      update_dgp(dgp2, "DGP3") |>
      update_vary_across(.method = method1, y = c("a", "b"))
    results9 <- experiment$run(
      n_reps = n_reps, use_cached = TRUE, save = TRUE, verbose = verbose
    )
    expect_equal(nrow(results9$fit_results), 4 * 2 * n_reps)

    exp |> add_dgp(dgp2, "DGP3")
    exp |>
      update_dgp(dgp2, "DGP3") |>
      update_vary_across(.method = method1, y = c("a", "b"))
    res9 <- exp$run(
      n_reps = n_reps, use_cached = TRUE, save = TRUE, verbose = verbose
    )
    expect_equal(nrow(res9$fit_results), 4 * 2 * n_reps)

    # check clear cache
    results10 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(results9, results10)
    experiment |> clear_cache()
    results11 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_false(identical(results11$fit_results, results10$fit_results))

    res10 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_equal(res9, res10)
    exp |> clear_cache()
    res11 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = TRUE,
                                verbose = verbose)
    expect_false(identical(res11$fit_results, res10$fit_results))

    # check caching works when not saving
    results12 <- experiment$run(n_reps = 1, use_cached = TRUE, save = FALSE,
                                verbose = verbose)
    expect_true(identical(results11$fit_results |>
                            dplyr::filter(as.numeric(.rep) <= 1),
                          results12$fit_results))
    results13 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                                verbose = verbose)
    expect_true(identical(results13, results11))

    res12 <- exp$run(n_reps = 1, use_cached = TRUE, save = FALSE,
                                verbose = verbose)
    expect_true(identical(res11$fit_results |>
                            dplyr::filter(as.numeric(.rep) <= 1),
                          res12$fit_results))
    res13 <- exp$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                                verbose = verbose)
    expect_true(identical(res13, res11))

    # check running fit, evaluate, and visualize separately
    fit_results <- experiment$fit(n_reps = 4, use_cached = TRUE, save = TRUE,
                                  verbose = verbose)
    eval_results <- experiment$evaluate(fit_results, use_cached = TRUE,
                                        save = TRUE, verbose = verbose)
    viz_results <- experiment$visualize(fit_results, eval_results,
                                        use_cached = TRUE, save = TRUE,
                                        verbose = verbose)
    fit_results <- experiment$fit(n_reps = 2, use_cached = TRUE, save = FALSE,
                                  verbose = verbose)
    eval_results <- experiment$evaluate(fit_results, use_cached = TRUE,
                                        save = FALSE, verbose = verbose)
    viz_results <- experiment$visualize(fit_results, eval_results,
                                        use_cached = TRUE, save = FALSE,
                                        verbose = verbose)

    fit_res <- exp$fit(n_reps = 4, use_cached = TRUE, save = TRUE,
                                  verbose = verbose)
    eval_res <- exp$evaluate(fit_res, use_cached = TRUE,
                                        save = TRUE, verbose = verbose)
    viz_res <- exp$visualize(fit_res, eval_res,
                                        use_cached = TRUE, save = TRUE,
                                        verbose = verbose)
    fit_res <- exp$fit(n_reps = 2, use_cached = TRUE, save = FALSE,
                                  verbose = verbose)
    eval_res <- exp$evaluate(fit_res, use_cached = TRUE,
                                        save = FALSE, verbose = verbose)
    viz_res <- exp$visualize(fit_res, eval_res,
                                        use_cached = TRUE, save = FALSE,
                                        verbose = verbose)

    # check with non-standard combos of save = T and F are used
    fit_results2 <- experiment$fit(n_reps = 6, use_cached = TRUE, save = FALSE,
                                   verbose = verbose)
    experiment |>
      add_evaluator(vary_params_eval, "Evaluator2")
    eval_results2 <- experiment$evaluate(fit_results2, use_cached = TRUE,
                                         save = TRUE, verbose = verbose)
    expect_false(identical(eval_results2$Evaluator1, eval_results$Evaluator1))
    viz_results2 <- experiment$visualize(fit_results2, eval_results2,
                                         use_cached = TRUE, save = FALSE,
                                         verbose = verbose)
    expect_false(identical(viz_results, viz_results2))
    cached_params <- readRDS(
      file.path("results", "test-cache", "DGP1-Method1", "Varying x-y",
                "experiment_cached_params.rds")
    )
    expect_equal(cached_params$fit$fit$.n_reps, rep(4, 8))
    expect_equal(cached_params$evaluate$fit$.n_reps, rep(6, 8))
    expect_equal(cached_params$visualize$fit$.n_reps, rep(4, 8))

    fit_res2 <- exp$fit(n_reps = 6, use_cached = TRUE, save = FALSE,
                                   verbose = verbose)
    exp |>
      add_evaluator(vary_params_eval, "Evaluator2")
    eval_res2 <- exp$evaluate(fit_res2, use_cached = TRUE,
                                         save = TRUE, verbose = verbose)
    expect_false(identical(eval_res2$Evaluator1, eval_res$Evaluator1))
    viz_res2 <- exp$visualize(fit_res2, eval_res2,
                                         use_cached = TRUE, save = FALSE,
                                         verbose = verbose)
    expect_false(identical(viz_res, viz_res2))
    cached_params <- readRDS(
      file.path("results", "test-cache-save-per-rep", "DGP1-Method1", "Varying x-y",
                "experiment_cached_params.rds")
    )
    expect_equal(cached_params$fit$fit$.n_reps, rep(4, 8))
    expect_equal(cached_params$evaluate$fit$.n_reps, rep(6, 8))
    expect_equal(cached_params$visualize$fit$.n_reps, rep(4, 8))

    # check if caching works for functions
    my_mean <- function(x) mean(x)
    my_median <- function(x) median(x)
    dgp_fun1 <- function(f) f(rnorm(100))
    dgp1 <- DGP$new(dgp_fun1, f = my_mean)
    dgp_fun2 <- function(g) g(runif(100))
    dgp2 <- DGP$new(dgp_fun2, g = my_mean)

    method_fun1 <- function(g) g
    method1 <- Method$new(method_fun1)

    fit_results_fun <- function(fit_results, f = mean) f(rnorm(100))
    fit_results_eval <- create_evaluator(.eval_fun = fit_results_fun)

    fit_plot_fun <- function(fit_results, f = mean) f(rnorm(100))
    fit_plot <- create_visualizer(.viz_fun = fit_plot_fun)

    clean_fun <- function(function_col) purrr::map(function_col, deparse)

    experiment <- create_experiment(name = "test-cache-function") |>
      add_dgp(dgp1, name = "DGP1") |>
      add_method(method1, name = "Method1") |>
      add_evaluator(fit_results_eval, name = "Evaluator1") |>
      add_visualizer(fit_plot, name = "Visualizer1")

    # remove cache
    if (dir.exists(file.path("results", "test-cache-function"))) {
      for (fname in list.files(file.path("results", "test-cache-function"),
                               recursive = TRUE, full.names = TRUE)) {
        file.remove(fname)
      }
    }

    # basic cache usage
    results0 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                               verbose = verbose)
    results1 <- experiment$run(n_reps = n_reps, save = TRUE, verbose = verbose)
    expect_false(identical(results0$fit_results, results1$fit_results))
    results2 <- experiment$run(n_reps = n_reps, use_cached = TRUE, verbose = verbose)
    expect_equal(results1, results2)

    # try caching with function in vary across
    experiment |>
      add_vary_across(.dgp = "DGP1", f = list(my_mean, my_median))
    results0 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                               verbose = verbose)
    results1 <- experiment$run(n_reps = n_reps, save = TRUE, verbose = verbose)
    expect_false(identical(results0$fit_results, results1$fit_results))
    results2 <- experiment$run(n_reps = n_reps, use_cached = TRUE, verbose = verbose)
    # to ignore function source bytecode
    results1$fit_results$f <- clean_fun(results1$fit_results$f)
    results2$fit_results$f <- clean_fun(results2$fit_results$f)
    expect_equal(results1, results2)

    # try caching with function in visualizer and evaluator
    new_eval <- create_evaluator(.eval_fun = fit_results_fun, f = my_median)
    new_plot <- create_visualizer(.viz_fun = fit_plot_fun, f = my_median)
    experiment |>
      update_evaluator(new_eval, "Evaluator1") |>
      update_visualizer(new_plot, "Visualizer1")
    results0 <- experiment$run(n_reps = n_reps, use_cached = TRUE, save = FALSE,
                               verbose = verbose)
    results0$fit_results$f <- clean_fun(results0$fit_results$f)
    results1 <- experiment$run(n_reps = n_reps, save = TRUE, verbose = verbose)
    results1$fit_results$f <- clean_fun(results1$fit_results$f)
    expect_false(identical(results0$fit_results, results1$fit_results))
    results2 <- experiment$run(n_reps = n_reps, use_cached = TRUE, verbose = verbose)
    results2$fit_results$f <- purrr::map(results2$fit_results$f, deparse)
    expect_equal(results1, results2)

    # check caching with function and different n_reps
    results2 <- experiment$run(n_reps = 10, use_cached = FALSE, save = TRUE,
                               verbose = verbose)
    results2$fit_results$f <- purrr::map(results2$fit_results$f, deparse)
    results3 <- experiment$run(n_reps = 4, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    results3$fit_results$f <- clean_fun(results3$fit_results$f)
    extra_reps_fpath <- file.path(
      "results", "test-cache-function", "DGP1", "Varying f",
      "fit_results_extra_cached_reps.rds"
    )
    extra_fit_results3 <- readRDS(extra_reps_fpath)
    extra_fit_results3$f <- clean_fun(extra_fit_results3$f)
    expect_equal(nrow(results3$fit_results), 4 * 2)
    expect_equal(nrow(extra_fit_results3), 6 * 2)
    expect_equal(results3$fit_results,
                 results2$fit_results |> dplyr::filter(as.numeric(.rep) <= 4))
    expect_equal(simplify_tibble(extra_fit_results3),
                 results2$fit_results |> dplyr::filter(as.numeric(.rep) > 4))

    results4 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    results4$fit_results$f <- clean_fun(results4$fit_results$f)
    fit_results4 <- readRDS(
      file.path("results", "test-cache-function", "DGP1", "Varying f",
                "fit_results.rds")
    )
    fit_results4$f <- clean_fun(fit_results4$f)
    expect_equal(results4$fit_results, simplify_tibble(fit_results4))
    expect_equal(nrow(results4$fit_results), 10 * 2)
    expect_false(file.exists(extra_reps_fpath))
    expect_true(identical(results3$fit_results,
                          results4$fit_results |>
                            dplyr::filter(as.numeric(.rep) <= 4)))
    expect_true(identical(results2$fit_results, results4$fit_results))

    results5 <- experiment$run(n_reps = 15, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    results5$fit_results$f <- clean_fun(results5$fit_results$f)
    expect_equal(nrow(results5$fit_results), 15 * 2)
    expect_equal(results4$fit_results |> dplyr::filter(as.numeric(.rep) <= 10),
                 results2$fit_results)

    experiment |> add_dgp(dgp2, name = "DGP2")
    results6 <- experiment$run(n_reps = 10, use_cached = TRUE, save = TRUE,
                               verbose = verbose)
    results6$fit_results$f <- clean_fun(results6$fit_results$f)
    expect_equal(nrow(results6$fit_results), 10 * 3)
    expect_equal(results6$fit_results |>
                   dplyr::filter(as.numeric(.rep) <= 10, .dgp_name == "DGP1"),
                 results2$fit_results)

    # caching works with function arguments
    n <- 50
    p <- 3
    dgp_fun <- function(n, p, func_arg) {
      X <- matrix(rnorm(n * p), nrow = n, ncol = p)
      y <- rnorm(n)
      return(list(X = X, y = y))
    }
    method_fun <- function(X, y) {
      lm_df <- cbind(data.frame(X), .y = y)
      lm_fit <- lm(.y ~ ., dat = lm_df)
      return(coef(lm_fit))
    }
    method <- create_method(method_fun, .name = "Method")

    helper_fun <- function(x) sum(x)
    working_func_arg <- function() {
      z <- apply(X, 1, FUN = helper_fun)
    }
    working_dgp <- create_dgp(dgp_fun, .name = "Working DGP", n = n, p = p, func_arg = working_func_arg)
    experiment <- create_experiment(
      name = "test-cache-function-args1"
    ) |>
      add_dgp(working_dgp) |>
      add_method(method)
    fit_results <- experiment$fit(save = TRUE, use_cached = FALSE)
    fit_results2 <- experiment$fit(save = TRUE, use_cached = TRUE)
    expect_equal(fit_results, fit_results2)

    buggy_func_arg1 <- function() {
      z <- apply(X, 1, FUN = function(x) sum(x))
    }
    buggy_dgp1 <- create_dgp(dgp_fun, .name = "Buggy DGP1", n = n, p = p, func_arg = buggy_func_arg1)
    buggy_func_arg2 <- function() {
      function(x) sum(x)
      return("hi")
    }
    buggy_dgp2 <- create_dgp(dgp_fun, .name = "Buggy DGP2", n = n, p = p, func_arg = buggy_func_arg2)
    experiment <- create_experiment(
      name = "test-cache-function-args2"
    ) |>
      add_dgp(buggy_dgp1) |>
      add_dgp(buggy_dgp2) |>
      add_method(method)
    fit_results <- experiment$fit(save = TRUE, use_cached = FALSE, verbose = verbose)
    fit_results2 <- experiment$fit(save = TRUE, use_cached = TRUE, verbose = verbose)
    expect_equal(fit_results, fit_results2)

    exp <- create_experiment(
      name = "test-cache-function-args3",
      save_in_bulk = FALSE
    ) |>
      add_dgp(buggy_dgp1) |>
      add_dgp(buggy_dgp2) |>
      add_method(method)
    fit_results <- experiment$fit(save = TRUE, use_cached = FALSE, verbose = verbose)
    fit_results2 <- experiment$fit(save = TRUE, use_cached = TRUE, verbose = verbose)
    expect_equal(fit_results, fit_results2)
  })

})
