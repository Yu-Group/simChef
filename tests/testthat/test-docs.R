withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {

  # put all tests inside of this block to avoid leaving temporary files laying
  # around when tests fail (for some reason withr::local_tempdir() isn't
  # working)

  test_that("Automated R Markdown documentation works properly", {
    skip_on_cran()
    skip_on_ci()

    withr::local_options(list(simChef.debug = FALSE))

    dgp_fun <- function(x) x + 1
    dgp <- create_dgp(dgp_fun, x = 1)
    method_fun <- function(x) x + 2
    method <- create_method(method_fun)
    eval_fun <- function() "Evaluation."
    evaluator <- create_evaluator(eval_fun)
    viz_fun <- function() {
      ggplot2::ggplot(iris) +
        ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
        ggplot2::geom_point()
    }
    visualizer <- create_visualizer(viz_fun)

    base_experiment <- create_experiment(name = "test-rmd") |>
      add_dgp(dgp, "DGP") |>
      add_method(method, "Method") |>
      add_evaluator(evaluator, "Evaluator") |>
      add_visualizer(visualizer, "Plot")
    results <- base_experiment$run(save = TRUE, verbose = 0)
    export_visualizers(base_experiment)

    child1 <- create_experiment(
      name = "child1",
      clone_from = base_experiment,
      save_dir = file.path(base_experiment$get_save_dir(), "child1")
    )
    results <- child1$run(save = TRUE, verbose = 0)
    export_visualizers(child1)
    child2 <- create_experiment(
      name = "child2",
      clone_from = base_experiment,
      save_dir = file.path(base_experiment$get_save_dir(), "child2")
    )
    results <- child2$run(save = TRUE, verbose = 0)
    export_visualizers(child2)

    grandchild1a <- create_experiment(
      name = "grandchild1a",
      clone_from = child1,
      save_dir = file.path(child1$get_save_dir(), "grandchild1a")
    )
    results <- grandchild1a$run(save = TRUE, verbose = 0)
    export_visualizers(grandchild1a)

    grandchild1b <- create_experiment(
      name = "grandchild1b",
      clone_from = child1,
      save_dir = file.path(child1$get_save_dir(), "grandchild1b")
    )
    results <- grandchild1b$run(save = TRUE, verbose = 0)
    export_visualizers(grandchild1b)

    grandchild2 <- create_experiment(
      name = "grandchild2",
      clone_from = child2,
      save_dir = file.path(child2$get_save_dir(), "grandchild2")
    )
    results <- grandchild2$run(save = TRUE, verbose = 0)
    export_visualizers(grandchild2)

    greatgrandchild2 <- create_experiment(
      name = "greatgrandchild2",
      clone_from = grandchild2,
      save_dir = file.path(grandchild2$get_save_dir(), "greatgrandchild2"),
      save_in_bulk = FALSE
    )
    results <- greatgrandchild2$run(save = TRUE, verbose = 0)
    export_visualizers(greatgrandchild2)

    expect_error(render_docs(base_experiment, verbose = 0), NA)
    expect_error(
      render_docs(
        save_dir = base_experiment$get_save_dir(), verbose = 0
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        output_format = rmarkdown::html_document()
      ),
      NA
    )

    # check different cache modes for evaluators and visualizers
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_cache = ".rds", viz_cache = "none"
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_cache = "none", viz_cache = ".rds"
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_cache = "none", viz_cache = "none"
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_cache = "none", viz_cache = ".png"
      ),
      NA
    )

    # check ... arguments to rmarkdown::render()
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        output_options = list(css = "css/simchef.css")
      ),
      NA
    )

    # test eval_order and viz_order
    base_experiment <- base_experiment |>
      add_evaluator(evaluator, "Evaluator2") |>
      add_visualizer(visualizer, "Plot2")
    results <- base_experiment$run(save = TRUE, verbose = 0)

    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_order = "Evaluator", viz_order = c("Plot2", "Plot3", "Plot")
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_order = "Evaluator", viz_order = c("Plot2", "Plot3", "Plot")
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_order = c("Evaluator2", "Evaluator"), viz_order = c("Plot2", "Plot")
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        eval_order = c("Evaluator2", "Evaluator"), viz_order = c("Plot2", "Plot")
      ),
      NA
    )

    # check use_icons
    expect_error(
      render_docs(
        base_experiment, verbose = 0,
        output_format = rmarkdown::pdf_document()
      )
    )
    # # ERROR: ! LaTeX Error: Unknown float option `H'.
    # expect_error(
    #   render_docs(
    #     base_experiment, use_icons = FALSE, verbose = 0,
    #     output_format = rmarkdown::pdf_document()
    #   ),
    #   NA
    # )

    # test write_rmd = TRUE
    expect_error(
      render_docs(
        base_experiment, write_rmd = TRUE, verbose = 0,
        output_file = file.path(base_experiment$get_save_dir(), "test1")
      ),
      NA
    )
    # expect_error(
    #   render_docs(
    #     base_experiment, write_rmd = TRUE, verbose = 0,
    #     use_icons = FALSE, output_format = rmarkdown::pdf_document(),
    #     output_file = file.path(base_experiment$get_save_dir(), "test2")
    #   ),
    #   NA
    # )
    expect_error(
      render_docs(
        base_experiment, write_rmd = TRUE, verbose = 0,
        output_file = file.path(base_experiment$get_save_dir(), "test3"),
        eval_cache = "none", viz_cache = "none"
      ),
      NA
    )
    expect_error(
      rmarkdown::render(file.path(base_experiment$get_save_dir(), "test3.Rmd")),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, write_rmd = TRUE, verbose = 0,
        output_file = file.path(base_experiment$get_save_dir(), "test4"),
        eval_cache = "none", viz_cache = ".png"
      ),
      NA
    )
    expect_error(
      rmarkdown::render(file.path(base_experiment$get_save_dir(), "test4.Rmd")),
      NA
    )

    # test viz_interactive = TRUE
    expect_error(
      render_docs(base_experiment, viz_interactive = TRUE, verbose = 0),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, write_rmd = TRUE, viz_interactive = TRUE, verbose = 0
      ),
      NA
    )
    expect_error(
      render_docs(
        base_experiment, viz_interactive = TRUE, viz_cache = ".png", verbose = 0
      ),
      NA
    ) # > static viz images

    # check defuse requirement
    output_format <- quote(rmarkdown::html_document())
    expect_error(
      render_docs(
        base_experiment, write_rmd = FALSE, verbose = 0,
        output_format = eval(output_format)
      ),
      NA
    )
  })

  test_that("Visualizations in R Markdown documentation render correctly", {
    skip_on_cran()
    skip_on_ci()

    withr::local_options(list(simChef.debug = FALSE))

    dgp_fun <- function(x) x + 1
    dgp <- create_dgp(dgp_fun, x = 1)
    method_fun <- function(x) x + 2
    method <- create_method(method_fun)
    eval_fun <- function() "Evaluation."
    evaluator <- create_evaluator(eval_fun)
    viz_fun <- function() {
      ggplot2::ggplot(iris) +
        ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
        ggplot2::geom_point()
    }
    visualizer <- create_visualizer(viz_fun)
    tab_fun <- function() vthemes::pretty_DT(iris)
    tabler <- create_visualizer(tab_fun)
    text_fun <- function() "Hello world!"
    texter <- create_visualizer(text_fun)

    experiment <- create_experiment(name = "test-visualizers") |>
      add_dgp(dgp, "DGP") |>
      add_method(method, "Method") |>
      add_evaluator(evaluator, "Evaluator") |>
      add_visualizer(visualizer, "Plot") |>
      add_visualizer(tabler, "Table") |>
      add_visualizer(texter, "Text")
    results <- run_experiment(experiment, save = TRUE, verbose = 0)

    expect_error(render_docs(experiment, verbose = 0), NA)
  })

  test_that("R Markdown options work properly", {
    skip_on_cran()
    skip_on_ci()

    withr::local_options(list(simChef.debug = FALSE))

    dgp_fun <- function(x) x + 1
    dgp <- create_dgp(dgp_fun, x = 1)
    method_fun <- function(x) x + 2
    method <- create_method(method_fun)
    eval_fun <- function() iris
    evaluator1 <- create_evaluator(eval_fun)
    evaluator2 <- create_evaluator(eval_fun, .doc_options = list(digits = 3))
    evaluator3 <- create_evaluator(eval_fun)
    evaluator4 <- create_evaluator(eval_fun)
    viz_fun <- function() {
      ggplot2::ggplot(iris) +
        ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
        ggplot2::geom_point()
    }
    visualizer1 <- create_visualizer(viz_fun)
    visualizer2 <- create_visualizer(viz_fun, .doc_options = list(height = 3))
    visualizer3 <- create_visualizer(viz_fun)
    visualizer4 <- create_visualizer(viz_fun)

    experiment <- create_experiment(name = "test-doc-options") |>
      add_dgp(dgp, "DGP") |>
      add_method(method, "Method") |>
      add_evaluator(evaluator1, "Evaluator (digits = 2)") |>
      add_evaluator(evaluator2, "Evaluator (digits = 3)") |>
      add_evaluator(evaluator3, "Evaluator (digits = 4)") |>
      add_evaluator(evaluator4, "Evaluator (no show)") |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (digits = 4)",
                      digits = 4) |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (digits = 4)",
                      nrows = 10) |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (no show)",
                      show = FALSE) |>
      add_visualizer(visualizer1, "Visualizer (height = 6)") |>
      add_visualizer(visualizer2, "Visualizer (height = 3)") |>
      add_visualizer(visualizer3, "Visualizer (height = 9)") |>
      add_visualizer(visualizer4, "Visualizer (no show)") |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (height = 9)",
                      height = 9) |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (no show)",
                      show = FALSE)
    results <- run_experiment(experiment, save = TRUE, verbose = 0)

    experiment <- experiment |>
      add_vary_across(.dgp = "DGP", x = 1:3)
    results <- run_experiment(experiment, save = TRUE, verbose = 0)

    expect_error(render_docs(experiment, verbose = 0), NA)
    expect_snapshot(render_docs(experiment), transform = remove_tempdir)

    expect_equal(purrr::map_lgl(experiment$get_evaluators(), "doc_show"),
                 c(T, T, T, F) |> setNames(names(experiment$get_evaluators())))
    expect_equal(purrr::map_dbl(experiment$get_evaluators(),
                                ~.x$doc_options$digits),
                 c(2, 3, 4, 2) |> setNames(names(experiment$get_evaluators())))
    expect_equal(purrr::map(experiment$get_evaluators(), "doc_nrows"),
                 list(NULL, NULL, 10, NULL) |> setNames(names(experiment$get_evaluators())))
    expect_equal(purrr::map_lgl(experiment$get_visualizers(), "doc_show"),
                 c(T, T, T, F) |> setNames(names(experiment$get_visualizers())))
    expect_equal(purrr::map_dbl(experiment$get_visualizers(),
                                ~.x$doc_options$height),
                 c(6, 3, 9, 6) |> setNames(names(experiment$get_visualizers())))

    evaluator1 <- create_evaluator(eval_fun)
    evaluator2 <- create_evaluator(eval_fun, .doc_options = list(digits = 3))
    evaluator3 <- create_evaluator(eval_fun)
    evaluator4 <- create_evaluator(eval_fun)
    visualizer1 <- create_visualizer(viz_fun)
    visualizer2 <- create_visualizer(viz_fun, .doc_options = list(height = 3))
    visualizer3 <- create_visualizer(viz_fun)
    visualizer4 <- create_visualizer(viz_fun)

    experiment <- create_experiment(name = "test-doc-options-2") |>
      add_dgp(dgp, "DGP") |>
      add_method(method, "Method") |>
      add_evaluator(evaluator1, "Evaluator (digits = 2)") |>
      add_evaluator(evaluator2, "Evaluator (digits = 3)") |>
      add_evaluator(evaluator3, "Evaluator (digits = 4)") |>
      add_evaluator(evaluator4, "Evaluator (no show)") |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (digits = 4)",
                      digits = 4) |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (no show)",
                      show = FALSE) |>
      add_visualizer(visualizer1, "Visualizer (height = 6)") |>
      add_visualizer(visualizer2, "Visualizer (height = 3)") |>
      add_visualizer(visualizer3, "Visualizer (height = 9)") |>
      add_visualizer(visualizer4, "Visualizer (no show)") |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (height = 9)",
                      height = 9) |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (no show)",
                      show = FALSE) |>
      add_vary_across(.dgp = "DGP", x = 1:3)
    results <- run_experiment(experiment, save = TRUE, verbose = 0)
    expect_error(render_docs(experiment, verbose = 0), NA)
    expect_snapshot(render_docs(experiment), transform = remove_tempdir)

    evaluator1 <- create_evaluator(eval_fun)
    evaluator2 <- create_evaluator(eval_fun, .doc_options = list(digits = 3))
    evaluator3 <- create_evaluator(eval_fun)
    evaluator4 <- create_evaluator(eval_fun)
    visualizer1 <- create_visualizer(viz_fun)
    visualizer2 <- create_visualizer(viz_fun, .doc_options = list(height = 3))
    visualizer3 <- create_visualizer(viz_fun)
    visualizer4 <- create_visualizer(viz_fun)

    experiment <- create_experiment(name = "test-doc-options-3") |>
      add_dgp(dgp, "DGP") |>
      add_method(method, "Method") |>
      add_evaluator(evaluator1, "Evaluator (digits = 2)") |>
      add_evaluator(evaluator2, "Evaluator (digits = 3)") |>
      add_evaluator(evaluator3, "Evaluator (digits = 4)") |>
      add_evaluator(evaluator4, "Evaluator (no show)") |>
      add_visualizer(visualizer1, "Visualizer (height = 6)") |>
      add_visualizer(visualizer2, "Visualizer (height = 3)") |>
      add_visualizer(visualizer3, "Visualizer (height = 9)") |>
      add_visualizer(visualizer4, "Visualizer (no show)")
    results <- run_experiment(experiment, save = TRUE, verbose = 0)

    experiment <- experiment |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (digits = 4)",
                      digits = 4) |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (no show)",
                      show = FALSE) |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (height = 9)",
                      height = 9) |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (no show)",
                      show = FALSE)

    expect_error(render_docs(experiment, verbose = 0), NA)
    expect_snapshot(render_docs(experiment), transform = remove_tempdir)
    save_experiment(experiment)
    expect_error(render_docs(experiment, verbose = 0), NA)
    expect_snapshot(render_docs(experiment), transform = remove_tempdir)

    evaluator1 <- create_evaluator(eval_fun)
    evaluator2 <- create_evaluator(eval_fun, .doc_options = list(digits = 3))
    evaluator3 <- create_evaluator(eval_fun)
    evaluator4 <- create_evaluator(eval_fun)
    visualizer1 <- create_visualizer(viz_fun)
    visualizer2 <- create_visualizer(viz_fun, .doc_options = list(height = 3))
    visualizer3 <- create_visualizer(viz_fun)
    visualizer4 <- create_visualizer(viz_fun)

    experiment <- create_experiment(name = "test-doc-options-4") |>
      add_dgp(dgp, "DGP") |>
      add_method(method, "Method") |>
      add_evaluator(evaluator1, "Evaluator (digits = 2)") |>
      add_evaluator(evaluator2, "Evaluator (digits = 3)") |>
      add_evaluator(evaluator3, "Evaluator (digits = 4)") |>
      add_evaluator(evaluator4, "Evaluator (no show)") |>
      add_visualizer(visualizer1, "Visualizer (height = 6)") |>
      add_visualizer(visualizer2, "Visualizer (height = 3)") |>
      add_visualizer(visualizer3, "Visualizer (height = 9)") |>
      add_visualizer(visualizer4, "Visualizer (no show)") |>
      add_vary_across(.dgp = "DGP", x = 1:3)
    results <- run_experiment(experiment, save = TRUE, verbose = 0)

    experiment <- experiment |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (digits = 4)",
                      digits = 4) |>
      set_doc_options(field_name = "evaluator", name = "Evaluator (no show)",
                      show = FALSE) |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (height = 9)",
                      height = 9) |>
      set_doc_options(field_name = "visualizer", name = "Visualizer (no show)",
                      show = FALSE)

    expect_error(render_docs(experiment, verbose = 0), NA)
    expect_snapshot(render_docs(experiment), transform = remove_tempdir)
    save_experiment(experiment)
    expect_error(render_docs(experiment, verbose = 0), NA)
    expect_snapshot(render_docs(experiment), transform = remove_tempdir)
  })

}) # withr::with_tempdir(pattern = "simChef-test-checkpointing-temp", code = {
