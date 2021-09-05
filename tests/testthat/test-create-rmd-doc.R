test_that("Automated R Markdown documentation works properly", {
  skip_on_cran()
  skip_on_ci()
  dgp_fun <- function(x) x + 1
  dgp <- create_dgp(dgp_fun, x = 1)
  method_fun <- function(x) x + 2
  method <- create_method(method_fun)
  eval_fun <- function() "Evaluation."
  evaluator <- create_evaluator(eval_fun)
  plot_fun <- function() {
    ggplot2::ggplot(iris) +
      ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
      ggplot2::geom_point()
  }
  plotter <- create_plotter(plot_fun)
  
  base_experiment <- create_experiment(name = "test-rmd") %>%
    add_dgp(dgp, "DGP") %>%
    add_method(method, "Method") %>%
    add_evaluator(evaluator, "Evaluator") %>%
    add_plotter(plotter, "Plot")
  results <- base_experiment$run(save = TRUE, verbose = 0)
  
  child1 <- create_experiment(
    name = "child1", 
    clone_from = base_experiment,
    save_dir = file.path(base_experiment$get_save_dir(), "child1")
  )
  results <- child1$run(save = TRUE, verbose = 0)
  child2 <- create_experiment(
    name = "child2", 
    clone_from = base_experiment,
    save_dir = file.path(base_experiment$get_save_dir(), "child2")
  )
  results <- child2$run(save = TRUE, verbose = 0)
  
  grandchild1a <- create_experiment(
    name = "grandchild1a", 
    clone_from = child1,
    save_dir = file.path(child1$get_save_dir(), "grandchild1a")
  )
  results <- grandchild1a$run(save = TRUE, verbose = 0)
  
  grandchild1b <- create_experiment(
    name = "grandchild1b", 
    clone_from = child1,
    save_dir = file.path(child1$get_save_dir(), "grandchild1b")
  )
  results <- grandchild1b$run(save = TRUE, verbose = 0)
  
  grandchild2 <- create_experiment(
    name = "grandchild2", 
    clone_from = child2,
    save_dir = file.path(child2$get_save_dir(), "grandchild2")
  )
  results <- grandchild2$run(save = TRUE, verbose = 0)
  
  greatgrandchild2 <- create_experiment(
    name = "greatgrandchild2", 
    clone_from = grandchild2,
    save_dir = file.path(grandchild2$get_save_dir(), "greatgrandchild2")
  )
  results <- greatgrandchild2$run(save = TRUE, verbose = 0)
  
  expect_error(base_experiment$create_rmd(open = FALSE, verbose = 0), NA)
})

test_that("Visualizations in R Markdown documentation render correctly", {
  skip_on_cran()
  skip_on_ci()
  dgp_fun <- function(x) x + 1
  dgp <- create_dgp(dgp_fun, x = 1)
  method_fun <- function(x) x + 2
  method <- create_method(method_fun)
  eval_fun <- function() "Evaluation."
  evaluator <- create_evaluator(eval_fun)
  plot_fun <- function() {
    ggplot2::ggplot(iris) +
      ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
      ggplot2::geom_point()
  }
  plotter <- create_plotter(plot_fun)
  tab_fun <- function() prettyDT(iris)
  tabler <- create_plotter(tab_fun)
  text_fun <- function() "Hello world!"
  texter <- create_plotter(text_fun)
  
  experiment <- create_experiment(name = "test-visualizers") %>%
    add_dgp(dgp, "DGP") %>%
    add_method(method, "Method") %>%
    add_evaluator(evaluator, "Evaluator") %>%
    add_plotter(plotter, "Plot") %>%
    add_plotter(tabler, "Table") %>%
    add_plotter(texter, "Text") 
  results <- run_experiment(experiment, save = TRUE, verbose = 0)
  
  expect_error(experiment$create_rmd(open = FALSE, verbose = 0), NA)
})

test_that("R Markdown options work properly", {
  skip_on_cran()
  skip_on_ci()
  dgp_fun <- function(x) x + 1
  dgp <- create_dgp(dgp_fun, x = 1)
  method_fun <- function(x) x + 2
  method <- create_method(method_fun)
  eval_fun <- function() iris
  evaluator1 <- create_evaluator(eval_fun)
  evaluator2 <- create_evaluator(eval_fun, rmd_options = list(digits = 3))
  evaluator3 <- create_evaluator(eval_fun)
  evaluator4 <- create_evaluator(eval_fun)
  plot_fun <- function() {
    ggplot2::ggplot(iris) +
      ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
      ggplot2::geom_point()
  }
  plotter1 <- create_plotter(plot_fun)
  plotter2 <- create_plotter(plot_fun, rmd_options = list(height = 3))
  plotter3 <- create_plotter(plot_fun)
  plotter4 <- create_plotter(plot_fun)
  
  experiment <- create_experiment(name = "test-rmd-options") %>%
    add_dgp(dgp, "DGP") %>%
    add_method(method, "Method") %>%
    add_evaluator(evaluator1, "Evaluator (digits = 2)") %>%
    add_evaluator(evaluator2, "Evaluator (digits = 3)") %>%
    add_evaluator(evaluator3, "Evaluator (digits = 4)") %>%
    add_evaluator(evaluator4, "Evaluator (no show)") %>%
    set_rmd_options(field_name = "evaluator", name = "Evaluator (digits = 4)", 
                    digits = 4) %>%
    set_rmd_options(field_name = "evaluator", name = "Evaluator (no show)", 
                    show = FALSE) %>%
    add_plotter(plotter1, "Plotter (height = 6)") %>%
    add_plotter(plotter2, "Plotter (height = 3)") %>%
    add_plotter(plotter3, "Plotter (height = 9)") %>%
    add_plotter(plotter4, "Plotter (no show)") %>%
    set_rmd_options(field_name = "plotter", name = "Plotter (height = 9)", 
                    height = 9) %>%
    set_rmd_options(field_name = "plotter", name = "Plotter (no show)", 
                    show = FALSE)
  results <- run_experiment(experiment, save = TRUE, verbose = 0)
  
  expect_error(create_rmd(experiment, open = FALSE, verbose = 0), NA)
  
  expect_equal(purrr::map_lgl(experiment$get_evaluators(), "show"),
               c(T, T, T, F) %>% setNames(names(experiment$get_evaluators())))
  expect_equal(purrr::map_dbl(experiment$get_evaluators(), 
                              ~.x$rmd_options$digits),
               c(2, 3, 4, 2) %>% setNames(names(experiment$get_evaluators())))
  expect_equal(purrr::map_lgl(experiment$get_plotters(), "show"),
               c(T, T, T, F) %>% setNames(names(experiment$get_plotters())))
  expect_equal(purrr::map_dbl(experiment$get_plotters(), 
                              ~.x$rmd_options$height),
               c(6, 3, 9, 6) %>% setNames(names(experiment$get_plotters())))
})