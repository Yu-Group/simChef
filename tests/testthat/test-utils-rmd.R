test_that("subchunkify works properly", {
  data(iris)
  plt <- ggplot2::ggplot(iris) +
    ggplot2::aes(x = Sepal.Length, y = Sepal.Width) +
    ggplot2::geom_point()
  
  expect_error(subchunkify())
  expect_error(subchunkify(plt))
  expect_error(subchunkify(plt, i = 1, caption = "Iris data"))
  
  expect_snapshot_output(
    subchunkify(plt, i = 1)
  )
  expect_snapshot_output(
    subchunkify(plt, i = 2, fig_height = 10, fig_width = 5)
  )
  expect_snapshot_output(
    subchunkify(plt, i = 3, caption = "'Iris data'")
  )
  expect_snapshot_output(
    subchunkify(plt, i = 4, add_class = "new_class")
  )
  expect_snapshot_output(
    subchunkify(plt, i = 5, other_args = "results = 'asis'")
  )
})

test_that("pasteMd works properly", {
  
  expect_error(pasteMd())
  expect_error(pasteMd("path/to/file"))
  
  filename <- file.path("..", "md", "Linear Gaussian DGP.md")
  expect_snapshot_output(filename)
})