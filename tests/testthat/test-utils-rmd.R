test_that("pasteMd works properly", {

  skip("not used? remove?")

  expect_error(pasteMd())
  expect_error(pasteMd("path/to/file"))

  filename <- file.path("..", "md", "test_doc.md")
  expect_snapshot_output(filename)
})
