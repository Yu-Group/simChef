test_that("pasteMd works properly", {

  expect_error(pasteMd())
  expect_warning(expect_error(pasteMd("path/to/file")))

  filename <- file.path("md", "test_doc.md")
  out <- pasteMd(filename)
  expect_snapshot_output(cat(out))
  expect_equal(class(out), "character")
  expect_equal(length(out), 1)
})
