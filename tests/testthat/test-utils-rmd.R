test_that("paste_md works properly", {

  expect_error(paste_md())
  expect_warning(expect_error(paste_md("path/to/file")))

  filename <- file.path("md", "test_doc.md")
  out <- paste_md(filename)
  expect_snapshot_output(cat(out))
  expect_equal(class(out), "character")
  expect_equal(length(out), 1)
})
