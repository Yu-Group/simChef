test_that("prettyKable works", {
  data(iris)
  
  expect_error(prettyKable())
  
  ## Show iris data
  expect_snapshot_output(prettyKable(iris, return_df = TRUE)$df)
  expect_s3_class(prettyKable(iris), "knitr_kable")
  
  ## Show iris data table with caption
  expect_snapshot_output(
    prettyKable(iris, align = "c", caption = "'Iris Data Table'", 
                return_df = TRUE)$df
  )
  expect_s3_class(
    prettyKable(iris, align = "c", caption = "'Iris Data Table'"),
    "knitr_kable"
  )

  ## Bold max value of each numeric column of Iris data in red
  expect_snapshot_output(
    prettyKable(iris, caption = "'Iris Data Table'", scroll = TRUE,
                bold_function = ". == max(.)", bold_margin = 2,
                bold_scheme = c(T, T, T, T, F), bold_color = "red",
                return_df = TRUE)$df
  )
  expect_s3_class(
    prettyKable(iris, caption = "'Iris Data Table'", scroll = TRUE,
                bold_function = ". == max(.)", bold_margin = 2,
                bold_scheme = c(T, T, T, T, F), bold_color = "red"),
    "knitr_kable"
  )

  ## Bold min value of each row in Iris data
  expect_snapshot_output(
    prettyKable(iris %>% dplyr::select(-Species), sigfig = T,
                caption = "'Iris Data Table'", format = "latex",
                scroll = T, na_disp = "NA",
                bold_function = ". == min(.)", bold_margin = 1,
                bold_scheme = T, bold_color = "black",
                return_df = TRUE)$df
  )
  expect_s3_class(
    prettyKable(iris %>% dplyr::select(-Species), sigfig = T,
                caption = "'Iris Data Table'", format = "latex",
                scroll = T, na_disp = "NA",
                bold_function = ". == min(.)", bold_margin = 1,
                bold_scheme = T, bold_color = "black"),
    "knitr_kable"
  )
})


test_that("prettyDT works", {
  data(iris)
  
  expect_error(prettyDT())
  
  ## Show iris data
  expect_snapshot_output(prettyDT(iris, return_df = TRUE)$df)
  expect_s3_class(prettyDT(iris), "datatables")
  
  ## Show iris data table with caption
  expect_snapshot_output(
    prettyDT(iris, caption = "'Iris Data Table'", return_df = TRUE)$df
  )
  expect_s3_class(
    prettyDT(iris, caption = "'Iris Data Table'"),
    "datatables"
  )
  
  ## Bold max value of each numeric column of Iris data in red
  expect_snapshot_output(
    prettyDT(iris, caption = "'Iris Data Table'",
             bold_function = ". == max(.)", bold_margin = 2,
             bold_scheme = c(T, T, T, T, F), bold_color = "red",
             return_df = TRUE)$df
  )
  expect_s3_class(
    prettyDT(iris, caption = "'Iris Data Table'",
             bold_function = ". == max(.)", bold_margin = 2,
             bold_scheme = c(T, T, T, T, F), bold_color = "red"),
    "datatables"
  )
  
  ## Bold min value of each row in Iris data
  expect_snapshot_output(
    prettyDT(iris %>% dplyr::select(-Species),
             sigfig = T, caption = "'Iris Data Table'",
             na_disp = "NA", bold_function = ". == min(.)", bold_margin = 1,
             bold_scheme = T, bold_color = "black",
             return_df = TRUE)$df
  )
  expect_s3_class(
    prettyDT(iris %>% dplyr::select(-Species),
             sigfig = T, caption = "'Iris Data Table'",
             na_disp = "NA", bold_function = ". == min(.)", bold_margin = 1,
             bold_scheme = T, bold_color = "black"),
    "datatables"
  )
})