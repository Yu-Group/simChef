test_that("pretty_kable works", {
  data(iris)
  
  expect_error(pretty_kable())
  
  ## Show iris data
  expect_snapshot_output(pretty_kable(iris, return_df = TRUE)$df)
  expect_s3_class(pretty_kable(iris), "knitr_kable")
  
  ## Show iris data table with caption
  expect_snapshot_output(
    pretty_kable(iris, align = "c", caption = "'Iris Data Table'", 
                return_df = TRUE)$df
  )
  expect_s3_class(
    pretty_kable(iris, align = "c", caption = "'Iris Data Table'"),
    "knitr_kable"
  )

  ## Bold max value of each numeric column of Iris data in red
  expect_snapshot_output(
    pretty_kable(iris, caption = "'Iris Data Table'", scroll = TRUE,
                bold_function = ". == max(.)", bold_margin = 2,
                bold_scheme = c(T, T, T, T, F), bold_color = "red",
                return_df = TRUE)$df
  )
  expect_s3_class(
    pretty_kable(iris, caption = "'Iris Data Table'", scroll = TRUE,
                bold_function = ". == max(.)", bold_margin = 2,
                bold_scheme = c(T, T, T, T, F), bold_color = "red"),
    "knitr_kable"
  )

  ## Bold min value of each row in Iris data
  expect_snapshot_output(
    pretty_kable(iris %>% dplyr::select(-Species), sigfig = T,
                caption = "'Iris Data Table'", format = "latex",
                scroll = T, na_disp = "NA",
                bold_function = ". == min(.)", bold_margin = 1,
                bold_scheme = T, bold_color = "black",
                return_df = TRUE)$df
  )
  expect_s3_class(
    pretty_kable(iris %>% dplyr::select(-Species), sigfig = T,
                caption = "'Iris Data Table'", format = "latex",
                scroll = T, na_disp = "NA",
                bold_function = ". == min(.)", bold_margin = 1,
                bold_scheme = T, bold_color = "black"),
    "knitr_kable"
  )
})


test_that("pretty_DT works", {
  data(iris)
  
  expect_error(pretty_DT())
  
  ## Show iris data
  expect_snapshot_output(pretty_DT(iris, return_df = TRUE)$df)
  expect_s3_class(pretty_DT(iris), "datatables")
  
  ## Show iris data table with caption
  expect_snapshot_output(
    pretty_DT(iris, caption = "'Iris Data Table'", return_df = TRUE)$df
  )
  expect_s3_class(
    pretty_DT(iris, caption = "'Iris Data Table'"),
    "datatables"
  )
  
  ## Bold max value of each numeric column of Iris data in red
  expect_snapshot_output(
    pretty_DT(iris, caption = "'Iris Data Table'",
             bold_function = ". == max(.)", bold_margin = 2,
             bold_scheme = c(T, T, T, T, F), bold_color = "red",
             return_df = TRUE)$df
  )
  expect_s3_class(
    pretty_DT(iris, caption = "'Iris Data Table'",
             bold_function = ". == max(.)", bold_margin = 2,
             bold_scheme = c(T, T, T, T, F), bold_color = "red"),
    "datatables"
  )
  
  ## Bold min value of each row in Iris data
  expect_snapshot_output(
    pretty_DT(iris %>% dplyr::select(-Species),
             sigfig = T, caption = "'Iris Data Table'",
             na_disp = "NA", bold_function = ". == min(.)", bold_margin = 1,
             bold_scheme = T, bold_color = "black",
             return_df = TRUE)$df
  )
  expect_s3_class(
    pretty_DT(iris %>% dplyr::select(-Species),
             sigfig = T, caption = "'Iris Data Table'",
             na_disp = "NA", bold_function = ". == min(.)", bold_margin = 1,
             bold_scheme = T, bold_color = "black"),
    "datatables"
  )
})