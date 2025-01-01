withr::with_tempdir(pattern = "simChef-test-use-templates-temp", code = {

  test_that("use_*_template() works", {

    modify_template <- function(template_str) {
      modified_template_str <- template_str |>
        stringr::str_replace(pattern = "n_reps = stop\\(.*\\)",
                             replace = "n_reps = 2")
      return(modified_template_str)
    }

    expect_snapshot_output(use_prediction_template(type = "regression"))
    expect_snapshot_output(use_prediction_template(type = "classification"))
    expect_snapshot_output(use_prediction_template(type = "regression",
                                                   support = TRUE))
    expect_snapshot_output(use_prediction_template(type = "classification",
                                                   support = TRUE))

    out1 <- capture_output(
      use_prediction_template(type = "regression",
                              include_dgp_example = TRUE,
                              include_method_example = TRUE)
    )
    expect_error(eval(parse(text = out1)))
    expect_no_error(suppressMessages(eval(parse(text = modify_template(out1)))))

    out2 <- capture_output(
      use_prediction_template(type = "regression",
                              include_dgp_example = TRUE,
                              include_method_example = TRUE,
                              support = TRUE)
    )
    expect_error(eval(parse(text = out2)))
    expect_no_error(suppressMessages(eval(parse(text = modify_template(out2)))))

    out3 <- capture_output(
      use_prediction_template(type = "classification",
                              include_dgp_example = TRUE,
                              include_method_example = TRUE)
    )
    expect_error(eval(parse(text = out3)))
    expect_no_error(suppressMessages(eval(parse(text = modify_template(out3)))))

    out4 <- capture_output(
      use_prediction_template(type = "classification",
                              include_dgp_example = TRUE,
                              include_method_example = TRUE,
                              support = TRUE)
    )
    expect_error(eval(parse(text = out4)))
    expect_no_error(suppressMessages(eval(parse(text = modify_template(out4)))))

    expect_snapshot_output(use_feature_selection_template())
    out5 <- capture_output(
      use_feature_selection_template(include_dgp_example = TRUE,
                                     include_method_example = TRUE)
    )
    expect_error(eval(parse(text = out5)))
    expect_no_error(suppressMessages(eval(parse(text = modify_template(out5)))))

    expect_snapshot_output(use_inference_template())
    out6 <- capture_output(
      use_inference_template(include_dgp_example = TRUE,
                             include_method_example = TRUE)
    )
    expect_error(eval(parse(text = out6)))
    expect_no_error(suppressMessages(eval(parse(text = modify_template(out6)))))
  })

})

