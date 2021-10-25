test_that("use_*_template() works", {
  expect_snapshot_output(use_prediction_template(type = "regression"))
  expect_snapshot_output(use_prediction_template(type = "classification"))
  expect_snapshot_output(use_prediction_template(type = "regression", 
                                                 support = TRUE))
  expect_snapshot_output(use_prediction_template(type = "classification", 
                                                 support = TRUE))
  
  expect_snapshot_output(
    use_prediction_template(type = "regression", 
                            include_dgp_example = TRUE,
                            include_method_example = TRUE,
                            support = TRUE)
  )
  expect_snapshot_output(
    use_prediction_template(type = "classification", 
                            include_dgp_example = TRUE,
                            include_method_example = TRUE,
                            support = TRUE)
  )
  
  expect_snapshot_output(use_feature_selection_template())
  expect_snapshot_output(
    use_feature_selection_template(include_dgp_example = TRUE,
                                   include_method_example = TRUE)
  )
  
  expect_snapshot_output(use_inference_template())
  expect_snapshot_output(
    use_inference_template(include_dgp_example = TRUE,
                           include_method_example = TRUE)
  )
})
