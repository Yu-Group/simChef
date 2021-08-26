test_that("Experiment initialization works", {

  exper <- Experiment$new(n_reps = 1)
  expect_equal(exper$n_reps, 1)
  expect_equal(exper$name, "experiment")

  exper <- Experiment$new(n_reps = 10, name = "exper_name")
  expect_equal(exper$n_reps, 10)
  expect_equal(exper$name, "exper_name")

})

test_that("Experiment initialization with create_experiment works", {
  exper <- create_experiment(n_reps = 1)
  expect_equal(exper$n_reps, 1)
  expect_equal(exper$name, "experiment")

  exper <- create_experiment(n_reps = 10, name = "exper_name")
  expect_equal(exper$n_reps, 10)
  expect_equal(exper$name, "exper_name")
})
