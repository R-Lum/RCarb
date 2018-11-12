context("Test model_DoseRate()")

test_that("Full function test", {
  testthat::skip_on_cran()

  ##load Example dataset
  data("Example_Data", envir = environment())

  ##break function
  expect_error(model_DoseRate(data = "test"), regexp = "'data' is not a 'data.frame'")
  expect_error(model_DoseRate(data = data.frame(x = 1)), regexp = "The column names of your input data.frame do not match the requirements.")
  ##expect_warning(model_DoseRate(data = Example_Data[23,], n.MC = 10),regexp = "Extrem case: DE > max cumulative dose rate!")

  ##run simple example
  expect_type(model_DoseRate(data = Example_Data[14,],
    n.MC = 2,
    txtProgressBar = FALSE
  ), type = "list")

  ##run two example
  expect_type(model_DoseRate(
    data = Example_Data[13:14, ],
    n.MC = 2,
    txtProgressBar = TRUE
  ),
  type = "list")

  ##run with n.MC == 1
  expect_type(model_DoseRate(
    data = Example_Data[14, ],
    n.MC = 1,
    txtProgressBar = FALSE
  ),
  type = "list")



})