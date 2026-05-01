test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##simple run
  expect_type(write_InputTemplate(), type = "list")

  ##write on template folder
  expect_silent(write_InputTemplate(file = tempfile()))

  ##test nrows
  expect_equal(nrow(write_InputTemplate(nrows = 10)), 10)
  expect_equal(nrow(write_InputTemplate(nrows = "number")), 1)
  expect_equal(nrow(write_InputTemplate(nrows = TRUE)), 1)
  expect_equal(nrow(write_InputTemplate(nrows = 1.2)), 1)
  expect_equal(nrow(write_InputTemplate(nrows = -1.2)), 1)

})