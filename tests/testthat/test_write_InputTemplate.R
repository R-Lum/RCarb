test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##simple run
  expect_type(write_InputTemplate(), type = "list")

  ##write on template folder
  expect_silent(write_InputTemplate(file = tempfile()))


})