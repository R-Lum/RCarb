test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load Example dataset
  data("Example_Data", envir = environment())

  ## crash function with wrong times settings
  expect_error(RCarb:::.calc_DoseRate(
    x = 10,
    data = Example_Data[14, ],
    ref = NULL,
    max_time = 10
  ),
  regexp = "\\[\\.calc_DoseRate\\(\\)\\] max\\_time setting too small!")

  ##simple run to check the 'ref' fall back
  expect_type(RCarb:::.calc_DoseRate(x = 10, data = Example_Data[14, ], ref = NULL),
              type = "list")

})