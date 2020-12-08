test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load Example dataset
  data("Example_Data", envir = environment())

  ##break function
  expect_error(model_DoseRate(data = "test"), regexp = "'data' is not a 'data.frame'")
  expect_error(model_DoseRate(data = data.frame(x = 1)), regexp = "The column names of your input data.frame do not match the requirements.")
  expect_error(model_DoseRate(data = data.frame()), regexp = "'data' is empty!")
  expect_error(suppressWarnings(model_DoseRate(data = data.frame(x = NA)), regexp = "'data' is empty!"))
  expect_error(
    suppressWarnings(model_DoseRate(data = Example_Data[23,], n.MC = 10)),
    regexp = "Modelling failed, please check your input data, they may not be meaningful!")
  expect_type(suppressWarnings(model_DoseRate(
    data = Example_Data[23:24, ],
    n.MC = 10,
    verbose = FALSE
  )), type = "list")

  expect_error(suppressWarnings(model_DoseRate(data = Example_Data[14,],
                             DR_conv_factors = "error",
                             n.MC = 2,
                             txtProgressBar = FALSE
  )), regexp = "'error' does not correspond to an available dose rate conversion dataset.\n        Allowed are: Carb2007, Adamiec_Aitken_1998, Guerin_et_al_2011, Liritzis_et_al_2013")


  ##run simple example
  expect_type(suppressWarnings(model_DoseRate(data = Example_Data[14,],
    n.MC = 2,
    txtProgressBar = FALSE
  )), type = "list")

  ##run with different conversion factors
  expect_type(
    suppressWarnings(model_DoseRate(
      data = Example_Data[14, ],
      DR_conv_factors = "Adamiec_Aitken_1998",
      n.MC = 2,
      txtProgressBar = FALSE
    )),
    type = "list"
  )

  ##run extreme case warning
  temp <- Example_Data[14, ]
  temp$DE <- 550
  ##TODO the warning appears 16 times ... to much
  # expect_warning(
  #   model_DoseRate(
  #     data = temp,
  #     n.MC = 2,
  #     txtProgressBar = FALSE
  #   ),
  #   regexp = "\\[\\.calc_DoseRate\\(\\)\\] Extrem case detected: DE > max cumulative dose rate!"
  # )

  ##run two example
  expect_type(model_DoseRate(
    data = Example_Data[13:14, ],
    n.MC = 5,
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
