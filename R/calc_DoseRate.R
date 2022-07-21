#' @title Internal function to calculate date for a given maximum time
#'
#' @description This is the real worker function based on the *Carb* MATLAB function `'datalu1.m'`. To
#' avoid overhead, the function does not double check the input parameters and is thus not exposed
#' and it does not appear in the manual
#'
#' @param x [numeric] (**required**): time for which the scenario is calculated
#'
#' @param data [data.frame] (**required**): input data, only a data.frame is allowed
#'
#' @param DR_conv_factors [character] (*optional*): select dose rate conversion factors, `NULL`
#' uses the original *Carb* values
#'
#' @param ref [list] (**optional**): list of reference data provided with this package, if nothing
#' is provided the function will load package reference data via `data("Reference_Data", envir = environment())`.
#'
#' @param length_step [numeric] (with default): step length used for the calculation
#'
#' @param max_time [numeric] (with default): maximum time span to be evaluated
#'
#' @param mode_optim [logical] (with default): switch output mode, if `TRUE` only a scalar is returned,
#' this reduces the used memory consumption for the calculation
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) based
#' on the MATLAB code in the file `'datalu1.m'` from `Carb_2007a`
#'
#' @section Function version: 0.2.0
#'
#' @example
#' ## the function can be run manually, however, this is not recommended, example:
#' data("Example_Data", envir = environment())
#' RCarb:::.calc_DoseRate(
#' x = 10,
#' data = Example_Data[14,]
#' )
#'
#' @md
#' @noRd
.calc_DoseRate <- function(
  x,
  data,
  DR_conv_factors = NULL,
  ref = NULL,
  length_step = 1,
  max_time = 500,
  mode_optim = FALSE
  ){

  ## load reference data if needed; we should try to load them externally before,
  ## however if this function is run internally for tests we want to have
  ## the comfort
  if(is.null(ref)){
    Reference_Data <- NULL
    data("Reference_Data", envir = environment())
    ref <- Reference_Data
    rm(Reference_Data)

  }

  ##rewrite variables, to keep the naming consistent with the original MATLAB code
  TIMEMAX <- x
  STEP1 <- length_step[1]

  ##initialise dose rate conversion factors (error catching we do a level up)
  if(is.null(DR_conv_factors)){
   DR_ID <- 1

  }else{
   DR_ID <- grep(
     x = ref[["DR_conv_factors"]][["REFERENCE"]], pattern = DR_conv_factors, fixed = TRUE)

  }

  handles.UB <- ref[["DR_conv_factors"]][["UB"]][DR_ID]
  handles.TB <- ref[["DR_conv_factors"]][["TB"]][DR_ID]
  handles.KB <- ref[["DR_conv_factors"]][["KB"]][DR_ID]
  handles.UG <- ref[["DR_conv_factors"]][["UG"]][DR_ID]
  handles.TG <- ref[["DR_conv_factors"]][["TG"]][DR_ID]
  handles.KG <- ref[["DR_conv_factors"]][["KG"]][DR_ID]

# STEP 1: Find U, Th, K values for the sediment -----------------------------------------------

  ##transfer U, Th, and K value
  KA <- data[["K"]]
  UA <- data[["U"]]
  TA <- data[["T"]]

  K <- KA * (1 + data[["CC"]] / 100)
  U <- UA * (1 + data[["CC"]] / 100)
  T <- TA * (1 + data[["CC"]] / 100)

# STEP 2: Derive carbonate model (daterlu1.m) -------------------------------------------------

  ##2.1. - create vectors with variables, the maxium length is determined by max_time
  ##C >> CARBONATE
  C <- c(
    rep(1, data[["FINISH"]] / STEP1) * data[["CC"]] / 100,
    seq(data[["CC"]] / 100, 1e-05, length.out = (data[["ONSET"]] - data[["FINISH"]]) / STEP1 + 1),
    rep(0, (max_time - data[["ONSET"]]) / STEP1) + 1e-05
  )

  ##WC >> WATER CONTENT
  WC <- c(
    rep(1, data[["FINISH"]] / STEP1) * data[["WCF"]] / 100,
    seq(data[["WCF"]] / 100, data[["WCI"]] / 100, length.out = (data[["ONSET"]] - data[["FINISH"]]) / STEP1 + 1),
    rep(0, (max_time - data[["ONSET"]]) / STEP1) + data[["WCI"]] / 100
  )

  ##2.2. - create additional objects
  WF <- C + WC
  WFA <- data[["WCF"]] / 100
  LEN <- length(C)

  ##2.3 - create time matrix
  TIME <- matrix(seq(0, max_time, STEP1), nrow = 1)
  TIME_ <- seq(0,round(TIMEMAX * STEP1)/STEP1)
  TIME_ <- c(rep(0,max_time - max(TIME_)), TIME_)

# STEP 3: Derive linear uptake mode of uranium (code from Forbes Quarry work) (daterlu1.m) ---------
  lam_u235 <- log(2) / 703800000
  lam_u238 <- log(2) / 4.4680e+09

  ##combine in matrix
  Aa <- .rad_pop_LU(data[["U234_U238"]], TIME_)

  ##flip matrix row order
  Aa <- apply(Aa, 2, rev)

  ##note: the MATLAB code now throws all variables,
  ##namely, "N_u238", "N_u234", "N_t230", "N_u235", "N_p231"
  ##Of course we do not do this here, since this is an R package and the last thing
  ##we want to have is a package writting unexpectedly into the Global Environment
  ##In the following the objects are accessed by accessing the matrix

  ## Parent activity (Bq/mg) ... from A&A (1998)
  A_u238 <- lam_u238 * 6.022e+023 * 1e-3 / (238 * 31.56e+06)
  A_u235 <- lam_u235 * 6.022e+23 * 1e-03 / (235 * 31.56e+06)

  ## dose rate per ppm of parent
  conv_const <- 5.056e-03
  CONST_Q_U238 <- 0.9927
  CONST_Q_U235 <- 1 - CONST_Q_U238

  D_b_u238 <- conv_const * A_u238 * 0.8860 * CONST_Q_U238
  D_b_u234 <- conv_const * A_u238 * 0.0120 * CONST_Q_U238
  D_b_t230 <- conv_const * A_u238 * 1.3850 * CONST_Q_U238
  D_b_u235 <- conv_const * A_u235 * 0.1860 * CONST_Q_U235
  D_g_u238 <- conv_const * A_u238 * 0.0290 * CONST_Q_U238
  D_g_u234 <- conv_const * A_u238 * 0.0020 * CONST_Q_U238
  D_g_t230 <- conv_const * A_u238 * 1.7430 * CONST_Q_U238

  ## i-m dose rate - no grain size correction
  U238_b_diseq <- D_b_u238 * Aa[,"N_u238"] * data[["U238"]]
  U234_b_diseq <- D_b_u234 * Aa[,"N_u234"] * data[["U238"]]
  T230_b_diseq <- D_b_t230 * Aa[,"N_t230"] * data[["U238"]]
  U238_g_diseq <- D_g_u238 * Aa[,"N_u238"] * data[["U238"]]
  U234_g_diseq <- D_g_u234 * Aa[,"N_u234"] * data[["U238"]]
  T230_g_diseq <- D_g_t230 * Aa[,"N_t230"] * data[["U238"]]

# STEP 4: Calculate time-dependent dose rate --------------------------------------------------

  ##this step uses the data by Mejdahl and interpolates the value for the given diamter
  MK <- 1 - exp(
    approx(x = log(ref$mejdahl[[1]]), y = log(ref$mejdahl[[2]]), xout = log(data[["DIAM"]]/1000),
           rule = 2)$y)
  MT <- 1 - exp(
    approx(x = log(ref$mejdahl[[1]]), y = log(ref$mejdahl[[3]]), xout = log(data[["DIAM"]]/1000),
           rule = 2)$y)
  MU <- 1 - exp(
    approx(x = log(ref$mejdahl[[1]]), y = log(ref$mejdahl[[4]]), xout = log(data[["DIAM"]]/1000),
           rule = 2)$y)

  MU238 <- MU234 <- MT230 <- MU235 <- MP231 <- 1

  ##prepare data, to avoid that this is called over and over again when .griddata is called
  x_grid <- as.numeric(colnames(ref$DATAek))
  y_grid <- x_grid

  x_grid <- log(rep(x_grid, length(x_grid)))
  y_grid <- log(rep(y_grid, each = length(y_grid)))
  xo <- log(WC)
  yo <- log(C)

  ##run surface interpolation calculations to get the
  ##water correction factors
  ##nomenclature:
  ## ++++++++++++++++ RCarb calculation ++++++++++++++++++++++++

    ##beta water correction factors
    XKB <- .griddata(x_grid, y_grid, ref$DATAek, xo, yo)
    XTB <- .griddata(x_grid, y_grid, ref$DATAet, xo, yo)
    XUB <- .griddata(x_grid, y_grid, ref$DATAeu, xo, yo)

    XU238B <- .griddata(x_grid, y_grid, ref$DATAeu238, xo, yo)
    XU234B <- .griddata(x_grid, y_grid, ref$DATAeu234, xo, yo)
    XT230B <- .griddata(x_grid, y_grid, ref$DATAet230, xo, yo)

    ##gamma water correction factors
    XKG <- .griddata(x_grid, y_grid, ref$DATApk, xo, yo)
    XTG <- .griddata(x_grid, y_grid, ref$DATApt, xo, yo)
    XUG <- .griddata(x_grid, y_grid, ref$DATApu, xo, yo)

    XU238G <- .griddata(x_grid, y_grid, ref$DATApu238, xo, yo)
    XU234G <- .griddata(x_grid, y_grid, ref$DATApu234, xo, yo)
    XT230G <- .griddata(x_grid, y_grid, ref$DATApt230, xo, yo)

  ##perform dose rate correction with the before set factors
  ##beta water correction factors
  DRKB <- MK * K * handles.KB / (1 + XKB * WF)
  DRTB <- MT * T * handles.TB / (1 + XTB * WF)
  DRUB <- MU * U * handles.UB / (1 + XUB * WF)
  DRU238B <- MU238 * U238_b_diseq / (1 + XU238B * WF)
  DRU234B <- MU238 * U234_b_diseq / (1 + XU234B * WF)
  DRT230B <- MU238 * T230_b_diseq / (1 + XT230B * WF)

  ##gamma water corrections factors
  DRKG <- MK * K * handles.KG / (1 + XKG * WF)
  DRTG <- MT * T * handles.TG / (1 + XTG * WF)
  DRUG <- MU * U * handles.UG / (1 + XUG * WF)
  DRU238G <- MU238 * U238_g_diseq / (1 + XU238G * WF)
  DRU234G <- MU238 * U234_g_diseq / (1 + XU234G * WF)
  DRT230G <- MU238 * T230_g_diseq / (1 + XT230G * WF)


  ##calculate final dose rate
  DR <-
    DRKB + DRTB + DRUB + DRKG + DRTG + DRUG + data[["COSMIC"]] +
    data[["INTERNAL"]] + DRU238B + DRU234B + DRT230B + DRU238G + DRU234G + DRT230G


  ## ++++++++++++++++ Conventinal calculation ++++++++++++++++++++++++
  ##set alternative, the commonly used water correction factors
  ##after Zimmerman, 1971 (Archaeometry); cf. also Aitken (1985)
  ##after no interpolation is needed since these factors remain constant
  XKBA <- XTBA <- XUBA <- XU238BA <- XU234BA <- XT230BA <- 1.25
  XKGA <- XTGA <- XUGA <- XU238GA <- XU234GA <- XT230GA <- 1.14

  ##beta
  DRKBA <- MK * KA * handles.KB / (1 + XKBA * WFA)
  DRTBA <- MT * TA * handles.TB / (1 + XTBA * WFA)
  DRUBA <- MU * UA * handles.UB / (1 + XUBA * WFA)
  DRU238BA <- MU238 * U238_b_diseq / (1 + XU238BA * WFA)
  DRU234BA <- MU238 * U234_b_diseq / (1 + XU234BA * WFA)
  DRT230BA <- MU238 * T230_b_diseq / (1 + XT230BA * WFA)

  ##gamma
  DRKGA <- MK * KA * handles.KG / (1 + XKGA * WFA)
  DRTGA <- MT * TA * handles.TG / (1 + XTGA * WFA)
  DRUGA <- MU * UA * handles.UG / (1 + XUGA * WFA)
  DRU238GA <- MU238 * U238_g_diseq / (1 + XU238GA * WFA)
  DRU234GA <- MU238 * U234_g_diseq / (1 + XU234GA * WFA)
  DRT230GA <- MU238 * T230_g_diseq / (1 + XT230GA * WFA)

  ##calculate alternative (conventional) dose rate
  DRA <-
    DRKBA + DRTBA + DRUBA + DRKGA + DRTGA + DRUGA + data[["COSMIC"]] + data[["INTERNAL"]] +
    DRU238BA + DRU234BA + DRT230BA + DRU238GA + DRU234GA + DRT230GA

  ##replace NA values by 0, otherwise the function crashes
  ##The NA values come from the surface (line 175) interpolation and replacing them by 0
  ##does not affect the results, but allows the code to run without taking other precautions.
  DR[is.na(DR)] <- 0
  DRA[is.na(DRA)] <- 0

# STEP 5: Calculate date ----------------------------------------------------------------------

  ##calculate the cummulated absorbed dose
  ##Is this correct, starting from index 1? Yes, it is, we have set the maximum manually to
  ##500, if we would reverse the vector, we would pretend that we know the age of the sample
  ##already, but then we would not need this modelling.
  CUMDR <- cumsum(c(0, (DR[1:(length(DR) - 1)] + DR[2:length(DR)]) * STEP1 / 2))
  CUMDRA <- cumsum(c(0, (DRA[1:(length(DRA) - 1)] + DRA[2:length(DRA)]) * STEP1 / 2))

  if(data[["DE"]] > max(CUMDR)) ##modified before it was Age > 500 ka, which did not fit.
      warning("[.calc_DoseRate()] Extrem case detected: DE > max cumulative dose rate!", call. = FALSE)

  ##RCarb ages
  AGE <- try(
    approx(x = CUMDR, y = as.numeric(TIME), xout = data[["DE"]], method = "linear", rule = 2)$y,
    silent = TRUE)

  ##conventional age
  AGEA <- try(
    approx(x = CUMDRA, y = as.numeric(TIME), xout = data[["DE"]], method = "linear", rule = 2)$y,
    silent = TRUE)

  ##sometimes the input values are not meaningful (for example data row 23 in the example dataset)
  ##and the approximation failed,
  ##we here provide a clean crash
  if(inherits(AGE, 'try-error') || inherits(AGEA, 'try-error'))
    stop("[.calc_DoseRate()] Modelling failed, please check your input data, they may not be meaningful!",
         call. = FALSE)

  ##calculate age
  ABS <- abs(AGE - TIMEMAX)

# RETURN --------------------------------------------------------------------------------------
  if(mode_optim){
    results <- ABS

  }else{
    results <- list(
      ABS = ABS,
      AGE = AGE,
      AGEA = AGEA,
      LEN = LEN,
      DR = DR,
      DRA = DRA,
      CUMDR = CUMDR,
      CUMDRA = CUMDRA
      )

  }

  return(results)
}
