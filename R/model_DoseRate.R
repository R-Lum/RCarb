#' @title Model dose rate evolution in carbonate-rich samples
#'
#' @description This function models the dose rate evolution in carbonate enrich environments. For the
#' calculation internal functions are called.
#'
#' @details This function is the starting point for the dose rate modelling for carbonat enrich
#' environments. It provides basically the same functionality as the original version of 'Carb', i.e.
#' you should be also aware of the limitations of this modelling approach. In particular: The model
#' assumes a linear carbonate mass increase due to post-depositional processes. Please read the
#' references cited blow.\cr
#'
#' **Uncertainty estimation**
#'
#' For estimating the uncertainties, Monte-Carloe (MC) simulation runs are used. For very
#' small values (close to 0) this can, however, lead to edge effects (similar in 'Carb') since
#' values below 0 are set to 0.
#'
#' @param data [data.frame] (**required**): input data following the structure given
#' in the example data set `data(Example_Data)`. The input [data.frame] should have at least
#' one row (i.e. values for one sample). For multiple rows the function is automatically re-called.
#'
#' @param DR_conv_factors [character] (*optional*): applied dose rate conversion factors,
#' allowed input values are `"Carb2007"`, `"Adamiec_Aitken_1998"`, `"Guerin_et_al_2011"`,
#' `"Liritzis_et_al_2013"`. `NULL` triggers the default, which is `"Carb2007"`
#'
#' @param length_step [numeric] (with default): step length used for the calculation
#'
#' @param max_time [numeric] (with default): maximum temporal search range
#'
#' @param n.MC [numeric] (with default): number of Monte Carlo runs used for the error calculation
#'
#' @param method_control (*optional*): additional arguments that can be provided to the control the
#' the modelling. See details for further information.
#'
#' @param txtProgressBar [logical] (with default): enables/disables the `txtProgressBar` for the MC runs
#'
#' @param verbose [logical] (with default): enables/disables verbose mode
#'
#' @param plot [logical] (with default): enables/disables plot output
#'
#' @param ... further arguments passed to the underyling plot functions, see also details for further information. Supported standard arguments are `mfrow`, `xlim`, `xlab`.
#'
#' @return The function returns numerical and graphical output
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#' * A [data.frame] which is the combination of the input and values calculated by this function.
#'
#' -----------------------------------\cr
#' `[ GRAPHICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **Upper plot:** Dose rate evolution over time backwards. The solid black line is the calculation
#' output, the grey shaded area indicates the 2-sigma error margins. The dashed blue line is an indicator
#' of the quality of the error estimations based on Monte Carlo (MC) runs.The closer it follows the
#' black line, the more reliable are the given error margins. \cr
#'
#' **Lower plot:** Totally absorbed dose over time. The plot is an representation of the 'new'
#' age based on the carbonat modelling.
#'
#'
#' @examples
#' ##load example data
#' data("Example_Data", envir = environment())
#'
#' ##run the function for one sample from
#' ##the dataset
#' model_DoseRate(
#' data = Example_Data[14,],
#' n.MC = 2,
#' txtProgressBar = FALSE
#' )
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montagine (France); based
#' on 'MATLAB' code given in file Carb_2007a.m of *Carb*
#'
#' @section Function version: 0.2.1
#'
#' @references
#' Mauz, B., Hoffmann, D., 2014. What to do when carbonate replaced water: Carb, the model for estimating the
#' dose rate of carbonate-rich samples. Ancient TL 32, 24-32. \url{http://ancienttl.org/ATL_32-2_2014/ATL_32-2_Mauz_p24-32.pdf}
#'
#' Nathan, R.P., Mauz, B., 2008. On the dose-rate estimate of carbonate-rich sediments for trapped charge dating.
#' Radiation Measurements 43, 14-25. \doi{10.1016/j.radmeas.2007.12.012} \cr
#'
#' **Further reading**
#'
#' Nathan, R.P., 2010. Numerical modelling of environmental dose rate and its application to trapped-charge dating.
#' DPhil thesis, St Hugh's College, Oxford. \url{https://ora.ox.ac.uk/objects/ora:6421}
#'
#' Zimmerman, D.W., 1971. Thermoluminescent dating using fine grains from pottery.
#' Archaeometry 13, 29–52.\doi{10.1111/j.1475-4754.1971.tb00028.x}
#'
#' @keywords dplot manip
#' @md
#' @export
model_DoseRate <- function(
  data,
  DR_conv_factors = NULL,
  length_step = 1L,
  max_time = 500L,
  n.MC = 100,
  method_control = list(),
  txtProgressBar = TRUE,
  verbose = TRUE,
  plot = TRUE,
  ...
){


# Self-call -----------------------------------------------------------------------------------

  ##we keep it as simple as possible, only a data.frame is allowed, all subsequent tests
  ##are handed over to the code below
  if(class(data) == "data.frame" &&
     nrow(data) > 1) {

    ##split input in a list
    data_list <- split(data, f = 1:nrow(data))

    ##get provided arguments
    ##by using this option we do not have to double check in future whether we missed arguments
    args <- as.list(match.call())

    ##remove first (the function name) and 'data'
    args[[1]] <- NULL
    args$data <- NULL


    ##run function
    results_list <- lapply(data_list, function(x){
      temp <- try(do.call(model_DoseRate, c(list(data = x),args)))

      if(class(temp) == "try-error"){
        try(stop(paste0("[model_DoseRate()] Calculation for sample ", x[[1]], " failed. NULL returned!"),
                 call. = FALSE))
        return(NULL)

      }else{
        return(temp)

      }
    })

    ##remove NULL elements from failed attempts
    results_list <- results_list[!sapply(results_list, is.null)]

    ##combine into one single data.frame
    results <- do.call(rbind, results_list)

    ##return
    return(results)

  }

# Integrity tests -----------------------------------------------------------------------------

  ##NOTE: The integrity tests are done mainly here and not in the function '.calc_DoseRate' to
  ##avoid additional overhead

  ##checks for data
  if (class(data) != "data.frame")
    stop("[model_DoseRate()] 'data' is not a 'data.frame'", call. = FALSE)
  if (nrow(data) == 0)
    stop("[model_DoseRate()] 'data' is empty!", call. = FALSE)

  ##remove NA values
  if (any(is.na(data))) {
    data <- na.exclude(data)
    warning("[model_DoseRate()] 'data' contained NA values; removed.",
            call. = FALSE)

    ##re-check; maybe it is empty
    if (nrow(data) == 0)
      stop("[model_DoseRate()] 'data' is empty!", call. = FALSE)

  }

  ##Now check against the example data
  Example_Data <- NULL
  data("Example_Data", envir = environment())

  ##check columns
  if (ncol(Example_Data) != ncol(data) &&
      !all(colnames(Example_Data) == colnames(data))) {
    stop(
      "[model_DoseRate()] The column names of your input data.frame do not match the requirements.
    Please check the example dataset via 'data(head(Example_Data))' to see how it should look like!",
      call. = FALSE
    )

  }

   ##remove example data
   rm(Example_Data)

   ##check n.MC
   if(is.null(n.MC) || n.MC[1] <= 1){
     n.MC <- 1
     txtProgressBar <- FALSE

   }

# Rewrite variables names to match the MATLAB code --------------------------------------------
  ERROR <- n.MC[1]
  STEP1 <- length_step[1]


# Prepare data --------------------------------------------------------------------------------

  ##load reference data here; we do not provide the option to the user to add it here
  Reference_Data <- NULL
  data("Reference_Data", envir = environment())
  ref <- Reference_Data
  rm(Reference_Data)

  #select dose rate conversion factors and check allowed values
  if(!is.null(DR_conv_factors) && !any(DR_conv_factors %in% ref$DR_conv_factors$REFERENCE)){
    temp_allowed <- paste(ref$DR_conv_factors$REFERENCE, collapse = ", ")
    stop(paste0(
        "[model_DoseRate()] '",
         DR_conv_factors,
         "' does not correspond to an available dose rate conversion dataset.
        Allowed are: ",
        temp_allowed),
      call. = FALSE)
  }

  ##set what is set (but not with the same name here)
  set_DR_conv_factors <- DR_conv_factors

  ##minimise potential user problems
  max_time <- max_time[1]

  ##method control
  method_control_default <- list(
    trace = FALSE,
    lower = 0,
    upper = max_time

  )

  method_control <- modifyList(x = method_control_default, val = method_control)

# Run optimisation ----------------------------------------------------------------------------

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##AGE
  ##find minimum
  DATE <- nlminb(
    start = STEP1,
    objective = .calc_DoseRate,
    control = list(
      trace = method_control$trace),
    lower = method_control$lower,
    upper = method_control$upper,
    data = data,
    ref = ref,
    DR_conv_factors = set_DR_conv_factors,
    length_step = STEP1,
    mode_optim = TRUE
  )

  ##calculate values with minimum value
  DATE <- .calc_DoseRate(
    x = DATE$par,
    data = data,
    ref = ref,
    DR_conv_factors = set_DR_conv_factors,
    length_step = STEP1,
    max_time = max_time
  )

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##UNCERTAINTIES
  ##ceate needed objects according to MATLAB code
  DE_ <- numeric(ERROR)
  DR_ <- matrix(0, nrow = DATE[["LEN"]], ncol = ERROR)
  DRA_ <- matrix(0, nrow = DATE[["LEN"]], ncol = ERROR)
  CUMDR_ <- matrix(0, nrow = DATE[["LEN"]], ncol = ERROR)
  AGE_ <- numeric(ERROR)
  AGEA_ <- numeric(ERROR)

  ##create variables we want to use (we do not do this in the loop as in MATLAB)
  ##TODO: We should evtl. set all values below 0 as NA and remove such lines; this might
  ##be better than setting it to 0. However, for the moment this is precisely what Carb did before
  DE <- data[["DE"]] + rnorm(n.MC) * data[["DE_X"]]
  DE[DE < 0] <- 0

  COSMIC <- data[["COSMIC"]] + rnorm(n.MC) * data[["COSMIC_X"]]
  COSMIC[COSMIC < 0] <- 0

  INTERNAL <- data[["INTERNAL"]] + rnorm(n.MC) * data[["INTERNAL_X"]]
  INTERNAL[INTERNAL < 0] <- 0

  ONSET <- round(data[["ONSET"]] + rnorm(n.MC) * data[["ONSET_X"]],0)
  ONSET[ONSET < 0] <- 0
  ONSET[ONSET > max_time] <- max_time

  FINISH <- round(data[["FINISH"]] + rnorm(n.MC) * data[["FINISH_X"]],0)
  FINISH[FINISH < 0] <- 0
  FINISH[FINISH > ONSET] <- ONSET[FINISH > ONSET]

  DIAM <- data[["DIAM"]] + rnorm(n.MC) * data[["DIAM_X"]]
  DIAM[DIAM < 0] <- 0

  CC <- data[["CC"]] + rnorm(n.MC) * data[["CC_X"]]
  CC[CC < 1e-03] <- 1e-03

  WCF <- data[["WCF"]] + rnorm(n.MC) * data[["WCF_X"]]
  WCF[WCF < 1e-03] <- 1e-03

  WCI <- data[["WCI"]] + rnorm(n.MC) * data[["WCI_X"]]
  WCI[WCI < 1e-03] <- 1e-03

  K <- data[["K"]] + rnorm(n.MC) * data[["K_X"]]
  K[K < 0] <- 0

  U <- data[["U"]] + rnorm(n.MC) * data[["U_X"]]
  U[U < 0] <- 0

  T <- data[["T"]] + rnorm(n.MC) * data[["T_X"]]
  T[T < 0] <- 0

  ##now combine everything in a data.frame, not efficient but
  ##we want to stick as close
  ##as possible with the MATLAB code (the order does not matter)
  data_MC <- cbind(K, T, U,
                   U238 = rep(data[["U238"]],n.MC),
                   U234_U238 = rep(data[["U234_U238"]],n.MC),
                   WCI, WCF, CC, DIAM, COSMIC, INTERNAL, ONSET, FINISH, DE
                   )
  rm(K,T,U,WCI,WCF, CC, DIAM, COSMIC, INTERNAL, ONSET, FINISH, DE)

  ##set txtprogressbar
  if(verbose && txtProgressBar)
    pb <- txtProgressBar(min = 1, max = n.MC, style = 3)


  ##start loop
  for(i in 1:n.MC){
    DATE_MC <- suppressWarnings(nlminb(
      start = STEP1,
      objective = .calc_DoseRate,
      control = list(
        trace = FALSE),
      lower = method_control$lower,
      upper = method_control$upper,
      data = data_MC[i,],
      ref = ref,
      DR_conv_factors = set_DR_conv_factors,
      length_step = STEP1,
      mode_optim = TRUE
    ))

    ##calculate values with minium value
    DATE_MC <-
      .calc_DoseRate(
        x = DATE_MC$par,
        data = data_MC[i,],
        ref = ref,
        DR_conv_factors = set_DR_conv_factors,
        length_step = STEP1,
        max_time = max_time
      )


    ##fill variables
    DE_[i] <- data_MC[i,"DE"]
    DR_[,i] <- DATE_MC$DR
    DRA_[,i] <- DATE_MC$DRA
    CUMDR_[,i] <- DATE_MC$CUMDR
    AGE_[i] <- DATE_MC$AGE
    AGEA_[i] <- DATE_MC$AGEA

    ##update progressbar
    if(verbose && txtProgressBar) setTxtProgressBar(pb,i)
  }

  ##close pb
  if(verbose && txtProgressBar) close(pb)


# Extract final values ------------------------------------------------------------------------
  ##extract all values we want to return in the terimal and in the results data.frame
  data_results <- round(data.frame(
    AGE_CONV = DATE$AGEA,
    AGE_CONV_X = sd(AGEA_),
    AGE = DATE$AGE,
    AGE_X = sd(AGE_),
    DR_CONV = DATE$DRA[1],
    DR_CONV_X = sd(DRA_),
    DR_ONSET = rowMeans(DR_)[nrow(DR_)],
    DR_ONSET_X = sd(DR_[nrow(DR_),1:n.MC]),
    DR_FINAL = rowMeans(DR_)[1],
    DR_FINAL_X = sd(DR_[1,1:n.MC]),
    n.MC = as.integer(n.MC[1])
  ),3)

# Terminal output -----------------------------------------------------------------------------
if(verbose){
  cat("\n[model_DoseRate()]\n\n")
  cat(" Sample ID:\t\t", as.character(data[["SAMP_NAME"]]), "\n")
  cat(" Equivalent dose:\t", round(data[["DE"]],2), " \u00b1 ", round(data[["DE_X"]],2), "Gy\n")
  cat(" Diameter:\t\t", data[["DIAM"]], "\u00b5m \n")
  cat(" MC runs error estim.:\t", n.MC," \n")

  cat(" ------------------------------------------------ \n")
  cat(" Age (conv.):\t\t",data_results[["AGE_CONV"]], " \u00b1 ",
      data_results[["AGE_CONV_X"]], " ka\n")
  cat(" Age (new):\t\t", data_results[["AGE"]],
      " \u00b1 ", data_results[["AGE_X"]], " ka\n\n")
  cat(" Dose rate (conv.):\t",data_results[["DR_CONV"]], " \u00b1 ",
      data_results[["DR_CONV_X"]], " Gy/ka\n")
  cat(" Dose rate (onset):\t",data_results[["DR_ONSET"]], " \u00b1 ",
      data_results[["DR_ONSET_X"]], " Gy/ka\n")
  cat(" Dose rate (final):\t", data_results[["DR_FINAL"]], " \u00b1 ",
      data_results[["DR_FINAL_X"]], " Gy/ka\n")

  cat(" ------------------------------------------------ \n")

}

# Plotting ------------------------------------------------------------------------------------
if(plot){

  ##set plot settings
  plot_settings <- modifyList(x = list(
    mfrow = c(2,1),
    xlim = c(0, max(c(DATE[["AGE"]], DATE[["AGEA"]])) * 1.1),
    xlab = "Time [ka]"

  ),
  val = list(...))

  ##par settings; including restoring them
  par.default <- list(mfrow = par()$mfrow)
  on.exit(do.call(par, args = par.default))
  par(mfrow = plot_settings$mfrow)

  ##calculate some variables needed later
  DR_rowMeans <- rowMeans(DR_)
  DR_rowSds <- matrixStats::rowSds(DR_)
  CUMDR_rowMeans <- rowMeans(CUMDR_)
  CUMDR_rowSds <- matrixStats::rowSds(CUMDR_)

  ## +++++++++++++++++
  ##(A) dose rate plot
  plot(
    NA,
    NA,
    xlim = plot_settings$xlim,
    ylim = if(n.MC > 2){
      range(c(DR_rowMeans - DR_rowSds, DR_rowMeans + DR_rowSds))
      }else{
      range(DR_rowMeans)
      },
    xlab = plot_settings$xlab,
    ylab = "Dose rate [Gy/ka]",
    main = data[["SAMP_NAME"]]
  )

  ##abline
  abline(v = 0, lty = 2)
  text(x = 0, mean(DATE[["DR"]]), labels = "(sampling)", srt = 90, cex = .5, pos = 2)
  axis(
    side = 3,
    at = c(data[["ONSET"]], data[["FINISH"]], DATE[["AGE"]]),
    tick = FALSE,
    labels = c(expression(t[m[0]]),expression(t[m[1]]), expression(t[0])),
    padj = 1.2)

  ##error polygon
  polygon(
    x = c(0:max_time, max_time:0),
    y = c(
      DR_rowMeans + DR_rowSds,
      rev(DR_rowMeans - DR_rowSds)),
    col = rgb(0, 0, 0, .1),
    border = NA
  )

  ##add center line
  lines(x = 0:max_time, y = DATE[["DR"]], lwd = 1)

  ##set mean line (give a good indication whether the n.MC runs had been enough)
  lines(x = 0:max_time, y = rowMeans(DR_), lwd = 1, lty = 2, col = "blue")

  ## +++++++++++++++++
  ##(B) accummulated dose and age
  plot(
    NA,
    NA,
    xlim = plot_settings$xlim,
    ylim = c(0, data[["DE"]] * 1.2),
    xlab = plot_settings$xlab,
    ylab = "Absorbed dose [Gy]",
    main = data[["SAMP_NAME"]],
    sub = paste0("(n.MC = ",n.MC,")")
  )

  ##add error polygon
  polygon(
    x = c(0:max_time, max_time:0),
    y = c(
      CUMDR_rowMeans + CUMDR_rowSds,
      rev(CUMDR_rowMeans - CUMDR_rowSds)),
    col = rgb(0, 0, 0, .1),
    border = NA
  )

  ##lines showing the De distribution used for MC runs
  density_De_y <- seq(0, data[["DE"]] * 1.2, length.out = 100)
  density_De_x <- (1 / sqrt(2 * pi * data[["DE_X"]]^2)) *
    exp(-(density_De_y - data[["DE"]])^2 / 2 * data[["DE_X"]]^2)

  ##this looks weird, otherwise the plot is not really right and we have overplotting
  ##issues
  density_De_x <- (density_De_x * -par()$usr[1])/ max(density_De_x) + par()$usr[1]
  density_De_x[which.min(density_De_x)] <- min(density_De_x)
  density_De_x[density_De_x <= par()$usr[1]] <-  par()$usr[1] - 0.2
  lines(x = density_De_x, density_De_y, col = "red")


  ##centre lines horizontal (De)
  lines(
    x = c(0, DATE[["AGE"]]),
    y = rep(data[["DE"]], 2),
    col = "red",
    lty = 2
  )

  ##center lines vertical (age)
  lines(x = rep(DATE[["AGE"]], 2),
        y = c(0, data[["DE"]]),
        col = "red",
        lty = 2)

  ##add density (two times, 1st density lines, then colour)
  if(n.MC > 1){
    temp_density <- density(AGE_)
    polygon(
      x = c(temp_density$x, rev(temp_density$x)),
      y = c((temp_density$y * data[["DE"]]) / max(temp_density$y), rep(0,length(temp_density$x))),
      lty = 1,
      density = 10,
      col = "grey"
    )
    polygon(
      x = c(temp_density$x, rev(temp_density$x)),
      y = c((temp_density$y * data[["DE"]]) / max(temp_density$y), rep(0,length(temp_density$x))),
      lty = 0,
      col = rgb(1,0,0,.2)
    )
  }

  ##add lines of absorbed dose
  lines(x = 0:max_time, y = DATE[["CUMDR"]])

  ##add central point
  points(x = DATE[["AGE"]], y = data[["DE"]], cex = 1.4, pch = 21, col = "red", bg = "grey")

  ##add mtext
  mtext(side = 3, text = paste0("Age: ", round(DATE$AGE,2), " \u00b1 ", round(sd(AGE_),2), " ka"))


}#end plot

# Return value --------------------------------------------------------------------------------

  ##the return value is the input data.frame + added lines
  results <- cbind(
    data,
    data_results,
    stringsAsFactors = FALSE
  )

  ##add attributes to data.frame
  attr(results, which = "package") <- "RCarb"


  ##return
  return(results)

}
