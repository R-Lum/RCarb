#' @title RCarb - Dose Rate Modelling of Carbonate-Rich Samples
#'
#' @description The package provides a dose rate modelling for carbonate-rich samples in the
#' context of trapped charged dating (e.g., luminescence dating) applications.
#'
#'
#' \if{html}{
#' \figure{Logo_RCarb.png}{options: width="50px" alt="https://github.com/R-Lum/RCarb"}\cr
#' }
#'
#'
#' \tabular{ll}{
#' **Package:** \tab RCarb \cr
#' **Type:** \tab Package \cr
#' **Version:** \tab 0.1.0 \cr
#' **Date:** \tab 2018-10-03 \cr
#' **License:** \tab GPL-3 \cr
#' }
#'
#' @name RCarb-package
#'
#' @aliases RCarb-package RCarb
#'
#' @docType package
#'
#' @keywords package
#'
#' @import utils
#'
#' @references
#'
#' This package bases on a MATLAB programme with name 'Carb', details can be found the
#' following references:\cr
#'
#' Mauz, B., Hoffmann, D., 2014. What to do when carbonate replaced water: Carb, the model for estimating the
#' dose rate of carbonate-rich samples. Ancient TL 32, 24–32.
#'
#' Nathan, R.P., Mauz, B., 2008. On the dose-rate estimate of carbonate-rich sediments for trapped charge dating.
#' Radiation Measurements 43, 14–25. doi:10.1016/j.radmeas.2007.12.012
#'
#' @importFrom grDevices rgb
#' @importFrom graphics plot plot.default abline lines par mtext polygon
#' @importFrom stats approx nlminb rnorm sd na.exclude
#'
#' @md
NULL


#' Example data
#'
#' @name Example_Data
#'
#' @format
#'
#' `Example_Data`: [data.frame]
#'
#' @section Version: 0.1.0
#'
#' @keywords datasets
#'
#' @examples
#'
#' ## show first 5 elements of the METADATA and DATA elements in the terminal
#' data(Example_Data, envir = environment())
#' head(Example_Data)
#'
#' @docType data
#' @md
NULL


#' Reference data
#'
#' @name Reference_Data
#'
#' @format
#'
#' `ref`: [data.frame]
#'
#' @section Version: 0.1.0
#'
#' @keywords datasets
#'
#' @docType data
#' @md
NULL