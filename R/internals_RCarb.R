####################################################################################################
##                     INTERNAL HELPER FUNCTIONS      RCarb                                       ##
####################################################################################################

#+++++++++++++++++++++
#+ .rad_pop_LU()     +
#+++++++++++++++++++++
#' @title Calculate Ra population based in a given activity ratio and time
#'
#' @description This function is a direct translation from the Matlab function `rad_pop_LU` to R from the
#' software 'Carb' (version 2007a). The function is called from within the function [calc_DoseRate].
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montagine (France); based on Matlab
#' code provided by the authors of 'Carb'
#'
#' @param ACT_RATIO [numeric] (**required**): activity ratio (a scalar)
#'
#' @param t [numeric] (**required**): time for which the Ra populations is calculated
#'
#' @section Function version: 0.1.0
#'
#' @return This function returns a [matrix] of dimension `length(t) x 4`
#'
#' @md
#' @noRd
.rad_pop_LU <- function(
  ACT_RATIO,
  t
){

  a <- 1 / rev(t)[1]

  L234 <- log(2) / 2.455e+05
  L235 <- log(2) / 7.038e+08
  L238 <- log(2) / 4.468e+09
  L230 <- log(2) / 7.538e+04
  L231 <- log(2) / 3.276e+04

  c <- ACT_RATIO * L238 / L234

  U238 <- a * t

  b <- a
  U235 <- b * t

  P231 <- (b * L235 / L231^2) * (exp(-L231 * t) + L231 * t - 1)
  P231 <- P231 * (L231 / L235)

  U234 <- ((L238 * a / L234 ^ 2) - (a * c / L234)) * exp(-L234 * t) +
    (L238 * a / L234) * t + a * c / L234 - L238 * a / L234 ^ 2
  U234 <- U234 * (L234/L238)

  T230 <- (L238 * a/(L234*(L230-L234)) - a*c/(L230-L234)) * exp(-L234 * t) +
    (a*c/(L230-L234) - L238*a/(L234*(L230-L234)) + L238*a/L230^2 +
       L238*a/(L230*L234) - a*c/L230 ) * exp(-L230 * t) + (L238*a/L230) * t +
    a*c/L230 - L238*a/(L230*L234) - L238*a/L230^2

  T230 <- T230 * (L230 / L238)

  ##combine in matrix
  m <- matrix(c(U238, U234, T230, U235, P231), ncol = 5, byrow = FALSE)

  ##provide column headers (this helps later)
  colnames(m) <- c("N_u238", "N_u234", "N_t230", "N_u235", "N_p231")

  return(m)
}

#+++++++++++++++++++++
#+ .griddata()     +
#+++++++++++++++++++++
#' @title Point interpolation on for irregular surface data
#'
#' @description This function mimics the MATLAB function 'griddata', addpated for the specific
#' problemns in context of this package. Internally the function [interp::interpp] is used. Please
#' note that this function should not be used outside of the context of this package.
#'
#' @return The function returns a vector with the interpolated values
#'
#' @param x [numeric] (**required**): vector of x-coordinates
#'
#' @param y [numeric] (**required**): vector of y-coordinates
#'
#' @param z [numeric] (**required**): vector of z-coordinates
#'
#' @param xo [numeric] (**required**): vector of x-coordinates for the output grid
#'
#' @param yo [numeric] (**required**): vector of y-coordinates for the output grid
#'
#' @param ... further arguments passed to [inter::interpp]
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montagine (France)
#'
#' @section Function version: 0.1.0
#'
#' @seealso [interp::interpp], [calc_DoseRate]
#'
#'@md
#'@noRd
.griddata <- function(x_grid = x_grid, y_grid = y_grid, z, xo = xo, yo = yo, ...){
  interp::interpp(
    x = x_grid,
    y = y_grid,
    z = as.numeric(z),
    xo = xo,
    yo = yo,
    ...
    )$z
}


