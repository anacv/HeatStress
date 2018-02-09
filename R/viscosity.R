#' Compute the viscosity of air, kg/(m s).
#' 
#' Compute the viscosity of air, kg/(m s) given temperature (K).
#' 
#' @param Tk value of air temperature in Kelvin.
#' 
#' @return viscosity of air, kg/(m s). 
#'
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: BSL, page 23.

viscosity <- function(Tk){

  # assertion statements
  assertthat::assert_that(is.numeric(Tk), msg="'Tk' is not an integer")
  assertthat::assert_that(Tk > 200, msg="'Tk' should be in Kelvin")
  
  omega <- (Tk / 97 - 2.9) / 0.4 * (-0.034) + 1.048
  viscosity <- 0.0000026693 * (28.97 * Tk) ^ 0.5 / (3.617 ^ 2 * omega)
  
  return(viscosity)
}