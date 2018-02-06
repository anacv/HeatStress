#' Calculate the atmospheric emissivity.
#' 
#' Calculate the atmospheric emissivity.
#' 
#' @param Tk value of air temperature in Kelvin.
#' @param RH value of relative humidity in fraction.
#' 
#' @return atmospheric emissivity.
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: Oke (2nd edition), page 373.


emis_atm <- function(Tk, RH){
  
  # assertion statements
  assertthat::assert_that(is.numeric(Tk), msg="'Tk' is not an integer")
  assertthat::assert_that(is.numeric(RH), msg="'RH' is not an integer")
  assertthat::assert_that(Tk > 273.15, msg="'Tk' should be in Kelvin")
  assertthat::assert_that(RH >= 0 & RH <= 1, msg="'RH' should be [0,1]")
  
  e <- RH * esat(Tk)
  emis_atm <- 0.575 * e ^ 0.143
  return(emis_atm)
}