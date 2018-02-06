#' Calculate the heat of evaporation, J/(kg K).
#' 
#' Calculate the heat of evaporation, J/(kg K), for temperature in the range 283-313 K.
#' 
#' @param Tk value of air temperature in Kelvin.
#' 
#' @return Heat of evaporation, J/(kg K).
#'
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: Van Wylen and Sonntag, Table A.1.1


h_evap <- function(Tk){
  
  # assertion statements
  assertthat::assert_that(is.numeric(Tk), msg="'Tk' is not an integer")
  assertthat::assert_that(Tk > 273.15, msg="'Tk' should be in Kelvin")
  
  evap <- (313.15 - Tk) / 30 * (-71100) + 2407300
  return(evap)
}