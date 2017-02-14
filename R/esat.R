#' Calculate the saturation vapor pressure (hPa) over water.
#' 
#' Calculate the saturation vapor pressure (hPa) over water.
#' 
#' @param Tk: value of air temperature in Kelvin.
#' 
#' @return  saturation vapor pressure (hPa).
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: Buck's (1981) approximation (eqn 3) of Wexler's (1976) formulae over liquid water.
#' 

esat <- function(Tk){
  
  esat <- 6.1121 * exp(17.502 * (Tk - 273.15) / (Tk - 32.18))
  esat <- 1.004 * esat  #correction for moist air, if pressure is not available; for pressure > 800 mb
  
  return(esat)
}