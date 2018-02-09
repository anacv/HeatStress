#' Calculate the thermal conductivity of air, W/(m K).
#' 
#' Calculate the thermal conductivity of air, W/(m K).
#' 
#' @param Tk value of air temperature in Kelvin.
#' 
#' @return Thermal conductivity of air, W/(m K).
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: BSL, page 257.


##############################################################################
thermal_cond <- function(Tk){

  # assertion statements
  assertthat::assert_that(is.numeric(Tk), msg="'Tk' is not an integer")
  assertthat::assert_that(Tk > 200, msg="'Tk' should be in Kelvin")
  
  # Constants
  m.air <- 28.97
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  cp <- 1003.5 # heat capaticy at constant pressure of dry air
  
  # Calculate the thermal conductivity of air, W/(m K)
  therm.con <- (cp + 1.25 * r.air) * viscosity(Tk)
  
  return(therm.con)
}