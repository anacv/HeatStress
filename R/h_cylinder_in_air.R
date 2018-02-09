#' Calculate the convective heat transfer coefficient for a long cylinder in cross flow.
#' 
#' Calculate the convective heat transfer coefficient for a long cylinder in cross flow.
#' 
#' @param Tk value of air temperature in Kelvin.
#' @param Pair value of air pressure in hPa.
#' @param speed value of wind speed in m/s.
#' @param min.speed value of minimum wind speed in m/s.
#' @param diam.wick diameter of the cylinder in m.
#' 
#' @return Convective heat transfer coefficient for a long cylinder, W/(m2 K).
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: Bedingfield and Drew, eqn 32.


h_cylinder_in_air <- function(Tk, Pair, speed, min.speed, diam.wick){
  
  # assertion statements
  assertthat::assert_that(is.numeric(Tk), msg="'Tk' is not an integer")
  assertthat::assert_that(Tk > 200, msg="'Tk' should be in Kelvin")
  assertthat::assert_that(is.numeric(Pair), msg="'Pair' is not an integer")
  assertthat::assert_that(is.numeric(speed), msg="'speed' is not an integer")
  assertthat::assert_that(is.numeric(min.speed), msg="'min.speed' is not an integer")
  assertthat::assert_that(is.numeric(diam.wick), msg="'diam.wick' is not an integer")
  assertthat::assert_that(diam.wick < 1, msg="'diam.wick' should be meters")
  
  # Constants
  m.air <- 28.97
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  cp <- 1003.5 # heat capaticy at constant pressure of dry air
  Pr <- cp / (cp + (1.25 * r.air))
  
  # Calculate the thermal conductivity of air, W/(m K)
  therm.con <- thermal_cond(Tk)
  
  # Density of the air
  density <- Pair * 100 / (r.air * Tk)
  if(speed < min.speed) speed <- min.speed
  
  # Reynolds number
  Re <- speed * density * diam.wick / viscosity(Tk)
  
  # Nusselt number
  Nu <- 0.281 * Re ^ 0.6 * Pr ^ 0.44
  
  # Convective heat transfer coefficient in W/(m2 K) for a long cylinder in cross flow
  h_cylinder_in_air <- Nu * therm.con / diam.wick  
  
  return(h_cylinder_in_air)
}