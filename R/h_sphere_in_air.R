#' Calculate the convective heat tranfer coefficient for flow around a sphere.
#' 
#' Calculate the convective heat tranfer coefficient for flow around a sphere.
#' 
#' @param Tk: value of air temperature in Kelvin.
#' @param Pair: value of air pressure in hPa.
#' @param speed: value of wind speed in m/s.
#' @param min.speed: value of minimum wind speed in m/s.
#' @param diam.globe: diameter of the sphere in m.
#' 
#' @return Convective heat tranfer coefficient for flow around a sphere, W/(m2 K).
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Reference: Bird, Stewart, and Lightfoot (BSL), page 409.


h_sphere_in_air <- function(Tk, Pair, speed, min.speed, diam.globe){
  
  m.air <- 28.97
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  cp <- 1003.5 # heat capaticy at constant pressure of dry air
  Pr <- cp / (cp + 1.25 * r.air)
  
  # Calculate the thermal conductivity of air, W/(m K)
  therm.con <- thermal_cond(Tk)
  
  # Density of the air
  density <- Pair * 100 / (r.air * Tk)
  if(speed < min.speed) speed <- min.speed
  
  # Reynolds number
  Re <- speed * density * diam.globe / viscosity(Tk)
  
  # Nusselt number
  Nu <- 2 + 0.6 * Re ^ 0.5 * Pr ^ 0.3333
  
  # Convective heat tranfer coefficient for flow around a sphere, W/(m2 K)
  h_sphere_in_air <- Nu * therm.con / diam.globe 
  
  return(h_sphere_in_air)
}
