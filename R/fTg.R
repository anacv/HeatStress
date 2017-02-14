#' Calculation of the globe temperature.
#' 
#' Calculation of the globe temperature.
#' 
#' @param Ta: value of air temperature in degC.
#' @param relh: value of relative humidity in \%.
#' @param Pair: value of atmospheric pressure in hPa.
#' @param ws: value of wind speed in m/s.
#' @param min.speed: value of minimum wind speed in m/s.
#' @param solar: value of solar shortwave downwelling radiation in W/m2.
#' @param propDirect: proportion of direct radiation = direct/(diffuse + direct).
#' @param zenith: zenith angle in radians.
#' @param SurfAlbedo (optional): albedo in the surface. Default: 0.4.
#' @param tolerance (optional): tolerance value for the iteration. Default: 1e-6.
#' 
#' @return Globe temperature in degC.
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA).
#' @export
#' 


fTg <- function(Ta, relh, Pair, ws, min.speed, solar, propDirect, zenith, SurfAlbedo=0.4, tolerance=1e-6){
  
  # Physical constants
  stefanb <- 0.000000056696
  cp <- 1003.5 # heat capaticy at constant pressure of dry air
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  ratio <- cp * m.air/ m.h2o
  Pr <- cp / (cp + (1.25 * r.air))
  
  # Globe constants
  emis.globe <- 0.95 # emissivity
  alb.globe <- 0.05 # albedo
  diam.globe <- 0.05 #0.05 = 50mm diam globe
  
  # Surface constants
  emis.sfc <- 0.999
  alb.sfc <- SurfAlbedo
  
  # Fix up out-of bounds problems with zenith
  if(zenith <= 0) zenith <- 0.0000000001
  if(zenith > 1.57) zenith <- 1.57
  
  # Change units
  Tair <- Ta + 273.15
  RH <- relh * 0.01
  
  # cosine of zenith angle
  cza <- cos(zenith)
  
  # Set values for iteration
  Tsfc <- Tair
  Tglobe_prev <- Tair # first guess is the air temperature
  
  # Function to minimize
  fr <- function(Tglobe_prev,Tair,Pair) {  
    Tref <- 0.5 * (Tglobe_prev + Tair) # Evaluate properties at the average temperature
    
    # Calculate the convective heat transfer coefficient, W/(m2 K) for flow around a sphere.
    h <- h_sphere_in_air(Tref, Pair, ws, min.speed, diam.globe)
    
    # Calculate the globe temperature
    Tglobe <- (0.5 * (emis_atm(Tair, RH) * Tair ^ 4 + emis.sfc * Tsfc ^ 4) - h / (emis.globe * stefanb) * (Tglobe_prev - Tair) + solar / (2 * emis.globe * stefanb) * (1 - alb.globe) * (propDirect * (1 / (2 * cza) - 1) + 1 + alb.sfc)) ^ 0.25
    
    abs(Tglobe - Tglobe_prev)
  }
  
  # Minimization (iteratively)
  opt <- optimize(fr, c(Tglobe_prev-20, Tglobe_prev+20),Tair,Pair, tol=tolerance)
  
  Tg <- opt$minimum - 273.15
  
  return(Tg)
  
}
