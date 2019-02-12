#' Calculation of the globe temperature.
#' 
#' Calculation of the globe temperature.
#' 
#' @inheritParams fTnwb 
#' @return Globe temperature in degC.
#' 
#' @author Ana Casanueva (05.01.2017).
#' @details Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA).
#' @export
#' 


fTg <- function(tas, relh, Pair, wind, min.speed, radiation, propDirect, 
                zenith, SurfAlbedo=0.4, tolerance=1e-4){

 
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
  if(radiation > 0 & zenith > 1.57) zenith <- 1.57 # 90°
  if(radiation > 15 & zenith > 1.54)  zenith <- 1.54 # 88°
  if(radiation > 900 & zenith > 1.52) zenith <- 1.52 # 87°
  if(radiation < 10 & zenith == 1.57) radiation <- 0 
 

 # Change units
  Tair <- tas + 273.15
  RH <- relh * 0.01
  
  # cosine of zenith angle
  cza <- cos(zenith)
  
  # Set values for iteration
  Tsfc <- Tair
  
  # Function to minimize
  fr <- function(Tglobe_prev,Tair,Pair) {  
    Tref <- 0.5 * (Tglobe_prev + Tair) # Evaluate properties at the average temperature
    
    # Calculate the convective heat transfer coefficient, W/(m2 K) for flow around a sphere.
    h <- h_sphere_in_air(Tref, Pair, wind, min.speed, diam.globe)

    # Calculate the globe temperature
    Tglobe <- (0.5 * (emis_atm(Tair, RH) * Tair ^ 4 + emis.sfc * Tsfc ^ 4) - h / (emis.globe * stefanb) * (Tglobe_prev - Tair) + radiation / (2 * emis.globe * stefanb) * (1 - alb.globe) * (propDirect * (1 / (2 * cza) - 1) + 1 + alb.sfc)) ^ 0.25
    abs(Tglobe - Tglobe_prev)
  }
  
  # Minimization (iteratively)
  opt <- stats::optimize(fr, range(Tair-2, Tair+10),Tair,Pair, tol=tolerance)
  Tg <- opt$minimum - 273.15

  return(Tg)
  
}
