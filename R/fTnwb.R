#' Calculation of the natural wet bulb temperature.
#' 
#' Calculation of the natural wet bulb temperature.
#' 
#' @param Ta: value of air temperature in degC.
#' @param relh: value of relative humidity in \%.
#' @param Pair: value of atmospheric pressure in hPa.
#' @param ws: value of wind speed in m/s.
#' @param min.speed: value of minimum wind speed in m/s.
#' @param solar: value of solar shortwave downwelling radiation in W/m2.
#' @param propDirect: proportion of direct radiation = direct/(diffuse + direct).
#' @param zenith: zenith angle in radians.
#' @param irad (optional): include radiation (1) or not (irad=0, psychrometric web bulb temp). Default: 1.
#' @param SurfAlbedo (optional): albedo in the surface. Default: 0.4.
#' @param tolerance (optional): tolerance value for the iteration. Default: 1e-6.
#' 
#' @return Natural wet bulb globe temperature in degC.
#' @author Ana Casanueva (05.01.2017).
#' @details Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA).
#' @export
#' 



fTnwb <- function(Ta, Td, relh, Pair, ws, min.speed, solar, propDirect, zenith, irad=1, SurfAlbedo=0.4, tolerance=1e-6){
  
  # Physical constants
  stefanb <- 0.000000056696
  cp <- 1003.5 # heat capaticy of dry air at constant pressure 
  m.air <- 28.97
  m.h2o <- 18.015
  r.gas <- 8314.34
  r.air <- r.gas / m.air
  ratio <- cp * m.air/ m.h2o
  Pr <- cp / (cp + (1.25 * r.air))
  
  # Wick constants
  emis.wick <- 0.95 # emissivity
  alb.wick <- 0.4 # albedo
  diam.wick <- 0.007 # diameter (in m)
  len.wick <- 0.0254 # length (in m)
  
  # Globe constants
  emis.globe <- 0.95 # emissivity
  alb.globe <- 0.05 # albedo
  diam.globe <- 0.0508 # diameter (in m)
  
  # Surface constants
  emis.sfc <- 0.999
  alb.sfc <- SurfAlbedo
  
  # Fix up out-of bounds problems with zenith
  if(zenith <= 0) zenith <- 0.0000000001
  if(solar > 0 & zenith > 1.57) zenith <- 1.57 # 90°
  if(solar > 15 & zenith > 1.54)  zenith <- 1.54 # 88°
  if(solar > 900 & zenith > 1.52) zenith <- 1.52 # 87°
  
  # Change units
  Tdew <- Td + 273.15 # to Kelvin
  Tair <- Ta + 273.15 # to Kelvin
  RH <- relh * 0.01 # to fraction
  
  # Calculate vapour pressure
  eair <- RH * esat(Tair) 
  
  # Calculate the atmospheric emissivity
  emis.atm <- emis_atm(Tair, RH)
  
  # Set values for iteration
  Tsfc <- Tair
  Twb_prev <- Tdew # First guess is the dew point temperature
  
  # Function to minimize
  fr <- function(Twb_prev,Tair,Pair) {  
    Tref <- 0.5 * (Twb_prev + Tair) # Evaluate properties at the average temperature
    
    # Radiative heating term	
    Fatm <- stefanb * emis.wick * (0.5 * (emis.atm * Tair ^ 4 + emis.sfc * Tsfc ^ 4) - Twb_prev ^ 4) + (1 - alb.wick) * solar * ((1 - propDirect) * (1 + 0.25 * diam.wick / len.wick) + ((tan(zenith) / 3.1416) + 0.25 * diam.wick / len.wick) * propDirect + alb.sfc)
    
    # Density of the air
    density <- Pair * 100 / (Tair * r.air)
    
    # Schmidt number
    Sc <- viscosity(Tair) / (density * diffusivity(Tref, Pair)) 
    
    # Calculate the convective heat transfer coefficient for a long cylinder in cross flow
    h <- h_cylinder_in_air(Twb_prev, Pair, ws, min.speed, diam.wick)
    
    # Calculate the saturation vapor pressure (hPa) over liquid water
    ewick <- esat(Twb_prev)
    
    # Calculate the heat of evaporation, J/(kg K)
    evap <- h_evap(Twb_prev)
    
    # Calculate the natural wet bulb temperature
    Twb <- Tair - evap / ratio * (ewick - eair) / (Pair - ewick) * (Pr / Sc) ^ 0.56 + Fatm / h * irad
    abs(Twb - Twb_prev)
    
  }
  
  # Minimization (iteratively)
  opt <- optimize(fr, range(Tdew-1, Tair+1),Tair,Pair, tol=tolerance)
  
  Tnwb <- opt$minimum - 273.15
  
  return(Tnwb)
  
}