#' Calculation of wet bulb globe temperature, following Liljegren's method.
#' 
#' Calculation of wet bulb globe temperature from air temperature, dew point temperature, radiation and wind. 
#' 
#' @param tas: vector of air temperature in degC
#' @param dewp: vector of dew point temperature in degC
#' @param wind: vector of wind speed in m/s
#' @param radiation: vector of solar shortwave downwelling radiation in W/m2
#' @param dates: vector with dates in format yyyy-mm-dd H:M:S, same length as the other inputs. If format yyyy-mm-dd is provided, the default hour is 12:00:00.
#' @param lon: value for the longitude of the location.
#' @param lat: value for the latitude of the location.
#' @param tolerance (optional): tolerance value for the iteration. Default: 1e-6
#' 
#' @return A list of:
#' @return $value: wet bulb globe temperature in degC
#' @return $tnwb: natural wet bulb temperature (Tpwb) in degC
#' @return $tg: globe temperature in degC
#' @author A.Casanueva (17.01.2017).
#' @details This corresponds to the implementation for outdoors or in the sun conditions. Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA) and Ana Casanueva into R.
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("eca_salam_jja_2003.Rdata") 
#' wbgt.outdoors <- wbgt.Liljegren(eca_salam_jja_2003$tasmean, eca_salam_jja_2003$dewp, eca_salam_jja_2003$wind, eca_salam_jja_2003$solar, eca_salam_jja_2003$Dates, -5.66, 40.96)
#' }
#' 



wbgt.Liljegren <- function(tas, dewp, wind, radiation, dates, lon, lat, tolerance=1e-6){
  

  ##################################################
  ##################################################
  # Assumptions
  propDirect <- 0.8  # Assume a proportion of direct radiation = direct/(diffuse + direct)
  Pair <- 1010  # Atmospheric pressure in hPa
  MinWindSpeed <- 0.1   # 0 wind speed upsets log function
  
  
  ######################
  ######################
  ndates <- length(tas)
  Tpwb <- rep(NA, ndates)
  data <- rep(NA, ndates)
  wbgt <- NULL
  
  for (i in 1:ndates){
    Ta <- tas[i]
    Td <- dewp[i]
    ws <- wind[i]
    solar <- radiation[i]
    date <- dates[i]
    
    # nlm does not ignore NA
    if (!is.na(Ta) & !is.na(Td) & !is.na(ws) & !is.na(solar)){
      
      # Check to make sure Td < Ta
      if((Td-Ta)>0.1) {
        print("Warning: Dew point temperature larger than air temperature, result set to NA")
        wbgt$value[i]<- NA; wbgt$tnwb[i]<- NA; wbgt$tg[i]<- NA
      } else{
        
        # Calculate zenith angle (radians are needed)
        zenithDeg <- calZenith(date, lon, lat)
        ZenithAngle <- degToRad(zenithDeg)
        
        # Calculate relative humidity from air temperature and dew point temperature
        relh <- dewp2hurs(Ta,Td) # input in degC, output in %
        
        # Calculate globe temperature
        Tg <- fTg(Ta, relh, Pair, ws, MinWindSpeed, solar, propDirect, ZenithAngle)
        
        # Calculate natural wet bulb temperature
        Tnwb <- fTnwb(Ta, Td, relh, Pair, ws, MinWindSpeed, solar, propDirect, ZenithAngle)
        
        # Calculate WBGT outdoors/in the sun
        WBGTo <- 0.7 * Tnwb + 0.2 * Tg + 0.1 * Ta
        
        wbgt$value[i] <- WBGTo
        wbgt$tnwb[i] <- Tnwb
        wbgt$tg[i] <- Tg
        
        rm(WBGTo, Tnwb, Tg, relh)
      }
      
    } else{wbgt$value[i]<- NA; wbgt$tnwb[i]<- NA; wbgt$tg[i]<- NA}
    
  }
  
  return(wbgt)
}
