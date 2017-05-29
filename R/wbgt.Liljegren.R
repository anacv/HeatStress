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
#' @param tolerance (optional): tolerance value for the iteration. Default: 1e-4
#' @param noNAs: logical, should NAs be introduced when dewp>tas? If TRUE specify how to deal in those cases (swap argument)
#' @param swap: logical, should \code{tas >= dewp} be enforced by swapping? Otherwise, dewp is set to tas. This argument is needed when noNAs=T.
#' @param hour logical. If TRUE, allow working with hourly data for zenith angle calculation. Default: FALSE (12 UTC is used).
#' 
#' @return A list of:
#' @return $data: wet bulb globe temperature in degC
#' @return $Tnwb: natural wet bulb temperature (Tnwb) in degC
#' @return $Tg: globe temperature in degC
#' @author A.Casanueva (21.02.2017).
#' @details This corresponds to the implementation for outdoors or in the sun conditions. Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA) and Ana Casanueva into R.
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data(eca_salam_jja_2003.Rdata) 
#' wbgt.outdoors <- wbgt.Liljegren(eca_salam_jja_2003$tasmean, eca_salam_jja_2003$dewp, eca_salam_jja_2003$wind, eca_salam_jja_2003$solar, eca_salam_jja_2003$Dates, -5.66, 40.96)
#' }
#' 

wbgt.Liljegren <- function(tas, dewp, wind, radiation, dates, lon, lat, tolerance=1e-4, noNAs=TRUE, swap=FALSE, hour=FALSE){
  
  ##################################################
  ##################################################
  # Assumptions
  propDirect <- 0.8  # Assume a proportion of direct radiation = direct/(diffuse + direct)
  Pair <- 1010  # Atmospheric pressure in hPa
  MinWindSpeed <- 0.1   # 0 wind speed upsets log function
  ######################
  ######################
  
  ndates <- length(tas)
  Tnwb <- rep(NA, ndates)
  Tg <- rep(NA, ndates)
  
  # Do not allow negative wind and radiation
  radiation[radiation<0] <- 0
  wind[wind<0] <- 0
  
  # Filter data to calculate the WBGT with optimization function
  xmask <- !is.na(tas + dewp + wind + radiation)
  
  if (noNAs & swap){
    tastmp <- pmax(tas, dewp)
    dewp <- pmin(tas, dewp)
    tas <- tastmp
  } else if(noNAs & !swap){
    noway <- (dewp - tas) > tolerance
    dewp[which(noway)] <- tas[which(noway)]
  } else if(!noNAs){
    xmask <- xmask & tas >= dewp
  }
 
  # Calculate relative humidity from air temperature and dew point temperature
  relh <- dewp2hurs(tas,dewp) # input in degC, output in %

  # **************************************
  # *** Calculation of the Tg and Tnwb ***
  # **************************************
  for (i in which(xmask)){
  
    # Calculate zenith angle (radians are needed)
    zenithDeg <- calZenith(dates[i], lon, lat, hour)
    ZenithAngle <- degToRad(zenithDeg)
    
    # Calculate globe temperature
    Tg[i] <- fTg(tas[i], relh[i], Pair, wind[i], MinWindSpeed, radiation[i], propDirect, ZenithAngle, tol=tolerance)
    
    # Calculate natural wet bulb temperature
    Tnwb[i] <- fTnwb(tas[i], dewp[i], relh[i], Pair, wind[i], MinWindSpeed, radiation[i], propDirect, ZenithAngle, tol=tolerance)

    rm(zenithDeg, ZenithAngle)
  }

  
  # *******************************
  # *** Calculation of the WBGT ***
  # *******************************
  wbgt <- list(data = 0.7 * Tnwb + 0.2 * Tg + 0.1 * tas, 
               Tnwb = Tnwb,
               Tg = Tg)
  
  return(wbgt)
}
