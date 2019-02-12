#' Calculation of wet bulb globe temperature, following Liljegren's method.
#' 
#' Calculation of wet bulb globe temperature from air temperature, dew point temperature, radiation and wind. 
#' 
#' @param noNAs logical, should NAs be introduced when dewp>tas? If TRUE specify how to deal in those cases (swap argument)
#' @param swap logical, should \code{tas >= dewp} be enforced by swapping? Otherwise, dewp is set to tas. This argument is needed when noNAs=T.
#' @inheritParams fTnwb
#' @inheritParams calZenith
#' @importFrom stats optimize
#' 
#' @return A list of:
#' @return $data: wet bulb globe temperature in degC
#' @return $Tnwb: natural wet bulb temperature (Tnwb) in degC
#' @return $Tg: globe temperature in degC
#' @author A.Casanueva (21.02.2017).
#' @details This corresponds to the implementation for outdoors or in the sun conditions (Liljegren et al. 2008). Original fortran code by James C. Liljegren, translated by Bruno Lemke into Visual Basic (VBA) and Ana Casanueva into R.
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' wbgt.outdoors <- wbgt.Liljegren(tas=data_obs$tasmean, dewp=data_obs$dewp, 
#' wind=data_obs$wind, radiation=data_obs$solar, dates= data_obs$Dates, lon=-5.66, lat=40.96)
#' }
#' 

wbgt.Liljegren <- function(tas, dewp, wind, radiation, dates, lon, lat, tolerance=1e-4, 
                           noNAs=TRUE, swap=FALSE, hour=FALSE){

  
  ##################################################
  ##################################################
  # Assumptions
  propDirect <- 0.8  # Assume a proportion of direct radiation = direct/(diffuse + direct)
  Pair <- 1010  # Atmospheric pressure in hPa
  MinWindSpeed <- 0.1   # 0 wind speed upsets log function
  
  ##################################################
  ##################################################
  # Assertion statements
  assertthat::assert_that(is.logical(hour), msg="'hour' should be logical")
  assertthat::assert_that(is.logical(noNAs), msg="'noNAs' should be logical")
  assertthat::assert_that(is.logical(swap), msg="'swap' should be logical")
  assertthat::assert_that(length(tas)==length(dewp) & length(dewp)==length(wind)
                          & length(wind)==length(radiation), 
                          msg="Input vectors do not have the same length")
  assertthat::assert_that(is.numeric(Pair), msg="'Pair' is not an integer")
  assertthat::assert_that(is.numeric(MinWindSpeed), msg="'min.speed' is not an integer")
  assertthat::assert_that(propDirect < 1, msg="'propDirect' should be [0,1]")  
  assertthat::assert_that(is.numeric(lon), msg="'lon' is not an integer")
  assertthat::assert_that(is.numeric(lat), msg="'lat' is not an integer")
  assertthat::assert_that(lon <= 180 & lon >=-180, msg="Invalid lon")
  assertthat::assert_that(lat <= 90 & lon >=-90, msg="Invalid lat")
  
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
    Tg[i] <- fTg(tas[i], relh[i], Pair, wind[i], MinWindSpeed, radiation[i], 
                 propDirect, ZenithAngle, tolerance=tolerance)
 
    # Calculate natural wet bulb temperature
    Tnwb[i] <- fTnwb(tas[i], dewp[i], relh[i], Pair, wind[i], MinWindSpeed, 
                     radiation[i], propDirect, ZenithAngle, tolerance=tolerance)

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
