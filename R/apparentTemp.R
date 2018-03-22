#' Calculation of the apparent temperature.
#' 
#' Calculation of the apparent temperature from temperature, relative humidity and wind.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' @param wind vector of wind at 10m in m/s.
#' 
#' @return Apparent temperature in degC.
#' @author A.Casanueva (22.03.2018).
#' @details Formula based on air temperature, relative humidity and wind only, as it is calculated in Steadman 1994, Buzan et al. 2015 GMD and references therein. There is a version including radiation (net radiation absorved per unit area of human body surface), but it is not implemented here.
#'   
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' at <- apparentTemp(data_obs$tasmean, hurs=data_obs$hurs, wind= data_obs$wind)
#' }
#' 

apparentTemp <- function(tas,hurs, wind){
  
  # assertion statements
  assertthat::assert_that(is.numeric(tas), msg="'tas' is not an integer")
  assertthat::assert_that(is.numeric(hurs), msg="'hurs' is not an integer")
  assertthat::assert_that(is.numeric(wind), msg="'wind' is not an integer")
  assertthat::assert_that(length(hurs)==length(tas) & length(tas)==length(wind), msg="Input vectors do not have the same length")
  
  # Calculate vapour pressure in hPa
  vp <- tashurs2vap.pres(tas, hurs)
  
  # Calculate apparent temperature
  result <- tas + 0.33*vp - 0.7*wind -4
  
  return(result)
  
}
