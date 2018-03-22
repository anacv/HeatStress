#' Calculation of humidex.
#' 
#' Calculation of humidex from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Humidex values in degC.
#' @author A.Casanueva (22.03.2018).
#' @details Formula based on air temperature and relative humidity, as it is calculated in Buzan 2015 GMD and references therein.
#'  
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' hum <- humidex(data_obs$tasmean, hurs=data_obs$hurs)
#' }
#' 



humidex <- function(tas,hurs){

  # assertion statements
  assertthat::assert_that(is.numeric(tas), msg="'tas' is not an integer")
  assertthat::assert_that(is.numeric(hurs), msg="'hurs' is not an integer")
  assertthat::assert_that(length(hurs)==length(tas), msg="Input vectors do not have the same length")
  
  # Calculate vapour pressure in hPa
  vp <- tashurs2vap.pres(tas, hurs)
  
  # calculation of the humidex
  result <- tas + 5/9 * (vp - 10)
  
  return(result)
}