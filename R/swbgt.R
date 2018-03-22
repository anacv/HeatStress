#' Calculation of the simplified wet bulb globe temperature.
#' 
#' Calculation of the simplified wet bulb globe temperature from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Simplified wet bulb globe temperature in degC.
#' @author A.Casanueva (22.03.2018).
#' @details Formula based on air temperature and relative humidity, as it is calculated in Buzan et al. 2015 GMD with a small correction in the constants from Lemke and Kjellstrom 2012.
#'  
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' swbgt <- swbgt(data_obs$tasmean, hurs=data_obs$hurs)
#' }
#' 



swbgt <- function(tas,hurs){
  
  
  # constants according to Lemke and Kjellstrom 2012, who modified the constants from te Australian Bureau of Meteorology
  c1 <- 0.567
  #c2 <- 0.393
  #c3 <- 3.94
  c2 <- 0.216
  c3 <- 3.38

  # assertion statements
  assertthat::assert_that(is.numeric(tas), msg="'tas' is not an integer")
  assertthat::assert_that(is.numeric(hurs), msg="'hurs' is not an integer")
  assertthat::assert_that(length(hurs)==length(tas), msg="Input vectors do not have the same length")
  
  # Calculate vapour pressure in hPa
  vp <- tashurs2vap.pres(tas, hurs)
  
  # calculation of the swbgt
  result <- c1 * tas + c2 * vp + c3
  
  return(result)
}
