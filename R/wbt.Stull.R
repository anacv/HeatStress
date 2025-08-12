#' Calculation of wet bulb temperature, following Stull's method.
#' 
#' Calculation of wet bulb temperature from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Wet bulb temperature in degC.
#' @author A.Casanueva (15.08.2016).
#' @details Formulation from Stull 2011, Journal of Applied Meteorology and Climatology.
#' 
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' wbt <- wbt.Stull(data_obs$tasmean, hurs=data_obs$hurs)
#' }
#' 

wbt.Stull <- function(tas,hurs){

  # assertion statements
  assertthat::assert_that(length(hurs)==length(tas), msg="Input vectors do not have the same length")
  assertthat::assert_that(all(hurs <= 100, na.rm = TRUE), msg="Some values in hurs are greater than 100")
  
  # Constants
  c1 <- 0.151977
  c2 <- 8.313659
  c3 <- 1.676331
  c4 <- 0.00391838
  c5 <- 0.023101
  c6 <- 4.686035
  
  # calculation of the wet bulb temperature in degC (Stull 2011)
  wetbulb <- tas * atan(c1 * sqrt(hurs + c2)) + atan(tas + hurs) - atan(hurs - c3) + c4 * (hurs^(3/2)) * atan(c5 * hurs) - c6 
  
  return(wetbulb)

}
