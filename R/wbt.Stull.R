#' Calculation of wet bulb temperature, following Stull's method.
#' 
#' Calculation of wet bulb temperature from temperature and relative humidity.
#' 
#' @param tas: vector of air temperature in degC.
#' @param hurs: vector of relative humidity in \%.
#' 
#' @return Wet bulb temperature in degC.
#' @author A.Casanueva (15.08.2016).
#' @details Formulation from Stull 2011, Journal of Applied Meteorology and Climatology.
#' 
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data(eca_salam_jja_2003.Rdata) 
#' wbt <- wbt.Stull(eca_salam_jja_2003$tasmean, eca_salam_jja_2003$hurs)
#' }
#' 

wbt.Stull <- function(tas,hurs){

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
