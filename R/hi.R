#' Calculation of the heat index.
#' 
#' Calculation of the heat index from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Heat index in degrees Fahrenheit.
#' @author A.Casanueva (22.03.2018). Modified in 12.08.2025.
#' @details Formula based on air temperature and relative humidity, following Rothfusz 1990 (National Weather Service Technical Attachment, SR 90-23), but adapted for degrees Celsius. This implementation includes some adjustments for high and low relative humidity values. Also, the original formula is not appropriate for a heat index value below about 80ºF. In those cases, a simpler formula is applied to calculate values consistent with Steadman's results. See: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml    
#'  
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' heatindex <- hi(data_obs$tasmean, hurs=data_obs$hurs)
#' }
#' 



hi <- function(tas,hurs){
  
  # Constants
  a <- -42.379
  b <- 2.04901523
  c <- 10.14333127
  d <- -0.22475541
  e <- -6.83783e-3 
  f <- -5.481717e-2
  g <- 1.22874e-3
  h <- 8.5282e-4
  i <- -1.99e-6

  # assertion statements
  assertthat::assert_that(length(hurs)==length(tas), msg="Input vectors do not have the same length")
  assertthat::assert_that(all(hurs <= 100, na.rm = TRUE), msg="Some values in hurs are greater than 100")
  
  # Temperature to Fahrenheit
  tasf <- tas*1.8 +32

  # calculation of the heat index
  result <- a + b * tasf + c * hurs + d * tasf * hurs + e * tasf^2 + f * hurs^2 + g * tasf^2 * hurs + h * tasf * hurs^2 + i * tasf^2 * hurs^2
  
  # Adjust values
  for(i in 1:length(result)){
    
    if(!is.na(result[i])){
      if(hurs[i]<13 & tasf[i]>=80 & tasf[i]<=112) result[i] <- result[i] - ((13-hurs[i])/4)*sqrt((17-abs(tasf[i]-95))/17)
      if(hurs[i]>85 & tasf[i]>=80 & tasf[i]<=87) result[i] <- result[i] + ((hurs[i]-85)/10) * ((87-tasf[i])/5)
      if(result[i]<80) result[i] <-  0.5*(tasf[i] + 61.0 + ((tasf[i]-68.0)*1.2) + (hurs[i]*0.094)) 
    }
  }
  
  return(result)
}