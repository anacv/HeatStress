#' Calculation of the heat index.
#' 
#' Calculation of the heat index from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Heat index in degrees Fahrenheit.
#' @author A.Casanueva (22.03.2018).
#' @details Formula based on air temperature and relative humidity, as it is calculated in Buzan et al. 2015, but adapted for degrees Celsius.
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
  assertthat::assert_that(is.numeric(tas), msg="'tas' is not an integer")
  assertthat::assert_that(is.numeric(hurs), msg="'hurs' is not an integer")
  assertthat::assert_that(length(hurs)==length(tas), msg="Input vectors do not have the same length")
  
  # Temperature to Fahrenheit
  tasf <- tas*1.8 +32

  # calculation of the heat index
  result <- a + b * tasf + c * hurs + d * tasf * hurs + e * tasf^2 + f * hurs^2 + g * tasf^2 * hurs + h * tasf * hurs^2 + i * tasf^2 * hurs^2
  
  # Adjust values
  for(i in 1:length(result)){
    
    if(!is.na(result[i])){
      if(hurs[i]<13 & tasf[i]<=112) result[i] <- result[i] - ((13-hurs[i])/4)*sqrt((17-abs(tasf[i]-95))/17)
      if(hurs[i]>85 & tasf[i]<=87) result[i] <- result[i] + ((hurs[i]-85)/10) * ((87-tasf[i])/5)
      if(tasf[i]<80) result[i] <- 0 
    }
  }
  
  return(result)
}