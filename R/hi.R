#' Calculation of the heat index.
#' 
#' Calculation of the heat index from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Heat index in degrees Fahrenheit.
#' @author A.Casanueva (22.03.2018). Modified in 12.08.2025.
#' @details Formula based on air temperature and relative humidity, following Rothfusz 1990 (National Weather Service Technical Attachment, SR 90-23), but adapted for degrees Celsius. This implementation includes some adjustments for high and low relative humidity values. Also, the original formula is not appropriate for low temperatures and heat index values. In those cases, a simpler formula is applied to calculate values consistent with Steadman's results. See: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml and https://github.com/ecmwf/thermofeel/blob/master/thermofeel/thermofeel.py#L782  
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
  
  # Helper to make filters NA-safe
  safe_filter <- function(cond) {
    cond[is.na(cond)] <- FALSE
    cond
  }
  
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

  # Calculation of the heat index
  result_simple <-  0.5*(tasf + 61.0 + ((tasf-68.0)*1.2) + (hurs*0.094)) 
  result <- a + b * tasf + c * hurs + d * tasf * hurs + e * tasf^2 + f * hurs^2 + g * tasf^2 * hurs + h * tasf * hurs^2 + i * tasf^2 * hurs^2
  
  # Define filters for adjustments
  f_adjust1 <- safe_filter((tasf >= 80) & (tasf <= 112) & (hurs <= 13))
  f_adjust2 <- safe_filter((tasf >= 80) & (tasf <= 87) & (hurs > 85))
  tas_filter <- safe_filter(tasf < 80)
  hi_filter <- safe_filter(((result_simple + tasf) / 2) < 80)
  
  # Adjustments
  adjustment1 <- (13-hurs[f_adjust1])/4 *sqrt(17-abs(tasf[f_adjust1]-95)/17)
  adjustment2 <- (hurs[f_adjust2]-85)/10 * ((87-tasf[f_adjust2])/5)
  
  # Apply adjustments
  result[f_adjust1] <- result[f_adjust1] - adjustment1
  result[f_adjust2] <- result[f_adjust2] + adjustment2
  result[tas_filter] <- result_simple[tas_filter]
  result[hi_filter] <- result_simple[hi_filter]

  return(result)
}