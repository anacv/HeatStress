#' Calculation of the discomfort index.
#' 
#' Calculation of the discomfort index from temperature and relative humidity.
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Discomfort index in degC.
#' @author A.Casanueva (22.03.2018).
#' @details Formula based on air temperature and relative humidity, as it is calculated in Coccolo et al. 2016 and references therein.
#'  
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' di <- discomInd(data_obs$tasmean, hurs=data_obs$hurs)
#' }
#' 



discomInd <- function(tas,hurs){

  # assertion statements
  assertthat::assert_that(length(hurs)==length(tas), msg="Input vectors do not have the same length")
  
  # calculation of the discomfort index
  result <- tas - 0.55 *(1 - 0.01*hurs)*(tas-14.5)
  
  return(result)
}