#' Convert degree angle to radians.
#' 
#' Convert degree angle to radians.
#' 
#' @param angleDeg angle in degree.
#' 
#' @return  Angle in radians.
#' @author Ana Casanueva (10.01.2017).
#' 

degToRad <- function(angleDeg){

  # assertion statements
  assertthat::assert_that(is.numeric(angleDeg), msg="'angleDeg' is not an integer")

  degToRad <- pi * angleDeg / 180
  return(degToRad)
}