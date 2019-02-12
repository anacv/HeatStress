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

  degToRad <- pi * angleDeg / 180
  return(degToRad)
}