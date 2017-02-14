#' Convert radian angle to degrees.
#' 
#' Convert radian angle to degrees.
#' 
#' @param angleRad: angle in radians.
#' 
#' @return  Angle in degrees.
#' @author Ana Casanueva (10.01.2017).
#' 

radToDeg <- function(angleRad){
  
  radToDeg <- 180 * angleRad / pi
  return(radToDeg)
}