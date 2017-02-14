#' Convert degree angle to radians.
#' 
#' Convert degree angle to radians.
#' 
#' @param angleDeg: angle in degree.
#' 
#' @return  Angle in radians.
#' @author Ana Casanueva (10.01.2017).
#' 

degToRad <- function(angleDeg){
  
  ## DESCRIPTION: Convert degree angle to radians
  ## -------------------------------------------------------------------------
  ## ARGUMENTS
  ## Required: angleDeg: angle in degree
  ## -------------------------------------------------------------------------
  ## VALUE: angle in radians
  ## -------------------------------------------------------------------------
  ## other functions needed: 
  ## -------------------------------------------------------------------------
  ## DETAILS: 
  ## -------------------------------------------------------------------------
  ## Author: Ana Casanueva, Date: 10.01.2017
  
  degToRad <- pi * angleDeg / 180
  return(degToRad)
}