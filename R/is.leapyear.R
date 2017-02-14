#' Check whether a year is a leap year.
#' 
#' Check whether a year is a leap year.
#' 
#' @param Year to be checked.
#' 
#' @return logical, TRUE/FALSE.
#' 
#' @author Sven Kotlarski (20.12.2016).


is.leapyear <- function(year){
  
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}