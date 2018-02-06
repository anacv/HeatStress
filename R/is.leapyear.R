#' Check whether a year is a leap year.
#' 
#' Check whether a year is a leap year.
#' 
#' @param year to be checked.
#' 
#' @return logical, TRUE/FALSE.
#' 
#' @author Sven Kotlarski (20.12.2016).


is.leapyear <- function(year){

  # assertion statements
  assertthat::assert_that(is.numeric(year), msg="'year' is not an integer")
  
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}