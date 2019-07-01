#' @title List all available heat indices
#' @description Print a table with a summary of the available indices
#' @return Print a table on the screen with the following columns:
#' \itemize{
#' \item \strong{code}: Code of the index.
#' \item \strong{longname}: Long description of the index
#' \item \strong{index.fun}: The name of the internal function used to calculate it
#' \item \strong{tas, dewp, hurs, wind, radiation}: A logical value (0/1) indicating the input variables required for index calculation. Temperature and either dew point temperature or relative humidity need to be always provided.
#' \item \strong{units}: The units of the index. 
#' }
#' @author A. Casanueva
#' @export

indexShow <- function() {
  read.master()
}

#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom utils read.table

read.master <- function() {
  system.file("master", package = "HeatStress") %>% read.table(header = TRUE,
                                                                      sep = ";",
                                                                      stringsAsFactors = FALSE,
                                                                      na.strings = "")
}