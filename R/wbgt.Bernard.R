#' Calculation of wet bulb globe temperature, following Bernard's method.
#' 
#' Calculation of wet bulb globe temperature from air temperature and dew point temperature. This corresponds to the implementation for indoors or shadow conditions.
#' 
#' @param tas vector of air temperature in degC.
#' @param dewp vector of dew point temperature in degC.
#' @param tolerance (optional): tolerance value for the iteration. Default: 1e-4.
#' @param noNAs logical, should NAs be introduced when dewp>tas? If TRUE specify how to deal in those cases (swap argument)
#' @param swap logical, should \code{tas >= dewp} be enforced by swapping? Otherwise, dewp is set to tas. This argument is needed when noNAs=T.
#' 
#' @importFrom stats optimize
#' @return A list of:
#' @return $data: wet bulb globe temperature in degC.
#' @return $Tpwb: phychrometric wet bulb temperature (Tpwb) in degC.
#' @author A.Casanueva, P. Noti, J. Bhend (21.02.2017).
#' @details Based on Lemke and Kjellstrom 2012, using the formulation from Bernard et al. 1999.
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' wbgt.indoors <- wbgt.Bernard(tas=data_obs$tasmean, dewp=data_obs$dewp)
#' }
#' 


wbgt.Bernard <- function(tas, dewp, tolerance= 1e-4, noNAs=TRUE, swap=FALSE){

  # assertion statements
  assertthat::assert_that(length(tas)==length(dewp), 
                          msg="Input vectors do not have the same length")
  
  ##################################################
  ##################################################
  # Constants (see Lemke and Kjellstrom 2012)
  c1 <- 6.106
  c2 <- 17.27
  c3 <- 237.3
  
  c4 <- 1556
  c5 <- 1.484
  c6 <- 1010
  
  # pre-allocate output
  ndates <- length(tas)
  Tpwb <- rep(NA, ndates)
  
  
  # **************************************************************************************
  # *** calculate the phychrometric wet bulb temperature (Tpwb) in degC, by iteration ***
  # **************************************************************************************
  # Function to minimize
  fr <- function(TT,tasi,edi) {  
    abs(c4*edi - c5*edi*TT - c4*c1*exp((c2*TT)/(c3+TT)) + c5*c1*exp((c2*TT)/(c3+TT))*TT + c6*(tasi-TT))
  }
  
  # Filter the case when tas=dewp (RH=100)
  trivial <- abs(tas - dewp) < tolerance
  Tpwb[which(trivial)] <- tas[which(trivial)]
  
  # Filter data to calculate the WBGT with optimization function
  xmask <- !is.na(tas + dewp) & !trivial 
  
  # Swap temperature and dewpoint if necessary
  if (noNAs & swap){
    tastmp <- pmax(tas, dewp)
    dewp <- pmin(tas, dewp)
    tas <- tastmp
  } else if(noNAs & !swap){
    noway <- (dewp - tas) > tolerance
    xmask <- xmask & !noway
    Tpwb[which(noway)] <- tas[which(noway)]
  } else if(!noNAs){
    xmask <- xmask & tas >= dewp
  }
  
  # ********************************************************************
  # *** calculate the vapour pressure from the dew point (ed) in hPa ***
  # ********************************************************************
  ed <- c1 * exp((c2*dewp)/(c3+dewp))
  
  for (i in which(xmask)){
  
    # ********************
    # *** minimization ***
    # ********************
    opt <- optimize(fr, range(tas[i]+1, dewp[i]-1),tasi=tas[i], edi=ed[i], tol=tolerance)
    Tpwb[i]=opt$minimum
    
  }
  
  # *******************************
  # *** Calculation of the WBGT ***
  # *******************************
  wbgt <- list(Tpwb = Tpwb, 
               data = 0.67*Tpwb + 0.33 * tas)
  
  return(wbgt)
}

