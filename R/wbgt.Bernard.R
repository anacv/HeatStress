#' Calculation of wet bulb globe temperature, following Bernard's method.
#' 
#' Calculation of wet bulb globe temperature from air temperature and dew point temperature. This corresponds to the implementation for indoors or shadow conditions.
#' 
#' @param tas: vector of air temperature in degC.
#' @param dewp: vector of dew point temperature in degC.
#' @param tolerance (optional): tolerance value for the iteration. Default: 1e-6.
#' 
#' @return A list of:
#' @return $value: wet bulb globe temperature in degC.
#' @return $tpwb: phychrometric wet bulb temperature (Tpwb) in degC.
#' @author A.Casanueva, P. Noti (21.02.2017).
#' @details Based on Lemke and Kjellstrom 2012, using the formulation from Bernard et al. 1999.
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data(eca_salam_jja_2003.Rdata) 
#' wbgt.indoors <- wbgt.Bernard(eca_salam_jja_2003$tasmean, eca_salam_jja_2003$dewp)
#' }
#' 


wbgt.Bernard <- function(tas, dewp, tolerance= 1e-6){


##################################################
##################################################
# Constants (see Lemke and Kjellstrom 2012)
c1 <- 6.106
c2 <- 17.27
c3 <- 237.3

c4 <- 1556
c5 <- 1.484
c6 <- 1010

ndates <- length(tas)


# **************************************************************************************
# *** calculate the phychrometric wet bulb temperature (Tpwb) in degC, by iteration ***
# **************************************************************************************
Tpwb <- rep(NA, ndates)
data <- rep(NA, ndates)
wbgt <- list()

# Function to minimize
fr <- function(TT,tasi,edi) {  
  abs(c4*edi - c5*edi*TT - c4*c1*exp((c2*TT)/(c3+TT)) + c5*c1*exp((c2*TT)/(c3+TT))*TT + c6*(tasi-TT))
}

# Filter the case when tas=dewp (RH=100)
trivial <- abs(tas - dewp) < 1e-4 
Tpwb[which(trivial)] <- tas[which(trivial)]
data[which(trivial)] <- 0.67*Tpwb[which(trivial)] + 0.33*tas[which(trivial)]

# Filter data to calculate the WBGT with optimization function
xmask <- !is.na(tas + dewp) & !trivial 

for (i in which(xmask)){
  # if dewp>tas, use dewp for tas and viceversa
  tas1 <- max(tas[i], dewp[i])
  dewp1 <- min(tas[i], dewp[i])
  
  # ********************************************************************
  # *** calculate the vapour pressure from the dew point (ed) in hPa ***
  # ********************************************************************
  ed1 <- c1 * exp((c2*dewp1)/(c3+dewp1))
  
  # ********************
  # *** minimization ***
  # ********************
  opt <- optimize(fr, range(tas1+1, dewp1-1),tasi=tas1, edi=ed1, tol=tolerance)
  Tpwb[i]=opt$minimum
  
  # *******************************
  # *** Calculation of the WBGT ***
  # *******************************
  data[i] <- 0.67*Tpwb[i] + 0.33*tas1
  
}

wbgt$value <- data
wbgt$tpwb <- Tpwb

return(wbgt)
}

