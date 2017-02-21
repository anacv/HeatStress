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
# ********************************************************************
# *** calculate the vapour pressure from the dew point (ed) in hPa ***
# ********************************************************************
ed <- c1 * exp((c2*dewp)/(c3+dewp))

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

# Filter data to calculate the WBGT with optimization function
noway <- (dewp-tas)>1e-4 # not physically possible, returns NA
trivial <- abs(tas - dewp) < 1e-4 # possible when RH=100, returns tas
xmask <- !is.na(tas + ed) & !trivial & !noway # values to calculate WBGT via the optimization function

Tpwb[which(trivial)] <- tas[which(trivial)]

for (i in which(xmask)){
  
  # minimization
  opt <- optimize(fr, range(tas[i]+1, dewp[i]-1),tasi=tas[i], edi=ed[i], tol=tolerance)
  Tpwb[i]=opt$minimum
  
}

# Calculation of the WBGT with the air temperature and the phychrometric wet bulb temperature (Tpwb)
data <- 0.67*Tpwb + 0.33*tas

wbgt$value <- data
wbgt$tpwb <- Tpwb

return(wbgt)
}
