#' Calculation of vapour pressure.
#' 
#' Calculation of vapour pressure from temperature and relative humidity
#' 
#' @param tas vector of air temperature in degC.
#' @param hurs vector of relative humidity in \%.
#' 
#' @return Vapour pressure in hPa.
#' @author A.Casanueva (11.08.2016).
#' @details Formulation from Dosseger et al. 1992. Formula 16 in MCH document.
#'  
#' @export
#' 
#' @examples \dontrun{ 
#' # load the meteorological variables for example data in Salamanca:
#' data("data_obs") 
#' vp <- tashurs2vap.pres(data_obs$tasmean, hurs=data_obs$hurs)
#' }
#' 


tashurs2vap.pres <- function(tas,hurs){

# Constants (see Dosseger et al. 1992)
c1 <- 0.06107
a1 <- 17.368
b1 <- 2388.3
c2 <- 0.06108
a2 <- 17.856
b2 <- 2455.2
T0 <- 0

# units
tas <- 10*tas
hurs[hurs>100] <- 100 # some SMN stations with hurs >100

iceMask <- which(tas<T0)
waterMask <- which(tas>=T0)
vap.pres <- rep(NA,length(tas))

vap.pres[waterMask] <- hurs[waterMask] * c1 *exp((a1*tas[waterMask])/(b1+tas[waterMask]))
vap.pres[iceMask] <- hurs[iceMask] * c2 *exp((a2*tas[iceMask])/(b2+tas[iceMask]))

return(vap.pres) 

}
