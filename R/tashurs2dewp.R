tashurs2dewp <- function(tas,hurs){

## DESCRIPTION: Calculation of dewpoint temperature from temperature and relative humidity.
## -------------------------------------------------------------------------
## ARGUMENTS
## Required: tas: vector of temperature in degC
##           hurs: vector of relative humidity in %
## Optional:
## -------------------------------------------------------------------------
## VALUE: dewpoint temperature in degC
## -------------------------------------------------------------------------
## other functions needed:
## -------------------------------------------------------------------------
## DETAILS: Formulation from Dosseger et al. 1992. Formula 19 in MCH document.
## -------------------------------------------------------------------------
## Author: Ana Casanueva, Date: 19.10.2016


##################################################
##################################################
# Constants (see Dosseger et al. 1992)
c1 <- 0.06107
a1 <- 17.368
b1 <- 238.83
c2 <- 0.06108
a2 <- 17.856
b2 <- 245.52
T0 <- 0

# units
hurs[hurs>100] <- 100 # some SMN stations with hurs >100
hurs <- hurs/100

iceMask <- which(tas<T0)
waterMask <- which(tas>=T0)
dewp <- rep(NA,length(tas))

dewp[waterMask] <- b1 * log(hurs[waterMask] * c1 *100 *exp((a1*tas[waterMask])/(b1+tas[waterMask])) / (c1 * 100)) / (a1 - log(hurs[waterMask] * c1 *100 *exp((a1*tas[waterMask])/(b1+tas[waterMask])) / (c1 * 100)))
dewp[iceMask] <- b2 * log(hurs[iceMask] * c2 *100 *exp((a2*tas[iceMask])/(b2+tas[iceMask])) / (c2 * 100)) / (a2 - log(hurs[iceMask] * c2 *100 *exp((a2*tas[iceMask])/(b2+tas[iceMask])) / (c2 * 100)))


return(dewp) 
}
