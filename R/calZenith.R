#' Calculate zenith angle in degrees.
#' 
#' Calculate zenith angle in degrees.
#' 
#' @param date date in the format yyyy-mm-dd (or 'yyyy-mm-dd HH:MM:SS'). For daily data, the hour 12 UTC is assumed.
#' @param lon value for the longitude of the location.
#' @param lat value for the latitude of the location.
#' @param hour logical. If TRUE, allow working with hourly data. Default: FALSE (12 UTC is used).
#' 
#' @return  zenith angle in degrees
#' @author Anke Duguay-Tetzlaff, Translated to R by Ana Casanueva (17.01.2017)
#' @export
#' 
#' @examples \dontrun{ 
#' calZenith("1981-06-15",  -5.66, 40.96)
#' calZenith("1981-06-15 10:00:00",  -5.66, 40.96, hour=TRUE)
#' }
#' 
calZenith <- function(date,lon,lat, hour=FALSE){
  
  # Internal constants used for conversion 
  EQTIME1 <- 229.18
  EQTIME2 <- 0.000075
  EQTIME3 <- 0.001868
  EQTIME4 <- 0.032077
  EQTIME5 <- 0.014615
  EQTIME6 <- 0.040849
  
  DECL1 <- 0.006918
  DECL2 <- 0.399912
  DECL3 <- 0.070257
  DECL4 <- 0.006758
  DECL5 <- 0.000907
  DECL6 <- 0.002697
  DECL7 <- 0.00148
 
  # Translate from date to utc.hour and year. If daily, set time to 12.
  if(hour){
    d0<- strftime(date, format = "%Y-%m-%d %H:%M:%S", usetz=TRUE, tz="UTC") 
    d1 <- strptime(d0, format = "%Y-%m-%d %H:%M:%S", tz="UTC") # these lines are need to avoid problems with 00:00:00
    utc.hour <- as.numeric(format(d1,'%H'))
  } else {
    d1 <- strptime(date, format = "%Y-%m-%d")
    utc.hour <- 12
  }
  year <- as.numeric(format(d1,'%Y'))
  
  # Translate from date to doy
  doy <- as.numeric(strftime(d1, format = "%j"))

  # Number of day per year (check if it is leap year)
  if (is.leapyear(year)) dpy=366 else dpy=365 
  
  # Evaluate the input lat and lon in radians 
  RadLon <- degToRad(lon) 
  RadLat <- degToRad(lat)
  
  # Evaluate the fractional year in radians 
  Gamma <-  2*pi*((doy-1)+(utc.hour/24))/dpy
  
  # Evaluate the Equation of time in minutes 
  EquTime <- EQTIME1*(EQTIME2+EQTIME3*cos(Gamma)-EQTIME4*sin(Gamma)- EQTIME5*cos(2*Gamma)-EQTIME6 * sin(2*Gamma))
  
  # Evaluate the solar declination angle in radians (must be between -23.5 and 23.5 degrees)
  Decli <- DECL1-DECL2*cos(Gamma)+DECL3*sin(Gamma)- DECL4*cos(2*Gamma)+DECL5 * sin(2*Gamma)- DECL6 * cos(3*Gamma) + DECL7 * sin(3*Gamma)
  
  #Time offset in minutes (needed only for satellites)
  # --> convention: Longitude < 0 => westwards, Longitude > 0 => Eastwards
  # --> 1° east or west <==> +/- 4 min
  #    TimeOffset  = EquTime + (4.*Lon)
  TimeOffset <- 0
  
  # True solar time in minutes 
  TrueSolarTime <- (utc.hour*60)+TimeOffset
  
  # Solar hour angle in degrees and in radians 
  HaDeg <- ((TrueSolarTime/4)-180)
  HaRad <- degToRad(HaDeg)
  
  # Calculate the cosin of zenith angle (from lat, declination and hour angle)
  CosZen <- (sin(RadLat)*sin(Decli)+cos(RadLat) * cos(Decli)*cos(HaRad))
  if (CosZen > 1.0)  CosZen <- 1.0
  if (CosZen < -1.0) CosZen <- -1.0
  
  # Calculate the zenith angle
  SZARad <- acos(CosZen)  
  SZA <- radToDeg(SZARad)
  
  return(SZA)
}