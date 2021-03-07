#' Instant Ocean alkaliniy from salinity with only DI water addition at 400 uatm pCO2
#' @description  this function is just a placeholder
#' assuming a linear relationship with alk at 30 psu = 3100 and alk at 35 psu = 3200;
#' need to develop an empircal curve in the range 25-60 psu
#' @param salinity Instant Ocean water salinity (psu)
#' @return alkalinity of Instant Ocean (umol/kg)
alkIO <- function(salinity){
  alk <- salinity * 3200/35
  return(alk)
}
