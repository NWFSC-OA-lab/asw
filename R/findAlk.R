#' function to minimize to determine the amount of acid to add for target alk and dic
#' @description  used as input function for optimize() in addAcid()
#' @param acid volume of acid added (L); this is the optimization parameter
#' @param targetAlk target alkalinity (mol/kg)
#' @param initAlk intial alkalinity (mol/kg)
#' @param initDIC intial DIC (mol/kg)
#' @param pCO2 atmospheric pCO2 (uatm)
#' @param acidConc acid concentration (N)
#' @param targetS target salinity (psu)
#' @param targetT target temperature (degrees C)
#' @return squared difference between target alkalinty and alkalinity with particular input parameters
findAlk <- function(acid, targetAlk, initAlk, initDIC, pCO2, acidConc, targetS, targetT){
  deltaSQ <- .Machine$double.xmax
  acid <- -1 * acid
  pph <- NULL
  try(pph <- seacarb::ppH(flag = 15, initAlk, initDIC, sys = 1, pCO2a = pCO2,
                 vol =  acid, N = acidConc, S = targetS, T = targetT),
      silent = TRUE)
  if(!is.null(pph)){
    testAlk <- (pph$ALK)[2]
    deltaSQ  <- (targetAlk - testAlk)^2
  }
  return(deltaSQ)
}
