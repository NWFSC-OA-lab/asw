
#' Create Add Acid Look Up Table
#'
#' @param pCO2 Seawater pCO2 (uatm)
#' @param salinityTarget Salinity target of final solution (psu)
#' @param temperatureTarget Temperature target for final solution (degrees C)
#' @param acidConc  HCl concentration (Normality)
#' @param targetAlk Alkalinity target for the final solution (umol/kg)
#' @param minVolRatio LUT parameter. min(volume of final solution / initial
#'   volume)
#' @param maxVolRatio LUT parameter. max(volume of final solution / initial
#'   volume)
#' @param byVolRatio LUT parameter. Table increment for volume ratio.
#' @param minInitPH LUT parameter. min(initial solution pH)
#' @param maxInitPH LUT parameter. max(initial solution pH)
#' @param byInitPH LUT parameter. Table increment for initial pH
#' @param minInitSal LUT parameter. min(initial solution salinity). psu.
#' @param maxInitSal LUT parameter. max(initial solution salinity). psu.
#' @param byInitSal LUT parameter. Table increment for initial salinity
#' @param minInitTemp LUT parameter. min(initial solution temperature). degrees
#'   C
#' @param maxInitTemp LUT parameter. max(initial solution temperature). degrees
#'   C
#' @param byInitTemp LUT parameter. Table increment for initial temperature.
#'
#' @return Add Acid look table. The addAcidPerLiter column is the amount of HCl
#'   (ml) at the specified normality to add per liter of salt water added to the
#'   initial solution.
#' @export

acid_add_lut <- function(pCO2, salinityTarget, temperatureTarget, acidConc, targetAlk,
                         minVolRatio = 2, maxVolRatio = 10, byVolRatio = 2,
                         minInitPH = 7.5, maxInitPH = 8.5, byInitPH = 0.25,
                         minInitSal = 20, maxInitSal = 40, byInitSal = 10,
                         minInitTemp = 15, maxInitTemp = 30, byInitTemp = 5) {


  volRatio <- seq(minVolRatio, maxVolRatio, by = byVolRatio)
  initPH <- seq(minInitPH, maxInitPH, by = byInitPH)
  initSalinity <- seq(minInitSal, maxInitSal, by = byInitSal)
  initTemperature <- seq(minInitTemp, maxInitTemp, by = byInitTemp)

  d <- NULL
  c = 0
  for(h in 1:length(volRatio)){
    for(i in 1:length(initPH)){
      for(j in 1:length(initSalinity)){
        for(k in 1:length(initTemperature)){
          #Note that the high salinity value is irrelevant for this application
          aa <- NULL
          try(aa <- addAcid(initVol = 1, initS = initSalinity[j], initT = initTemperature[k],
                            initPH = initPH[i], targetVol = volRatio[h], targetS = salinityTarget,
                            targetT = temperatureTarget, targetAlk = targetAlk,
                            acidConc = acidConc, pCO2 = pCO2, highSwater = 60), silent = TRUE)
          if(is.null(aa)){
            aTotal <- NA
          } else {
            aTotal <- aa$totalAcid / (volRatio[h]-1)
          }
          dTemp <- data.frame(initPH = initPH[i], initSalinity = initSalinity[j],
                              initTemperature = initTemperature[k], volRatio = volRatio[h],
                              addAcidPerLiter = aTotal)
          d <- rbind(d, dTemp)
          c <- c + 1
          print(paste("Row ", c, ": volRatio = ", volRatio[h],
                "; initPH = ", initPH[i],
                "; initSalinity = ", initSalinity[j],
                "; initTemp = ", initTemperature[k], sep = ""))
        }
      }
    }
  }
  return(d)
}
