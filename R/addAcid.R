#'function to determine the amount of acid to add to reach target tank
#'conditions
#'@description starts with a specified volume of water with specified chemistry
#'  conditions in then tank then calculates the amount of acid to add to reach
#'  target volume and chemistry
#'@param initVol intial volume of water in the tank (L)
#'@param initS intial salinity of water in the tank (psu)
#'@param initT intial temperature of water in the tank (degrees C)
#'@param initPH intial pH of water in the tank
#'@param targetVol target volume of water in the tank (L)
#'@param targetS target salinity (psu)
#'@param targetT target temperature (degrees C)
#'@param targetAlk target alkalinity (umol/kg)
#'@param acidConc acid concentration (Normality)
#'@param pCO2 atmospheric pCO2 (uatm)
#'@param highSwater salinity of the high salinty water to be added (psu)
#'@return A list of the amount of acid to add and the final target pH #'
#'  \itemize{
#'  \item totalAcidAddInit - total vol of acid needed to adjust only
#'  the inital water in the tank (ml)
#'  \item aaNewTotal - total vol of acid
#'  needed to adjust only the new water added to the tanks (ml)
#'  \item totalAcid
#'  - total volume of acid to add (= totalAcidAddInit + aaNewTotal) (ml) note:
#'  any of the acid additions could be negative (indcating the need to add
#'  base), however, the need for base should not occur when using instant ocean
#'  to create west coast water
#'  \item finalPH - the target pH for the final
#'  solution (this will the the pH feedback setting) }
#'@export

addAcid <- function(initVol, initS, initT, initPH, targetVol, targetS, targetT,
                    targetAlk, acidConc, pCO2, highSwater){

  # initVol <- 20
  # initS <- 35
  # initT <- 25
  # initPH <- 8
  # targetVol <- 90
  # targetS <- 30
  # targetT <- 25
  # targetAlk <- 3000
  # acidConc <- 1
  # pCO2 <- 450
  # highSwater <- 61

  #convert target alkalinity from umol/kg to mol/kg
  targetAlk <- targetAlk / 1000000

  ####
  #determine how much acid to add to the initial solution to bring it to target alkalinity at target S and T
  #calculate initial alkalinity from pH and pCO2
  initCarb <- seacarb::carb(flag = 21, pCO2, initPH, S = initS, T = initT)
  initAlk <- initCarb$ALK
  initDIC <- initCarb$DIC

  # TODO Add test whether initial pH, pCO2, salinity relationship is feasable
  # given the instant ocean alkalinity vs salinity relationship in alkIO. The
  # value is not possible (given 20% tolerance), return NULL.

  boundsTest <- seq(-0.1, 0.1, by = 0.0001)
  minBound <- 0.1
  maxBound <- -0.1
  for(i in 1:length(boundsTest)){
    bt <- boundsTest[i]
    pph <- NULL
    try(pph <- seacarb::ppH(flag = 15, initAlk, initDIC, sys = 1, pCO2a = pCO2,
                   vol =  bt, N = acidConc, S = targetS, T = targetT),
        silent = TRUE)
    if(!is.null(pph)){
      if(minBound > bt) {
        minBound <- bt
      }
      if(maxBound < bt) {
        maxBound <- bt
      }
    }
  }

  #  return(c(minBound,maxBound))

  #calculate the amount of acid to add to the initial water to reach target
  #alkalinity (liters HCl/kg of seawater)
  acidInit <- stats::optimize(findAlk, c(-maxBound, -minBound), tol = 0.00001,
                       targetAlk = targetAlk, initAlk = initAlk,
                       initDIC = initDIC, pCO2 = pCO2, acidConc = acidConc,
                       targetS = initS, targetT = initT)

  # acid <- 0.0007098496
  # pph <- NULL
  # try(pph <- ppH(flag = 15, initAlk, initDIC, sys = 1, pCO2a = pCO2,
  #   vol =  acid, N = acidConc, S = targetS, T = targetT), silent = TRUE)
  #  testAlk <- (pph$ALK)[2]
  #  deltaSQ  <- (targetAlk - testAlk)^2

  #density of intial seawater (kg/L)
  densityOfInit <- seacarb::rho(S = initS, T = initT) / 1000
  #total volume of acid to add to the initial volume of water (ml)
  totalAcidAddInit <- acidInit$minimum * densityOfInit * initVol * 1000

  ### determine how much acid to add to new water based on Instant Ocean alk vs
  #salinity relationship and the salinity of the water added (ppt~psu)
  addS <- addHighSalwater(initVol = initVol, initS = initS, targetVol = targetVol,
                          targetS = targetS, highSwater = highSwater)$addS
  #get alkalinity of instant ocean at the salinity of new water added to the
  #tank (mol/kg)
  alkIOatTargetS <- alkIO(addS) / 1000000
  #calculate the DIC of the water added at instant ocean alkalinity and ambient
  #pCO2 (this is the initial DIC for the IO water added) (mol/kg)
  addInitDIC <- seacarb::carb(flag = 24, pCO2, alkIOatTargetS, S = targetS, T = targetT)$DIC
  #find the amount of acid (L/kg) to add to reach target alkalinty considering
  #just the water added

  boundsTest <- seq(-0.1, 0.1, by = 0.0001)
  minBound <- 0.1
  maxBound <- -0.1
  for(i in 1:length(boundsTest)){
    bt <- boundsTest[i]
    pph <- NULL
    try(pph <- seacarb::ppH(flag = 15, initAlk, initDIC, sys = 1, pCO2a = pCO2,
                   vol =  bt, N = acidConc, S = targetS, T = targetT),
        silent = TRUE)
    if(!is.null(pph)){
      if(minBound > bt) {
        minBound <- bt
      }
      if(maxBound < bt) {
        maxBound <- bt
      }
    }
  }

  aaNew <- stats::optimize(findAlk, c(-maxBound, -minBound), targetAlk = targetAlk,
                    initAlk = alkIOatTargetS, initDIC = addInitDIC,
                    pCO2 = pCO2, acidConc = acidConc, targetS = targetS,
                    targetT = targetT)
  #volume of water added
  addVol <- targetVol - initVol
  #density of the final seawater
  densityOfFinal <- seacarb::rho(S = targetS, T = targetT) / 1000
  #total volume of acid to add (ml)
  aaNewTotal <- aaNew$minimum * densityOfFinal * addVol * 1000
  # pH of the final solution
  finalPH <- seacarb::carb(flag = 24, pCO2, targetAlk, S = targetS, T = targetT)$pH

  #total combines acid (ml) to adjust initial and add water
  totalAcid <- totalAcidAddInit + aaNewTotal

  #creat output list
  rList <- list(
    totalAcidAddInit = totalAcidAddInit,
    aaNewTotal = aaNewTotal,
    totalAcid = totalAcid,
    finalPH = finalPH )
  #set unit attributes for the output list
  attr(rList$totalAcidAddInit, "unit") <- "ml"
  attr(rList$aaNewTotal, "unit") <- "ml"
  attr(rList$totalAcid, "unit") <- "ml"
  return(rList)
}
