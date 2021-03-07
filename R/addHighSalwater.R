#' function to calculate the volume of high salinity water to add to head tank
#' @param initVol intial volume of water in the  head tank (L)
#' @param initVol intial salinity of water in the  head tank (ppt)
#' @param targetVol target final of water in the  head tank (L)
#' @param targetS target final salinitiy of water in the head tank (ppt)
#' @param highSwater salinity of high salinity water that will be added to the tank (ppt)
#' @return A list of salt weights (g) and volumes of water to add to tank (L)
#' #' \itemize{
#'   \item targetSgram - target salt weight (g) (used for internal calculation)
#'   \item initSgram - initial salt weight (g) (used for internal calculation)
#'   \item addSgram - salt weight to add (g) (used for internal calculation)
#'   \item addHighSwaterVol - volume of high salinity water to add to the tank (L)
#'   \item addDIwaterVol - volume of DI water to add to the tank (L)
#' }
addHighSalwater <- function(initVol, initS, targetVol, targetS, highSwater){
  # these these estimation calculation assume psu ~ ppt
  # target final total grams of salt in tank
  targetSgram <- targetVol * targetS
  # current grams of salt in tank
  initSgram <- initVol * initS
  # grams of salt that need to be added to the tank
  addSgram <- targetSgram - initSgram
  #volume of high salinity seawater (L) that needs to added to the tank
  addHighSwaterVol <- addSgram / highSwater
  # add DI water volume to fill the tank (L)
  addDIwaterVol <- targetVol - addHighSwaterVol - initVol
  # salinity of the water added
  addS <- addSgram / (targetVol - initVol)
  #return list of head tank calculation values
  return(list(targetSgram = targetSgram,
              initSgram = initSgram,
              addSgram = addSgram,
              addHighSwaterVol = addHighSwaterVol,
              addDIwaterVol = addDIwaterVol,
              addS = addS))
}
