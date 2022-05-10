#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

# Catch in weight from catch in numbers

#' catchWfromcatchN
#'
#' Calculates the catch in weight from MFCL projected numbers at age and selection patterns.
#' Steps are:
#' 1 - from the catcheff data frame you know what total catches should be (in number)
#' 2 - use the selection params in the rep file to calculate vulnerable numbers
#' 3 - use the selection and weight at age params in the rep file to calculate vulnerable biomass
#' 4 - catch weight = vulB * (CatchN / vulN).
#'
#' @param pseudo An object of class MFCLPseudo,
#' @param projpar An object of class MFCLPar,
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return Returns a data frame of the converted catch in weight.
#' 
#' 
#'
#' @examples
#' #checkUnitDimnames(MFCLRep())


# projN   <- read.MFCLProjectedNatAge('projected_numbers_at_age', fyear=1952)
# projrep <- read.MFCLRep('plot-stoch_proj.rep')

catchWfromcatchN <- function(pseudo, projN, rep, ctrl, fsh, itn, ...){
  
  # using selection and Vulnerable N 
  # get pseudo catches in number from catcheff(pseudo)
  rgn     <- controls(ctrl)$region[fsh]
  
  catcheff(pseudo) <- cbind(catcheff(pseudo), area='unique')
  colnames(catcheff(pseudo))[c(2,4,5)] <- c('season','unit','data')
  pseudoN <- as.FLQuant(catcheff(pseudo)[,c('year','season','unit','area','iter', 'data')])
  
  yrs     <- as.character(min(catcheff(pseudo)$year):max(catcheff(pseudo)$year))
  pseudoN <- pseudoN[,yrs,as.character(fsh),,,as.character(itn)]
  
  # calculate vulnerable biomass and determine catch weight from ratio of catch numbers (pseudo vs vulnerable)
  vulN    <- quantSums(sweep(projN[,,,,rgn,itn], 1, sel(rep)[,,fsh,], '*' ))
  vulB    <- quantSums(sweep(projN[,,,,rgn,itn], 1, sel(rep)[,,fsh,] * c(aperm(mean_waa(rep), c(4,1,2,3,5,6))), '*'))
  
  pseudoW <- vulB[,yrs,] * pseudoN/vulN[,yrs,]
  
  return(pseudoW)
}


# plot(c(qts(pseudoN)), c(qts(pseudoW)/1000))












