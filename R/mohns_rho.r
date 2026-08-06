#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' mohns_rho
#'
#' Calculate mohns rho for retrospective analyses
#'
#' @param obj An object of class list containing either the rep files from the final assessment and each retrospective peel,
#' or an object of class FLQuants containing teh estimated depletion values for each retrospective peel.
#'
#' @param depletion_method The method for calculating depletion (default SBSBF0). Obsolte if passing an FLQuants object with 
#' pre-calculated depletion.
#'
#' @return A numeric value of mohn's rho.
#' 
#' @seealso \code{\link{SBSBF0}} and \code{\link{MFCLRep}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' \dontrun{
#' mohns_rho(retros)
#' }

setGeneric('mohns_rho', function(object, ...) standardGeneric('mohns_rho')) 

#' @rdname mfcl-methods
#' @aliases mfcl

setMethod("mohns_rho", signature(object="list"), function(object, depletion_method='SBSBF0', ...){
  
  dep <- FLQuants(mcf(lapply(object, depletion_method)))

  terminalyrs <- unlist(lapply(dep, function(x){max(as.numeric(dimnames(x)$year[!is.na(x)]))}))
  terminalyrs <- terminalyrs[order(terminalyrs)]

  final <- names(terminalyrs[length(terminalyrs)])

  bias <- NULL
  for(rr in names(terminalyrs)[-length(terminalyrs)])
    bias <- c(bias, (dep[[rr]][,as.character(terminalyrs[rr])] - dep[[final]][,as.character(terminalyrs[rr])]) / dep[[final]][,as.character(terminalyrs[rr])])

  mohnrho <- mean(bias)

  return(mohnrho)

})




setMethod("mohns_rho", signature(object='FLQuants'), 
          function(object,  ...){
  
  retroyrs <- unlist(lapply(object, function(x){max(as.integer(dimnames(x)$year))}))
  retroyrs <- sort(retroyrs, decreasing=TRUE)
  
  thetasum <- 0
  for(pp in 2:length(retroyrs))
    thetasum = thetasum + (object[[pp]][,as.character(retroyrs[pp])] - object[[1]][,as.character(retroyrs[pp])]) / object[[1]][,as.character(retroyrs[pp])]
 
  rho = thetasum/(length(retroyrs)-1)
  return(as.numeric(rho))
})




#load('/media/penguin/skj/2022/Assessment/Model_runs/Grid2022/retrodata.Rdata')

#pfunretro <- function(x,y,...){
#  panel.xyplot(x,y,...)
#  panel.abline(h=0.2, lty=2)
#}
#xyplot(data~year, group=qname, data=FLQuants(mcf(lapply(rep_retro_list, SBSBF0))), type="l", ylim=c(0,1), panel=pfunretro)

#mohns_rho(rep_retro_list[-1], 'SBSBF0')
