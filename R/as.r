#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' as.MFCLIni
#'
#' Convert an MFCLPar object to an MFCLIni object
#'
#' @param object:    An object of class MFCLPar
#'
#'
#' @return An MFCLIni object.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('as.MFCLIni', function(object, ...) standardGeneric('as.MFCLIni')) 


setMethod("as.MFCLIni", signature(object="MFCLPar"),
          function(object, ...){
            
          res <- MFCLIni()
          for(ss in c(names(getSlots("MFCLBiol")), names(getSlots("MFCLBase")), names(getSlots("MFCLRegion")), names(getSlots("MFCLTagRep"))))
            slot(res, ss) <- slot(object, ss)
          
          age_pars(res)         <- matrix(0, nrow=10, ncol=dimensions(object)['agecls'])
          sv(res)               <- steepness(object)
          rec_dist(res)         <- rep(1/dimensions(object)['regions'], dimensions(object)['regions'])
          sd_length_at_age(res) <- c(5,1,9)
          sd_length_dep(res)    <- c(0.5,0,3)
          
          return(res)
          })




