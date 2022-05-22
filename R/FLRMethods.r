#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott




#' @rdname rep-methods
#' @aliases ssb

setMethod("ssb", signature(object="MFCLRep"), 
          function(object, ...){
            
            return(adultBiomass(object))
            
          })


setMethod("rec", signature(object="MFCLRep"), 
          function(object, ...){
            
            return(popN(object)[1,])
          })

