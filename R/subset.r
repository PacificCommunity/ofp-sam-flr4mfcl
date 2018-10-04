#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' subset
#'
#' subsets a freq(frq) for specific groupings
#' 
#'
#' @param x:    An object of class MFCLFrq.
#'
#' @param ... Additional argument list 
#'
#' @return A data.frame of the selected information.
#' 
#' 
#' @export
#' @docType methods
#' @rdname subset-methods
#'
#' @examples
#' 

setGeneric('subset', function(x, ...) standardGeneric('subset')) 



#' @rdname subset-methods
#' @aliases subset

setMethod("subset", signature(x="MFCLFrq"), 
          function(x, ...){
            
            args <- list(...)
            
            res <- freq(x)
            
            for(ii in 1:length(args)){
              #res <- res[res[names(args)[ii]] == args[ii],]
              res <- res[is.element(res[,names(args[ii])], args[[ii]]),]
            }
            
            return(res)
          })

