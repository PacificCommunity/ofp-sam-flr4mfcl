#' update
#'
#' Update MFCL objects with new values
#'
#' @param obj:    An object of class MFCLX.
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return An updated object of the same class
#' 
#' @seealso \code{\link{read.MFCLFrq}} and \code{\link{read.MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' update(MFCLFrq())



#setGeneric('update', function(object, ...) standardGeneric('update')) 

#' @rdname mfcl-methods
#' @aliases mfcl



setMethod("update", signature(object="MFCLFrq"), 
          function(object, years, multiplier, quantity, ...){
            
            freq(object)[is.element(freq(object)$year, years) & freq(object)[,quantity]!=-1, quantity] <- 
              freq(object)[is.element(freq(object)$year, years) & freq(object)[,quantity]!=-1, quantity]*multiplier
            
            
          })


