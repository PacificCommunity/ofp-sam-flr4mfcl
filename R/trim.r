#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' trim
#'
#' trim MFCL objects using named dimensions
#'
#' @param x:    An object of class MFCLX.
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return An updated object of the same class
#' 
#' @seealso \code{\link{MFCLFrq}},  \code{\link{MFCLPar}} and \code{\link{MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' trim(MFCLFrq(), year=1990:1995)




#' @rdname mfcl-methods
#' @aliases mfcl


setMethod("trim", signature(x="MFCLFrqStats"), function(x, ...){
  
  args <- list(...)
  
  c1 <- args[[quant(region_size(x))]]
  c2 <- args[["year"]]
  c3 <- args[["unit"]]
  c4 <- args[["season"]]
  c5 <- args[["area"]]
  c6 <- args[["iter"]]
  
  names <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
  
  # FLQuants in MFCLFrq
  for(name in names)
    slot(x, name) <- trim(slot(x, name), ...)
  
  # Non FLQuants in MFCLFrq
  
  
  return(x)
            
})





