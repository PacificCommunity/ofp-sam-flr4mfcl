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



setMethod("update", signature(object="MFCLFrq"), function(object, years, fisheries, multiplier, quantity, ...){
            
  freq(object)[is.element(freq(object)$year, years) & is.element(freq(object)$fishery, fisheries) & freq(object)[,quantity]!=-1, quantity] <- 
      freq(object)[is.element(freq(object)$year, years) & is.element(freq(object)$fishery, fisheries) & freq(object)[,quantity]!=-1, quantity]*multiplier
  
  return(object)
            
})




setMethod("update", signature(object="MFCLIni"), function(object, tag.obj, ...){
  
  if(!is(tag.obj)=="MFCLTag")
    stop("Error: tag.obj must be a valid MFCLTag object")
  
  if(dim(tag_fish_rep_rate(object))[1] < release_groups(tag.obj))
    stop("Error: Tag release groups in MFCLTag object greater than MFCLIni object")
  
  trg   <- release_groups(tag.obj) 
  last <- dim(tag_fish_rep_rate(object))[1] 
  
  tag_fish_rep_rate(object)  <- tag_fish_rep_rate(object)[c(1:trg,last), ]
  tag_fish_rep_grp(object)   <- tag_fish_rep_grp(object)[c(1:trg,last), ]
  tag_fish_rep_flags(object) <- tag_fish_rep_flags(object)[c(1:trg,last), ]
  tag_fish_rep_target(object)<- tag_fish_rep_target(object)[c(1:trg,last),]
  tag_fish_rep_pen(object)   <- tag_fish_rep_pen(object)[c(1:trg,last), ]
  
  return(object)
  
})


