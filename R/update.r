#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' update
#'
#' Update MFCL objects with new values
#'
#' @param obj An object of class MFCL*.
#'
#' @param ... Additional argument list that might not ever be used.
#'
#' @return An updated object of the same class.
#' 
#' @seealso \code{\link{read.MFCLFrq}} and \code{\link{read.MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' \dontrun{
#' update(MFCLFrq())
#' }
#'
#' @aliases mfcl

#setGeneric('update', function(object, ...) standardGeneric('update'))

setMethod("update", signature(object="MFCLFrq"), function(object, years, fisheries, multiplier, quantity, ...){
            
  if(length(multiplier)>1)
    if(length(multiplier)!=length(fisheries))
      warning("You have different length vectors for fisheries and scalers")
  
  if(length(multiplier)==1 & length(fisheries)>1)
    multiplier <- rep(multiplier, length(fisheries))
  
  for(ff in fisheries){
    freq(object)[is.element(freq(object)$year, years) & is.element(freq(object)$fishery, ff) & freq(object)[,quantity]!=-1, quantity] <- 
      freq(object)[is.element(freq(object)$year, years) & is.element(freq(object)$fishery, ff) & freq(object)[,quantity]!=-1, quantity] * multiplier[which(fisheries==ff)]
  }
  
  return(object)
})


setMethod("update", signature(object="MFCLIni"), function(object, tag.rel.grps, ...){
  
  #  if(!is(tag.obj)=="MFCLTag")
  #    stop("Error: tag.obj must be a valid MFCLTag object")
  
  #  if(dim(tag_fish_rep_rate(object))[1] < release_groups(tag.obj))
  #    stop("Error: Tag release groups in MFCLTag object greater than MFCLIni object")
  
  #  trg   <- release_groups(tag.obj) 
  #  last <- dim(tag_fish_rep_rate(object))[1] 
  if(length(tag.rel.grps)>0){
    tag_fish_rep_rate(object)  <- tag_fish_rep_rate(object)[-tag.rel.grps, ]
    tag_fish_rep_grp(object)   <- tag_fish_rep_grp(object)[-tag.rel.grps, ]
    tag_fish_rep_flags(object) <- tag_fish_rep_flags(object)[-tag.rel.grps, ]
    tag_fish_rep_target(object)<- tag_fish_rep_target(object)[-tag.rel.grps, ]
    tag_fish_rep_pen(object)   <- tag_fish_rep_pen(object)[-tag.rel.grps, ]
  }
  return(object)
  
})


# old update ini method
#setMethod("update", signature(object="MFCLIni"), function(object, tag.rel.grps, ...){
#  if(length(tag.rel.grps)>0){
#    tag_fish_rep_rate(object)  <- tag_fish_rep_rate(object)[-tag.rel.grps, ]
#    tag_fish_rep_grp(object)   <- tag_fish_rep_grp(object)[-tag.rel.grps, ]
#    tag_fish_rep_flags(object) <- tag_fish_rep_flags(object)[-tag.rel.grps, ]
#    tag_fish_rep_target(object)<- tag_fish_rep_target(object)[-tag.rel.grps, ]
#    tag_fish_rep_pen(object)   <- tag_fish_rep_pen(object)[-tag.rel.grps, ]
#  }
#  return(object)
#})


