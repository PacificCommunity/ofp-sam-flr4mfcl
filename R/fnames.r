#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


#lfile <- '/media/sf_assessments/yft/2023/model_runs/diagnostic/labels.tmp'

#' labels
#'
#' read in fishery labels from a labels.tmp file
#'
#' @param lfile character string: path to labels.tmp file
#'
#' @return A data.frame of fishery names.
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods


labels <- function(lfile){
  ll <-strsplit(readLines(lfile), split="[.]+")
  ll <- ll[unlist(lapply(ll, length))==2]
  names <- data.frame(fishery=as.numeric(unlist(lapply(ll, el, 1))),
                      names  =as.character(unlist(lapply(ll, function(x){paste(x[-1], collapse="-")}))))
  
  return(names)
}









#' fnames
#'
#' Add fishery names to an MFCL object
#'
#' @param object An object of class MFCLX.
#' @param names  An object of either class vector or class data.frame.
#'
#' @return An object of MFCLX.
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods

setGeneric('fnames', function(object, ...) standardGeneric('fnames')) 


#' @aliases fnames

setMethod("fnames", signature(object="MFCLLenFit"), 
          function(object, names, ...){
            #browser()
            if(is.vector(names) & length(names)!=length(unique(lenfits(object)$fishery)))
              stop("length of names vector not equal to number of fisheries")
            if(is.data.frame(names)) 
               if(!any(is.element(colnames(names), "fishery")) | !any(is.element(colnames(names), "names")))
                 stop("names dataframe must have colums 'fishery' and 'name' ")
            
            if(is.vector(names))
              names <- data.frame(fishery=uniqe(lenfits(object)$fishery), names=names)
            
            lenfits(object)    <- merge(lenfits(object), names)
            lenagefits(object) <- merge(lenagefits(object), names)
            
            return(object)
          })



setMethod("fnames", signature(object="MFCLWgtFit"), 
          function(object, names, ...){
            #browser()
            if(is.vector(names) & length(names)!=length(unique(wgtfits(object)$fishery)))
              stop("length of names vector not equal to number of fisheries")
            if(is.data.frame(names)) 
              if(!any(is.element(colnames(names), "fishery")) | !any(is.element(colnames(names), "names")))
                stop("names dataframe must have colums 'fishery' and 'name' ")
            
            if(is.vector(names))
              names <- data.frame(fishery=uniqe(wgtfits(object)$fishery), names=names)
            
            wgtfits(object)    <- merge(wgtfits(object), names)
            wgtagefits(object) <- merge(wgtagefits(object), names)
            
            return(object)
          })
