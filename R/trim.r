#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' trim
#'
#' Trim MFCL objects using named dimensions
#'
#' @param x An object of class MFCL*.
#'
#' @param ... Additional argument list that might not ever be used.
#'
#' @return An updated object of the same class.
#' 
#' @seealso \code{\link{MFCLFrq}}, \code{\link{MFCLPar}} and \code{\link{MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' \dontrun{
#' trim(MFCLFrq(), year=1990:1995)
#' }
#'
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


#' @rdname mfcl-methods
#' @aliases mfcl

setMethod("trim", signature(x="MFCLRep"), function(x, ...){

  args <- list(...)
  # Add warning if not area or unit
  if (!(all(names(args) %in% c("area", "unit")))){
    warning("trim() for MFCLRep only works 'area' and 'unit' dimensions. Other dimensions are ignored")
  }

  unit_trim <- args[["unit"]]
  area_trim <- args[["area"]]

  flqslots <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
  flqdims <- sapply(flqslots, function(sn) dim(slot(x, sn)))

  # Get list of slots that need area trim
  area_slots <- names(which(flqdims[5,] > 1))
  # Apply trim in for loop
  for(flqname in area_slots){
    slot(x, flqname) <- trim(slot(x, flqname), area=area_trim)
  }


  # Get list of slots that need unit trim
  unit_slots <- names(which(flqdims[3,] > 1))
  # Drop eq_biomass and eq_yield as they are weird and potentially wrong
  unit_slots <- unit_slots[!(unit_slots %in% c("eq_biomass", "eq_yield"))]
  # Apply trim in for loop
  for(flqname in unit_slots){
    slot(x, flqname) <- trim(slot(x, flqname), unit=unit_trim)
  }

  return(x)
            
})




