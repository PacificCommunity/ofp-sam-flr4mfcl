#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' trim
#'
#' Trim MFCL objects using named dimensions
#'
#' @description Trims an object to a new set of smaller dimensions eg. a reduced year range.
#' 
#' @param x An object of class MFCL*.
#'
#' @param ... Additional argument list that might not ever be used.
#'
#' @return An updated object of the same class trimmed to the new dimensions.
#' 
#' @details The dimensions over which an object can be trimmed will depend on the object itself. 
#' For example an FLQuant object can be trimmed over the 6 dimensions of an FLQuant (quant/age, year, unit, season, area, iter).
#' Objects having additional factor levels may allow additional functionality for 'trim'. For example MFCLLenFit() objects 
#' ma be trimmed on age, length, fishery, year and/or month. Note that where MFCLLenfit objects are trimmed on both age and length
#' the most restrictive condition is applied.
#' 
#' @seealso \code{\link{MFCLFrq}}, \code{\link{MFCLPar}} and \code{\link{MFCLPar} and \code{\link{MFCLLenFit}}}
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
#' @aliases trim

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



setMethod("trim", signature(x="MFCLLenFit"), function(x, ...){
  #browser()
  args      <- list(...)
  argnames  <- names(args)
  
  if(!is.element("age", argnames))
    args$age <- sort(unique(lenagefits(x)$age))
  if(!is.element("length", argnames))
    args$length <- seq(0, max(lenfits(x)$length))
  if(!is.element("fishery", argnames))
    args$fishery <- sort(unique(lenfits(x)$fishery))
  if(!is.element("year", argnames))
    args$year <- sort(unique(lenfits(x)$year))
  if(!is.element("month", argnames))
    args$month <- sort(unique(lenfits(x)$month))
  
  # Add warning if not correct trim call
  if(!(all(names(args) %in% c("age", "length", "fishery", "year", "month")))){
    warning("trim() for MFCLLenFit only works on 'age', 'length', 'fishery', 'year' and 'month'. Other dimensions are ignored")
  }
  
  obj <- x
  
  # trim laa for age first and then length
  slot(obj, 'laa')  <- trim(laa(obj), age=args[["age"]])
  slot(obj, 'laa')  <- laa(obj)[floor(laa(obj))%in%args[["length"]]]
  
  # trim lenfits
  slot(obj, 'lenfits')    <- subset(lenfits(obj), fishery%in%args[['fishery']] & year%in%args[['year']] & month%in%args[['month']] & 
                                      length%in%args[['length']])
  # trim lenagefits if present
  if(nrow(lenagefits(obj))>1)
    slot(obj, 'lenagefits') <- subset(lenagefits(obj), fishery%in%args[['fishery']] & year%in%args[['year']] & month%in%args[['month']] & 
                                        age%in%args[['age']] & length%in%args[['length']])
  
  range(obj) <- c(min=min(as.numeric(dimnames(laa(lfit))$age)), max=max(as.numeric(dimnames(laa(lfit))$age)), plusgroup=NA, 
                  minyear=min(lenfits(obj)$year), maxyear=max(lenfits(obj)$year))
  
  return(obj)
})



