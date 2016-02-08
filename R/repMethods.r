#' SBF0
#'
#' Calculates SB/SBF0 from a .rep object for a given year range. 
#' the 'latest' year is taken as the last year in the years range
#'
#' @param rep.obj:    An object of class MFCLRep.
#' @param year:       The year or years for which to calculate SBF0
#' @param sbf0.years: The year range to calculate average no_fishing_biomass 
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return An FLQuant object with annual SB/SBF0 values.
#' 
#' @seealso \code{\link{read.MFCLRep}}
#' 
#' @export
#' @docType methods
#' @rdname rep-methods
#'



setGeneric('SBSBF0', function(rep.obj, year, sbf0.years, ...) standardGeneric('SBSBF0')) 

#' @rdname rep-methods
#' @aliases SBSBF0

setMethod("SBSBF0", signature(rep.obj="MFCLRep", year="numeric", sbf0.years="numeric"), 
          function(rep.obj, year, sbf0.years=2002:2011, ...){
            
            if(!all(dim(adultBiomass(rep.obj))==dim(adultBiomass_nofish(rep.obj))))
              stop("Dimensions of adultBiomass and adultBiomass_nofish do not match")
            if(any(year>range(rep.obj)['maxyear'])|any(sbf0.years>range(rep.obj)['maxyear']) | 
               any(year<range(rep.obj)['minyear'])|any(sbf0.years<range(rep.obj)['minyear']))
              stop("Year ranges incompatible with MFCLRep object")
            
            ab     <- apply(trim(adultBiomass(rep.obj), year=year), c(2,4), mean)
            abnf   <- apply(trim(adultBiomass_nofish(rep.obj), year=sbf0.years), 4, mean)
            
            sbsbf0 <- suppressWarnings(apply(sweep(ab, c(2,4), abnf, '/'),2,mean))
            
            return(sbsbf0)
          })



setGeneric('SBF0', function(rep.obj, sbf0.years, ...) standardGeneric('SBF0')) 

#' @rdname rep-methods
#' @aliases SBSBF0

setMethod("SBF0", signature(rep.obj="MFCLRep", sbf0.years="numeric"), 
          function(rep.obj, sbf0.years=2002:2011, ...){
            
            if(!all(dim(adultBiomass(rep.obj))==dim(adultBiomass_nofish(rep.obj))))
              stop("Dimensions of adultBiomass and adultBiomass_nofish do not match")
            
            abnf   <- apply(apply(trim(adultBiomass_nofish(rep.obj), year=sbf0.years), c(2,4), sum), 4, mean)
            
            return(mean(abnf))
          })



