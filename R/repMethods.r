#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

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



setGeneric('SBSBF0', function(rep.obj, ...) standardGeneric('SBSBF0')) 

#' @rdname rep-methods
#' @aliases SBSBF0

setMethod("SBSBF0", signature(rep.obj="MFCLRep"), #, year="numeric", sbf0.years="numeric"), 
          function(rep.obj, year=c(range(rep.obj)['maxyear']), sbf0.years=c(range(rep.obj)['maxyear']-10:1), rolling=FALSE, ...){
            
            if(!all(dim(adultBiomass(rep.obj))==dim(adultBiomass_nofish(rep.obj))))
              stop("Dimensions of adultBiomass and adultBiomass_nofish do not match")
            if(any(year>range(rep.obj)['maxyear'])|any(sbf0.years>range(rep.obj)['maxyear']) | 
               any(year<range(rep.obj)['minyear'])|any(sbf0.years<range(rep.obj)['minyear']))
              stop("Year ranges incompatible with MFCLRep object")
            
            #ab     <- apply(trim(adultBiomass(rep.obj), year=year), c(2,4), mean)
            #abnf   <- apply(trim(adultBiomass_nofish(rep.obj), year=sbf0.years), 4, mean)
            #sbsbf0 <- apply(sweep(ab, c(2,4), abnf, '/'),2,mean)
            
            abnf    <- SBF0(rep.obj, year=year, sbf0.years=sbf0.years, rolling=rolling)
            
            if(!rolling)
              sbsbf0  <- areaSums(seasonMeans(trim(adultBiomass(rep.obj), year=year)))/abnf
            
            if(rolling)
              sbsbf0 <- trim(adultBiomass(rep.obj), year=min(as.numeric(dimnames(abnf)$year)):max(as.numeric(dimnames(abnf)$year)))/abnf
            
            return(sbsbf0)
          })



setGeneric('SBF0', function(rep.obj, ...) standardGeneric('SBF0')) 

#' @rdname rep-methods
#' @aliases SBF0

setMethod("SBF0", signature(rep.obj="MFCLRep"), 
          function(rep.obj, sbf0.years=c(range(rep.obj)['maxyear']-10:1), rolling=FALSE, ...){
            
            if(!all(dim(adultBiomass(rep.obj))==dim(adultBiomass_nofish(rep.obj))))
              stop("Dimensions of adultBiomass and adultBiomass_nofish do not match")
            
            abnf   <- yearMeans(areaSums(seasonMeans(trim(adultBiomass_nofish(rep.obj), year=sbf0.years))))
            
            if(rolling){
              abnf <- lapply((range(rep.obj)['minyear']+length(sbf0.years)):range(rep.obj)['maxyear'], 
                             function(x){yearMeans(trim(adultBiomass_nofish(rep.obj), year=(x-10:1)))})
              abnf <- FLQuant(aperm(array(unlist(abnf), c(1,1,4,5,47)), c(1,5,2,3,4)), 
                              dimnames=dimnames(trim(adultBiomass_nofish(rep.obj),
                                                     year=(range(rep.obj)['minyear']+length(sbf0.years)):range(rep.obj)['maxyear'])))
            }
            
            return(abnf)
          })



