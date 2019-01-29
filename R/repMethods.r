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



#' SBF0Alt
#'
#' Alternative method for calculating mean SBF0 for use in the SB/SBF0 calculation.
#'
#' The method calculates the mean SBF0 over a year range. The year range can be specified in two ways, by either specifying a year range with the \code{years} argument, or via the number of years over which to calculate the mean (the \code{years} argument as a single value) and the number of years between the final year and the last year of the year range (\code{lag}).
#' If rolling is FALSE, the mean is calculated for the year range only. If rolling is TRUE then the year range is relative to the final year and shifts about accordingly.
#'
#' For example, if the final year is 2042, the year range is 2011:2015 and rolling is FALSE, the returned FLQuant has the mean SBF0 over the years 2011:2015 for all years.
#' If rolling is TRUE, then year range 2011:2015 is used to calculate the SBF0 for 2042, the year range 2010:2014 is used for 2041 and so on.
#' If years is a single value of 5, and lag is 10, then the mean SBF0 in 2042 is calculated over the year range 2028:2032 (i.e. over years). If rolling is TRUE, the mean in SBF) in 2041 is calculated over the range 2027:2031 and so on.
#'
#' @param rep    An object of class MFCLRep.
#' @param years  Either a vector of years over which to calculate the mean over, or a single numeric vector giving the number of years over which to calculate the mean.
#' @param lag    If \code{years} is a single numeric vector, the number of years between the final year in the rep file and final year over which to calculate the mean.
#' @param rolling TRUE (a rolling mean, i.e. a moving average) or FALSE (the years over which to calculate the mean are fixed).  Default is TRUE.
#' @param combine_areas A logical argument. Should the areas (regions) be combined. Default is TRUE.
#'
#' @return An FLQuant object with annual mean SBF0 values.
#' 
#' @seealso \code{\link{read.MFCLRep}}
#' 
#' @export
#' @docType methods
#' @rdname SBF0-methods
#' @examples
#' \dontrun{
#' # Mean over a constant year range of 2011:2015
#' SBF0Alt(rep, years=2011:2015, rolling=FALSE)
#' # Mean over rolling year range, with the final year being the mean of 2011:2015, the second to last year being mean od 2010:2014 etc.
#' SBF0Alt(rep, years=2011:2015, rolling=TRUE)
#' # Mean over a constant year range of defined as number of years in the range and number or years from the final year.
#' # e.g. if the final year was 2042, return the mean over 2028:2032
#' SBF0Alt(rep, years=5, lag=10, rolling=FALSE)
#' # Mean over a rolling year range, e.g. value in 2042 is the mean over 2028:2032, value in 2041 is mean over 2027:2031 etc.
#' SBF0Alt(rep, years=5, lag=10, rolling=FALSE)
#' }
#'
setGeneric('SBF0Alt',function(rep, years, lag, ...) standardGeneric('SBF0Alt')) 

# No lag, and years is a year range
# If rolling is FALSE, SBF0 is just the mean of sbf0 over the year range
# If rolling is TRUE, the final year of sbf0 is the mean over the year range and previous years roll back from that.
# i.e. the final year is relative to the mean of years, final year-1 is relative to mean of years-1, etc

#' @rdname SBF0-methods
#' @aliases SBF0Alt
setMethod("SBF0Alt", signature(rep="MFCLRep",years="numeric", lag="missing"),
  function(rep, years, rolling=TRUE, combine_areas=TRUE ){
    # x is a vector of years - check they exist in sbf0
    if (combine_areas){
      sbf0 <- seasonMeans(areaSums(adultBiomass_nofish(rep)))
    }
    if (!combine_areas){
      sbf0 <- seasonMeans(adultBiomass_nofish(rep))
    }
    if(!all(years %in% as.numeric(dimnames(sbf0)$year))){
      stop("years is a vector of years that must exist in the rep file\n")
    }
    out <- sbf0
    out[] <- NA
    # Mean is fixed
    # Return an FLQuant with same values in all years
    if (!rolling){
      out[] <- apply(sbf0[,as.character(years)], c(1,3,4,5,6), mean)
    }
    # Final year is set to mean of x years, and rolled back from there
    if (rolling){
      # Final year is relative to the mean of years
      # Final year  - 1 is relative to mean of years-1, etc
      rolling_mean <- apply(sbf0, c(1,3,4,5,6), function(x, n, sides){filter(c(x),rep(1/n,n), sides=sides)}, n=length(years), sides=1)
      # final year of out becomes final year of years in rolling mean 
      rolling_mean[,as.numeric(dimnames(rolling_mean)$year) <= years[length(years)]]
      out_years <- dimnames(out)$year
      final_year <- out_years[length(out_years)]
      replacement_years <- as.numeric(out_years[1]):as.numeric(years[length(years)])
      replace_years <- (as.numeric(final_year) - years[length(years)]) + replacement_years
      out[,as.character(replace_years)] <- rolling_mean[,as.character(replacement_years)]
    }
    return(out)
  }
)

# years is single value specifying the number of years to average over
# lag is the number of years betwen the last year to take mean over and the final year of the rep object

#' @rdname SBF0-methods
#' @aliases SBF0Alt
setMethod("SBF0Alt", signature(rep="MFCLRep",years="numeric", lag="numeric"),
  function(rep, years, lag, rolling=TRUE, combine_areas=TRUE){
    if (length(years) > 1){
      stop("If supplying a lag, then years is nyears and must be of length 1\n")
    }
    # Get year range based on lag and final year
    final_year <- dimnames(adultBiomass_nofish(rep))$year
    final_year <- as.numeric(final_year[length(final_year)])
    year_range <- (final_year - lag - years + 1):(final_year - lag)
    out <- SBF0Alt(rep=rep, years=year_range, rolling=rolling,combine_areas=combine_areas)
    return(out)
  }
)

#' SBSBF0Alt
#'
#' Alternative method for calculating SB/SBF0 
#'
#' The method calculates the SB/SBF0 over a year range. The options relate to the way the SBF0 is calculated (see the documentation for \code{SBF0Alt}) The year range can be specified in two ways, by either specifying a year range with the \code{years} argument, or via the number of years over which to calculate the mean (the \code{years} argument as a single value) and the number of years between the final year and the last year of the year range (\code{lag}).
#' If rolling is FALSE, the mean is calculated for the year range only. If rolling is TRUE then the year range is relative to the final year and shifts about accordingly.
#'
#' For example, if the final year is 2042, the year range is 2011:2015 and rolling is FALSE, the returned FLQuant has the mean SBF0 over the years 2011:2015 for all years.
#' If rolling is TRUE, then year range 2011:2015 is used to calculate the SBF0 for 2042, the year range 2010:2014 is used for 2041 and so on.
#' If years is a single value of 5, and lag is 10, then the mean SBF0 in 2042 is calculated over the year range 2028:2032 (i.e. over years). If rolling is TRUE, the mean in SBF) in 2041 is calculated over the range 2027:2031 and so on.
#'
#' @param rep    An object of class MFCLRep.
#' @param years  Either a vector of years over which to calculate the mean over, or a single numeric vector giving the number of years over which to calculate the mean.
#' @param lag    If \code{years} is a single numeric vector, the number of years between the final year in the rep file and final year over which to calculate the mean.
#' @param rolling TRUE (a rolling mean, i.e. a moving average) or FALSE (the years over which to calculate the mean are fixed). Default is TRUE.
#' @param combine_areas A logical argument. Should the areas (regions) be combined. Default is TRUE.
#'
#' @return An FLQuant object with annual SB/SBF0 values.
#' 
#' @seealso \code{\link{read.MFCLRep}} \code{\link{SBF0Alt}}
#' 
#' @export
#' @docType methods
#' @rdname SBSBF0-methods
#' @examples
#' \dontrun{
#' # SB/SBF0 using a mean over a constant year range of 2011:2015
#' SBF0Alt(rep, years=2011:2015, rolling=FALSE)
#' # Using a mean over rolling year range, with the final year being the mean of 2011:2015, the second to last year being mean od 2010:2014 etc.
#' SBF0Alt(rep, years=2011:2015, rolling=TRUE)
#' # Using a mean over a constant year range of defined as number of years in the range and number or years from the final year.
#' # e.g. if the final year was 2042, return the mean over 2028:2032
#' SBF0Alt(rep, years=5, lag=10, rolling=FALSE)
#' # Using a mean over a rolling year range, e.g. value in 2042 is the mean over 2028:2032, value in 2041 is mean over 2027:2031 etc.
#' SBF0Alt(rep, years=5, lag=10, rolling=FALSE)
#' }
#'
setGeneric('SBSBF0Alt', function(rep, years, lag, ...) standardGeneric('SBSBF0Alt')) 

#' @rdname SBSBF0-methods
#' @aliases SBSBF0Alt
setMethod("SBSBF0Alt", signature(rep="MFCLRep",years="numeric", lag="missing" ),
  function(rep, years, rolling=TRUE, combine_areas=TRUE){
    if(combine_areas){
      sb <- seasonMeans(areaSums(adultBiomass(rep)))
    }
    if(!combine_areas){
      sb <- seasonMeans(adultBiomass(rep))
    }
    mean_sbf0 <- SBF0Alt(rep=rep, years=years,rolling=rolling, combine_areas=combine_areas)
    out <- sb/mean_sbf0
    return(out)
  }
)


#' @rdname SBSBF0-methods
#' @aliases SBSBF0Alt
setMethod("SBSBF0Alt", signature(rep="MFCLRep",years="numeric", lag="numeric"),
  function(rep, years, lag, rolling=TRUE, combine_areas=TRUE){
    if (length(years) > 1){
      stop("If supplying a lag, then years is nyears and must be of length 1\n")
    }
    # Get year range based on lag and final year
    final_year <- dimnames(adultBiomass_nofish(rep))$year
    final_year <- as.numeric(final_year[length(final_year)])
    year_range <- (final_year - lag - years + 1):(final_year - lag)
    out <- SBSBF0Alt(rep=rep, years=year_range, rolling=rolling, combine_areas=combine_areas)
    return(out)
  }
)
