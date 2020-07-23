# FLR4MFCL - R4MFCL built with FLR classes
# SB, SBF0 and SBSBF0 methods
# Copyright (C) 2020  Rob Scott and Finlay Scott



# Internal function used by SB and SBF0 methods
# Not exported
# This function fuels all the other methods here.
rolling_mean_and_lagging <- function(flq, mean_nyears, lag_nyears){
    if((mean_nyears < 1) | (lag_nyears < 0)){
      stop("mean_nyears >= 1 and lag_nyears must >= 0")
    }
    # Take a rolling mean over mean_nyears
    flq <- apply(flq, c(1,3,4,5,6), function(x, n, sides){stats::filter(c(x),rep(1/n,n), sides=sides)}, n=mean_nyears, sides=1)
    # Shunt everything up by lag_nyears and backfill with NAs
    if(lag_nyears > 0){
      lagged_flq <- flq[,1:(dim(flq)[2] - lag_nyears)]
      flq[,(lag_nyears + 1) :dim(flq)[2]] <- lagged_flq
      flq[,1:lag_nyears] <- NA
    }
    return(flq)
}

# # Tests if you need them
# # Testing
# library(testthat)
# # Testing rolling_mean_and_lagging()
# flq <- seasonMeans(adultBiomass(rep))
# # Test 1 - just returns it
# mean_nyears <- 1
# lag_nyears <- 0
# out <- rolling_mean_and_lagging(flq=flq, mean_nyears=mean_nyears, lag_nyears=lag_nyears)
# # out should match flq
# expect_equal(out,flq)
# # Test 2
# mean_nyears <- round(runif(1,min=2,max=6))
# lag_nyears <- 0 # No lag
# out <- rolling_mean_and_lagging(flq=flq, mean_nyears=mean_nyears, lag_nyears=lag_nyears)
# # Check first bunch of values
# dim2 <- dim(flq)[2]
# # Crappy for loop for calculating a rolling mean as a test
# for(i in 1:10){
#   expect_equal(c(apply(flq[,(dim2-mean_nyears-i+2):(dim2-i+1)],c(1,3,4,5,6),mean)), c(out[,dim2-i+1]))
# }
# # Test 3 - lagged with no mean
# mean_nyears <- 1
# lag_nyears <- round(runif(1,min=1,max=3))
# out <- rolling_mean_and_lagging(flq=flq, mean_nyears=mean_nyears, lag_nyears=lag_nyears)
# dim2 <- dim(flq)[2]
# expect_equal(dim(out),dim(flq))
# expect_equal(c(flq[,1:(dim2-lag_nyears)]), c(out[,(lag_nyears + 1):dim2]))
# # Test 4 - lagged and rolled
# lag_nyears <- round(runif(1,min=1,max=3))
# mean_nyears <- round(runif(1,min=2,max=6))
# out <- rolling_mean_and_lagging(flq=flq, mean_nyears=mean_nyears, lag_nyears=lag_nyears)
# expect_equal(dim(out),dim(flq))
# dim2 <- dim(flq)[2]
# for(i in 1:5){
#   a <- apply(flq[,(dim2-mean_nyears-i-lag_nyears+2):(dim2-i-lag_nyears+1)],c(1,3,4,5,6),mean)
#   b <- out[,dim2-i+1]
#   expect_equal(c(a), c(b))
# }



#------------------------------------------------------------------------
# Single rep methods

# SB methods

#' Methods for returns the spawning biomass (fished or unfished) and the depletion
#'
#' Methods for returning the spawning biomass (\code{SB()}), unfished spawning biomass (\code{SBF0()}) and depletion (\code{SBSBF0()}) from a single MFCLRep object, or a list of MFCLRep objects.
#' The biomass is averaged over seasons.
#' It is possible to calculate a rolling mean over a specified number of years
#' as well as lag the output.
#' These can specified separately for the SB and SBF0 parts of SBSBF0.
#' The number of years in the outputs will match that of the inputs.
#' The start of the returned FLQuant(s) will be padded with NAs if necessary (for example, where the output has been lagged).
#' # When using lists of rep objects the lagging and rolling mean arguments are applied to all objects in the list.
#' 
#' Shortcut methods are available (\code{SBlatest()}, \code{SBrecent()}, \code{SBF0recent()} and \code{SBSBF0recent()}). These set the \code{mean_nyears()} and \code{lag_nyears()} parameters to match those used in the assessment reports.
#' \code{SBlatest()} does not calculate a mean or lag the output.
#' \code{SBrecent()} uses a rolling mean of 4 years without a lag.
#' \code{SBF0recent()} uses a rolling mean of 10 years with a lag of 1 year.
#' \code{SBSBF0recent()} is a combination of \code{SBrecent()} and \code{SBF0recent()}.
#' \code{SBSBF0latest()} is a combination of \code{SBlatest()} and \code{SBF0recent()}.
#'
#' @param rep A single MFCLRep object or a list of MFCLRep objects.
#' @param mean_nyears The number of years to calculate the rolling mean over. A value of 1 is the same as not calculating a mean. For the SB() method the default value is 1.
#' @param lag_nyears The number of years to lag the output. A value of 0 is the same as not lagging the output. For the SB() method the default value is 0.
#' @param combine_areas If TRUE the biomassses in different areas are summed. Default value is TRUE.
#' @param sb_mean_nyears Equivalent to the argument \code{mean_nyears} for the SB part of the \code{SBSBF0()} method.
#' @param sb_lag_nyears Equivalent to the argument \code{lag_nyears} for the SB part of the \code{SBSBF0()} method.
#' @param sbf0_mean_nyears Equivalent to the argument \code{mean_nyears} for the SBF0 part of the \code{SBSBF0()} method.
#' @param sbf0_lag_nyears Equivalent to the argument \code{lag_nyears} for the SBF0 part of the \code{SBSBF0()} method.
#' @param ... Other arguments.
#' 
#' @return An FLQuant, or a list of FLQuant objects, depending on if the rep argument is a single MFCLRep object or a list.
#' @rdname SBmethods
#' @name SBmethods
#' @aliases SB SBmethods
#' @examples
#' \dontrun{
#' # Load up some example rep objects
#' rep1 <- read.MFCLRep(rep_file)
#' rep2 <- read.MFCLRep(rep_file)
#' rep3 <- read.MFCLRep(rep_file)
#' # SB 
#' SB(rep1) # equivalent to extracting the adultBiomass slot
#' SB(rep1, mean_nyears=4) # rolling mean with a window of 4 years
#' SBlatest(rep1) # no mean or lag
#' SBrecent(rep1) # Shortcut for a rolling mean of 4 years
#' # SBF0
#' SBF0(rep1) # equivalent to extracting the adultBiomass_nofish slot
#' SBF0(rep1, mean_nyears=10, lag_nyears=1) # a 10 year rolling window and a lag of 1
#' SBF0recent(rep1) # Shortcut for a 10 year rolling window and a lag of 1
#' # SBSBF0
#' SBSBF0(rep1) # equivalent to extracting dividing the adultBiomass slot by the adultBiomass_nofish slot
#' SBSBF0(rep1, sb_mean_nyears=4, sbf0_mean_nyears=10, sbf0_lag_nyears=1) # Depletion with SB with a 4 year rolling window and SBF0 with a 10 year rolling window and a lag of 1
#' SBSBF0recent(rep1) #  Shortcut for depletion with SB with a 4 year rolling window and SBF0 with a 10 year rolling window and a lag of 1
#' SBSBF0latest(rep1) #  Shortcut for depletion with SB without a rolling mean and no lag and SBF0 with a 10 year rolling window and a lag of 1
#' # Using lists of rep objects. The lagging and rolling mean arguments are applied to all objects in the list.
#' rep <- list(rep1=rep1, rep2=rep2, rep3=rep3)
#' SBSBF0recent(rep)
#' } 
#' @export
 
#---------------- SBF0 methods ---------------------------
setGeneric('SB',function(rep, mean_nyears, lag_nyears, ...) standardGeneric('SB')) 

# This is the main method that the other SB() methods call
#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="MFCLRep",mean_nyears="numeric", lag_nyears="numeric"),
  function(rep, mean_nyears, lag_nyears, combine_areas=TRUE, season_means=TRUE){
    
    out <- adultBiomass(rep)
    if(season_means==TRUE){
      out <- seasonMeans(out)
    }
    if(combine_areas==TRUE){
      out <- areaSums(out)
    }
    out <- rolling_mean_and_lagging(flq=out, mean_nyears=mean_nyears, lag_nyears=lag_nyears)
    return(out)
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="MFCLRep",mean_nyears="missing", lag_nyears="missing"),
  function(rep, ...){
    return(SB(rep=rep, mean_nyears=1, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="MFCLRep",mean_nyears="numeric", lag_nyears="missing"),
  function(rep, mean_nyears, ...){
    return(SB(rep=rep, mean_nyears=mean_nyears, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="MFCLRep",mean_nyears="missing", lag_nyears="numeric"),
  function(rep, lag_nyears, ...){
    return(SB(rep=rep, mean_nyears=1, lag_nyears=lag_nyears, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="list",mean_nyears="numeric", lag_nyears="numeric"),
  function(rep, mean_nyears, lag_nyears, combine_areas=TRUE, season_means=TRUE){
    guts <- unlist(lapply(rep, function(x){class(x)=="MFCLRep"}))
    if(!all(guts==TRUE)){
      stop("All elements in the list must be of class 'MFCLRep'")
    }
    out <- lapply(rep, SB, mean_nyears=mean_nyears, lag_nyears=lag_nyears, combine_areas=combine_areas, season_means=season_means)
    return(out)
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="list",mean_nyears="missing", lag_nyears="missing"),
  function(rep, ...){
    return(lapply(rep, SB, mean_nyears=1, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="list",mean_nyears="numeric", lag_nyears="missing"),
  function(rep, mean_nyears, ...){
    return(lapply(rep, SB, mean_nyears=mean_nyears, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SB", signature(rep="list",mean_nyears="missing", lag_nyears="numeric"),
  function(rep, lag_nyears, ...){
    return(lapply(rep, SB, mean_nyears=1, lag_nyears=lag_nyears, ...))
  }
)

#---------------- SBF0 methods ---------------------------

#' @rdname SBmethods
#' @export
setGeneric('SBF0',function(rep, mean_nyears, lag_nyears, ...) standardGeneric('SBF0')) 

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="MFCLRep",mean_nyears="numeric", lag_nyears="numeric"),
  function(rep, mean_nyears, lag_nyears, combine_areas=TRUE, season_means=TRUE){
    out <- adultBiomass_nofish(rep)
    if(season_means==TRUE){
      out <- seasonMeans(out)
    }
    if(combine_areas==TRUE){
      out <- areaSums(out)
    }
    out <- rolling_mean_and_lagging(flq=out, mean_nyears=mean_nyears, lag_nyears=lag_nyears)
    return(out)
  }
)

#' @rdname SBmethods
#' @export
# Default methods
setMethod("SBF0", signature(rep="MFCLRep",mean_nyears="missing", lag_nyears="missing"),
  function(rep, ...){
    return(SBF0(rep=rep, mean_nyears=1, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="MFCLRep",mean_nyears="numeric", lag_nyears="missing"),
  function(rep, mean_nyears, ...){
    return(SBF0(rep=rep, mean_nyears=mean_nyears, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="MFCLRep",mean_nyears="missing", lag_nyears="numeric"),
  function(rep, lag_nyears, ...){
    return(SBF0(rep=rep, mean_nyears=1, lag_nyears=lag_nyears, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="list",mean_nyears="numeric", lag_nyears="numeric"),
  function(rep, mean_nyears, lag_nyears, combine_areas=TRUE, season_means=TRUE){
    guts <- unlist(lapply(rep, function(x){class(x)=="MFCLRep"}))
    if(!all(guts==TRUE)){
      stop("All elements in the list must be of class 'MFCLRep'")
    }
    out <- lapply(rep, SBF0, mean_nyears=mean_nyears, lag_nyears=lag_nyears, combine_areas=combine_areas, season_means=season_means)
    return(out)
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="list",mean_nyears="missing", lag_nyears="missing"),
  function(rep, ...){
    return(lapply(rep, SBF0, mean_nyears=1, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="list",mean_nyears="numeric", lag_nyears="missing"),
  function(rep, mean_nyears, ...){
    return(lapply(rep, SBF0, mean_nyears=mean_nyears, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0", signature(rep="list",mean_nyears="missing", lag_nyears="numeric"),
  function(rep, lag_nyears, ...){
    return(lapply(rep, SBF0, mean_nyears=1, lag_nyears=lag_nyears, ...))
  }
)

#---------------- SBSBF0 methods ---------------------------

#' @rdname SBmethods
#' @export
setGeneric('SBSBF0',function(rep, sb_mean_nyears, sb_lag_nyears, sbf0_mean_nyears, sbf0_lag_nyears, ...) standardGeneric('SBSBF0')) 

#' @rdname SBmethods
#' @export
setMethod("SBSBF0", signature(rep="MFCLRep", sb_mean_nyears="numeric", sb_lag_nyears="numeric", sbf0_mean_nyears="numeric", sbf0_lag_nyears="numeric"),
  function(rep, sb_mean_nyears, sb_lag_nyears, sbf0_mean_nyears, sbf0_lag_nyears, ...){
    sbf0 <- SBF0(rep=rep, mean_nyears=sbf0_mean_nyears, lag_nyears=sbf0_lag_nyears, ...)
    sb <- SB(rep=rep, mean_nyears=sb_mean_nyears, lag_nyears=sb_lag_nyears, ...)
    out <- sb/sbf0
    return(out)
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBSBF0", signature(rep="MFCLRep",sb_mean_nyears="missing", sb_lag_nyears="missing", sbf0_mean_nyears="missing", sbf0_lag_nyears="missing"),
  function(rep, ...){
    return(SBSBF0(rep=rep, sb_mean_nyears=1, sb_lag_nyears=0, sbf0_mean_nyears=1, sbf0_lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBSBF0", signature(rep="list", sb_mean_nyears="numeric", sb_lag_nyears="numeric", sbf0_mean_nyears="numeric", sbf0_lag_nyears="numeric"),
  function(rep, sb_mean_nyears, sb_lag_nyears, sbf0_mean_nyears, sbf0_lag_nyears, ...){
    sbf0 <- SBF0(rep=rep, mean_nyears=sbf0_mean_nyears, lag_nyears=sbf0_lag_nyears, ...)
    sb <- SB(rep=rep, mean_nyears=sb_mean_nyears, lag_nyears=sb_lag_nyears, ...)
    out <- mapply(FUN = `/`, sb, sbf0, SIMPLIFY = FALSE)
    return(out)
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBSBF0", signature(rep="list",sb_mean_nyears="missing", sb_lag_nyears="missing", sbf0_mean_nyears="missing", sbf0_lag_nyears="missing"),
  function(rep, ...){
    return(lapply(rep, SBSBF0, sb_mean_nyears=1, sb_lag_nyears=0, sbf0_mean_nyears=1, sbf0_lag_nyears=0, ...))
  }
)

#---------------- Shortcut methods ---------------------------

#' @rdname SBmethods
#' @export
setGeneric('SBrecent',function(rep, ...) standardGeneric('SBrecent')) 

#' @rdname SBmethods
#' @export
setMethod("SBrecent", signature(rep="MFCLRep"),
  function(rep, ...){
    return(SB(rep=rep, mean_nyears=4, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBrecent", signature(rep="list"),
  function(rep, ...){
    return(lapply(rep, SBrecent, ...))
  }
)

#' @rdname SBmethods
#' @export
setGeneric('SBlatest',function(rep, ...) standardGeneric('SBlatest')) 

#' @rdname SBmethods
#' @export
setMethod("SBlatest", signature(rep="MFCLRep"),
  function(rep, ...){
    return(SB(rep=rep, mean_nyears=1, lag_nyears=0, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBlatest", signature(rep="list"),
  function(rep, ...){
    return(lapply(rep, SBlatest, ...))
  }
)

#' @rdname SBmethods
#' @export
setGeneric('SBF0recent',function(rep, ...) standardGeneric('SBF0recent')) 

#' @rdname SBmethods
#' @export
setMethod("SBF0recent", signature(rep="MFCLRep"),
  function(rep, ...){
    return(SBF0(rep=rep, mean_nyears=10, lag_nyears=1, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBF0recent", signature(rep="list"),
  function(rep, ...){
    return(lapply(rep, SBF0recent, ...))
  }
)

#' @rdname SBmethods
#' @export
setGeneric('SBSBF0recent',function(rep, ...) standardGeneric('SBSBF0recent')) 

#' @rdname SBmethods
#' @export
setMethod("SBSBF0recent", signature(rep="MFCLRep"),
  function(rep, ...){
    return(SBSBF0(rep=rep, sb_mean_nyears=4, sb_lag_nyears=0, sbf0_mean_nyears=10, sbf0_lag_nyears=1, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBSBF0recent", signature(rep="list"),
  function(rep, ...){
    return(lapply(rep, SBSBF0recent, ...))
  }
)

#' @rdname SBmethods
#' @export
setGeneric('SBSBF0latest',function(rep, ...) standardGeneric('SBSBF0latest')) 

#' @rdname SBmethods
#' @export
setMethod("SBSBF0latest", signature(rep="MFCLRep"),
  function(rep, ...){
    return(SBSBF0(rep=rep, sb_mean_nyears=1, sb_lag_nyears=0, sbf0_mean_nyears=10, sbf0_lag_nyears=1, ...))
  }
)

#' @rdname SBmethods
#' @export
setMethod("SBSBF0latest", signature(rep="list"),
  function(rep, ...){
    return(lapply(rep, SBSBF0latest, ...))
  }
)


