#' window
#'
#' subset MFCL objects between the times start and end.
#'
#' @param x:    An object of class MFCLX.
#' 
#' @param start:    The start time of the period of interest
#' 
#' @param end:    The end time of the period of interest.
#' 
#' @param extend: logical. If true the start and end times can extend the time series. If false any attempt to extend the series prompts a warning and is ignored.
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
#' window(MFCLFrq(), start = 1990, end = 1995)




#' @rdname mfcl-methods
#' @aliases mfcl


setMethod("window", signature(x="MFCLFrq"), 
          function(x, start=range(x)['minyear'], end=range(x)['maxyear'], extend=FALSE, ...){
            
            
            if(start < range(x)['minyear'] | end > range(x)['maxyear'])
              stop("Error: This method does not yet allow the extension of MFCL objects beyond their current year range")
            
            freq(x) <- freq(x)[freq(x)$year %in% start:end,] 
            
            #calculate the new number of fishing incidents
            noobs <- freq(x)[is.na(freq(x)$length)                  & is.na(freq(x)$weight), 1:7]
            lobs  <- freq(x)[freq(x)$length==lf_range(x)['LFFirst'] & is.na(freq(x)$weight), 1:7]
            wobs  <- freq(x)[is.na(freq(x)$length)                  & freq(x)$weight==lf_range(x)['WFFirst'], 1:7]
            
            lf_range(x)["Datasets"] <- sum(!duplicated(rbind(noobs, lobs, wobs)))-1       # why the -1 ??
            
            range(x)['minyear'] <- start
            range(x)['maxyear'] <- end
            
            return(x)
            
          })



setMethod("window", signature(x="MFCLTag"),
          function(x, start=range(x)['minyear'], end=range(x)['maxyear'], extend=FALSE, ...){
            
            if(start < range(x)['minyear'] | end > range(x)['maxyear'])
              stop("Error: This method does not yet allow the extension of MFCL objects beyond their current year range")
            
            # remove releases and recaptures from end year -1 (based on release year)
            releases(x)   <- releases(x)[releases(x)$year     %in% start:(end-1),]
            recaptures(x) <- recaptures(x)[recaptures(x)$year %in% start:(end-1),]
            
            # then remove any recaptures from end year (based on recapture year) 
            recaptures(x) <- recaptures(x)[recaptures(x)$recap.year %in% start:(end-1),]
            
            release_groups(x) <- max(releases(x)$rel.group)
            recoveries(x)     <- as.vector(rowSums(table(recaptures(x)$rel.group, recaptures(x)$recap.number)))
            
            range(x)[c("minyear", "maxyear")] <- c(start, end)
            
            return(x)
            
          })





