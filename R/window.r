#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

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
            
            n_fisheries(x) <- length(unique(freq(x)$fishery))
            
            return(x)
            
          })



setMethod("window", signature(x="MFCLTag"),
          function(x, start=range(x)['minyear'], end=range(x)['maxyear'], extend=FALSE, precheck=FALSE, ...){
            
            if(start < range(x)['minyear'] | end > range(x)['maxyear'])
              stop("Error: This method does not yet allow the extension of MFCL objects beyond their current year range")
            
            orig.rel.grps <- unique(releases(x)$rel.group)
            max.rel.year  <- max(releases(x)$year)
            max.rel.month <- max(releases(x)[releases(x)$year==max.rel.year,'month'])
            
            # remove releases and recaptures from end (based on release year)
            releases(x)   <- releases(x)[releases(x)$year     %in% start:(end-1),] # no rleases in final year
            # set the last release month 
            #releases(x)   <- releases(x)[!(releases(x)$year==end & releases(x)$month>max.rel.month),]
            
            recaptures(x) <- recaptures(x)[recaptures(x)$year %in% start:(end-1),]
            #recaptures(x) <- recaptures(x)[!(recaptures(x)$year==end & recaptures(x)$month>max.rel.month),]
            
            new.rel.grps <- unique(releases(x)$rel.group)
            
            # then remove any recaptures from end year (based on recapture year) 
            recaptures(x) <- recaptures(x)[recaptures(x)$recap.year %in% start:(end),]
            
            # map old release group numbers to new release group numbers 
            rel.num.map <- cbind(new = 1:length(sort(unique(releases(x)$rel.group))),
                                 old = sort(unique(releases(x)$rel.group)))
            
            if(precheck) # return the tag release groups that have been removed so you know how to modify the ini
              return(orig.rel.grps[!is.element(orig.rel.grps, rel.num.map[,'old'])])
            
            # renumber the release groups (and corresponding recapture groups)
            for(rr in rel.num.map[,'old']){
              releases(x)[releases(x)$rel.group==rr, "rel.group"]     <- rel.num.map[rel.num.map[,'old']==rr, 'new']
              recaptures(x)[recaptures(x)$rel.group==rr, "rel.group"] <- rel.num.map[rel.num.map[,'old']==rr, 'new']
            }
            
            # reset objects for new data structure
            release_groups(x) <- max(releases(x)$rel.group)
            
            dummydat <- rbind(recaptures(x), 
                              data.frame(rel.group = 1:release_groups(x), region=1, year=1900, month=1, program=as.factor('DD'), rel.length=1, 
                                         recap.fishery=1, recap.year=1900, recap.month=1, recap.number=1))
            
            recoveries(x)     <- as.vector(tapply(dummydat$recap.number>0, dummydat$rel.group, sum))-1
            
            range(x)[c("minyear", "maxyear")] <- c(start, end)
            
            return(x)
            
          })


setMethod("window", signature(x="MFCLPseudo"), 
          function(x, start=range(x)['minyear'], end=range(x)['maxyear'], extend=FALSE, ...){
            
            
            if(start < range(x)['minyear'] | end > range(x)['maxyear'])
              stop("Error: This method does not yet allow the extension of MFCL objects beyond their current year range")
            
            catcheff(x) <- catcheff(x)[catcheff(x)$year %in% start:end,] 
            l_frq(x)    <- l_frq(x)[l_frq(x)$year %in% start:end,]
            w_frq(x)    <- w_frq(x)[w_frq(x)$year %in% start:end,]
            
            range(x)['minyear'] <- start
            range(x)['maxyear'] <- end
            
            return(x)
            
          })

setMethod("window", signature(x="MFCLTagProj"), 
          function(x, start=range(x)['minyear'], end=range(x)['maxyear'], extend=FALSE, ...){
            
            if(start < range(x)['minyear'] | end > range(x)['maxyear'])
              stop("Error: This method does not yet allow the extension of MFCL objects beyond their current year range")
            
            selector <- which(releases_proj(x)$year>=start & releases_proj(x)$year<=end)
            
            releases_proj(x) <- releases_proj(x)[selector ,]
            rep_rate_proj(x) <- rep_rate_proj(x)[, selector]
            release_groups_proj(x) <- nrow(releases_proj(x))

            range(x)['minyear'] <- start
            range(x)['maxyear'] <- end
            
            return(x)
            
          })
