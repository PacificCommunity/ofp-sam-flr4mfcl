#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' subset
#'
#' subsets a freq(frq) for specific groupings
#' 
#'
#' @param x:    An object of class MFCLFrq.
#'
#' @param ... Additional argument list 
#'
#' @return A data.frame of the selected information.
#' 
#' 
#' @export
#' @docType methods
#' @rdname subset-methods

setGeneric('subset', function(x, ...) standardGeneric('subset')) 



#' @rdname subset-methods
#' @aliases subset

setMethod("subset", signature(x="MFCLFrq"), 
          function(x, ...){
            
            args <- list(...)
            
            res <- freq(x)
            
            for(ii in 1:length(args)){
              #res <- res[res[names(args)[ii]] == args[ii],]
              res <- res[is.element(res[,names(args[ii])], args[[ii]]),]
            }
            
            return(res)
          })



setMethod("subset", signature(x="MFCLTag"),
         function(x, ...){
            #browser()
            rel <- releases(x)
            rec <- recaptures(x)
            
            # apply the subset to both releases and recaptures
            rel <- subset(rel, ...)
            #rec <- subset(rec, ...)
            
            if(nrow(rel)==0)
              stop("Error: No release groups for current selection")
            
            # re-organise the object so everything is compatible with the revised data
            if(nrow(rel) != nrow(releases(x))){
              rel <- rel[order(rel$rel.group),]
              rel$new.rel.group <- rep(1:length(table(rel$rel.group)), table(rel$rel.group))  # re-number the release groups in ascending order
              recnew <- merge(rec, rel[,c(1:5,8)])
              recnew$rel.group <- recnew$new.rel.group
            }
            releases(x)   <- rel
            recaptures(x) <- recnew
            release_groups(x) <- max(releases(x)$rel.group)
            # count the number of strata in the recaptures (i.e. the unique year, season, region, length, etc)
            recoveries.obs      <- as.data.frame(table(recaptures(x)$rel.group))
            recoveries.missing  <- data.frame(Var1=as.factor((1:release_groups(x))[!is.element(1:release_groups(x), recoveries.obs$Var1)]), Freq=0)
            recoveries.all      <- rbind(recoveries.obs, recoveries.missing)
            recoveries(x)       <- recoveries.all$Freq[order(as.numeric(as.character(recoveries.all$Var1)))]
            
            range(x)[c('minyear', 'maxyear')] <- c(min(releases(x)$year), max(c(releases(x)$year, recaptures(x)$recap.year)))
            return(x)
          })



