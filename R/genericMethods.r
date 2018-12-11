#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' write
#'
#' Writes MFCL objects to a text file
#'
#' @param x An object of class MFCL eg. MFCLFrq, MFCLPar, etc.
#'
#' @param file The name and path of the file to be written
#'
#' @param append If True append to existing file, If False overwrite any existing file
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return Creates a text file at the specified location.
#' 
#' @seealso \code{\link{read.MFCLFrq}} and \code{\link{read.MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname write-methods
#'
#' @examples
#' write(MFCLFrqStats())

setGeneric('write', function(x, file, append=F, ...) standardGeneric('write')) 



#'@export version
setGeneric('version', function(x, ...) standardGeneric('version')) 
setMethod('version', signature(x='MFCLPar'),function(x) flagval(x, 1, 200)$value)  #flags(par)[flags(x)$flagtype==1 & flags(x)$flag==200,'value']) 

setMethod('version', signature(x='MFCLFrq'),function(x) return(slot(x,'frq_version'))) 

#'@export flagval
setGeneric('flagval', function(x, flagtype, flag, ...) standardGeneric('flagval'))
setMethod('flagval', signature(x='MFCLPar'), function(x, flagtype, flag) flags(x)[flags(x)$flagtype %in% flagtype & flags(x)$flag %in% flag,])

#'@export 
setGeneric('flagval<-', function(x, flagtype, flag, value) standardGeneric('flagval<-')) 
setReplaceMethod('flagval', signature(x='MFCLPar'),
                 function(x, flagtype, flag, value){flags(x)[flags(x)$flagtype %in% flagtype & flags(x)$flag %in% flag, 'value'] <- value; return(x)}) 

#'@export steepness
setGeneric('steepness', function(x) standardGeneric('steepness'))
setMethod('steepness', signature(x='MFCLIni'), function(x) return(slot(x, 'sv')))
setMethod('steepness', signature(x='MFCLPar'), function(x) return(slot(x, 'season_growth_pars')[29]))

setGeneric('steepness<-', function(x, value) standardGeneric('steepness<-'))
setReplaceMethod('steepness', signature(x='MFCLIni'), function(x, value){slot(x, 'sv') <- value; return(x)})
setReplaceMethod('steepness', signature(x='MFCLPar'), function(x, value){slot(x, 'season_growth_pars')[29] <- value; return(x)})

#'@export lw_params
setMethod('lw_params', signature(object='MFCLPar'), function(object) return(slot(object, 'season_growth_pars')[27:28]))
setReplaceMethod('lw_params', signature(object='MFCLPar'), function(object, value){slot(object, 'season_growth_pars')[27:28] <- value; return(x)})

#'@export realisations
setGeneric('realisations', function(object,...) standardGeneric('realisations'))
setMethod('realisations', signature(object='MFCLFrq'), 
          function(object){ 
            return(slot(object, 'freq')[is.element(slot(object, 'freq')$length, c(NA, slot(object, 'lf_range')['LFFirst'])) &
                                        is.element(slot(object, 'freq')$weight, c(NA, slot(object, 'lf_range')['WFFirst'])),])})






# iter {{{
setMethod("iter", signature(obj="MFCLPseudo"),
          function(obj, iter) {
            
            if(iter > max(slot(obj, "catcheff")$iter))
              stop("max iter exceeded")
            
            slot(obj, "catcheff") <- slot(obj, "catcheff")[slot(obj, "catcheff")$iter==iter,]
            
            for(ss in c("l_frq", "w_frq")){
              if(nrow(slot(obj, ss))>0)
                slot(obj, ss) <- slot(obj, ss)[slot(obj, ss)$iter==iter ,]
            }
            return(obj)
          }
) # }}}


setMethod("+", signature(e1="MFCLFrq", e2="MFCLFrq"),
          function(e1, e2) {
            
            if(frq_version(e1) != frq_version(e2))
              stop("Error : different frq versions")
            if(n_regions(e1) != n_regions(e2) | n_fisheries(e1) != n_fisheries(e2))
              warning("Objects may not be compatible")
            if(any(is.element(apply(freq(e1)[,1:4],1,paste, collapse="_"), apply(freq(e2)[,1:4],1,paste, collapse="_"))))
              warning("Looks like you are duplicating fishery realisations!")
            
            freq(e1) <- rbind(freq(e1), freq(e2))
            
            lf_range(e1)['Datasets'] <- nrow(freq(e1)[is.element(freq(e1)$length, c(lf_range(e1)['LFFirst'],NA)),])
            range(e1)[c('minyear','maxyear')] <- range(freq(e1)$year)
            n_tag_groups(e1) <- n_tag_groups(e1) + n_tag_groups(e2)
            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLFrq", e2="MFCLPseudo"),
          function(e1, e2) {
            
            if(any(range(e1)[c("minyear","maxyear")] != range(e2)[c("minyear","maxyear")]))
              freq(e1) <- rbind(freq(e1), catcheff(e2)[,1:10])
            
            if(all(range(e1)[c("minyear","maxyear")] == range(e2)[c("minyear","maxyear")]))
              freq(e1) <- catcheff(e2)[,1:10]
            
            lf_range(e1)['Datasets'] <- nrow(freq(e1)[is.element(freq(e1)$length, c(lf_range(e1)['LFFirst'],NA)),])
            range(e1)[c('minyear','maxyear')] <- range(freq(e1)$year)
            
            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLTag", e2="MFCLTag"),
          function(e1, e2) {
            
            mrg.hist       <- max(releases(e1)$rel.group)
            releases(e2)$rel.group   <- releases(e2)$rel.group   + mrg.hist
            recaptures(e2)$rel.group <- recaptures(e2)$rel.group + mrg.hist
            
            releases(e1)   <- rbind(releases(e1), releases(e2))
            recaptures(e1) <- rbind(recaptures(e1), recaptures(e2))
            
            release_groups(e1) <- max(releases(e1)$rel.group)
            recoveries(e1)     <- c(recoveries(e1), recoveries(e2))
            releases(e1)       <- rbind(releases(e1), releases(e2))
            
            range(e1)['maxyear'] <- range(e2)['maxyear']
            
            return(e1)
          }
) # }}}






