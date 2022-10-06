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
#'\dontrun{
#' write(MFCLFrqStats(), "file.txt")
#' }

setGeneric('write', function(x, file, append=F, ...) standardGeneric('write')) 



#'@export version
setGeneric('version', function(x, ...) standardGeneric('version')) 
setMethod('version', signature(x='MFCLPar'),function(x) flagval(x, 1, 200)$value)  #flags(par)[flags(x)$flagtype==1 & flags(x)$flag==200,'value']) 

setMethod('version', signature(x='MFCLFrq'),function(x) return(slot(x,'frq_version'))) 

setMethod('version', signature(x='MFCLIni'),function(x) return(slot(x,'ini_version')))

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
setMethod('lw_params', signature(object='MFCLBiol'), function(object) return(slot(object, 'season_growth_pars')[27:28]))
setReplaceMethod('lw_params', signature(object='MFCLBiol'), function(object, value){slot(object, 'season_growth_pars')[27:28] <- value; return(x)})

#'@export adultBiomass
setMethod('adultBiomass', signature(object='FLQuant'), function(object, par){ 
  ab <- quantSums(sweep(object, 1, c(qts(mat(par)))*waa(par), '*'))/1000
  return(ab)
  }
)

#' realisations
#'
#' Returns the unique fishery realisations. 
#' Essentially this is the contents of the \code{freq} slot of an \code{\link{MFCLFrq}} object 
#' without the replications for each length or weight category.
#'
#' @param object An object of class MFCLLenFreq.
#'
#' @return An object of class data.frame.
#' 
#' @seealso \code{\link{read.MFCLFrq}} 
#' 
#' @export
#' @export realisations
#' @docType methods
#' @rdname genericMethods
#'
#' @examples
#'\dontrun{
#' realisations(frq)
#' }


setGeneric('realisations', function(object,...) standardGeneric('realisations'))
setMethod('realisations', signature(object='MFCLLenFreq'), 
          function(object){ 
#            return(slot(object, 'freq')[is.element(slot(object, 'freq')$length, c(NA, slot(object, 'lf_range')['LFFirst'])) &
#                                        is.element(slot(object, 'freq')$weight, c(NA, slot(object, 'lf_range')['WFFirst'])),])
  length_realisations <- is.element(slot(object,'freq')$length, c(NA, lf_range(object)['LFFirst']))
  weight_realisations <- is.element(slot(object,'freq')$weight, c(NA, lf_range(object)['WFFirst']))
  lw_realisations <- length_realisations & weight_realisations
  # But some of these lw_realisations may be in the same timestep / fishery
  # So we need to drag out the unique timestep / fishery combinations only
  freq2 <- slot(object,'freq')[lw_realisations,]
  realisations <- unique(freq2[,c("year","month","week","fishery")])
  # Drop penalty, length, weight and freq column
  #drop_cols <- c("penalty", "length", "weight", "freq")  # RDS 05/05/2020
  drop_cols <- c("length", "weight", "freq")
  realisations <- freq2[rownames(realisations),!(colnames(freq2) %in% drop_cols)]
  return(realisations)
})


#'@export as.MFCLLenFreq
setGeneric('as.MFCLLenFreq', function(object,...) standardGeneric('as.MFCLLenFreq'))
setMethod('as.MFCLLenFreq', signature(object='MFCLFrq'), 
          function(object){ 
            res <- MFCLLenFreq()
            ss <- names(getSlots(class(res)))
            for(sn in ss)
              slot(res, sn) <- slot(object, sn)
            return(res)})

setMethod('as.MFCLLenFreq', signature(object='MFCLPseudo'), 
          function(object){ 
            res <- MFCLLenFreq()
            ss <- names(getSlots(class(res)))
            for(sn in ss)
              slot(res, sn) <- slot(object, sn)
            return(res)})



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
            
            slot(obj, 'freq') <- slot(obj,'freq')[,c('year','month','week','fishery',paste0('catch_',iter), paste0('effort_',iter),'penalty','length','weight',paste0('freq_',iter))]
            colnames(slot(obj, 'freq')) <- c('year','month','week','fishery','catch','effort','penalty','length','weight','freq')
            return(obj)
          }
) # }}}



# summary {{{
setMethod("summary", signature(object="MFCLLikelihood"),
          function(object) {
            obj <- object
            res <- data.frame(component=c("bhsteep","effort_dev","catchability_dev","length_comp","weight_comp","tag_data", "total"),
                              likelihood=c(bh_steep_contrib(obj),
                                           sum(effort_dev_penalty(obj)),
                                           sum(q_dev_pen_fish_grp(obj)),
                                           sum(unlist(length_fish(obj))),
                                           sum(unlist(weight_fish(obj))),
                                           sum(unlist(tag_rel_fish(obj))),
                                           0))
            res[7,"likelihood"] <- abs(sum(res[,"likelihood"]))
            return(res)
          }
) # }}}






setMethod("+", signature(e1="MFCLLenFreq", e2="MFCLLenFreq"),
          function(e1, e2) {
            
            if(any(is.element(apply(freq(e1)[,1:4],1,paste, collapse="_"), apply(freq(e2)[,1:4],1,paste, collapse="_"))))
              warning("Looks like you are duplicating fishery realisations!")
            
            freq(e1) <- rbind(freq(e1), freq(e2))
            
            lf_range(e1)['Datasets'] <- nrow(realisations(freq(e1)))
            range(e1)[c('minyear','maxyear')] <- range(freq(e1)$year)
            
            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLFrq", e2="MFCLFrq"),
          function(e1, e2) {
            
            if(frq_version(e1) != frq_version(e2))
              stop("Error : different frq versions")
            if(n_regions(e1) != n_regions(e2) | n_fisheries(e1) != n_fisheries(e2))
              warning("Objects may not be compatible")
            
            lenfreq_e1 <- as.MFCLLenFreq(e1) + as.MFCLLenFreq(e2)
            
            freq(e1)     <- freq(lenfreq_e1)
            lf_range(e1) <- lf_range(lenfreq_e1)
            
            n_tag_groups(e1) <- n_tag_groups(e1) + n_tag_groups(e2)
            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLFrq", e2="MFCLPseudo"),
          function(e1, e2) {
            
            # add future pseudo data to the original FRQ
            if(any(range(e1)[c("minyear","maxyear")] != slot(e2, 'range')[c("minyear","maxyear")]))
              freq(e1) <- rbind(freq(e1), freq(e2))                                                      #catcheff(e2)[,1:10])
            
            # add historical pseudo data to the PROJFRQ
            if(all(range(e1)[c("minyear","maxyear")] == slot(e2, 'range')[c("minyear","maxyear")]))
              freq(e1) <- freq(e2)                                                                       #catcheff(e2)[,1:10]
            
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
            # Remove the following line
            #releases(e1)       <- rbind(releases(e1), releases(e2))
            
            range(e1)['maxyear'] <- range(e2)['maxyear']
            
            return(e1)
          }
) # }}}



##----------------------------- 
## Control objects

setMethod("+", signature(e1="MFCLPseudoControl", e2="MFCLprojControl"),
          function(e1, e2) {
            
            for(ss in slotNames(e2))
              slot(e1, ss) <- slot(e2, ss)
            
            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLMSEControl", e2="MFCLprojControl"),
          function(e1, e2) {
      
            for(ss in slotNames(e2))
              slot(e1, ss) <- slot(e2, ss)

            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLMSEControl", e2="MFCLPseudoControl"),
          function(e1, e2) {
            
            for(ss in slotNames(e2))
              slot(e1, ss) <- slot(e2, ss)
            
            return(e1)
          }
) # }}}

setMethod("+", signature(e1="MFCLMSEControl", e2="MFCLEMControl"),
          function(e1, e2) {
            
            for(ss in slotNames(e2))
              slot(e1, ss) <- slot(e2, ss)
            
            return(e1)
          }
) # }}}



#'@export modifyRRini
setGeneric('modifyRRini', function(ini,tag,rr,PlusGroup) standardGeneric('modifyRRini'))
setMethod('modifyRRini', signature(ini='MFCLIni',tag='MFCLTag',rr='data.frame',PlusGroup='character'), function(ini,tag,rr,PlusGroup) {
    Nfsh <- dim(rr)[1]
    matrixFormation = function(colName="Grp", plGrp=PlusGroup) {
        lnbins <- length(release_lengths(tag))
        tmpPrg <- c(as.character(releases(tag)$program[seq(1,length(releases(tag)$program),lnbins)]),PlusGroup)
        tmpMat <- matrix(NA, nrow = length(tmpPrg), ncol = Nfsh)
        tmpInd <- match(paste(colName, tmpPrg, sep ='.'), colnames(rr))
        for (x in 1:length(tmpPrg)) tmpMat[x,] <- rr[,tmpInd[x]]
        return(tmpMat)
    }

    tag_fish_rep_rate(ini) <- matrixFormation("Ini",PlusGroup)
    tag_fish_rep_target(ini) <- tag_fish_rep_rate(ini)*100
    tag_fish_rep_grp(ini) <- matrixFormation("Grp",PlusGroup)
    tag_fish_rep_pen(ini) <- matrixFormation("Pen",PlusGroup)
    tag_fish_rep_flags(ini) <- matrix(1,nrow=nrow(tag_fish_rep_grp(ini)),ncol=ncol(tag_fish_rep_grp(ini)))
    return(ini)
})



# Re-order dimnames

#' checkUnitDimnames
#'
#' Re-orders the unit dimnames in ascending order instead
#' of default ordering of FLQuant method which uses the year dimension
#'
#' @param obj An object of class MFCL eg. MFCLFrq, MFCLPar, etc.
#' @param nfisheries The number of fisheries in the model
#'
#' @param ... Additional argument list that might not ever be used.
#'
#' @return Returns an object of the same class but with FLQuant unit dimension
#' re-ordered as appropriate.
#'
#' @examples
#' \dontrun{
#' checkUnitDimnames(MFCLRep())
#' }

checkUnitDimnames <- function(obj,nfisheries){
      unit_order <- as.character(1:nfisheries)
      # ID FLQs with unit dimensions
      repslots <- getSlots(class(obj))
      # Get FLQ slots
      flqslots <- names(repslots[repslots == "FLQuant"])
      # Do they have a unit dimension of number of fisheries
      flqslots <- flqslots[sapply(flqslots, function(x) dim(slot(obj, x))[3]==nfisheries)]
      # Reorder the unit dimensions and put back in
      for(flq in flqslots){
        slot(obj, flq) <- slot(obj,flq)[,,unit_order]
      }
      return(obj)
}
