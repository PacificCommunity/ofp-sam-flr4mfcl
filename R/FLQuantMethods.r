#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott





#' @rdname FLQuant
#' @aliases FLQuant,MFCLLenFreq-method
#' @examples
#'

setMethod("FLQuant", signature(object="MFCLLenFreq"),
          function(object,  data='catch', ...) {
            
            quant <- switch(data, "catch" = "all",
                                  "effort"= "all",
                                  "length"= "length",
                                  "weight"= "weight")
              
            if(data=='catch' | data=='effort'){
              dd <- cbind(quant, realisations(object)[,c('year','fishery','month')], 'unique', 1, realisations(object)[,data])
              colnames(dd) <- colnames(as.data.frame(FLQuant()))
              return(as.FLQuant(dd))
            }
          
            if(data=="length" | data=='weight'){
              dd <- cbind(freq(object)[,c(quant,'year','fishery','month')], 'unique', 1, freq(object)[,'freq'])
              colnames(dd) <- c(quant, colnames(as.data.frame(FLQuant()))[-1])
              return(as.FLQuant(dd))
            }
          }
) # }}}


setMethod("FLQuant", signature(object="MFCLLenFreq2"),
          function(object,  data='catch', ...) {
            
            quant <- switch(data, "catch" = "all",
                            "effort"= "all",
                            "length"= "length",
                            "weight"= "weight")
            
            if(data=='catch' | data=='effort'){
              dd <- cbind(quant, realisations(object)[,c('year','fishery','month')], 'unique', 1, realisations(object)[,data])
              colnames(dd) <- colnames(as.data.frame(FLQuant()))
              return(as.FLQuant(dd))
            }
            
            if(data=="length"){
              ## if(data=="length" | data=='weight'){
              ## dd <- cbind(freq(object)[,c(quant,'year','fishery','month')], 'unique', 1, freq(object)[,'freq'])
              ## colnames(dd) <- c(quant, colnames(as.data.frame(FLQuant()))[-1])
              lnfrq <- lnfrq(object)
              nobs=dim(lnfrq)[1]
              nLbins <- lf_range(object)['LFIntervals']; Lwidth <- lf_range(res)["LFWidth"]; Lfirst <- lf_range(res)["LFFirst"]
              frqlen <- seq(Lfirst, Lwidth*nLbins+Lfirst-Lwidth, by=Lwidth)
              dd <- cbind(rep(frqlen,nobs),rep(lnfrq$year,each=nLbins),rep(lnfrq$fishery,each=nLbins),rep(lnfrq$month,each=nLbins),'unique',1,unlist(lnfrq[,-4:-1]))
              colnames(dd) <- c(quant, colnames(as.data.frame(FLQuant()))[-1])
              return(as.FLQuant(dd))
            }
            if(data=='weight'){
              wtfrq <- wtfrq(object)
              nWbins <- lf_range(res)['WFIntervals']; Wwidth <- lf_range(res)["WFWidth"]; Wfirst <- lf_range(res)["WFFirst"]
              frqwt  <- seq(Wfirst, Wwidth*nWbins+Wfirst-Wwidth, by=Wwidth)
              dd <- cbind(rep(frqwt,nobs),rep(wtfrq$year,each=nWbins),rep(wtfrq$fishery,each=nWbins),rep(wtfrq$month,each=nWbins),'unique',1,unlist(wtfrq[,-4:-1]))
              
              return(as.FLQuant(dd))
            }
          }
) # }}}




#' qts
#'
#' Returns a quarterly time series from a seasonally structured FLQuant.
#'
#' @param quant:  An FLQuant object 
#' 
#'
#' @return An object of class FLQuant
#'
#' @examples
#' flq(stock.n(ple4))
#'
#' @export

qts <- function(quant){
  
  if(!is.FLQuant(quant))
    stop('quant must be an object of class FLQuant')
  
  ssns <- dim(quant)[4]
  if(ssns<=1){
    warning("object has only 1 season: nothing changed")
    return(quant)
  }
  
  q2 <- aperm(quant, c(4,2,1,3,5,6))
  q3 <- array(c(q2), dim=c(dim(q2)[3], dim(q2)[1]*dim(q2)[2], dim(q2)[4], 1, dim(q2)[5:6]))
  
  # dirty hack to force the first dimension to be age - not ideal but OK for now I think
  names(dimnames(quant))[1] <- "age"
  
  dimnames(q3) <- list(age = dimnames(quant)$age,
                       year= as.character(rep(as.numeric(dimnames(quant)$year),each=ssns)+(1/(2*ssns))*seq(1, by=2, length.out=ssns)),
                       unit= dimnames(quant)$unit,
                       season="all",
                       area= dimnames(quant)$area,
                       iter= dimnames(quant)$iter)
  
  return(as.FLQuant(q3))
  
}


#' ats
#'
#' Converts a quarterly time series object into a 6 dimensional, seasonally structured FLQuant (i.e. the opposite of qts).
#'
#' @param quant:  An FLQuant object 
#' 
#'
#' @return An object of class FLQuant
#'
#'
#' @export

ats <- function(quant){
  
  if(!is.FLQuant(quant))
    stop('quant must be an object of class FLQuant')
  
  ssns <- unique(as.numeric(unlist(dimnames(quant)['year']))-floor(as.numeric(unlist(dimnames(quant)['year']))))
  if(length(ssns)<=1){
    warning("object has only 1 season: nothing changed")
    return(quant)
  }
  
  qdf <- as.data.frame(quant)
  
  for(ss in 1:length(ssns))
    qdf$new.season[qdf$year-floor(qdf$year)==ssns[ss]] <- factor(ss, levels=as.character(1:length(ssns)))
  
  qdf$season <- qdf$new.season
  qdf$year   <- floor(qdf$year)
  
  return(as.FLQuant(qdf[,c(1:7)]))
  
}




