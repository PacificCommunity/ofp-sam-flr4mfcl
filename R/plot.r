#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' plot
#'
#' Plot MFCL objects 
#'
#' @param obj:    An object of class MFCLX.
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' 
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' plot(MFCLFrq())



#' @rdname mfcl-methods
#' @aliases mfcl



setMethod("plot", signature(x="MFCLLenFreq"), function(x, y="missing", ...){
  
  minfrq <- freq(x)[is.element(freq(x)$length, c(NA, lf_range(x)['LFFirst'])) | is.element(freq(x)$weight, lf_range(x)['WFFirst']),]
  minfrq$yrqtr <- round(minfrq$year+(minfrq$month+1)/12,2)
  
  catch <- tapply(minfrq$catch, list(minfrq$yrqtr), sum)
  effort <- tapply(minfrq$effort, list(minfrq$yrqtr), sum)
  par(mfrow=c(1,2))  
  
  barplot(catch, main="catch")
  barplot(effort, main='effort')
  
})



setMethod("plot", signature(x="MFCLLenFreq", y="MFCLprojControl"), function(x, y, fleets='all', ...){
  
  if(is.numeric(fleets)){
    freq(x) <- freq(x)[is.element(freq(x)$fishery, fleets),]
  }
  
  minfrq <- freq(x)[is.element(freq(x)$length, c(NA, lf_range(x)['LFFirst'])) | is.element(freq(x)$weight, lf_range(x)['WFFirst']),]
  minfrq$yrqtr <- round(minfrq$year+(minfrq$month+1)/12,2)
  
  catch <- tapply(minfrq$catch, list(minfrq$yrqtr), sum)
  effort <- tapply(minfrq$effort, list(minfrq$yrqtr), sum)
  
  # catch and effort for the fleets projected on catch and effort accordingly
  catchcatch   <- tapply(minfrq$catch[is.element(minfrq$fishery, (1:n_fisheries(x))[caeff(y)==1])], minfrq$yrqtr[is.element(minfrq$fishery, (1:n_fisheries(x))[caeff(y)==1])], sum)  
  efforteffort <- tapply(minfrq$effort[is.element(minfrq$fishery, (1:n_fisheries(x))[caeff(y)==2])], minfrq$yrqtr[is.element(minfrq$fishery, (1:n_fisheries(x))[caeff(y)==2])], sum)   
  
  cols <- rep(c(rep("wheat", length(range(x)['minyear']:(min(as.numeric(avyrs(y)))-1))),
                rep("red",   length(as.numeric(avyrs(y)))),
                rep("beige", length(max(as.numeric(avyrs(y))+1):range(x)['maxyear']))), each=n_recs_yr(x))
      
  par(mfrow=c(1,1))
  
  if(length(catchcatch)>1 & length(efforteffort)>1)
    par(mfrow=c(1,2))
        
  if(length(catchcatch>1))
    barplot(catchcatch,   main="catch (catch projected fleets)", col=cols)
  
  if(length(efforteffort>1))
    barplot(efforteffort, main="effort (effort projected fleets)", col=cols)

  #barplot(catch, main="catch")
  #barplot(effort, main='effort')
  
})



setMethod("plot", signature(x="MFCLRep", y="MFCLPar"), function(x, y, ...){
  
  rec_range <- range(y)['maxyear']-(recPeriod(y)[c(1,2)]/4)
  rec_yrs   <- seq(rec_range[1], rec_range[2])+1
  
  ssbx <- seq(0, max(ssb(x))*1.5, length=150)
  
  plot(ssbx, (c(srr(x)['a'])*ssbx)/(c(srr(x)['b'])+ssbx), type="l", ylim=c(0, max(rec(x))*1.4), xlab='SSB', ylab="Recruitment (annual)")
  points(ssb(x), rec(x), col="grey")
  points(trim(ssb(x), year=rec_yrs), trim(rec(x),year=rec_yrs))
  
  
})



setMethod("plot", signature(x="array"), function(x,...){
  
  if(any(names(dimnames(x)) != c("to",  "from",   "age",    "period")))
    plot(x, ...)
  
  if(all(names(dimnames(x)) == c("to",  "from",  "age",    "period"))){
    oldpar <- par()
    
    move <- data.frame(from  = 1:dim(x)[1], 
                       to    = rep(1:dim(x)[2], each=dim(x)[1]), 
                       age   = rep(1:dim(x)[3], each=prod(dim(x)[c(1,2)])),
                       period= rep(1:dim(x)[4], each=prod(dim(x)[c(1,2,3)])),
                       move  = c(x))
    barchart(move~period|to*from, data=move, horiz=F, xlab="Season", ylab="Diffusion Coefficient", ...)
  }
    
})


setMethod("plot", signature(x="MFCLMSEControl"), function(x, y="missing", ...){
  
  sbsbf0 <- seq(0, 1, by=0.001)
  
  plot(sbsbf0, eval_hcr(x, sbsbf0), type="l", bty='n', ylim=c(0, max(eval_hcr(x, sbsbf0))), xlab="SB/SBF=0", ylab="Scalar")

})




