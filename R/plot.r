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

  minfrq <- cateffpen(x)
  minfrq$yrqtr <- round(minfrq$year+(minfrq$month+1)/12,2)

  catch <- tapply(minfrq$catch, list(minfrq$yrqtr), sum)
  effort <- tapply(minfrq$effort, list(minfrq$yrqtr), sum)
  par(mfrow=c(1,2))

  barplot(catch, main="catch")
  barplot(effort, main='effort')

})



setMethod("plot", signature(x="MFCLLenFreq", y="MFCLprojControl"), function(x, y, fleets='all', ...){

  if(is.numeric(fleets)){
    cateffpen(x) <- cateffpen(x)[is.element(cateffpen(x)$fishery, fleets),]
  }

  minfrq <- cateffpen(x)
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





