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



setMethod("plot", signature(x="MFCLLenFreq", y="MFCLprojControl"), function(x, y, ...){
  
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
      
  
  par(mfrow=c(2,2))
        
  barplot(catchcatch,   main="catch (catch projected fleets)", col=cols)
  barplot(efforteffort, main="effort (effort projected fleets)", col=cols)

  barplot(catch, main="catch")
  barplot(effort, main='effort')
  
})









