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
  minfrq$yrqtr <- round(minfrq$year+minfrq$month/12,2)
  
  catch <- tapply(minfrq$catch, list(minfrq$yrqtr), sum)
  effort <- tapply(minfrq$effort, list(minfrq$yrqtr), sum)
  
  par(mfrow=c(2,2))
  barplot(catch, main="catch")
  barplot(effort, main='effort')
  
  
})


