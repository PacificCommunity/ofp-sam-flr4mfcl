


#' effort_dev_coffs
#'
#' Strip out effort devs from an MFCLPar object including the realisations information from the frq file
#'
#' @param object An object of class MFCLPar.
#' @param frq The corresponding MFCLFrq for the MFCLPar.
#' 
#' @seealso \code{\link{MFCLFrq}},  \code{\link{MFCLPar}} and \code{\link{MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#' @aliases mfcl

setMethod("effort_dev_coffs", signature(object="MFCLPar", frq="MFCLFrq"), 
          function(object, frq, ...){
            
            effdevs <- data.frame(cbind(realisations(frq), effdevs  = unlist(effort_dev_coffs(object)) ))
                                        #yrqtr = realisations(frq)$year+(realisations(frq)$month-0.5)/12, 
            
            effdevs_full <- expand.grid(year=unique(effdevs$year), fishery=unique(effdevs$fishery), month=unique(effdevs$month))
            
            effdevs_fullx<- merge.data.frame(effdevs_full, effdevs[,c('year','month','fishery','effdevs')], all.x=TRUE)
            
            effdevs_quant <- cbind(age='all', effdevs_fullx,  area='unique', iter=1)
            
            names(effdevs_quant)    <- c('age','year','unit','season', 'data', 'area','iter')
            
            return(as.FLQuant(effdevs_quant))
          })

#par    <- read.MFCLPar('/home/rob/MSE/ofp-sam-mixed-fishery-MSE/BET/assessment/2020/1_TagInt_Hi_20_0.65/09.par')
#frq    <- read.MFCLFrq('/home/rob/MSE/ofp-sam-mixed-fishery-MSE/BET/assessment/2020/1_TagInt_Hi_20_0.65/bet.frq')
#effdev <- effort_dev_coffs(par, frq)
#xyplot(data~year|unit, group=iter, data=qts(effort_dev_coffs(par, frq)), type="l")
