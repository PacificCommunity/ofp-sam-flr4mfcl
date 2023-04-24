#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' as.MFCLIni
#'
#' Convert an MFCLPar object to an MFCLIni object
#'
#' @param object An object of class MFCLPar.
#'
#' @return An MFCLIni object.
#' 
#' @export
#' @docType methods
#' @rdname par-methods

setGeneric('as.MFCLIni', function(object, ...) standardGeneric('as.MFCLIni')) 


setMethod("as.MFCLIni", signature(object="MFCLPar"),
          function(object, ...){
            
          res <- MFCLIni()
          for(ss in c(names(getSlots("MFCLBiol")), names(getSlots("MFCLBase")), names(getSlots("MFCLRegion")), names(getSlots("MFCLTagRep"))))
            slot(res, ss) <- slot(object, ss)
          
          # temporary hack because the region_control_flags have different names in the ini and par objects - needs fixing!. 17/01/2022
          region_flags(res) <- control_flags(res)
          
          age_pars(res)         <- matrix(0, nrow=10, ncol=dimensions(object)['agecls'])
          sv(res)               <- steepness(object)
          rec_dist(res)         <- rep(1/dimensions(object)['regions'], dimensions(object)['regions'])
          sd_length_at_age(res) <- c(5,1,9)
          sd_length_dep(res)    <- c(0.5,0,3)
          lw_params(res)        <- lw_params(object)
          
          ini_version(res)      <- ifelse(length(mat_at_length(res))>1, 1003, 1001)  # 17/01/2022 adding version control to as.ini
          return(res)
          })


## Matt's convertFreq code - renamed as.MFCLFrq
## 30/04/2020
setGeneric('as.MFCLFrq', function(frq, ...) standardGeneric('as.MFCLFrq'))

setMethod('as.MFCLFrq',signature(frq="MFCLFrq2"), function(frq) {
  cep <- cateffpen(frq)
  lnfrq <- lnfrq(frq)
  wtfrq <- wtfrq(frq)
  LnMatcher <- match(interaction(lnfrq[,1:4]),interaction(cep[,1:4]),nomatch=0)
  if(any(LnMatcher==0)) warning("There are entries in the length composition data frame that aren't in the catch effort penalty data frame")
  WtMatcher <- match(interaction(wtfrq[,1:4]),interaction(cep[,1:4]),nomatch=0)
  if(any(WtMatcher==0)) warning("There are entries in the weight composition data frame that aren't in the catch effort penalty data frame")
  
  LengthOnly <- (!(LnMatcher%in%WtMatcher))
  WeightOnly <- (!(WtMatcher%in%LnMatcher))
  LengthWeight <- (LnMatcher%in%WtMatcher)
  WeightLength <- (WtMatcher%in%LnMatcher)
  NoMatch=!(1:dim(cep)[1]%in%c(LnMatcher,WtMatcher))
  nLbins <- lf_range(frq)['LFIntervals']; Lwidth <- lf_range(frq)["LFWidth"]; Lfirst <- lf_range(frq)["LFFirst"]
  nWbins <- lf_range(frq)['WFIntervals']; Wwidth <- lf_range(frq)["WFWidth"]; Wfirst <- lf_range(frq)["WFFirst"]
  if(Lfirst>0) {frqlen <- seq(Lfirst, Lwidth*nLbins+Lfirst-Lwidth, by=Lwidth)}
  if(Wfirst>0) {frqwt  <- seq(Wfirst, Wwidth*nWbins+Wfirst-Wwidth, by=Wwidth)}
  ## No Length or weight
  dfall <- cbind(cep[NoMatch,],length=NA,weight=NA,freq=-1)
  ## Length only
  if (length(LnMatcher[!(LnMatcher%in%WtMatcher)])>0){
    dfall <- rbind(dfall,cbind(apply(cep[LnMatcher[LengthOnly],],2,rep,each=nLbins),length=frqlen,weight=NA,freq=as.vector(t(lnfrq[LengthOnly,-4:-1]))))
  }
  ## Weight only
  if (length(WtMatcher[!(WtMatcher%in%LnMatcher)])>0){
    dfall <- rbind(dfall,cbind(apply(cep[WtMatcher[WeightOnly],],2,rep,each=nWbins),length=NA,weight=frqwt,freq=as.vector(t(wtfrq[WeightOnly,-4:-1]))))
  }
  
  ## Length and weight
  if (length(WtMatcher[(WtMatcher%in%LnMatcher)])>0){
    dfall <- rbind(dfall,cbind(apply(cep[LnMatcher[LengthWeight],],2,rep,each=nLbins),length=frqlen,weight=NA,freq=as.vector(t(lnfrq[LengthWeight,-4:-1]))))
    dfall <- rbind(dfall,cbind(apply(cep[WtMatcher[WeightLength],],2,rep,each=nWbins),length=NA,weight=frqwt,freq=as.vector(t(wtfrq[WeightLength,-4:-1]))))
  }
  res <- MFCLFrq()
  for(ss in slotNames(res))
    slot(res, ss) <- slot(frq, ss)
  #frq <- as.MFCLLenFreq2(frq)
  freq(res) <- dfall[order(dfall$fishery,dfall$year,dfall$month),]
  return(res)
} )




## 04/05/2020
setGeneric('as.MFCLFrq2', function(frq, ...) standardGeneric('as.MFCLFrq2'))

setMethod('as.MFCLFrq2',signature(frq="MFCLFrq"), function(frq) {
  
  lnfrq_stuff <- subset(freq(frq), length==lf_range(frq)["LFFirst"])[,1:4]
  lnfrq_freq  <- t(matrix(subset(freq(frq), !is.na(length))$freq, nrow=lf_range(frq)['LFIntervals']))
  lnfrq       <- cbind(lnfrq_stuff, lnfrq_freq)
  if(nrow(lnfrq)>0)
    colnames(lnfrq)[5:(lf_range(frq)['LFIntervals']+4)] <- seq(lf_range(frq)['LFFirst'], by=lf_range(frq)['LFWidth'], length.out=lf_range(frq)['LFIntervals'])

  wtfrq_stuff <- subset(freq(frq), weight==lf_range(frq)["WFFirst"])[,1:4]
  wtfrq_freq  <- t(matrix(subset(freq(frq), !is.na(weight))$freq, nrow=lf_range(frq)['WFIntervals']))
  wtfrq       <- cbind(wtfrq_stuff, wtfrq_freq)
  if(nrow(wtfrq)>0)
    colnames(wtfrq)[5:(lf_range(frq)['WFIntervals']+4)] <- seq(lf_range(frq)['WFFirst'], by=lf_range(frq)['WFWidth'], length.out=lf_range(frq)['WFIntervals'])
  
  res <- new("MFCLFrq2")
  
  for(sn in slotNames(MFCLFrq()))
    slot(res, sn) <- slot(frq, sn)
    
  slot(res, "cateffpen") <- realisations(frq)
  slot(res, "lnfrq")     <- lnfrq
  slot(res, "wtfrq")     <- wtfrq
    
  return(res)
}
)
