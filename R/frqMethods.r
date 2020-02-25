#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' Methods for accessing and creating MFCLFrq and MFCLFrqOld objects
#'
#' Create a function that transforms the new 3 dataframe frq object into the old freq object
#' Keep this for backward compatability of code
#'

#'@export convertFreq
setGeneric('convertFreq',function(frq,...) standardCeneric('convertFreq'))

setMethod('convertFreq',signature(frq="MFCLLenFreq"), function(frq) {
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
    frq <- as.MFCLLenFreq2(frq)
    frq@freq <- dfall[order(dfall$fishery,dfall$year,dfall$month),]
    return(frq)
} )
