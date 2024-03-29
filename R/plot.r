#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' plot
#'
#' Plot MFCL objects.
#'
#' @param obj An object of class MFCLX.
#' @param \dots Additional argument list that might not ever be used.
#'
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' \dontrun{
#' plot(MFCLFrq())
#' }
#'
#' @aliases mfcl-plots

setMethod("plot", signature(x="MFCLLenFreq"), function(x, y="missing", ...){
  
  minfrq <- freq(x)[is.element(freq(x)$length, c(NA, lf_range(x)['LFFirst'])) | is.element(freq(x)$weight, lf_range(x)['WFFirst']),]
  minfrq$yrqtr <- round(minfrq$year+(minfrq$month+1)/12,2)
  
  catch <- tapply(minfrq$catch, list(minfrq$yrqtr), sum)
  effort <- tapply(minfrq$effort, list(minfrq$yrqtr), sum)
  par(mfrow=c(1,2))  
  
  barplot(catch, main="catch")
  barplot(effort, main='effort')
  
  par(mfrow=c(1,1))
  lff <- unique(subset(freq(x), !is.na(length) & freq>0)[,c('year','month','fishery')])
  lff$year <- lff$year + (lff$month+1)/12-0.125
  
  wff <- unique(subset(freq(x), !is.na(weight) & freq>0)[,c('year','month','fishery')])
  wff$year <- wff$year + (wff$month+1)/12-0.125
  
  plot(lff$year, lff$fishery, pch=3, col='red', 
       ylim=c(0, max(c(lff$fishery, wff$fishery))),
       xlab="Year", ylab="Fishery", main="Available Length and Weight Frequency Data")
  abline(h=1:max(c(lff$fishery, wff$fishery)), col="grey")
  
  points(wff$year, wff$fishery, pch=1, col="blue")
})


#' @export
#' @aliases mfcl-plots

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



#' @export
#' @aliases mfcl-plots

setMethod("plot", signature(x="MFCLRep", y="MFCLPar"), function(x, y, ...){
  
  rec_range <- range(y)['maxyear']-(recPeriod(y)[c(1,2)]/4)
  rec_yrs   <- seq(rec_range[1], rec_range[2])+1
  
  ssbx <- seq(0, max(ssb(x))*1.5, length=150)
  
  plot(ssbx, (c(srr(x)['a'])*ssbx)/(c(srr(x)['b'])+ssbx), type="l", ylim=c(0, max(rec(x))*1.4), xlab='SSB', ylab="Recruitment (annual)")
  points(ssb(x), rec(x), col="grey")
  points(trim(ssb(x), year=rec_yrs), trim(rec(x),year=rec_yrs))
  
  
})



#' @export
#' @aliases mfcl-plots

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


#' @export
#' @aliases mfcl-plots

setMethod("plot", signature(x="MFCLMSEControl"), function(x, y="missing", ...){
  
  #args <- list(...)
  sbsbf0 <- seq(0, 1, by=0.001)
  
  # remove the constrained option so you're just looking at the HCR
  if(grepl('_constrained', hcr(x)))
    hcr(x) <- gsub('_constrained', '', hcr(x))
  
  plot(sbsbf0, eval_hcr(x, sbsbf0), type="l", bty='n', xlab="SB/SBF=0", ylab="Scalar", ...)
})


#' @export
#' @aliases mfcl-plots
#' 
#lfityft <- read.MFCLLenFit2('/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/length.fit', get_lenage=F)
setMethod("plot", signature(x="MFCLLenFit"),
          function(x,y,..., col=c('orange2','steelblue4')){
            #browser()
            sd_dat <- aggregate(lenfits(x)$obs, by=list(lenfits(x)$month, lenfits(x)$year, lenfits(x)$fishery), sd)
            colnames(sd_dat) <- c('month', 'year', 'fishery', 'sd')
            
            lenfits(x)$yrqtr <- lenfits(x)$year+lenfits(x)$month/12
            lenfits(x) <- merge(lenfits(x), sd_dat)   
            lenfits(x)$pearson <- (lenfits(x)$obs - lenfits(x)$pred)/lenfits(x)$sd
            if(!('names' %in% colnames(lenfits(x))))
              lenfits(x)$names <-  lenfits(x)$fishery
              
            #llx <<- lenfits(x)
            
            pfun <- function(x,y, cexdat, ...){
              #browser()
              cex <- subset(cexdat, fishery==unique(cexdat$fishery)[panel.number()])$pearson
              cols <- rep(col[2], length(cex))
              cols[cex<0] <- col[1]
              scalar <- max(cex)/max(cexdat$pearson)
              panel.xyplot(x,y,..., type='p', cex=abs(cex)*scalar, col=cols)
            }
            xyplot(length~yrqtr|as.factor(names), data=lenfits(x), xlab='YrQtr', ylab="Length", panel=pfun, cexdat=lenfits(x))
            #xyplot(length~year|as.factor(month)*as.factor(fishery), data=lenfits(x), xlab='YrQtr', ylab="Length", panel=pfun, cexdat=lenfits(x))
            
          })


#' @export
#' @aliases mfcl-plots

# wfityft <- read.MFCLWgtFit('/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/weight.fit')

setMethod("plot", signature(x="MFCLWgtFit"),
          function(x,y,..., col=c('tomato', 'steelblue4')){
            
            sd_dat <- aggregate(wgtfits(x)$obs, by=list(wgtfits(x)$month, wgtfits(x)$year, wgtfits(x)$fishery), sd)
            colnames(sd_dat) <- c('month', 'year', 'fishery', 'sd')
            
            wgtfits(x)$yrqtr   <- wgtfits(x)$year+wgtfits(x)$month/12
            wgtfits(x)         <- merge(wgtfits(x), sd_dat)   
            wgtfits(x)$pearson <- (wgtfits(x)$obs - wgtfits(x)$pred)/wgtfits(x)$sd
            if(!('names' %in% colnames(wgtfits(x))))
              wgtfits(x)$names <-  wgtfits(x)$fishery
            #wwx <<- wgtfits(x)
            
            pfun <- function(x,y, cexdat, ...){
              #browser()
              cex <- subset(cexdat, fishery==unique(cexdat$fishery)[panel.number()])$pearson
              cols <- rep(col[2], length(cex))
              cols[cex<0] <- col[1]
              scalar <- max(cex)/max(cexdat$pearson)
              panel.xyplot(x,y,..., type='p', cex=abs(cex)*scalar, col=cols)
            }
            xyplot(weight~yrqtr|as.factor(names), data=wgtfits(x), xlab='YrQtr', ylab="Weight", panel=pfun, cexdat=wgtfits(x))
            #xyplot(length~year|as.factor(month)*as.factor(fishery), data=lenfits(x), xlab='YrQtr', ylab="Length", panel=pfun, cexdat=lenfits(x))
            
          })




#tag <- read.MFCLTag('/media/sf_assessments/yft/2023/model_runs/diagnostic/yft.tag')
#ttr <- read.temporary_tag_report('/media/sf_assessments/yft/2023/model_runs/diagnostic/temporary_tag_report', year1 = 1952)

#' @export
#' @aliases mfcl-plots

#tag <- read.MFCLTag('/media/sf_assessments/yft/2023/model_runs/diagnostic/yft.tag')
#ttr <- read.temporary_tag_report('/media/sf_assessments/yft/2023/model_runs/diagnostic/temporary_tag_report', year1 = 1952)


setMethod("plot", signature(x="MFCLTag", y="data.frame"),
          function(x,y,..., col=c('steelblue4', 'tomato'), program=NA){

            if(!is.element(program, c("NA", unique(releases(tag)$program))))
              stop(paste("program must be one of ", paste(c("NA", unique(releases(tag)$program)), collapse=" ")))
               
            tagdat <- merge(ttr, recaptures(tag)[,c('rel.group', 'region', 'year', 'month', 'program')])
            tagdat <- tagdat[!duplicated(tagdat),]

            tagdat$recap.obs[tagdat$recap.obs==0] <- NA
            tagdat$logResid <- log(tagdat$recap.obs)-log(tagdat$recap.pred)
            tagdat$tlib     <- (tagdat$recap.year+tagdat$recap.month/12) - (tagdat$year+tagdat$month/12)

            tagdat2    <- tagdat[!is.na(tagdat$logResid),]
            if(!is.na(program))
              tagdat2 <- tagdat2[tagdat2$program==program ,]

            pfun.tagresids <- function(x,y,...){
              panel.xyplot(x,y,...)
              panel.abline(h=1, col="grey")
              if(length(x)>3){
                if(summary(lm(y~x))$coefficients[2,4] < 0.05){
                  panel.abline(lm(y~x), col=col[2])
                  panel.lines(seq(0,3,by=0.1), predict(lm(y~x), newdata=list(x=seq(0, 3, by=0.1)), interval="confidence")[,'lwr'], col="grey")
                  panel.lines(seq(0,3,by=0.1), predict(lm(y~x), newdata=list(x=seq(0, 3, by=0.1)), interval="confidence")[,'upr'], col="grey")
                }
              }
            } 

            xyplot(logResid~tlib|as.factor(rel.group), data=tagdat2, col=col[1], panel=pfun.tagresids, 
                    xlab="Time at Liberty (years)", ylab="log(ObsRec) - log(PredRec)")
})


