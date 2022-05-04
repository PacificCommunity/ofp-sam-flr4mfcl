#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


#' 'read.MFCLPseudoCatchEffort()' function for FLR4MFCL
#'
#' Reads in simulated catch and effort data from the MFCL generated files 'catch_sim' and 'effort_sim' and
#' returns a partially complete object of MFCLPseudo. The size composition slots of the MFCLPseudo object are not filled.
#'
#' There are now several functions for reading pseudo data :
#' \itemize{
#'   \item read.MFCLPseudo - reads both catch, effort and size comps. Works when you only have either pseudo length or weight composition data (e.g. skipjack). 
#'   \item read.MFCLCatchEffort - only reads in the simulated catch and effort data
#'   \item read.MFCLPseudoSizeComp - only reads the size comp data from test_lw_sim but works when you have both length and weight data.
#' }
#' @param catch A character string for the 'catch_sim' input file name.
#' @param effort A character string for the 'effort_sim' input file name.
#' @param projfrq An \linkS4class{MFCLFrq} object for the projection period over which the pseudo data have been generated.
#' @param ctrl An \linkS4class{MFCLMSEControl}
#' @param historical Boolean TRUE or FALSE If the simulated data include the historical period or just the projecion period.
#' @return An object of class \linkS4class{MFCLPseudo}.
#' @export
#' @seealso \code{\link{MFCLprojContrl}} \code{\link{MFCLFrq}} \code{\link{MFCLPar}}
#' @examples
#' \dontrun{
#' # Expanding an MFCLFrq, e.g. that was used from in an assessment
#' pseudo <- read.MFCLPseudoCatchEffort('catch_sim', 'effort_sim', projfrq, mseCtrl, historical=FALSE)
#' }
read.MFCLPseudoCatchEffort <- function(catch="catch_sim", effort="effort_sim", projfrq=projfrq, ctrl="missing", historical=FALSE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  res <- MFCLPseudo() 
  
  # read in pseudo catch data
  cc    <- readLines(catch)
  cdat  <- matrix(as.numeric(unlist(strsplit(trim.leading(cc[-grep("#", cc)]), split="[[:blank:]]+"))), nrow=2)
  cseed <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed", cc)], split="[[:blank:]]+"), el, 3)))
  
  # read in pseudo effort data
  ee    <- readLines(effort)
  edat  <- unlist(lapply(1:length(ee[-grep("#",ee)]), 
                         function(ii){as.numeric(unlist(strsplit(trim.leading(ee[-grep("#",ee)][ii]), split="[[:blank:]]+")))[-1]}))
  eseed <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed", ee)], split="[[:blank:]]+"), el, 3)))
  
  # create data frame for obs and pseudo data 
  projfrq_realz <- realisations(projfrq)
  
  if(!historical)
    projfrq_realz <- subset(projfrq_realz, year>=fprojyr(ctrl))
    
  tempdat       <- cbind(projfrq_realz, iter       =rep(0:nsims(ctrl), each=nrow(projfrq_realz)),
                                        catch.seed =rep(c(NA,cseed),   each=nrow(projfrq_realz)),
                                        effort.seed=rep(c(NA,eseed),   each=nrow(projfrq_realz)), row.names=NULL)
    
  tempdat[tempdat$iter>0,'catch']  <- cdat[2,]
  tempdat[tempdat$iter>0,'effort'] <- edat
  
  slot(res, "catcheff") <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month, tempdat$week),]
  
  # not fixed yet
  #slot(res, "freq")     <- cbind(freq(projfrq)[,1:4], ce_itns, freq(projfrq)[,c("penalty","length","weight")], lf_itns)         
  
  slot(res, "range")[c("minyear","maxyear")] <- range(tempdat$year)
  
  
  return(res)
}


#' 'read.MFCLPseudoSizeComp()' function for FLR4MFCL
#'
#' Reads in simulated length and weight size composition data from the MFCL generated file 'test_lw_sim' and
#' returns a partially complete object of MFCLPseudo. The 'catcheff' slot of the MFCLPseudo object is not filled.
#'
#' There are now several functions for reading pseudo data :
#' \itemize{
#'   \item read.MFCLPseudo - reads both catch, effort and size comps. Works when you only have either pseudo length or weight composition data (e.g. skipjack). 
#'   \item read.MFCLCatchEffort - only reads in the simulated catch and effort data
#'   \item read.MFCLPseudoSizeComp - only reads the size comp data from test_lw_sim but works when you have both length and weight data.
#' }
#' @param lw_sim A character string for the input file name.
#' @param projfrq An \linkS4class{MFCLFrq} object for the projection period over which the pseudo data have been generated.
#' @param ctrl An \linkS4class{MFCLMSEControl}
#' @param historical Boolean TRUE or FALSE If the simulated data include the historical period or just the projecion period.
#' @return An object of class \linkS4class{MFCLPseudo}.
#' @export
#' @seealso \code{\link{MFCLprojContrl}} \code{\link{MFCLFrq}} \code{\link{MFCLPar}}
#' @examples
#' \dontrun{
#' # Expanding an MFCLFrq, e.g. that was used from in an assessment
#' pseudo <- read.MFCLPseudoSizeComp('test_lw_sim', projfrq, mseCtrl, historical=FALSE)
#' }
read.MFCLPseudoSizeComp <- function(lw_sim='test_lw_sim', projfrq=projfrq, ctrl="missing", historical=FALSE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  res <- MFCLPseudo() 
  
  lfirst <- lf_range(projfrq)['LFFirst']; lwidth <- lf_range(projfrq)['LFWidth']; nlbins <- lf_range(projfrq)['LFIntervals']
  ll     <- seq(lfirst, lwidth*nlbins+lfirst-lwidth, by=lwidth)
  
  wfirst <- lf_range(projfrq)['WFFirst']; wwidth <- lf_range(projfrq)['WFWidth']; nwbins <- lf_range(projfrq)['WFIntervals']
  ww     <- seq(wfirst, wwidth*nwbins+wfirst-wwidth, by=wwidth)
  
  pobs <- readLines(lw_sim)                                # read in the pseudo length frequencies
  markers <- c(grep('Simulated', pobs), length(pobs)+1)    # identify each iteration (both length and weight)
  pobs.w.df <- pobs.l.df <- data.frame()
  
  for(ii in 2:length(markers)){
    pobstemp <- pobs[markers[ii-1]:(markers[ii]-1)]
    nrecords <- (length(pobstemp)-3)/3
    iter     <- as.numeric(unlist(strsplit(pobstemp[2], "[[:blank:]]+"))[3])
    realzid  <- as.numeric(c(unlist(strsplit(trim.leading(pobstemp[seq(4, length=nrecords, by=3)]), split="[[:blank:]]+"))))
    
    if(grepl('length', pobstemp[1])){
      pobs.l.df  <- rbind(pobs.l.df, data.frame(year   =rep(realzid[seq(1, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                            month  =rep(realzid[seq(2, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                            week   =rep(realzid[seq(3, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                            fishery=rep(realzid[seq(4, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                            length = ll, 
                                            weight = NA,
                                            freq   =as.numeric(c(unlist(strsplit(trim.leading(pobstemp[seq(6, length=nrecords, by=3)]), split="[[:blank:]]+")))),
                                            iter   = iter)) 
    }
    if(grepl('weight', pobstemp[1])){
      pobs.w.df  <- rbind(pobs.w.df, data.frame(year   =rep(realzid[seq(1, length=nrecords, by=4)], each=lf_range(projfrq)['WFIntervals']),
                                            month  =rep(realzid[seq(2, length=nrecords, by=4)], each=lf_range(projfrq)['WFIntervals']),
                                            week   =rep(realzid[seq(3, length=nrecords, by=4)], each=lf_range(projfrq)['WFIntervals']),
                                            fishery=rep(realzid[seq(4, length=nrecords, by=4)], each=lf_range(projfrq)['WFIntervals']),
                                            length = NA, 
                                            weight = ww,
                                            freq   =as.numeric(c(unlist(strsplit(trim.leading(pobstemp[seq(6, length=nrecords, by=3)]), split="[[:blank:]]+")))),
                                            iter   = iter)) 
    }
  }
  
  l_frq(res)    <- pobs.l.df #subset(pobs.df, !is.na(pobs.df$length))
  w_frq(res)    <- pobs.w.df #subset(pobs.df, !is.na(pobs.df$weight))
  slot(res, 'range')    <- c(min=min(ll), max=max(ll), plusgroup=NA, minyear=min(pobs.l.df$year), maxyear=max(pobs.l.df$year))
  age_nage(res) <- age_nage(projfrq)
  
  return(res)
}







#' read.MFCLPseudo
#'
#' Reads information from the pseudo generation files and creates an MFCLPseudo object.
#' This method is a complicated mess and really needs to be simplified.
#'
#' @param catch:  A character string giving the name and path of the catch.sim file to be read 
#' @param effort: A character string giving the name and path of the effort.sim file to be read 
#' @param lw_sim: A character string giving the name and path of the lw_sim file to be read 
#' @param range: 
#' 
#'
#' @return An object of class MFCLPseudo
#'
#' @examples
#'
#' @export

# kk <- read.MFCLPseudo(catch="catch_sim", effort="effort_sim", lw_sim="test_lw_sim", projfrq=projfrq, ctrl=projCtrl)

read.MFCLPseudo <- function(catch="missing", effort="missing", lw_sim="missing", projfrq="missing", ctrl="missing", historical=TRUE) {
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  #browser()
  res <- MFCLPseudo() 
  
  if(!missing(catch) & !file.exists(catch))
    stop("catch.sim file does not exist")
  if(!missing(effort) & !file.exists(effort))
    stop("effort.sim file does not exist")
  if(!missing(lw_sim) & !file.exists(catch))
    stop("lw_sim file does not exist")
  if(!(class(projfrq)=="MFCLFrq"))
    stop("projfrq must be an object of class MFCLFrq")
  if(!inherits(ctrl, "MFCLprojControl"))
    stop("ctrl must be an object of class MFCLprojControl")  
  if(length(fprojyr(ctrl))==0)
    warning("fprojyr(ctrl)==0, object may be incomplete")
  
  # CATCH AND EFFORT 
  if(!missing(catch)&!missing(effort)){
    # read in pseudo catch data
    cc    <- readLines(catch)
    cdat  <- matrix(as.numeric(unlist(strsplit(trim.leading(cc[-grep("#", cc)]), split="[[:blank:]]+"))), nrow=2)
    cseed <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed", cc)], split="[[:blank:]]+"), el, 3)))
  
    # read in pseudo effort data
    ee    <- readLines(effort)
    edat  <- unlist(lapply(1:length(ee[-grep("#",ee)]), 
                           function(ii){as.numeric(unlist(strsplit(trim.leading(ee[-grep("#",ee)][ii]), split="[[:blank:]]+")))[-1]}))
    eseed <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed", ee)], split="[[:blank:]]+"), el, 3)))
  
    # create data frame for obs and pseudo data 
    
    if(!historical){
      len     <- nrow(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),])
      tempdat <- cbind(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),], 
                       iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len), 
                       row.names=NULL)
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
      
      tempdat$catch[tempdat$iter>0] <- rep(cdat[2,], each=length(unique(tempdat$length)))
      tempdat$effort[tempdat$iter>0]<- rep(edat,     each=length(unique(tempdat$length)))
      }
    
    if(historical){
      tempdat <- realisations(projfrq)
      len <- dim(tempdat)[1] 
      tempdat <- cbind(tempdat,
                  iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len), row.names=NULL)
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
      tempdat$catch[tempdat$iter>0] <- cdat[2,]
      tempdat$effort[tempdat$iter>0]<- edat
    }
  }
  
  # LENGTH COMPOSITIONS
  if(!missing(lw_sim)){
    ## read in the pseudo length frequencies
    pobs <- readLines(lw_sim)
    markers <- lapply(1:nsims(ctrl), function(x){grep(paste('# projection',x), pobs)})
  
    lfirst <- lf_range(projfrq)['LFFirst']; lwidth <- lf_range(projfrq)['LFWidth']; nlbins <- lf_range(projfrq)['LFIntervals']
    ll     <- seq(lfirst, lwidth*nlbins+lfirst-lwidth, by=lwidth)
  
    pobs.df <- data.frame()
    for(ss in 1:nsims(ctrl)){
      tempdat2<- pobs[(markers[[ss]][1]+2):(markers[[ss]][2]-2)]
      # strip out the fishery realization data
      realzid <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(1, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
      # strip out the length frequency data
      llfreq  <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(3, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
    
      pobs.df <- rbind(pobs.df, data.frame(year   =rep(realzid[seq(1, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           month  =rep(realzid[seq(2, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           week   =rep(realzid[seq(3, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           fishery=rep(realzid[seq(4, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           length =ll, weight=NA, freq= llfreq, iter=ss))
    }
  }
 
  slot(res, "catcheff") <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month, tempdat$week),]
  if(exists('pobs.df'))
    slot(res, "l_frq")    <- pobs.df[order(pobs.df$iter,  pobs.df$fishery, pobs.df$year, pobs.df$month, pobs.df$week, pobs.df$length),]  

  
  if(!historical) { # think this is OK !!
    slot(res, "catcheff")[slot(res, "catcheff")$iter>0,]$freq <- slot(res, "l_frq")$freq  
    slot(res, 'freq') <- slot(res, 'catcheff')[slot(res, "catcheff")$iter==0,c(1:4,7:9)]
    for(ii in 0:nsims(ctrl)){
      tmpdat <-  slot(res,'catcheff')[slot(res,'catcheff')$iter==ii, c("catch",'effort','freq')]
      colnames(tmpdat) <- paste(c('catch','effort','freq'), '_', ii, sep="")
      slot(res,'freq') <- cbind(slot(res,'freq'), tmpdat)           
    }
  }
  
  if(historical){
  # catch and effort vectors in projfrq format - ie you can just paste these into the catch and effort columns of the freq(projfrq)
  # first of all re-order your projfrq  
  freq(projfrq) <- freq(projfrq)[order(freq(projfrq)$fishery, freq(projfrq)$year, freq(projfrq)$month, freq(projfrq)$week, freq(projfrq)$length),]
      
  lfs      <- c(table(freq(projfrq)$month, freq(projfrq)$year, freq(projfrq)$fishery))
  ce_itns  <- cbind(freq(projfrq)$catch, freq(projfrq)$effort)
  
  for(ii in 1:nsims(ctrl))
    ce_itns <- cbind(ce_itns, rep(catcheff(res)[catcheff(res)$iter==ii, 'catch'], lfs[lfs>0]), rep(catcheff(res)[catcheff(res)$iter==ii, 'effort'], lfs[lfs>0]))
  
  colnames(ce_itns) <- paste(c("catch","effort"), rep(0:nsims(ctrl),each=2), sep="_")

  # size frequency data in projfrq format
  tmpfreq1 <- cbind(freq(projfrq), mkr=1:nrow(freq(projfrq)))
  tmpfreq  <- rbind(tmpfreq1[!is.na(tmpfreq1$length),], tmpfreq1[is.na(tmpfreq1$length),])

  lf_itns <- tmpfreq$freq
  for(ii in 1:nsims(ctrl))
    lf_itns <- cbind(lf_itns, c(l_frq(res)[l_frq(res)$iter==ii, 'freq'], freq(projfrq)[is.na(freq(projfrq)$length),'freq']))

  lf_itns <- lf_itns[order(tmpfreq$mkr),]
  colnames(lf_itns) <- paste("freq", 0:nsims(ctrl), sep="_")
  
  slot(res, "freq") <- cbind(freq(projfrq)[,1:4], ce_itns, freq(projfrq)[,c("penalty","length","weight")], lf_itns)         
  }

  slot(res, "range")[c("minyear","maxyear")] <- range(tempdat$year)
  
  if(exists('pobs.df'))
    slot(res, "range")[c("min","max")]         <- range(pobs.df$length)
  
  return(res)
}

# Alternative function that uses the alternative length composition output file.
# The result is the same but the code is marginally clearer.
read.MFCLPseudoAlt <- function(catch="missing", effort="missing", lw_sim="missing", projfrq="missing", ctrl="missing", historical=TRUE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  res <- MFCLPseudo() 
  
  if(!missing(catch) & !file.exists(catch))
    stop("catch.sim file does not exist")
  if(!missing(effort) & !file.exists(effort))
    stop("effort.sim file does not exist")
  if(!missing(lw_sim) & !file.exists(catch))
    stop("lw_sim file does not exist")
  if(!(class(projfrq)=="MFCLFrq"))
    stop("projfrq must be an object of class MFCLFrq")
  if(!inherits(ctrl, "MFCLprojControl"))
    stop("ctrl must be an object of class MFCLprojControl")  
  if(length(fprojyr(ctrl))==0)
    warning("fprojyr(ctrl)==0, object may be incomplete")

  nlbins <- lf_range(projfrq)['LFIntervals']

  # CATCH AND EFFORT 
  if(!missing(catch)&!missing(effort)){
    # read in pseudo catch data
    cc    <- readLines(catch)
    cdat  <- matrix(as.numeric(unlist(strsplit(trim.leading(cc[-grep("#", cc)]), split="[[:blank:]]+"))), nrow=2)
    cseed <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed", cc)], split="[[:blank:]]+"), el, 3)))
  
    # read in pseudo effort data
    ee    <- readLines(effort)
    edat  <- unlist(lapply(1:length(ee[-grep("#",ee)]), 
                           function(ii){as.numeric(unlist(strsplit(trim.leading(ee[-grep("#",ee)][ii]), split="[[:blank:]]+")))[-1]}))
    eseed <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed", ee)], split="[[:blank:]]+"), el, 3)))
  
    # create data frame for obs and pseudo data 
    
    if(!historical){
      # MFCL output files will not include historical data
      len     <- nrow(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),])
      tempdat <- cbind(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),], 
                       iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len), 
                       row.names=NULL)
      # Ordering of tempdat must be the same as cdat:  timestep (month, year, week), fishery, iter
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
      tempdat$catch[tempdat$iter>0] <- rep(cdat[2,], each=nlbins)
      tempdat$effort[tempdat$iter>0]<- rep(edat,     each=nlbins)
      }
    
    if(historical){
      len     <- nrow(freq(projfrq)[is.element(freq(projfrq)$length, c(NA,lf_range(projfrq)['LFFirst'])),])
      tempdat <- cbind(freq(projfrq)[is.element(freq(projfrq)$length, c(NA,lf_range(projfrq)['LFFirst'])),], 
                       iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len), 
                       row.names=NULL)
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
      tempdat$catch[tempdat$iter>0] <- cdat[2,]
      tempdat$effort[tempdat$iter>0]<- edat
    }
    
    
  }
  # LENGTH COMPOSITIONS
  if(!missing(lw_sim)){
    lfirst <- lf_range(projfrq)['LFFirst']; lwidth <- lf_range(projfrq)['LFWidth']; 
    ll     <- seq(lfirst, lwidth*nlbins+lfirst-lwidth, by=lwidth)

    # Read in the pseudo length frequencies
    # This file only has the pseudo length frequencies, the weight frequencies will be in another file
    pobs <- readLines(lw_sim)
    # Drop all the lines beginning with #
    pobs <- pobs[-c(grep("# ", pobs))]
    # length of data should be same as length of ll
    # Should check first line to make sure

    # Data structure is: projection seed year month week fishery sum(lensamp) data, separated by species
    pobs.df <- read.table(text = pobs)
    # drop the seed and sum columns 
    pobs.df <- pobs.df[,-c(2,7)]
    pobs.df <- reshape(pobs.df, idvar=1:5, varying=list(6:ncol(pobs.df)), v.names = "V", direction = "long") # melt using stats!
    colnames(pobs.df) <- c("iter", "year", "month", "week", "fishery", "length", "freq")
    pobs.df$weight <- NA
    # reorder before adding length
    pobs.df <- pobs.df[order(pobs.df$iter,  pobs.df$fishery, pobs.df$year, pobs.df$month, pobs.df$week),]
    rownames(pobs.df) <- 1:nrow(pobs.df)
    pobs.df$length <- ll
  }
 
  slot(res, "catcheff") <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month, tempdat$week),]
  slot(res, "l_frq")    <- pobs.df[order(pobs.df$iter,  pobs.df$fishery, pobs.df$year, pobs.df$month, pobs.df$week, pobs.df$length),]

  if(!historical) { # think this is OK !!
    slot(res, "catcheff")[slot(res, "catcheff")$iter>0,]$freq <- slot(res, "l_frq")$freq  
    slot(res, 'freq') <- slot(res, 'catcheff')[slot(res, "catcheff")$iter==0,c(1:4,7:9)]
    for(ii in 0:nsims(ctrl)){
      tmpdat <-  slot(res,'catcheff')[slot(res,'catcheff')$iter==ii, c("catch",'effort','freq')]
      colnames(tmpdat) <- paste(c('catch','effort','freq'), '_', ii, sep="")
      slot(res,'freq') <- cbind(slot(res,'freq'), tmpdat)           
    }
  }
  
  if(historical){
    # catch and effort vectors in projfrq format - ie you can just paste these into the catch and effort columns of the freq(projfrq)
    # first of all re-order your projfrq - this order needs to be the same as the l_freq slot of res 
    freq(projfrq) <- freq(projfrq)[order(freq(projfrq)$fishery, freq(projfrq)$year, freq(projfrq)$month, freq(projfrq)$week, freq(projfrq)$length),]
      
    lfs      <- c(table(freq(projfrq)$week, freq(projfrq)$month, freq(projfrq)$year, freq(projfrq)$fishery))
    ce_itns  <- cbind(freq(projfrq)$catch, freq(projfrq)$effort)
    for(ii in 1:nsims(ctrl)){
      ce_itns <- cbind(ce_itns, rep(catcheff(res)[catcheff(res)$iter==ii, 'catch'], lfs[lfs>0]), rep(catcheff(res)[catcheff(res)$iter==ii, 'effort'], lfs[lfs>0]))
    }
    colnames(ce_itns) <- paste(c("catch","effort"), rep(0:nsims(ctrl),each=2), sep="_")

    # Can do above with reshape - but takes slightly longer
    #spread_catcheff <- reshape(catcheff(res)[,c("year","month","week","fishery","catch","effort","iter")],
    #                idvar=c("year","month","week","fishery"), timevar="iter", direction="wide")
    ## Why can't I include penalty, length and weight here?
    #spread_catcheff <- spread_catcheff[order(spread_catcheff$fishery, spread_catcheff$year, spread_catcheff$month, spread_catcheff$week),]
    #ce_itns <- spread_catcheff[rep(seq_len(nrow(spread_catcheff)), lfs[lfs>0]),]


    # size frequency data in projfrq format
    tmpfreq1 <- cbind(freq(projfrq), mkr=1:nrow(freq(projfrq)))
    tmpfreq  <- rbind(tmpfreq1[!is.na(tmpfreq1$length),], tmpfreq1[is.na(tmpfreq1$length),])

    lf_itns <- tmpfreq$freq
    for(ii in 1:nsims(ctrl))
      lf_itns <- cbind(lf_itns, c(l_frq(res)[l_frq(res)$iter==ii, 'freq'], freq(projfrq)[is.na(freq(projfrq)$length),'freq']))


    lf_itns <- lf_itns[order(tmpfreq$mkr),]
    colnames(lf_itns) <- paste("freq", 0:nsims(ctrl), sep="_")

    slot(res, "freq") <- cbind(freq(projfrq)[,1:4], ce_itns, freq(projfrq)[,c("penalty","length","weight")], lf_itns)         
    #slot(res, "freq") <- cbind(ce_itns, freq(projfrq)[,c("penalty","length","weight")], lf_itns)         
  }

  slot(res, "range")[c("minyear","maxyear")] <- range(tempdat$year)
  slot(res, "range")[c("min","max")]         <- range(pobs.df$length)
  
  return(res)
}





#' read.MFCLCatchSim
#'
#' Reads information from the pseudo generation catch_sim file and creates a data.frame  object.
#' The object here is to break down the read.MFCLPseudo function in smaller, more manageable code units.
#'
#' @param catch:     A character string giving the name and path of the catch.sim file to be read 
#' @param projfrq:   The projfrq object used to generate the pseudo data
#' @param ctrl:      The control object used to generate teh pseudo data
#' @param historical Boolean flag specifying if historical data included 
#' 
#'
#' @return An object of class data.frame that can be subset to freq
#'
#' @examples
#'
#' @export

# kk <- read.MFCLCatch_sim(catch="catch_sim", projfrq=projfrq, ctrl=projCtrl, historical=TRUE)

read.MFCLCatchSim <- function(catch="catch_sim", projfrq="missing", ctrl="missing", historical=TRUE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  if(missing(projfrq) | missing(ctrl))
    stop("Error: The projfrq and ctrl are also reqquired.")
  if(!missing(catch) & !file.exists(catch))
    stop("catch_sim file does not exist")
  if(!(class(projfrq)=="MFCLFrq"))
    stop("projfrq must be an object of class MFCLFrq")
  if(!inherits(ctrl, "MFCLprojControl"))
    stop("ctrl must be an object of class MFCLprojControl")  
  if(length(fprojyr(ctrl))==0)
    warning("fprojyr(ctrl)==0, object may be incomplete")
    
    cc    <- readLines(catch)
    cdat  <- matrix(as.numeric(unlist(strsplit(trim.leading(cc[-grep("#", cc)]), split="[[:blank:]]+"))), nrow=2)
    cseed <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed", cc)], split="[[:blank:]]+"), el, 3)))
    
    if(!historical){
      # MFCL output files will not include historical data
      len     <- nrow(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),])
      tempdat <- cbind(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),], 
                       iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), 
                       row.names=NULL)
      # Ordering of tempdat must be the same as cdat:  timestep (month, year, week), fishery, iter
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
      tempdat$catch[tempdat$iter>0] <- rep(cdat[2,], each=nlbins)
      tempdat$effort[tempdat$iter>0]<- rep(edat,     each=nlbins)
    }
    
    if(historical){
      len     <- nrow(realisations(projfrq))
      tempdat <- cbind(realisations(projfrq), iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len),row.names=NULL)
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
      tempdat$catch[tempdat$iter>0] <- cdat[2,]
    }
    
    return(tempdat)
}


## Finlays code - stolen from the mixed fishery folder - with modified inputs to be consistent with read.MFCLCatchSim
##
read.MFCLEffortSim <- function(effort="effort_sim", projfrq='missing', ctrl='missing', historical=TRUE){
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  ee    <- readLines(effort)
  edat  <- unlist(lapply(1:length(ee[-grep("#",ee)]), 
                         function(ii){as.numeric(unlist(strsplit(trim.leading(ee[-grep("#",ee)][ii]), split="[[:blank:]]+")))[-1]}))
  eseed <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed", ee)], split="[[:blank:]]+"), el, 3)))
  realprojfrq <- realisations(projfrq)
  # I don't know if this works yet
  if (!historical){
    realprojfrq <- subset(realprojfrq, year>= fprojyr(ctrl))
  }
  
  tempdat <- realprojfrq
  len <- dim(tempdat)[1] 
  tempdat <- cbind(tempdat, iter=rep(0:nsims(ctrl), each=len), effort.seed=rep(c(NA,eseed), each=len), row.names=NULL)
  tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
  tempdat$effort[tempdat$iter>0] <- edat
  return(tempdat)
}
