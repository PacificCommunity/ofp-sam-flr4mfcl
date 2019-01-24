#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

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






