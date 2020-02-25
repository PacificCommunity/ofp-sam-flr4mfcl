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
#' @param projfrq: An object of class MFCLFrq with projections
#' @param ctrl:an object of class MFCLprojControl
#' @param historical:Binary whether to get catch and effort for the historical period from projfrq (TRUE) or catch and effort files (FALSE)
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
  ## splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))

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
      len     <- nrow(cateffpen(projfrq)[cateffpen(projfrq)$year>=fprojyr(ctrl),])
      tempdat <- cbind(cateffpen(projfrq)[cateffpen(projfrq)$year>=fprojyr(ctrl),],
                       iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len),
                       row.names=NULL)
      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]

      tempdat$catch[tempdat$iter>0] <- cdat[2,]
      tempdat$effort[tempdat$iter>0]<- edat
      }

    if(historical){
      len     <- nrow(cateffpen(projfrq))
      tempdat <- cbind(cateffpen(projfrq),
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
    ## markers <- lapply(1:nsims(ctrl), function(x){grep(paste('# projection',x), pobs)})
    LnMarkers <- grep("# Simulated length frequencies",pobs)
    WtMarkers <- grep("# Simulated weight frequencies",pobs)


    lfirst <- lf_range(projfrq)['LFFirst']; lwidth <- lf_range(projfrq)['LFWidth']; nLbins <- lf_range(projfrq)['LFIntervals']
    if(lfirst==0){ ll <- NA} else{ll <- seq(lfirst, lwidth*nLbins+lfirst-lwidth, by=lwidth)}

    wfirst <- lf_range(projfrq)['WFFirst']; wwidth <- lf_range(projfrq)['WFWidth']; nWbins <- lf_range(projfrq)['WFIntervals']
    if(wfirst==0){ wts <- NA} else{wts <- seq(wfirst, wwidth*nWbins+wfirst-wwidth, by=wwidth)}

    lobs.df <- data.frame()
    wtobs.df <- data.frame()
    for(ss in 1:length(LnMarkers)){
      if ((LnMarkers[ss]+3)!=WtMarkers[ss]){                   #Check there is length simulated
        tempdat2<- pobs[(LnMarkers[ss]+3):(WtMarkers[ss]-1)]
        # strip out the fishery realization data
        realzid <- matrix(as.numeric(unlist(strsplit(trim.leading(tempdat2[seq(1, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))),ncol=4,byrow=TRUE)
        # strip out the length frequency data
        ## llfreq  <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(3, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
        llfreq=matrix(as.numeric(unlist(strsplit(trim.leading(tempdat2[seq(3, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))),ncol=nLbins,byrow=TRUE)
        lobs.df <- rbind(lobs.df,cbind(realzid,ss,llfreq))
      }

      if((WtMarkers[ss]+3)!=LnMarkers[ss+1] & wfirst>0){ #Check there is weight composition
        tempdat3<- pobs[(WtMarkers[ss]+3):ifelse(ss==length(WtMarkers),length(pobs),LnMarkers[ss+1]-1)]
        # strip out the fishery realization data
        wid <- as.numeric(c(unlist(strsplit(trim.leading(tempdat3[seq(1, length=length(tempdat3)/3, by=3)]), split="[[:blank:]]+"))))
        # strip out the weight frequency data
        wtfreq  <- as.numeric(c(unlist(strsplit(trim.leading(tempdat3[seq(3, length=length(tempdat3)/3, by=3)]), split="[[:blank:]]+"))))
        wtobs.df <- rbind(wtobs.df,cbind(wid,ss,wtfreq))
      }
    }
    if(length(lobs.df)>0)  names(lobs.df) <- c("year","month","week","fishery","iter",ll)
    if(length(wtobs.df)>0)  names(wtobs.df) <- c("year","month","week","fishery","iter",wts)
  }


  slot(res, "catcheff") <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month, tempdat$week),]
  if(length(lobs.df)>0) slot(res, "l_frq")    <- lobs.df[order(lobs.df$iter,  lobs.df$fishery, lobs.df$year, lobs.df$month, lobs.df$week),]
  if(length(wtobs.df)>0) slot(res, "w_frq")    <- wtobs.df[order(wtobs.df$iter,  wtobs.df$fishery, wtobs.df$year, wtobs.df$month, wtobs.df$week),]


  ## if(!historical) { # think this is OK !!
  ##   slot(res, "catcheff")[slot(res, "catcheff")$iter>0,]$freq <- slot(res, "l_frq")$freq
  ##   slot(res, 'freq') <- slot(res, 'catcheff')[slot(res, "catcheff")$iter==0,c(1:4,7:9)]
  ##   for(ii in 0:nsims(ctrl)){
  ##     tmpdat <-  slot(res,'catcheff')[slot(res,'catcheff')$iter==ii, c("catch",'effort','freq')]
  ##     colnames(tmpdat) <- paste(c('catch','effort','freq'), '_', ii, sep="")
  ##     slot(res,'freq') <- cbind(slot(res,'freq'), tmpdat)
  ##   }
  ## }

  if(historical){
    # catch and effort vectors in projfrq format - ie you can just paste these into the catch and effort columns of the cateffpen(projfrq)
    warning("This probably doesn't work because the structure of the freq has changed")
    # first of all re-order your projfrq
    cateffpen(projfrq) <- cateffpen(projfrq)[order(cateffpen(projfrq)$fishery, cateffpen(projfrq)$year, cateffpen(projfrq)$month, freqcateffpen(projfrq)$week),]

    lfs      <- c(table(lnfrq(projfrq)$month, lnfrq(projfrq)$year, lnfrq(projfrq)$fishery))
  ce_itns  <- cbind(cateffpen(projfrq)$catch, cateffpen(projfrq)$effort)

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
  slot(res, "range")[c("min","max")]         <- range(ll)

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






