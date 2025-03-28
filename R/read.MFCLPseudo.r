#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


# a bunch of functions to read in pseudo generated data
#
# read.MFCLPseudoCatch          - breaking down functions to smaller chunks
# read.MFCLPseudoEffort         - breaking down functions to smaller chunks
# read.MFCLPseudoSizeComp       - the main workhorse at the moment
# read.MFCLPseudoCatchEffort    - the main workhorse at the moment
# fillfreq
# read.MFCLPseudo               - calls read.MFCLPseudoCatchEffort and read.MFCLPseudoSizeComp
# read.MFCLPseudoAlt            - Not used for much at the moment
# read.MFCLCatchSim
# read.MFCLEffortSim

# breaking it all down to much simpler functions
#
read.MFCLPseudoCatch <- function(catch="catch_sim", projfrq=projfrq, ctrl="missing", historical=FALSE) {
  
  # read in pseudo catch data and grab some dimensions stuff
  cc    <- readLines(catch)
  
  nsims <- as.numeric(unlist(lapply(strsplit(cc[grep("# projection", cc)], split="[[:blank:]]+"), el, 3)))
  seeds <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed",       cc)], split="[[:blank:]]+"), el, 3)))
  nfish <- unique(as.numeric(unlist(lapply(strsplit(cc[grep("# fishery",    cc)], split="[[:blank:]]+"), el, 3))))
  
  # read in pseudo catch again - just the data
  cdat       <- read.table(catch)
  
  freqdat    <- realisations(projfrq)        ## function not yet tested with historical data !
  
  if(!historical)
    freqdat    <- subset(realisations(projfrq), year>=fprojyr(ctrl))
  
  freqdat    <- freqdat[order(freqdat$fishery, freqdat$year, freqdat$month),]
  freqdat    <- cbind(freqdat, catch_sim=unlist(c(cdat[,-1])), 
                               cseed    =rep(seeds, each=nrow(cdat)/length(seeds)), 
                               iter     =rep(1:length(seeds), each=nrow(cdat)/length(seeds)), 
                      row.names=NULL)
  
  return(freqdat)
}


# breaking it all down to much simpler functions
#
read.MFCLPseudoEffort <- function(effort="effort_sim", projfrq=projfrq, ctrl="missing", historical=FALSE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  
  # read in pseudo catch data and grab some dimensions stuff
  ee    <- readLines(effort)
  
  nsims <- as.numeric(unlist(lapply(strsplit(ee[grep("# projection", ee)], split="[[:blank:]]+"), el, 3)))
  seeds <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed",       ee)], split="[[:blank:]]+"), el, 3)))
  
  edat     <- strsplit(trim.leading(readLines(effort)), split="[[:blank:]]+")
  projfish <- unique(as.numeric(unlist(lapply(edat[-c(grep('#', edat))], el, 1))))   # projected fisheries - not all of them are projected
  
  edat     <- edat[-c(grep('#', edat))]           # drop the header information
  edat     <- lapply(edat, function(x){x[-1]})    # drop the fishery identifier
  
  freqdat  <- realisations(projfrq)
  
  if(!historical)
    freqdat    <- subset(realisations(projfrq), year>=fprojyr(ctrl))
  
  freqdat    <- freqdat[order(freqdat$fishery, freqdat$year, freqdat$month),]
  
  freqdat    <- cbind(freqdat, effort_sim = as.numeric(unlist(edat)), 
                               eseed      = rep(seeds, each=sum(unlist(lapply(edat, length)))/length(seeds)), 
                               iter       = rep(1:length(seeds), each=sum(unlist(lapply(edat, length)))/length(seeds)), row.names=NULL)
  
  return(freqdat)
  
}



#' Read MFCLPseudo Size Comp
#'
#' Reads in simulated length and weight size composition data from the MFCL generated file 'test_lw_sim' (or 'test_lw_sim_alt') and
#' returns a partially complete object of MFCLPseudo. The 'catcheff' slot of the MFCLPseudo object is not filled.
#'
#' There are now several functions for reading pseudo data :
#' \itemize{
#'   \item read.MFCLPseudo - reads both catch, effort and size comps. Works when you only have either pseudo length or weight composition data (e.g. skipjack). 
#'   \item read.MFCLCatchEffort - only reads in the simulated catch and effort data
#'   \item read.MFCLPseudoSizeComp - only reads the size comp data from test_lw_sim but works when you have both length and weight data.
#' }
#'
#' @param lw_sim A character string for the input file name.
#' @param projfrq An \linkS4class{MFCLFrq} object for the projection period over which the pseudo data have been generated.
#' @param ctrl An \linkS4class{MFCLMSEControl}.
#' @param historical Boolean TRUE or FALSE If the simulated data include the historical period or just the projecion period.
#'
#' @return An object of class \linkS4class{MFCLPseudo}.
#'
#' @export
#'
#' @seealso \code{\link{MFCLprojControl}} \code{\link{MFCLFrq}} \code{\link{MFCLPar}}
#'
#' @examples
#' \dontrun{
#' # Expanding an MFCLFrq, e.g. that was used from in an assessment
#' pseudo <- read.MFCLPseudoSizeComp('test_lw_sim', projfrq, mseCtrl, historical=FALSE)
#' }

read.MFCLPseudoSizeComp <- function(lw_sim='test_lw_sim_alt', projfrq=projfrq, ctrl="missing", historical=FALSE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  res <- MFCLPseudo() 
  
  lfirst <- lf_range(projfrq)['LFFirst']; lwidth <- lf_range(projfrq)['LFWidth']; nlbins <- lf_range(projfrq)['LFIntervals']
  ll     <- seq(lfirst, lwidth*nlbins+lfirst-lwidth, by=lwidth)
  
  wfirst <- lf_range(projfrq)['WFFirst']; wwidth <- lf_range(projfrq)['WFWidth']; nwbins <- lf_range(projfrq)['WFIntervals']
  nwbins <- max(nwbins, 1)
  ww     <- seq(wfirst, wwidth*nwbins+wfirst-wwidth, by=wwidth)
  
  pobs <- readLines(lw_sim)                                              # read in the pseudo length frequencies
  alt <- !(any(grepl('# seed', pobs)))                                   # there are two forms of the test_lw_sim file
  # which lw_sim file do you have (FALSE=original, TRUE=alt)
  
  markers <- c(grep('Simulated', pobs), length(pobs)+1)                  # identify each iteration (both length and weight)
  
  pobs.w.df <- pobs.l.df <- data.frame()
  
  for(ii in 2:length(markers)){
    pobstemp <- pobs[markers[ii-1]:(markers[ii]-1)]
    
    if(!alt){
      nrecords <- (length(pobstemp)-3)/3
      iter     <- as.numeric(unlist(strsplit(pobstemp[2], "[[:blank:]]+"))[3])
      realzid  <- as.numeric(c(unlist(strsplit(trim.leading(pobstemp[seq(4, length=nrecords, by=3)]), split="[[:blank:]]+"))))
      freq     <- as.numeric(c(unlist(strsplit(trim.leading(pobstemp[seq(6, length=nrecords, by=3)]), split="[[:blank:]]+"))))
    }
    if(alt){
      nrecords <- length(pobstemp)-2
      iter     <- as.numeric(unlist(strsplit(pobstemp[3], "[[:blank:]]+"))[1])  
      realzid  <- c(t(matrix(as.numeric(c(unlist(strsplit(pobstemp[-c(1,2)], split="[[:blank:]]+")))), nrow=nrecords, byrow=T)[,3:6]))
      freq     <- c(t(matrix(as.numeric(c(unlist(strsplit(pobstemp[-c(1,2)], split="[[:blank:]]+")))), nrow=nrecords, byrow=T)[,-c(1:7)]))
      # function currently missing code to read in weight frequency data for test_lw_sim_alt
    }
    
    if(grepl('length', pobstemp[1]) && length(pobstemp[-c(grep('#', pobstemp))])>0){
      pobs.l.df  <- rbind(pobs.l.df, data.frame(year   =rep(realzid[seq(1, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                                month  =rep(realzid[seq(2, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                                week   =rep(realzid[seq(3, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                                fishery=rep(realzid[seq(4, length=nrecords, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                                length = ll, 
                                                weight = NA,
                                                freq   = freq,
                                                iter   = iter)) 
    }
    if(grepl('weight', pobstemp[1]) && length(pobstemp[-c(grep('#', pobstemp))])>0){
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
  
  freqdat <- realisations(projfrq)
  if(!historical)
    freqdat    <- subset(realisations(projfrq), year>=fprojyr(ctrl))
  
  l_frq(res)    <- pobs.l.df #subset(pobs.df, !is.na(pobs.df$length))
  w_frq(res)    <- pobs.w.df #subset(pobs.df, !is.na(pobs.df$weight))
  
  slot(res, 'range')    <- c(min=min(ll), max=max(ll), plusgroup=NA, minyear=min(pobs.l.df$year), maxyear=max(pobs.l.df$year))
  
  age_nage(res) <- age_nage(projfrq)
  
  lf_range(res) <- lf_range(projfrq)
  
  lf_range(res)['Datasets'] <- nrow(freqdat)
  
  
  return(res)
}




#' Read MFCLPseudo Catch and Effort
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
#'
#' @param catch A character string for the 'catch_sim' input file name.
#' @param effort A character string for the 'effort_sim' input file name.
#' @param projfrq An \linkS4class{MFCLFrq} object for the projection period over which the pseudo data have been generated.
#' @param ctrl An \linkS4class{MFCLMSEControl}.
#' @param historical Boolean TRUE or FALSE If the simulated data include the historical period or just the projecion period.
#'
#' @return An object of class \linkS4class{MFCLPseudo}.
#'
#' @export
#'
#' @seealso \code{\link{MFCLprojControl}} \code{\link{MFCLFrq}} \code{\link{MFCLPar}}
#'
#' @examples
#' \dontrun{
#' # Expanding an MFCLFrq, e.g. that was used from in an assessment
#' pseudo <- read.MFCLPseudoCatchEffort('catch_sim', 'effort_sim', projfrq, mseCtrl, historical=FALSE)
#' }

read.MFCLPseudoCatchEffort <- function(catch="catch_sim", effort="effort_sim", projfrq=projfrq, ctrl="missing", historical=FALSE) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  res <- MFCLPseudo() 
  
  catchdat  <- read.MFCLPseudoCatch(catch,   projfrq, ctrl, historical)
  effortdat <- read.MFCLPseudoEffort(effort, projfrq, ctrl, historical)
  
  freqdat  <- realisations(projfrq)
  freqdat  <- freqdat[order(freqdat$fishery, freqdat$year, freqdat$month),]
  
  if(!historical)
    freqdat    <- subset(realisations(projfrq), year>=fprojyr(ctrl))
  
  
  nsims <- length(unique(catchdat$iter))
  freqdat2        <- cbind(freqdat, iter=rep(0:nsims, each=nrow(freqdat)), row.names=NULL)
  freqdat2$catch  <- c(freqdat$catch,  catchdat$catch_sim)
  freqdat2$effort <- c(freqdat$effort, effortdat$effort_sim)
  freqdat2$cseed[freqdat2$iter>0]  <- catchdat$cseed
  freqdat2$eseed[freqdat2$iter>0]  <- effortdat$eseed
  
  slot(res, "catcheff") <- freqdat2[,c('year','month','week','fishery','catch','effort','penalty','cseed','eseed','iter')]
  
  # not fixed yet
  #slot(res, "freq")     <- cbind(freq(projfrq)[,1:4], ce_itns, freq(projfrq)[,c("penalty","length","weight")], lf_itns)         
  
  slot(res, "range")[c("minyear","maxyear")] <- range(catchdat$year)
  
  
  return(res)
}

## 24 March 2023 - pretty messy function to create a freq object from pseudo data for a specified iteration.
fillfreq <- function(res, projfrq, ctrl, historical=TRUE, ii=1){
  
  if(class(res) != "MFCLPseudo")
    stop('Error: invalid MFCLPseudo object')
  

  # things to know - there is a threshold on the minimum sample size so not all historical size comps will have corresponding pseudo data - therefore arrays not the same size!
  # first - order the dataframes (iteration, fishery, year, month)
  freq(res) <- realisations(projfrq)[order(realisations(projfrq)$fishery, realisations(projfrq)$year, realisations(projfrq)$month), c('year','month','week','fishery','penalty')]
  
  if(!historical)
    freq(res) <- freq(res)[freq(res)$year >= fprojyr(ctrl),]
    
  catcheff(res) <- catcheff(res)[order(catcheff(res)$iter, catcheff(res)$fishery, catcheff(res)$year, catcheff(res)$month),]
  catcheff(res) <- subset(catcheff(res), iter==ii)
    
  # add unique ids (year month week fishery) to enable checking later
  freq(res)$id     <- paste(freq(res)$year, freq(res)$month, freq(res)$week, freq(res)$fishery, sep="_")
  catcheff(res)$id <- paste(catcheff(res)$year, catcheff(res)$month, catcheff(res)$week, catcheff(res)$fishery, sep="_")
    
  # add pseudo catch and effort to new freq object
  freq(res) <- merge(freq(res), subset(catcheff(res), iter==ii)[,c('year','month','week','fishery','catch','effort')])
  # add pseudo length and weight comps to new freq object
  if(nrow(l_frq(res)) > 0)
    freq(res) <- merge(freq(res), subset(l_frq(res), iter==ii))
  if(nrow(w_frq(res)) > 0)
    freq(res) <- merge(freq(res), subset(w_frq(res), iter==ii))  # this hasn't been tested properly
    
  # re-order the columns of freq object
  freq(res) <- freq(res)[, c('year','month','week','fishery','catch','effort','penalty','id','length','weight','freq','iter')]
  #freq(res) <- freq(res)[order(freq(res)$fishery, freq(res)$year, freq(res)$month, freq(res)$length),]
    
  # add the rows that don't have size comps
  if(nrow(catcheff(res)[!(catcheff(res)$id %in% freq(res)$id),c('year','month','week','fishery','catch','effort','penalty','id')])>0)
    freq(res) <- rbind(freq(res), cbind(catcheff(res)[!(catcheff(res)$id %in% freq(res)$id),c('year','month','week','fishery','catch','effort','penalty','id')], 
                                        length=NA, weight=NA, freq=-1, iter=ii))
  
  # finally reorder the new freq object and chop out the unnecessary columns
  freq(res) <- freq(res)[order(freq(res)$fishery,freq(res)$year,freq(res)$month), c('year','month','week','fishery','catch','effort','penalty','length','weight','freq')]
  
  if(!historical)
    freq(res) <- rbind(freq(projfrq)[freq(projfrq)$year < fprojyr(ctrl),], freq(res))
  
  return(res)
}





#' Read MFCLPseudo
#'
#' Reads information from the pseudo generation files and creates an MFCLPseudo object.
#' 
#'
#' @param catch A character string giving the name and path of the catch.sim file to be read.
#' @param effort A character string giving the name and path of the effort.sim file to be read.
#' @param lw_sim A character string giving the name and path of the test_lw_sim (or test_lw_sim_alt) file to be read.
#' @param projfrq The projection *.frq object from which the pseudo data were generated.
#' @param ctrl The corresponding projection control object.
#'
#' @return An object of class MFCLPseudo
#'
#' @export

# kk <- read.MFCLPseudo(catch="catch_sim", effort="effort_sim", lw_sim="test_lw_sim", projfrq=projfrq, ctrl=projCtrl)

read.MFCLPseudo <- function(catch="missing", effort="missing", lw_sim="missing", projfrq="missing", ctrl="missing", historical=TRUE, ii=1) {
  
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
  if(!missing(catch) && !missing(effort))
    res <- read.MFCLPseudoCatchEffort(catch=catch, effort=effort, projfrq=projfrq, ctrl=ctrl, historical=historical)
  
  # SIZE COMP
  if(!missing(lw_sim))
    res_sc <- read.MFCLPseudoSizeComp(lw_sim=lw_sim, projfrq = projfrq, ctrl = ctrl, historical = historical)
  
  l_frq(res)    <- l_frq(res_sc)
  w_frq(res)    <- w_frq(res_sc)
  
  lf_range(res) <- lf_range(res_sc)
  age_nage(res) <- age_nage(res_sc)
  
  #slot(res, 'range') <- slot(res_sc, 'range')
  
  res <- fillfreq(res, projfrq, ctrl, historical = historical, ii=ii)
  
  return(res)
  
}











### OLD CODE - DEFUNCT

# kk <- read.MFCLPseudo(catch="catch_sim", effort="effort_sim", lw_sim="test_lw_sim", projfrq=projfrq, ctrl=projCtrl)

#read.MFCLPseudo <- function(catch="missing", effort="missing", lw_sim="missing", projfrq="missing", ctrl="missing", historical=TRUE) {
#
#  trim.leading  <- function(x) sub("^\\s+", "", x) 
#  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
#  
#  #browser()
#  res <- MFCLPseudo() 
#  
#  if(!missing(catch) & !file.exists(catch))
#    stop("catch.sim file does not exist")
#  if(!missing(effort) & !file.exists(effort))
#    stop("effort.sim file does not exist")
#  if(!missing(lw_sim) & !file.exists(catch))
#    stop("lw_sim file does not exist")
#  if(!(class(projfrq)=="MFCLFrq"))
#    stop("projfrq must be an object of class MFCLFrq")
#  if(!inherits(ctrl, "MFCLprojControl"))
#    stop("ctrl must be an object of class MFCLprojControl")  
#  if(length(fprojyr(ctrl))==0)
#    warning("fprojyr(ctrl)==0, object may be incomplete")
#  
#  # CATCH AND EFFORT 
#  if(!missing(catch)&!missing(effort)){
#    # read in pseudo catch data
#    cc    <- readLines(catch)
#    cdat  <- matrix(as.numeric(unlist(strsplit(trim.leading(cc[-grep("#", cc)]), split="[[:blank:]]+"))), nrow=2)
#    cseed <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed", cc)], split="[[:blank:]]+"), el, 3)))
#  
#    # read in pseudo effort data
#    ee    <- readLines(effort)
#    edat  <- unlist(lapply(1:length(ee[-grep("#",ee)]), 
#                           function(ii){as.numeric(unlist(strsplit(trim.leading(ee[-grep("#",ee)][ii]), split="[[:blank:]]+")))[-1]}))
#    eseed <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed", ee)], split="[[:blank:]]+"), el, 3)))
#  
#    # create data frame for obs and pseudo data 
#    
#    if(!historical){
#      len     <- nrow(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),])
#      tempdat <- cbind(freq(projfrq)[freq(projfrq)$year>=fprojyr(ctrl),], 
#                       iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len), 
#                       row.names=NULL)
#      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
#      
#      tempdat$catch[tempdat$iter>0] <- rep(cdat[2,], each=length(unique(tempdat$length)))
#      tempdat$effort[tempdat$iter>0]<- rep(edat,     each=length(unique(tempdat$length)))
#      }
#    
#    if(historical){
#      tempdat <- realisations(projfrq)
#      len <- dim(tempdat)[1] 
#      tempdat <- cbind(tempdat,
#                  iter=rep(0:nsims(ctrl), each=len), catch.seed=rep(c(NA,cseed), each=len), effort.seed=rep(c(NA,eseed), each=len), row.names=NULL)
#      tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
#      tempdat$catch[tempdat$iter>0] <- cdat[2,]
#      tempdat$effort[tempdat$iter>0]<- edat
#    }
#  }
#  
#  # LENGTH COMPOSITIONS
#  if(!missing(lw_sim)){
#    ## read in the pseudo length frequencies
#    pobs <- readLines(lw_sim)
#    markers <- lapply(1:nsims(ctrl), function(x){grep(paste('# projection',x), pobs)})
#  
#    lfirst <- lf_range(projfrq)['LFFirst']; lwidth <- lf_range(projfrq)['LFWidth']; nlbins <- lf_range(projfrq)['LFIntervals']
#    ll     <- seq(lfirst, lwidth*nlbins+lfirst-lwidth, by=lwidth)
#  
#    pobs.df <- data.frame()
#    for(ss in 1:nsims(ctrl)){
#      tempdat2<- pobs[(markers[[ss]][1]+2):(markers[[ss]][2]-2)]
#      # strip out the fishery realization data
#      realzid <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(1, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
#      # strip out the length frequency data
#      llfreq  <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(3, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
#    
#      pobs.df <- rbind(pobs.df, data.frame(year   =rep(realzid[seq(1, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
#                                           month  =rep(realzid[seq(2, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
#                                           week   =rep(realzid[seq(3, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
#                                           fishery=rep(realzid[seq(4, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
#                                           length =ll, weight=NA, freq= llfreq, iter=ss))
#    }
#  }
# 
#  slot(res, "catcheff") <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month, tempdat$week),]
#  if(exists('pobs.df'))
#    slot(res, "l_frq")    <- pobs.df[order(pobs.df$iter,  pobs.df$fishery, pobs.df$year, pobs.df$month, pobs.df$week, pobs.df$length),]  
#  
#  if(!historical) { # think this is OK !!
#    slot(res, "catcheff")[slot(res, "catcheff")$iter>0,]$freq <- slot(res, "l_frq")$freq  
#    slot(res, 'freq') <- slot(res, 'catcheff')[slot(res, "catcheff")$iter==0,c(1:4,7:9)]
#    for(ii in 0:nsims(ctrl)){
#      tmpdat <-  slot(res,'catcheff')[slot(res,'catcheff')$iter==ii, c("catch",'effort','freq')]
#      colnames(tmpdat) <- paste(c('catch','effort','freq'), '_', ii, sep="")
#      slot(res,'freq') <- cbind(slot(res,'freq'), tmpdat)           
#    }
#  }
#  
#  if(historical){
#  # catch and effort vectors in projfrq format - ie you can just paste these into the catch and effort columns of the freq(projfrq)
#  # first of all re-order your projfrq  
#  freq(projfrq) <- freq(projfrq)[order(freq(projfrq)$fishery, freq(projfrq)$year, freq(projfrq)$month, freq(projfrq)$week, freq(projfrq)$length),]
#      
#  lfs      <- c(table(freq(projfrq)$month, freq(projfrq)$year, freq(projfrq)$fishery))
#  ce_itns  <- cbind(freq(projfrq)$catch, freq(projfrq)$effort)
#  
#  for(ii in 1:nsims(ctrl))
#    ce_itns <- cbind(ce_itns, rep(catcheff(res)[catcheff(res)$iter==ii, 'catch'], lfs[lfs>0]), rep(catcheff(res)[catcheff(res)$iter==ii, 'effort'], lfs[lfs>0]))
#  
#  colnames(ce_itns) <- paste(c("catch","effort"), rep(0:nsims(ctrl),each=2), sep="_")
#
#  # size frequency data in projfrq format
#  tmpfreq1 <- cbind(freq(projfrq), mkr=1:nrow(freq(projfrq)))
#  tmpfreq  <- rbind(tmpfreq1[!is.na(tmpfreq1$length),], tmpfreq1[is.na(tmpfreq1$length),])
#
#  lf_itns <- tmpfreq$freq
#  for(ii in 1:nsims(ctrl))
#    lf_itns <- cbind(lf_itns, c(l_frq(res)[l_frq(res)$iter==ii, 'freq'], freq(projfrq)[is.na(freq(projfrq)$length),'freq']))
#
#  lf_itns <- lf_itns[order(tmpfreq$mkr),]
#  colnames(lf_itns) <- paste("freq", 0:nsims(ctrl), sep="_")
#  
#  slot(res, "freq") <- cbind(freq(projfrq)[,1:4], ce_itns, freq(projfrq)[,c("penalty","length","weight")], lf_itns)         
#  }
#
#  slot(res, "range")[c("minyear","maxyear")] <- range(tempdat$year)
#  
#  if(exists('pobs.df'))
#    slot(res, "range")[c("min","max")]         <- range(pobs.df$length)
#  
#  return(res)
#}



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


#' Read MFCL Catch Sim
#'
#' Reads information from the pseudo generation catch_sim file and creates a data.frame  object.
#' The object here is to break down the read.MFCLPseudo function in smaller, more manageable code units.
#'
#' @param catch A character string giving the name and path of the catch.sim file to be read.
#' @param projfrq The projfrq object used to generate the pseudo data.
#' @param ctrl The control object used to generate teh pseudo data.
#' @param historical Boolean flag specifying if historical data included.
#'
#' @return An object of class data.frame that can be subset to freq
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
    
  nlbins <- lf_range(projfrq)['LFIntervals']

    cc    <- readLines(catch)
    cdat  <- matrix(as.numeric(unlist(strsplit(trim.leading(cc[-grep("#", cc)]), split="[[:blank:]]+"))), nrow=2)
    cseed <- as.numeric(unlist(lapply(strsplit(cc[grep("# seed", cc)], split="[[:blank:]]+"), el, 3)))
    
    edat <- stop("edat is undefined")

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


## Finlay's code - stolen from the mixed fishery folder

#read.MFCLEffortSim <- function(effort="effort_sim", projfrq='missing', ctrl='missing', historical=TRUE){
read.MFCLEffortSim <- function(effort="effort_sim", projfrq, fprojyr='missing', historical=TRUE){
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  ee    <- readLines(effort)
  edat  <- unlist(lapply(1:length(ee[-grep("#",ee)]), 
                         function(ii){as.numeric(unlist(strsplit(trim.leading(ee[-grep("#",ee)][ii]), split="[[:blank:]]+")))[-1]}))
  eseed <- as.numeric(unlist(lapply(strsplit(ee[grep("# seed", ee)], split="[[:blank:]]+"), el, 3)))
  realprojfrq <- realisations(projfrq)
  # I don't know if this works yet
  if (!historical){
    # Add check for ctrl argument - if missing throw error - or fprojyr argument?
    #realprojfrq <- subset(realprojfrq, year>= fprojyr(ctrl))
    if(missing(fprojyr)){
      stop("If historical argument is FALSE you need to pass in fprojyr\n")
    }
    realprojfrq <- subset(realprojfrq, year>= fprojyr)
  }
  
  tempdat <- realprojfrq
  len <- dim(tempdat)[1] 
  # Have a reckon at the number of iters
  nsims <- length(edat) / len
  #tempdat <- cbind(tempdat, iter=rep(0:nsims(ctrl), each=len), effort.seed=rep(c(NA,eseed), each=len), row.names=NULL)
  tempdat <- cbind(tempdat, iter=rep(0:nsims, each=len), effort.seed=rep(c(NA,eseed), each=len), row.names=NULL)
  tempdat <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month),]
  tempdat$effort[tempdat$iter>0] <- edat

  return(tempdat)
}
