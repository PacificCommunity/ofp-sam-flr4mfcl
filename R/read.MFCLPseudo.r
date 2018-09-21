#' read.MFCLPseudo
#'
#' Reads information from the pseudo generation files and creates an MFCLPseudo object
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
  if(!(class(ctrl)=="MFCLprojControl"))
    stop("projfrq must be an object of class MFCLprojControl")  
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
      len     <- nrow(freq(projfrq)[is.element(freq(projfrq)$length, c(NA,2)),])
      tempdat <- cbind(freq(projfrq)[is.element(freq(projfrq)$length, c(NA,2)),], 
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
    for(ss in 1:nsims(projCtrl)){
      tempdat2<- pobs[(markers[[ss]][1]+2):(markers[[ss]][2]-2)]
      realzid <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(1, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
      llfreq  <- as.numeric(c(unlist(strsplit(trim.leading(tempdat2[seq(3, length=length(tempdat2)/3, by=3)]), split="[[:blank:]]+"))))
    
      pobs.df <- rbind(pobs.df, data.frame(year   =rep(realzid[seq(1, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           month  =rep(realzid[seq(2, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           week   =rep(realzid[seq(3, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           fishery=rep(realzid[seq(4, length=length(tempdat2)/3, by=4)], each=lf_range(projfrq)['LFIntervals']),
                                           length =ll, weight=NA, freq= llfreq, iter=ss))
    }
  }
 
  slot(res, "catcheff") <- tempdat[order(tempdat$iter, tempdat$fishery, tempdat$year, tempdat$month, tempdat$week),]
  slot(res, "l_frq")    <- pobs.df[order(pobs.df$iter,  pobs.df$fishery, pobs.df$year, pobs.df$month, pobs.df$week),]
  
  if(!historical)
    slot(res, "catcheff")[slot(res, "catcheff")$iter>0,]$freq <- slot(res, "l_frq")$freq
  
  if(historical){
    kk <- merge(slot(res, 'catcheff')[,c(1,2,3,4,5,6,7,11,12,13)], slot(res, 'l_frq'))
    
    tt2 <- unique(apply(tempdat[tempdat$iter>0,c('year','month','week','fishery','iter')], 1, paste, collapse='_'))
    kk2 <- unique(apply(kk[,c('year','month','week','fishery','iter')], 1, paste, collapse='_'))
    
    kk <- rbind(kk, tempdat[!is.element(tt2, kk2),names(kk)])
    kk <- kk[order(kk$iter, kk$fishery, kk$year, kk$month, kk$week, kk$length),]
    kk <- kk[kk$iter>0,]
    kk <- kk[,c('year','month','week','fishery','catch','effort','penalty','length','weight','freq','iter','catch.seed','effort.seed')]
    slot(res, 'catcheff') <- kk
  }
  
  slot(res, "range")[c("minyear","maxyear")] <- range(tempdat$year)
  slot(res, "range")[c("min","max")]         <- range(pobs.df$length)
  
  return(res)
}
  
  