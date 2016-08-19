


#' MFCL Frq Statistics
#'
#' Extracts the essential ranges and dimensions from the frq file
#'
#' @param frqfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLFrqStats
#'
#' @examples
#' read.MFCLFrqStats("C://R4MFCL//test_data//skj_ref_case//skj.frq")
#'
#' @export


read.MFCLFrqStats <- function(frqfile){
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  res <- new("MFCLFrqStats")
  
  quiet=TRUE
  tt <- scan(frqfile, nlines=100, comment.char='#', quiet=quiet)
  
  res@n_regions <- tt[1]
  res@n_fisheries <- tt[2]
  res@generic_diffusion <- as.logical(tt[3])
  res@n_tag_groups <- tt[4]
  res@range['minyear'] <- tt[5]
  res@frq_age_len <- as.logical(tt[7])
  res@n_recs_yr   <- tt[8]
  res@rec_month   <- tt[9]
  res@frq_version <- tt[10]
  
  frq  <- readLines(frqfile)  
  
  line <- grep("Relative Region Size", frq)+1
  dat  <- as.numeric(unlist(strsplit(frq[line], split=" ")))
  res@region_size <- FLQuant(dat[!is.na(dat)], 
                             dimnames=list(len='all',year='all',unit='unique',
                                           season='all',area=as.character(1:res@n_regions)))
  
#  line <- grep("Region in which each fishery is located", frq)+1
#  dat  <- as.numeric(unlist(strsplit(frq[line], split=" ")))
  dat  <- as.numeric(splitter(frq, "Region in which each fishery is located"))
  res@region_fish <- FLQuant(dat[!is.na(dat)], 
                             dimnames=list(len='all',year='all',unit=as.character(1:res@n_fisheries),
                                           season='all',area='all'))
  if(n_regions(res)>1){
    line <- grep("Incidence matrix", frq)
    res@move_matrix <- matrix(NA, nrow=res@n_regions, ncol=res@n_regions)
    for(i in 1:(res@n_regions-1)){
      dat <- as.numeric(unlist(strsplit(frq[line+i], split=" ")))[!is.na(as.numeric(unlist(strsplit(frq[line+i], split=" "))))]
      res@move_matrix[i,(i+1):res@n_regions] <- dat
    }
    line <- grep("Season-region flags", frq)
    res@season_flags <- matrix(as.numeric(unlist(strsplit(frq[line+1:res@n_recs_yr], split=" "))), nrow=res@n_recs_yr, ncol=res@n_regions, byrow=T)
  }
    
#  line <- grep("Data flags", frq)
#  res@data_flags <- matrix(as.numeric(unlist(strsplit(frq[line+1:5], split=" "))),nrow=5, ncol=res@n_fisheries, byrow=T)
  res@data_flags <- matrix(as.numeric(splitter(frq, "Data flags", 1:5)), nrow=5, byrow=T)
  
  res@n_move_yr <- as.numeric(frq[grep("Number of movements per year", frq)+1])
  
  line <- grep("Weeks in which movement occurs", frq)
  res@move_weeks <- as.numeric(unlist(strsplit(frq[line+1], split=" ")))
  
  return(res)
}


#' MFCL Length Frequency
#'
#' Extracts the length frequency data from the frq file
#'
#' @param frqfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLLenFreq
#'
#' @examples
#' read.MFCLLenFreq("C://R4MFCL//test_data//skj_ref_case//skj.frq")
#'
#' @export

read.MFCLLenFreq <- function(frqfile){
  
  quick.check <- function(obj, both, ...){
    if(!both){
      if(lf_range(obj)["WFIntervals"]>0 & lf_range(obj)["LFIntervals"]>0)
        stop("I don't know if the frequency data are by length or weight: check the lf_range inputs")
      if(lf_range(obj)["WFIntervals"]==0 & lf_range(obj)["LFIntervals"]==0)
        stop("Both the length intervals and weight intervals are set to 0: check the lf_range inputs")
    }
    return(TRUE)
  }
  
  build.df <- function(lffrq, arr.rows, nfields, frqlen=NA, frqwt=NA, inc=0, inc2=0, inc3=0){    
    mat  <- matrix(as.numeric(unlist(strsplit(lffrq[nfields==arr.rows], split="[[:blank:]]+"))), ncol=arr.rows, byrow=T)
    df   <- apply(mat[,1:7],2,rep,each=arr.rows-(7+inc))
    colnames(df) <- c("year", "month", "week", "fishery", "catch", "effort", "penalty")    
    #df   <- cbind(df, length=frqlen, weight=frqwt, freq=as.vector(t(mat[,(min(nfields)-inc2):(arr.rows-inc3)])))     
    df   <- cbind(df, length=frqlen, weight=frqwt, freq=as.vector(t(mat[,(8-inc2):(arr.rows-inc3)])))     
    return(df)
  }
  
  res  <- new("MFCLLenFreq")  
  frq  <- readLines(frqfile)  
  
  dat  <- as.numeric(unlist(strsplit(frq[grep("Datasets", frq)+1], split="[[:blank:]]+")))
  slot(res, "lf_range")[] <- dat[!is.na(dat)]
  
  dat  <- as.numeric(unlist(strsplit(frq[grep("age_nage", frq)+1], split="[[:blank:]]+")))
  slot(res, "age_nage")[] <- ifelse(length(dat)>1, dat[!is.na(dat)], NA)
  
  nLbins <- lf_range(res)['LFIntervals']; Lwidth <- lf_range(res)["LFWidth"]; Lfirst <- lf_range(res)["LFFirst"]
  nWbins <- lf_range(res)['WFIntervals']; Wwidth <- lf_range(res)["WFWidth"]; Wfirst <- lf_range(res)["WFFirst"]
  
  line1 <- ifelse(all(is.na(slot(res, "age_nage"))), grep("Datasets", frq)+2, grep("age_nage", frq)+2)  # first line of frequency data
  lffrq <- frq[line1:length(frq)]   # just the length frequency data 
  
  nfields <- count.fields(frqfile, skip=line1-1)          # number of fields in each line of the frequency data
  both    <- length(table(nfields))>2 & min(nfields) > 8  # check if you have both length and weight frequency data
  
  frqlen <- frqwt <- NA
  if(nLbins>0) {frqlen <- seq(Lfirst, Lwidth*nLbins+Lfirst-Lwidth, by=Lwidth)}
  if(nWbins>0) {frqwt  <- seq(Wfirst, Wwidth*nWbins+Wfirst-Wwidth, by=Wwidth)}
  
  
  if(!both & quick.check(res, both)){ # If only one type of frequency data - length or weight 
    
    # no frequency data-frame bit
    df1 <- as.data.frame(matrix(as.numeric(unlist(strsplit(lffrq[nfields==8], split="[[:blank:]]+"))), ncol=8, byrow=T)[,-8])
    colnames(df1) <- c("year", "month", "week", "fishery", "catch", "effort", "penalty")
    if(nrow(df1)>0)
      df1 <- cbind(df1, length=NA, weight=NA, freq=-1)
    
    # with frequency data-frame bit - array size depends if you have length or weight data
    df2      <- build.df(lffrq, arr.rows=(7+ifelse(nLbins>0,nLbins,nWbins)), nfields, frqlen, frqwt, inc=0)   
    dfall    <- rbind(df1, df2)
  }  
        
  if(both){  # if you have both length and weight frequency data ...
    
    # no frequency data-frame bit
    df1 <- as.data.frame(matrix(as.numeric(unlist(strsplit(lffrq[nfields==min(nfields)], split="[[:blank:]]+"))), ncol=min(nfields), byrow=T)[,1:(min(nfields)-2)])
    colnames(df1) <- c("year", "month", "week", "fishery", "catch", "effort", "penalty")
    df1 <- cbind(df1, length=NA, weight=NA, freq=-1)
    
    # length frequency data-frame bit
    dfL      <- build.df(lffrq, arr.rows=(7+nLbins+1), nfields, frqlen, frqwt=NA, inc=1, inc2=1, inc3=1)
    
    # weight frequency data-frame bit
    dfW      <- build.df(lffrq, arr.rows=(7+1+nWbins), nfields, frqlen=NA, frqwt, inc=1, inc2=0, inc3=0)
    
    # length and weight frequency data-frame bit
    dfLW      <- build.df(lffrq, arr.rows=(7+nLbins+nWbins), nfields, 
                         frqlen=c(frqlen, rep(NA, length(frqwt))), 
                         frqwt =c(rep(NA, length(frqlen)), frqwt), inc=0, inc2=1, inc3=0)
  
    dfall <- rbind(df1, dfL, dfW, dfLW) 
  }
  
  res@freq <- dfall
  res@freq <- res@freq[order(res@freq$fish, res@freq$year, res@freq$month),]
  
  return(res)
  
}








#' MFCL frq file reader
#'
#' Reads the entire contents of the frq file
#'
#' @param frqfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLFrq
#'
#' @examples
#' read.MFCLLenFreq(paste(system.file('data', package='FLR4MFCL'), 'skj.frq', sep='//'))
#'
#' @export

read.MFCLFrq <- function(frqfile){
  
  lenfreq <- read.MFCLLenFreq(frqfile)
  vitals  <- read.MFCLFrqStats(frqfile)
  
  res <- new("MFCLFrq")
  
  for(slotname in slotNames(lenfreq)){
    slot(res, slotname) <- slot(lenfreq, slotname)
  }
  
  for(slotname in slotNames(vitals)){
    slot(res, slotname) <- slot(vitals, slotname)
  }
  
  minlen <- lenfreq@lf_range['LFFirst']
  maxlen <- lenfreq@lf_range['LFFirst'] + lenfreq@lf_range['LFIntervals'] * lenfreq@lf_range['LFFirst']
  res@range[c('min','max')] <- c(minlen,maxlen)
  res@range['maxyear']      <- max(res@freq[,'year'])
  
  return(res)
    
}


#frqfile <- 'C://R4MFCL//test_data//skj_ref_case//skj.frq'
