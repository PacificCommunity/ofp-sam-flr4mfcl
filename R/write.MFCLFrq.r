
##########################################################################
#
#  UNEXPORTED FUNCTIONS
#
##########################################################################

write.stat <- function(x, file, append=F, ...){
  
  if(file.exists(file) && append==F)
    file.remove(file)
  if(!file.exists(file))
    file.create(file)
  
  cat("# \n# Definition of fisheries \n# \n", file=file, append=T)
  cat("# Number of   Number of   Use generic   Number of     Year1 \n",  file=file, append=T)
  cat("#  Region     Fisheries    diffusion    tag groups \n", file=file, append=T)
  cat(paste("", n_regions(x), n_fisheries(x), as.numeric(generic_diffusion(x)), n_tag_groups(x), sep='\t\t\t\t\t'), file=file, append=T)
  cat(paste('\t\t\t\t', range(x)['minyear'], "0", as.numeric(frq_age_len(x)), n_recs_yr(x), rec_month(x), frq_version(x), '\n', sep="\t"), file=file, append=T)
  cat("# Relative Region Size \n", file=file, append=T)
  write.table(matrix(region_size(x),nrow=1), file=file, append=T, col.names=F, row.names=F)
  cat("#\n# Region in which each fishery is located \n", file=file, append=T)
  write.table(t(matrix(region_fish(x))), file=file, append=T, col.names=F, row.names=F)
  cat("#\n# \n# Incidence matrix \n", file=file, append=T)
  write.table(move_matrix(x), file=file, append=T, col.names=F, row.names=F, na=" ")
  cat("#\n# \n# Data flags (for records 1, 0=catch in number; 1=catch in weight) \n", file=file, append=T)
  write.table(data_flags(x), file=file, append=T, col.names=F, row.names=F)
  cat("# Season-region flags \n", file=file, append=T)
  write.table(season_flags(x), file=file, append=T, col.names=F, row.names=F)
  cat("# Number of movements per year \n", file=file, append=T)
  cat(paste(n_move_yr(x), "\n"), file=file, append=T)
  cat("# Weeks in which movement occurs \n", file=file, append=T)
  write.table(matrix(move_weeks(x),nrow=1), file=file, append=T, col.names=F, row.names=F)
}



write.len <- function(x, file, append=T, ...){
  
  if(file.exists(file) && append==F)
    file.remove(file)
  
  if(!file.exists(file)){
    file.create(file)
    cat("Warning: There is no existing file. Your .frq file will be incomplete")
  }
  
  cat("# fishery data \n# \n# \n", file=file, append=T)
  cat("# Datasets / LFIntervals  LFFirst  LFWidth  LFFactor / WFIntervals  WFFirst  WFWidth \n",file=file, append=T)
  cat(paste(as.vector(lf_range(x)), collapse="      "), file=file, append=T)
  cat("\n# age_nage   age_age1 \n", file=file, append=T)
  cat(paste(paste(as.vector(age_nage(x)), collapse="      "),"\n"), file=file, append=T)
  
  # rows with no frequency data
  noobs <- freq(x)[is.na(freq(x)$length) & is.na(freq(x)$weight),]
  noobs <- cbind(noobs[1:7], length=-1, weight=-1)
  
  # rows with length frequency data
  lobs  <- freq(x)[!is.na(freq(x)$length) & is.na(freq(x)$weight),]
  lfwide <- noobs[0,]
  if(nrow(lobs)>0){ 
    lfwide<- lobs[lobs$length==lf_range(x)["LFFirst"],c("year","month","week","fishery","catch","effort","penalty")]    
    lfwide<- cbind(lfwide, t(array(lobs$freq, dim=c(lf_range(x)['LFIntervals'], nrow(lfwide)))), -1) }   
  
  # rows with weight frequency data
  wobs  <- freq(x)[!is.na(freq(x)$weight) & is.na(freq(x)$length),]
  wfwide<- noobs[0,]
  if(nrow(wobs)>0){
    wfwide<- wobs[wobs$weight==lf_range(x)["WFFirst"],c("year","month","week","fishery","catch","effort","penalty")]    
    wfwide<- cbind(wfwide, -1, t(array(wobs$freq, dim=c(lf_range(x)['WFIntervals'], nrow(wfwide))))) }
  
  # rows with both length and weight frequency data
  tp <- rbind(lobs[lobs$length==lf_range(x)["LFFirst"],], wobs[wobs$weight==lf_range(x)["WFFirst"],])
  lwfwide <- tp[duplicated(tp[,c(1:7)]),][,c(1:7)]

  if(nrow(tp)>0){
    lwobs <- freq(x)[with(freq(x), paste(year, month, week, fishery, sep = "\r")) %in% with(tp, paste(year, month, week, fishery, sep="\r")), ] 
  
    lwfwide <- cbind(lwfwide, t(array(lwobs[!is.na(lwobs$length),'freq'], dim=c(lf_range(x)['LFIntervals'], nrow(lwfwide)))),
                              t(array(lwobs[!is.na(lwobs$weight),'freq'], dim=c(lf_range(x)['WFIntervals'], nrow(lwfwide)))))
  }
  # remove duplicates from lfwide and wfwide
  lfwide <- lfwide[!(with(lfwide, paste(year, month, week, fishery, sep="\r")) %in% with(lwfwide, paste(year, month, week, fishery, sep="\r"))), ]
  wfwide <- wfwide[!(with(wfwide, paste(year, month, week, fishery, sep="\r")) %in% with(lwfwide, paste(year, month, week, fishery, sep="\r"))), ]
  
  # remove unnecessary -1s if you only have length or weight frequencies
  if(nrow(wfwide)==0 & nrow(lwfwide)==0) {
    noobs  <- noobs[,-9]
    lfwide <- lfwide[,-ncol(lfwide)]
  }
  if(nrow(lfwide)==0 & nrow(lwfwide)==0) {
    noobs  <- noobs[,-8]
    wfwide <- wfwide[, -8]
  }
    
  # Convert to character strings, put them all together, sort them and write it out
  char_all <- unlist(lapply(list(noobs, lfwide, wfwide, lwfwide), function(x){apply(x,1,'paste',collapse=" ")}))
  char_all <- paste(char_all, '\n')
  
  char_ord  <- rbind(noobs[,c(1:7)], lfwide[,c(1:7)], wfwide[,c(1:7)], lwfwide[,c(1:7)])
  char_ord  <- order(char_ord$fishery, char_ord$year, char_ord$month)
  
  cat(char_all[char_ord], file=file, append=T, sep="")
    
}



##########################################################################
#
#  METHODS
#
##########################################################################




#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLFrqStats"), function(x, file, append=F, ...){
  
  write.stat(x=x, file=file, append=append, ...)  
})



#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLLenFreq"), function(x, file, append=T, ...){
  
  write.len(x=x, file=file, append=append, ...)
})




#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLFrq"), function(x, file, append=F, ...){
  
  write.stat(x=x, file=file, append=append, ...)
  write.len(x=x, file=file, append=T, ...)  
})



