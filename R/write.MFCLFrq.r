
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
  
  lfwide<- lobs[lobs$length==lf_range(x)["LFFirst"],c("year","month","week","fishery","catch","effort","penalty")]    
  lfwide<- cbind(lfwide, t(array(lobs$freq, dim=c(lf_range(x)['LFIntervals'], nrow(lfwide))))) 
  
  
  
  
  
  
  lfnobs<- freq(x)[freq(x)$lenfrq==-1,]
  lfnobs<- cbind(lfnobs[,c("year","month","week","fish","catch","effort","pen")], 
                 array(-1, dim=c(nrow(lfnobs),lf_range(x)['LFIntervals'])))
  colnames(lfwide) <- colnames(lfnobs)
  rownames(lfwide) <- as.character(1:nrow(lfwide))
  rownames(lfnobs) <- as.character(1:nrow(lfnobs))
  
  dfall  <- rbind(lfwide, lfnobs)
  dfall  <- dfall[order(dfall$fish, dfall$year, dfall$month),]
  
  dfnobs <- dfall[,'2']==-1
  
  output <- apply(array(dfall),1,paste,collapse="  ")
  
  output[dfnobs] <- lapply(lapply(lapply(output[dfnobs], strsplit, split="[[:blank:]]+"),unlist), function(x) x[1:8])
  output[dfnobs] <- lapply(output[dfnobs], paste, collapse="   ")
  
  fullOutput <- c(readLines(file), as.character(output))
  writeLines(fullOutput, con=file)
  
}


#lfwide<- reshape(lfobs, timevar='length', idvar=c('year','month','week','fish','catch','effort','pen'), direction="wide")


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

