#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

##########################################################################
#
#  UNEXPORTED FUNCTIONS
#
##########################################################################


#alk <- read.MFCLALK("/media/sf_assessments/bet/2023/model_runs/diagnostic2023_jitter36/bet.age_length")
#file <- '/home/rob/temp/bet.age_length_test'


write.alk <- function(x, file, append=F, ...){
  
  slot(x, 'ALK') <- ALK(x)[order(ALK(x)$year, ALK(x)$month, ALK(x)$fishery, ALK(x)$species),]
  
  records <- unique(paste(ALK(x)$year, ALK(x)$month, ALK(x)$fishery, ALK(x)$species, sep = '_'))
  
  yy <- as.numeric(unlist(lapply(strsplit(records, split="_"), el, 1)))
  mm <- as.numeric(unlist(lapply(strsplit(records, split="_"), el, 2)))
  ff <- as.numeric(unlist(lapply(strsplit(records, split="_"), el, 3)))
  ss <- as.numeric(unlist(lapply(strsplit(records, split="_"), el, 4)))
  
  if(all(is.na(ALK(x)$length)))
    lbins <- 1:(nrow(subset(ALK(x), year==yy[1] & month==mm[1] & fishery==ff[1] & species==ss[1]))/range(x)['maxage'])
  
  if(file.exists(file) && append==F)
    file.remove(file)
  if(!file.exists(file))
    file.create(file)
  
  cat('# num age length records \n', file=file, append = T)
  cat(paste(length(records), '\n'),  file=file, append = T)
  cat('# effective sample size \n',  file=file, append = T)
  cat(ESS(x),                        file=file, append = T)
  cat('\n',                          file=file, append = T)
  
  for(rr in 1:length(records)){
    cat('# Year  Month  Fishery  Species \n', file=file, append = T)
    cat(paste(yy[rr], mm[rr], ff[rr], ss[rr], '\n', sep=" "), file=file, append = T)
    write.table(t(matrix(subset(ALK(x), year==yy[rr] & month==mm[rr] & fishery==ff[rr] & species==ss[rr])$obs, ncol=length(lbins))), 
                file=file, append=T, col.names = F, row.names = F)
  }
  
}    



##########################################################################
#
#  METHODS
#
##########################################################################


#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLALK"), function(x, file, append=F, ...){
  
  write.alk(x=x, file=file, append=append, ...)
})

