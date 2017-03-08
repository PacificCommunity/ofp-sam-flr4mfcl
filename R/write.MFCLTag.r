
##########################################################################
#
#  UNEXPORTED FUNCTIONS
#
##########################################################################



write.tag <- function(x, file, append=F, ...){
  
  if(file.exists(file) && append==F)
    file.remove(file)
  if(!file.exists(file))
    file.create(file)
  
  cat("# RELEASE GROUPS    STARTING LENGTH    NUMBER INTERVALS    INTERVAL LENGTH \n", file=file, append=T)
  cat(paste(release_groups(x), release_lengths(x)[1], length(release_lengths(x)), release_lengths(x)[2]-release_lengths(x)[1], sep="            "), file=file, append=T)
  
  cat(paste("\n#\n#\n# TAG RECOVERIES\n#    "), file=file, append=T)
  write.table(rbind(1:release_groups(x), recoveries(x)), row.names=FALSE, col.names=FALSE, , file=file, append=T)
  
  release.header1 <- "#\n#\n#---------------------------------\n# "
  release.header2 <- "- RELEASE REGION    YEAR    MONTH   Tag_program  "  
  recapture.header<-paste("\n#\n#\n# LENGTH RELEASE    FISHERY    RECAP YEAR    RECAP MONTH    NUMBER\n")
  
  for(rel in 1:release_groups(x)) {
    cat(paste(release.header1, rel, release.header2), file=file, append=T)
    cat(as.character(releases(x)[releases(x)$rel.group==rel, 'program'])[1], file=file, append=T)
    cat('\n', file=file, append=T)    
    write.table(releases(x)[releases(x)$rel.group==rel, c('region', 'year', 'month')][1,], 
                col.names=FALSE, row.names=FALSE, file=file, append=T) 
    #cat(releases(x)[releases(x)$rel.group==rel, 'lendist'], file=file, append=T)
    write.table(array(releases(x)[releases(x)$rel.group==rel, 'lendist'], dim=c(1,length(release_lengths(x)))), 
                col.names=FALSE, row.names=FALSE, file=file, append=T)
    
    if(rel %in% recaptures(x)$rel.group){
      cat(recapture.header, file=file, append=T)
#      write.table(recaptures(x)[recaptures(x)$rel.group==rel,c('rel.length','recap.fishery','recap.year','recap.month','recap.number')],
       write.table(subset(recaptures(x), rel.group==rel)[,c('rel.length','recap.fishery','recap.year','recap.month','recap.number')],
                        col.names=FALSE, row.names=FALSE, file=file, append=T)
    }
  }
}





##########################################################################
#
#  METHODS
#
##########################################################################




#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLTag"), function(x, file, append=F, ...){
  
  write.tag(x=x, file=file, append=append, ...)
})



