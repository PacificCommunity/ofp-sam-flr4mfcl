
##########################################################################
#
#  UNEXPORTED FUNCTIONS
#
##########################################################################



write.tagproj <- function(x, file, append=F, ...){
  
  if(file.exists(file) && append==F)
    file.remove(file)
  if(!file.exists(file))
    file.create(file)
  
  cat(paste("\n#\n"), file=file, append=T)
  
  release.header1 <- "#\n#\n#---------------------------------\n# "
  release.header2 <- "- RELEASE REGION    YEAR    MONTH   Fishery  Predicted numbers  "  
  
  for(rel in 1:release_groups_proj(x)) {
    cat(paste(release.header1, rel, release.header2), file=file, append=T)
    write.table(releases_proj(x)[rel,])
  }
  
    
    
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



