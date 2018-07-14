
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
  
  cat(paste("# RELEASE GROUPS \n"),        file=file, append=T)
  cat(paste(release_groups_proj(x), "\n"), file=file, append=T)
  
  release.header1 <- "#\n#\n#---------------------------------\n# "
  release.header2 <- "- RELEASE REGION    YEAR    MONTH   Fishery  Predicted numbers  \n \t\t"  
  
  for(rel in 1:release_groups_proj(x)) {
    cat(paste(release.header1, rel, release.header2), file=file, append=T)
    write.table(releases_proj(x)[rel,],  sep="\t ", col.names=F, row.names = F, file=file, append=T)
  }
  
  cat(paste("#\n#\n# Reporting rates for each event: rows = fisheries; cols = tag events \n"), file=file, append=T)
  write.table(rep_rate_proj(x), col.names = F, row.names = F, file=file, append=T)
  
}  
    




##########################################################################
#
#  METHODS
#
##########################################################################




#' @rdname write-methods
#' @aliases write

setMethod("write", signature("MFCLTagProj"), function(x, file, append=F, ...){
  
  write.tagproj(x=x, file=file, append=append, ...)
})

