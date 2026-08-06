#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

##########################################################################
#
#  UNEXPORTED FUNCTIONS
#
##########################################################################



write.MFCLRegScale <- function(x, file, append=F, ...){
  
  cat(range(x)[c('minyear','minmonth', 'maxyear', 'maxmonth')], file=file, append=append)
  cat('\n', file=file, append=T)
  
  write.table(index(x), col.names=F, row.names=F, file=file, append=T)
  
}    



##########################################################################
#
#  METHODS
#
##########################################################################


#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLRegScale"), function(x, file, append=F, ...){
  
  write.MFCLRegScale(x=x, file=file, append=append, ...)
})

