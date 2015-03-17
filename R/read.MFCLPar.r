
## read.MFCLPar
## author Rob Scott
## date 16/04/2015


read.MFCLBiol <- function(parfile, range=NULL){
  par <- readLines(parfile)
  
  ages <- grep("# The number of age classes", par)
}