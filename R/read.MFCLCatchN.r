
##
## SKIPJACK TO THE MOON CALCS

#file <- "/media/penguin/skj/2016/assessment/RefCase/ests.rep"
                    
read.MFCLCatchN <- function(file="ests.rep", first.yr=1972){

  ests <- readLines(file)
  
  nages      <- length(splitter2(ests, "Fishery Realization1"))
  nyears     <- (grep("Group 2", ests)-grep("Group 1", ests)-3)/4
  nfisheries <- length(grep("Catchability for fishery", ests))
  dmnms      <- list(age=seq(nages), year=first.yr:(first.yr+nyears-1), unit=seq(nfisheries), season="all", area="unique", iter="1")

  kk <- scan(file, what=numeric(), skip=grep("Predicted catch by fishery by year and age", ests)+1, comment.char="F", n=nages*nyears*nfisheries)  
      
  res <- FLQuant(array(kk, dim=c(nages, nyears, nfisheries,1,1,1)), dimnames=dmnms)
  
  res <- checkUnitDimnames(res,nfisheries=nfisheries)
  return(res)
}





