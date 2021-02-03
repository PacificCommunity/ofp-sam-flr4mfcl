#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2020  Rob Scott

#' read.simulated
#'
#' Reads information from the simulated_numbers_at_age file (terminal N or recruitment) and creates an FLQuant object
#'
#' @param file:  A character string giving the name and path of the simulated_numbers_at_age file to be read 
#' @param data:  A character (either "N" or "R") to specify whether termninal N values or recruitment values are to be read in.
#' @param dimensioins: numeric vector of dimensions (as returned from dimensions(par))
#' @param range: 
#' 
#'
#' @return An object of class FLQuant
#'
#' @examples
#' 
#' 
#'
#' @export

read.MFCLSimulatedNatAge <- function(file='simulated_numbers_at_age', data="N", dimensions=NULL, rnge=NULL, nssns=4) {
  
  res <- FLQuant() 
  
  if(!file.exists(file))
    stop("file does not exist")
  if(!is.element(data, c("N","R")))
    stop("data must be either 'N' or 'R")
#  if(is.na(dimensions['years']))
#    stop("Number of years not specified")
#  if(is.na(dimensions['fisheries']))
#    stop("Number of fisheries not specified")
 
  trim.leading  <- function(x) sub("^\\s+", "", x)
  trim.trailing <- function(x) sub("\\s+$", "", x) 
  trim.hash     <- function(x) sub("#",     "", x) # not used - maybe delete
  
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)+ll]),split="[[:blank:]]+"))
  
  dat <- readLines(file)
  
  if(is.null(dimensions))
    dimensions <- c(agecls = length(unlist(strsplit(trim.leading(dat[3]), " "))) + 1, 
                    years  = length(unlist(strsplit(trim.leading(dat[grep('# simulated_recruitments', dat)+1]), split=" "))), 
                    seasons= nssns, 
                    regions= (grep('# simulated_recruitments', dat)-3)/as.numeric(dat[2]),
                    fisheries=NA, taggrps=NA)
  
  itns  <- as.numeric(dat[2])
  nages <- dimensions['agecls']  
  nssns <- dimensions['seasons']
  nreg  <- dimensions["regions"]
  
  if(data=='N'){
    val <- lapply(strsplit(trim.leading(dat[1:(itns*nreg)+2]), split="[[:blank:]]+"), as.numeric)
    dms <- list(age=2:nages, year=1, unit="unique", season=nssns, area=1:nreg, iter=1:itns)
    res <- FLQuant(array(unlist(val), dim=c(nages-1,1,1,1,nreg,itns)), dimnames=dms)
  }
  
  if(data=='R'){
    val <- lapply(strsplit(trim.leading(dat[(itns*nreg+4):length(dat)]), split="[[:blank:]]+"), as.numeric)
    nyrs<- length(val[[1]])/nssns
    dms <- list(age=1, year=1:nyrs, unit="unique", season=1:nssns, area=1:nreg, iter=1:itns)
    res <- FLQuant(aperm(array(unlist(val), dim=c(nssns,nyrs,1,1,nreg,itns)),c(3,2,4,1,5,6)), dimnames=dms)
  }

  return(res)                
}                  
                     
  
  
