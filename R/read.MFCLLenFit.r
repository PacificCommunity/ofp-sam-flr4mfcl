#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLLenFit
#'
#' Reads information from the length.fit file and creates an MFCLLenFit object
#'
#' @param inifile:  A character string giving the name and path of the length.fit file to be read 
#' 
#'
#' @return An object of class MFCLLenFit
#'
#' @examples
#' read.MFCLLenFit("C://R4MFCL//test_data//skj_ref_case//length.fit")
#'
#' @export

# lffile <- "Q://skj//2016//assessment//RefCase//length.fit"

read.MFCLLenFit <- function(lffile) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 

  lff <- readLines(lffile)   # read file
  lff <- lff[nchar(lff)>=1]  # and remove all blank lines
  
  fsh.obs <- rev(rev(as.numeric(unlist(strsplit(trim.leading(lff[4]), split="[[:blank:]]+"))))[-1])  # records per fishery
  ages    <- 1:as.numeric(lff[5])
  
  temp    <- as.numeric(unlist(strsplit(lff[2], split="[[:blank:]]+")))
  lbins   <- seq(temp[2], temp[1]*temp[3], by=temp[3])
  
  fsh.markers <- grep("# fishery", lff)
  
  lf.obj <- MFCLLenFit()
  
  slot(lf.obj, 'laa') <- FLQuant(as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[1]+2]), split="[[:blank:]]+"))),
                                 dimnames=list(age=as.character(ages), year="all", unit="unique", season="all", area="unique"))
  
  df1 <- data.frame(fishery=NULL, year=NULL, month=NULL, length=NULL, obs=NULL, pred=NULL)
  df2 <- data.frame(fishery=NULL, year=NULL, month=NULL, length=NULL, age=NULL, pred=NULL)
  
  for(ii in 1:(length(fsh.markers)-1)){
    for(jj in (1:fsh.obs[ii]-1) * (5+length(ages))){
      df11 <- data.frame(fishery = ii, 
                         year    = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[1],
                         month   = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[2],
                         length  = lbins,
                         obs     = as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[ii]+jj+4]), split="[[:blank:]]+"))),
                         pred    = as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[ii]+jj+5]), split="[[:blank:]]+"))))
    
      df22 <- data.frame(fishery = ii, 
                         year    = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[1],
                         month   = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[2],
                         length  = lbins,
                         age     = rep(ages, each=length(lbins)),
                         pred    = as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[ii]+jj+6:(6+length(ages)-1)]), split="[[:blank:]]+"))))
    
      df1 <- rbind(df1, df11)
      df2 <- rbind(df2, df22)
    }
  }
  
  slot(lf.obj, 'lenfits') <- df1
  slot(lf.obj, 'lenagefits') <- df2
  
  slot(lf.obj, 'range')[] <- c(ages[1], ages[length(ages)], NA, min(df1$year), max(df1$year)) 
  
  lf.obj <- checkUnitDimnames(lf.obj)
  return(lf.obj)
}  
  
  
  
  
  