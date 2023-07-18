#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLALK
#'
#' Reads information from the conditional age length input file and creates an MFCLALK object.
#'
#' @param alfile A character string giving the name and path of the conditional age length input file to be read.
#'
#' @return An object of class MFCLALK
#'
#' @examples
#' \dontrun{
#' read.MFCLALK("C:/R4MFCL/test_data/skj_ref_case/yft.age_length")
#' }
#'
#' @export

# alfile <- "/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/yft.age_length"
# lenfit <- "/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/length.fit"

read.MFCLALK <- function(alfile, lenfit=NULL) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  if(is.character(lenfit))
    lenfit <- read.MFCLLenFit2(lenfit)
  
  alk <- MFCLALK()
  
  alf <- readLines(alfile)   # read file
  alf <- alf[nchar(alf)>=1]  # and remove all blank lines
  
  nrecords <- as.numeric(alf[grep("# num age length records", alf)+1])
  
  markers <- grep("# Year", alf)
  if(length(markers) != nrecords)
    warning(paste("Number of ALKs (",length(markers),") not consistent with 'num age length records' value (", nrecords, ")"))
  
  nlengths <- markers[2]-markers[1] -2 
  nages    <- length(unlist(strsplit(alf[markers[1]+2], split="[[:blank:]]+")))
  
  lbins    <- NA
  if(!is.null(lenfit))
    lbins <- sort(unique(lenfits(lenfit)$length))
  
  if(is.null(lenfit))
    warning("lbins not specified")
  
  if(!is.null(lenfit) & length(lbins) != nlengths)
    warning("lbins not consistent with alk dimensions")
  
  # array of ALK header information - year, month, fishery, ...
  hdat   <- t(array(as.numeric(unlist(strsplit(paste(alf[markers+1], collapse= " "), split="[[:blank:]]+"))), dim=c(4, nrecords)))
  # vector of observations - lots of zeroes!
  alkdat <- unlist(lapply(markers+2, function(mm){as.numeric(unlist(strsplit(alf[mm:(mm+nlengths-1)], split="[[:blank:]]+")))}))
  
  slot(alk, 'ESS') <- as.numeric(splitter(alf, "# effective sample size"))
  slot(alk, 'ALK') <- data.frame(year   =rep(hdat[,1], each=nages*nlengths),
                                 month  =rep(hdat[,2], each=nages*nlengths),
                                 fishery=rep(hdat[,3], each=nages*nlengths),
                                 species=rep(hdat[,4], each=nages*nlengths),
                                 age    =1:nages,
                                 length =rep(lbins, each=nages),
                                 obs    =alkdat)

  slot(alk, 'range') <- unlist(list(minage=1,maxage=nages,plusgroup=NA,minlength=min(lbins),maxlength=max(lbins),minyear=min(hdat[,1]),maxyear=max(hdat[,1])))
  
  return(alk)
}
    
