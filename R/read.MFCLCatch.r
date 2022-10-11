#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLCatch
#'
#' Reads information from the catch.rep file and creates an MFCLCatch object
#'
#' @param catchfile a character string giving the name and path of the catch.rep file to be read.
#' @param dimensions a numeric vector of dimensions (as returned from dimensions(par)).
#' @param rnge undocumented.
#'
#' @return An object of class MFCLCatch.
#'
#' @examples
#' \dontrun{
#' read.MFCLCatch("C:/R4MFCL/test_data/skj_ref_case/catch.rep", dimensions(par))
#' read.MFCLCatch("/home/robertsc/skj/HCR/run0/catch.rep", dimensions(par))
#' }
#'
#' @export

read.MFCLCatch <- function(catchfile, dimensions, rnge) {

  res <- MFCLCatch()

  if(!file.exists(catchfile))
    stop("Catch.rep file does not exist")
  if(is.na(dimensions['years']))
    stop("Number of years not specified")
  if(is.na(dimensions['fisheries']))
    stop("Number of fisheries not specified")

  nsns <- dimensions["seasons"]
  nyrs <- dimensions["years"]/nsns
  nfish<- dimensions["fisheries"]

  dmns1 <- list(age="all", year=rnge["minyear"]:rnge["maxyear"], unit="unique", season=1:nsns, area="unique")
  dmns2 <- list(age="all", year=rnge["minyear"]:rnge["maxyear"], unit=1:nfish,  season=1:nsns, area="unique")

  # Read in vectors and check dimensions
  total_catch <- scan(catchfile, skip=1, nlines=1)
  if(length(total_catch) != dimensions["years"]){
    stop("Length of total_catch vector does not equal 'years' element of dimensions argument\n")
  }
  fishery_catch <- scan(catchfile, skip=3, nlines=nfish)
  if(length(fishery_catch) != (dimensions["years"] * dimensions["fisheries"])){
    stop("Length of fishery_catch vector does not equal product of 'years' and 'fisheries' elements of dimensions argument\n")
  }

  slot(res, "total_catch")   <- FLQuant(aperm(array(total_catch,dim=c(nsns,nyrs,1,1,1)), c(3,2,4,1,5)),dimnames=dmns1)
  slot(res, "fishery_catch") <- FLQuant(aperm(array(fishery_catch, dim=c(nsns,nyrs,nfish,1,1)), c(4,2,3,1,5)),dimnames=dmns2)
  slot(res, "range")[] <- c(NA, NA, NA, rnge["minyear"], rnge["maxyear"])

  res <- checkUnitDimnames(res, nfisheries=nfish)
  return(res)
}
