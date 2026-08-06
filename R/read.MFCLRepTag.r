#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLRepTag
#'
#' Reads information from the tag file and creates object.
#'
#' @param tagrep A character string giving the name and path of the tag.rep file to be read.
#'
#' @return An object of class character data.frame.
#'
#' @examples
#' \dontrun{
#' read.MFCLRepTag("/home/robertsc/MSE/YFT/OM_development/OM_grid_fits/2026_OMs_test/OM0_growth_ini_repair/tag.rep")
#' }
#'
#' @export

read.MFCLRepTag <- function(tagrep) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  trim.trailing <- function(x) sub("\\s+$", "", x) # not used - maybe delete
  trim.hash     <- function(x) sub("#",     "", x) # not used - maybe delete
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)+ll]),split="[[:blank:]]+"))
  
  tagdat <- readLines(tagrep)
  
  mark1 <- grep("# Observed vs", tagdat)+1
  mark2 <- grep("# Movement", tagdat)-1
  
  tagfit <- matrix(as.numeric(unlist(strsplit(trim.leading(tagdat[mark1:mark2]), split="[[:blank:]]+"))),nrow=2)
  
  # pearson residual
  psontagresid <- (tagfit[1,]-tagfit[2,])/sqrt(tagfit[2,])
  
  return(psontagresid)
}