#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLRegScale
#'
#' Reads information from the regional scaling input file and creates an MFCLRegScale object.
#'
#' @param file A character string giving the name and path of the regional scaling input file to be read.
#' @param seasons The number of seasons - defaults to 4. This should be 1 for an annual model.
#'
#' @return An object of class MFCLRegScale
#'
#' @examples
#' \dontrun{
#' read.MFCLRegScale("C:/R4MFCL/test_data/skj_ref_case/yft.reg_scaling")
#' }
#'
#' @export

# file <- "/media/sf_Y_DRIVE/yft/2026/model_runs/stepwise/Hessian_16d_tau-2/yft.reg_scaling"
# yftregscale <- read.MFCLRegScale(file)

read.MFCLRegScale <- function(file, seasons=4, ...) {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))

  regscale <- MFCLRegScale()

  rgf <- readLines(file)   # read file
  rgf <- rgf[nchar(rgf)>=1]  # and remove all blank lines
  if(any(grepl("# ", rgf) & nchar(rgf)<3))
    rgf <- rgf[-seq(1,length(rgf))[grepl("# ", rgf) & nchar(rgf)<3]]   # remove single hashes with no text "# "

  range(regscale)[c('minyear', 'minmonth', 'maxyear', 'maxmonth')] <- as.numeric(unlist(strsplit(trim.leading(rgf[[1]]), split="[[:blank:]]+")))

  dimensions(regscale)['seasons'] <- seasons
  dimensions(regscale)['years']   <- (range(regscale)['maxyear'] - range(regscale)['minyear'] + 1) * seasons
  dimensions(regscale)['regions'] <- length(unlist(strsplit(rgf[2], split="[[:blank:]]+")))

  # checks
  if(dimensions(regscale)['years'] != length(rgf)-1)
    warning('You might want to check your inputs - year ranges might not be lining up')

  index(regscale) <- t(array(round(as.double(unlist(strsplit(rgf[2:length(rgf)], split="[[:blank:]]+"))),3),
                             dim=c(dimensions(regscale)['regions'], length(rgf)-1)))

  return(regscale)
}
