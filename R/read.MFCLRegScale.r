#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLRegScale
#'
#' Reads information from the regional scaling input file and creates an MFCLRegScale object.
#'
#' @param file A character string giving the name and path of the regional scaling input file to be read.
#' @param seasons The number of seasons, defaults to 4. This should be 1 for an annual model.
#' @param long Whether to return the regional scaling table in long data frame
#'        format rather than the default FLR format.
#'
#' @return An object of class MFCLRegScale, or a data frame if \code{long = TRUE}.
#'
#' @examples
#' \dontrun{
#' read.MFCLRegScale("yft.reg_scaling")
#' read.MFCLRegScale("yft.reg_scaling", long=TRUE)
#' }
#'
#' @export

# file <- "/media/sf_Y_DRIVE/yft/2026/model_runs/stepwise/Hessian_16d_tau-2/yft.reg_scaling"
# yftregscale <- read.MFCLRegScale(file)

read.MFCLRegScale <- function(file, seasons=4, long=FALSE, ...) {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))

  regscale <- MFCLRegScale()

  rgf <- readLines(file)   # read file
  rgf <- rgf[nchar(rgf)>=1]  # and remove all blank lines
  if(any(grepl("# ", rgf) & nchar(rgf)<3))
    rgf <- rgf[-seq(1,length(rgf))[grepl("# ", rgf) & nchar(rgf)<3]]   # remove single hashes with no text "# "

  if(long) {
    # Parse first line
    begyr <- as.integer(strsplit(rgf[1], " ")[[1]][1])
    begmon <- as.integer(strsplit(rgf[1], " ")[[1]][2])
    endyr <- as.integer(strsplit(rgf[1], " ")[[1]][3])
    endmon <- as.integer(strsplit(rgf[1], " ")[[1]][4])

    # Create year-month sequence
    n <- length(rgf) - 1
    year <- begyr + seq(begmon, by=3, length=n) %/% 12
    month <- seq(begmon, by=3, length=n) %% 12
    season <- as.integer((1 + month) / 3)
    if(endyr != year[n] || endmon != month[n])
      stop("year-month header does not match number of rows")

    # Create and rearrange data frame
    regdf <- read.table(text=paste(year, season, rgf[-1]))
    regdf <- data.frame(regdf[1:2], stack(regdf[-(1:2)]))
    regdf <- data.frame(
      year=as.integer(regdf[[1]]), season=as.integer(regdf[[2]]),
      area=as.integer(regdf[[4]]), value=regdf[[3]])
    regdf
  } else {
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
}
