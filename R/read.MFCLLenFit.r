#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLLenFit
#'
#' Reads information from the \verb{length.fit} file and creates an
#' \code{MFCLLenFit} object.
#'
#' @param lffile a character string giving the name and path of the
#'        \verb{length.fit} file to be read.
#' @param get_lenage whether to import the predicted \code{lenagefits} data
#'        frame, which is very large.
#'
#' @return An object of class MFCLLenFit.
#'
#' @note
#' \code{read.MFCLLenFit2} is an equivalent alias for \code{read.MFCLLenFit}.
#'
#' @examples
#' \dontrun{
#' read.MFCLLenFit("C:/R4MFCL/test_data/skj_ref_case/length.fit")
#' }
#'
#' @aliases read.MFCLLenFit2
#'
#' @export

#  kk <- read.MFCLLenFit2("/media/sf_assessments/bet/2023/model_runs/stepwise/02PreCatchCond/B06CatchCond4/length.fit")

# Speedier version of the original read.MFCLLenFit2
# With option not to get the predicted age-length data.frame (as it is huge)
read.MFCLLenFit <- function(lffile, get_lenage = FALSE) {

  # Helper function to clean up character strings
  trim.leading  <- function(x) sub("^\\s+", "", x)

  lff <- readLines(lffile)   # read file
  lff <- lff[nchar(lff)>=1]  # and remove all blank lines

  # First line is # FIT X, the version number
  vno <- lff[1] # This code may only work for FIT 3 versions
  if (vno != "# FIT 3"){
    warning("This method was expecting version '# FIT 3' in the first line\n. It may not work properly with whatever version your file is.")
  }
  # Next line is data for drawing histograms in the viewer and can be ignored (see MFCL manual)
  # Then there are the length bins
  length_intervals <-  as.numeric(unlist(strsplit(trim.leading(lff[3]), split="[[:blank:]]+")))
  names(length_intervals) <- c("no_intervals", "smallest_length", "interval")
  lbins <- seq(from=length_intervals[2], length=length_intervals[1], by=length_intervals[3])
  # Then integer of number of fisheries + 1 - ignore
  # Then samples per fishery - vector of length nfisheries + 1
  # Read and drop the last one
  fsh_nobs <- rev(rev(as.numeric(unlist(strsplit(trim.leading(lff[5]), split="[[:blank:]]+"))))[-1])
  # No. ages
  ages    <- 1:as.numeric(lff[6])
  nages <- length(ages)
  # Ignore fishery species pointer (lines 7 & 8)

  # Next: the fishery blocks - (nfisheries + 1) blocks (fishery 1, fishery 2, ..., fishery totals)
  fishery_blocks <- grep("fishery", lff)
  # Each fishery block is made up of sample blocks (see fsh_nobs for n samples per fishery)
  # Each sample block = 7 lines + matrix with nages rows
  sample_size <- nages + 7

  # Go fishery by fishery, and process all sample blocks at the same time
  nfisheries <- length(fsh_nobs)
  out <- list()
  out2 <- list()

  for (fishery in 1:nfisheries){
    if(fsh_nobs[fishery] == 0)
      next

    fishery_start <- fishery_blocks[fishery]
    # Get year and month of each sample block in each fishery
    time_ids_posns <- fishery_start + seq(from = 1, to = sample_size * fsh_nobs[fishery], by=sample_size)
    time_ids <- lff[time_ids_posns]
    time_ids <- strsplit(trim.leading(time_ids), split="[[:blank:]]+")
    year <- as.numeric(unlist(lapply(time_ids, "[[", 1)))
    month <- as.numeric(unlist(lapply(time_ids, "[[", 2)))
    # Get nsamples
    nsample_posns <- fishery_start + seq(from = 3, to = sample_size * fsh_nobs[fishery], by=sample_size)
    nsamples <- lff[nsample_posns]
    nsamples <- as.numeric(unlist(strsplit(trim.leading(nsamples), split="[[:blank:]]+")))

    # prop_obs - 1 line per sample block
    obs_posns <- fishery_start +  seq(from = 6, to = (sample_size* (fsh_nobs[fishery] - 1)) + 6, by=sample_size)
    prop_obs <- lff[obs_posns]
    prop_obs <- as.numeric(unlist(strsplit(trim.leading(prop_obs), split="[[:blank:]]+")))
    # prop_pred - 1 line per sample block
    pred_posns <- fishery_start +  seq(from = 7, to = (sample_size*(fsh_nobs[fishery] - 1)) + 7, by=sample_size)
    prop_pred <- lff[pred_posns]
    prop_pred <- as.numeric(unlist(strsplit(trim.leading(prop_pred), split="[[:blank:]]+")))

    # Put together into data.frame
    temp_df <- data.frame(fishery = fishery,
                            year = rep(year, each=length(lbins)),
                             month = rep(month, each=length(lbins)),
                             sample_size = rep(nsamples, each=length(lbins)),
                             length = rep(lbins, fsh_nobs[fishery]),
                             obs = prop_obs,
                             pred = prop_pred)
    out[[fishery]] <- temp_df

    if(get_lenage == TRUE){
      # The huge matrix of predicted props by age and length
      # Each matrix has nages rows, and length(lbins) columns
      # Each sample block has a matrix
      matrix_starts <- fishery_start + seq(from = 8, to=sample_size * fsh_nobs[fishery], by=sample_size)
      # From each start read in nages lines
      matrix_posns <- unlist(lapply(matrix_starts, function(x) return(x:(x+nages-1))))
      matdat <- lff[matrix_posns]
      matdat <- as.numeric(unlist(strsplit(trim.leading(matdat), split="[[:blank:]]+")))

      tempmatdf <- data.frame(fishery = fishery,
        year =rep(year, each=length(lbins)*nages),
        month =rep(month, each=length(lbins)*nages),
        length = rep(rep(lbins, by=nages), fsh_nobs[fishery]),
        age  = rep(rep(ages, each=length(lbins)), fsh_nobs[fishery]),
        pred = matdat)

      out2[[fishery]] <- tempmatdf
    }
  }

  lendat <- do.call("rbind", out)
  agelendat <- do.call("rbind", out2)

  # Get range
  range <- c(ages[1], ages[length(ages)], NA, min(lendat$year), max(lendat$year))
  # Get length-at-age - in the fishery blocks but same for each fishery

  laa <- lff[grep(paste0("# fishery ", which(fsh_nobs>0)[1]), lff) + 4]
  laa <- as.numeric(unlist(strsplit(trim.leading(laa), split="[[:blank:]]+")))
  laa <- FLQuant(laa, dimnames=list(age=as.character(ages), year="all", unit="unique", season="all", area="unique"))

  # Make new object and populate
  obj <- MFCLLenFit()
  slot(obj, 'range')[] <- range
  slot(obj, 'laa') <- laa
  slot(obj, 'lenfits') <- lendat
  slot(obj, 'lenagefits') <- as.data.frame(agelendat) # Force as data frame in case it is NULL (get_lenage==FALSE)
  return(obj)
}

#' @export

read.MFCLLenFit2 <- function(...) read.MFCLLenFit(...)
