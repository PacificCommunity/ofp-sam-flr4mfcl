#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLWgtFit
#'
#' Reads information from the weight.fit file and creates an MFCLWgtFit object.
#'
#' @param inifile A character string giving the name and path of the weight.fit file to be read.
#'
#' @return An object of class MFCLWgtFit
#'
#' @examples
#' \dontrun{
#' read.MFCLWgtFit("C:/R4MFCL/test_data/skj_ref_case/weight.fit")
#' }
#'
#' @export

# wffile <- "/media/sf_assessments/yft/2023/model_runs/stepwise/03_PreCatchCond/03j_No_Effort_Projections/weight.fit"
#  kk <- read.MFCLWgtFit("/media/sf_assessments/bet/2023/model_runs/stepwise/02PreCatchCond/B06CatchCond4/weight.fit")
  
# With option not to get the predicted age-length data.frame (as it is huge)
read.MFCLWgtFit <- function(wffile, get_wgtage = FALSE) {
  #browser()
  # Helper function to clean up character strings
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  
  wff <- readLines(wffile)   # read file
  wff <- wff[nchar(wff)>=1]  # and remove all blank lines
  
  # First line is # FIT X, the version number
  vno <- wff[1] # This code may only work for FIT 3 versions
  if (vno != "# FIT 3"){
    warning("This method was expecting version '# FIT 3' in the first line\n. It may not work properly with whatever version your file is.")
  }
  
  # remove comments to make similar format to length.fit
  if(length(grep("#wghtsum", wff))>0)
    wff <- wff[-grep("#wghtsum", wff)]
  if(length(grep("#wghtfrq", wff))>0)
    wff <- wff[-grep("#wghtfrq", wff)]
  
  # Next line is data for drawing histograms in the viewer and can be ignored (see MFCL manual)
  # Then there are the length bins
  weight_intervals <-  as.numeric(unlist(strsplit(trim.leading(wff[3]), split="[[:blank:]]+")))
  names(weight_intervals) <- c("no_intervals", "smallest_weight", "interval")
  wbins <- seq(from=weight_intervals[2], length=weight_intervals[1], by=weight_intervals[3])
  # Then integer of number of fisheries + 1 - ignore
  # Then samples per fishery - vector of length nfisheries + 1
  # Read and drop the last one
  fsh_nobs <- rev(rev(as.numeric(unlist(strsplit(trim.leading(wff[5]), split="[[:blank:]]+"))))[-1])
  # No. ages
  ages    <- 1:as.numeric(wff[6])
  nages <- length(ages)
  # Ignore fishery species pointer (lines 7 & 8)

  # Next: the fishery blocks - (nfisheries + 1) blocks (fishery 1, fishery 2, ..., fishery totals)
  fishery_blocks <- grep("fishery", wff)
  # Each fishery block is made up of sample blocks (see fsh_nobs for n samples per fishery)
  # Each sample block = 7 lines + matrix with nages rows
  sample_size <- nages + 9 

  # Go fishery by fishery, and process all sample blocks at the same time
  nfisheries <- length(fsh_nobs)
  out <- list()
  out2 <- list()

  for (fishery in 1:nfisheries){
    if(fsh_nobs[fishery] == 0)
      next
    #browser()  
    fishery_start <- fishery_blocks[fishery]
    # Get year and month of each sample block in each fishery
    time_ids_posns <- fishery_start + seq(from = 1, to = sample_size * fsh_nobs[fishery], by=sample_size)
    time_ids <- wff[time_ids_posns]
    time_ids <- strsplit(trim.leading(time_ids), split="[[:blank:]]+")
    year <- as.numeric(unlist(lapply(time_ids, "[[", 1)))
    month <- as.numeric(unlist(lapply(time_ids, "[[", 2)))
    # Get nsamples
    nsample_posns <- fishery_start + seq(from = 3, to = sample_size * fsh_nobs[fishery], by=sample_size)
    nsamples <- wff[nsample_posns]
    nsamples <- as.numeric(unlist(strsplit(trim.leading(nsamples), split="[[:blank:]]+")))
  
    # prop_obs - 1 line per sample block
    obs_posns <- fishery_start +  seq(from = 6, to = (sample_size* (fsh_nobs[fishery] - 1)) + 6, by=sample_size)
    prop_obs <- wff[obs_posns]
    prop_obs <- as.numeric(unlist(strsplit(trim.leading(prop_obs), split="[[:blank:]]+")))
    # prop_pred - 1 line per sample block
    pred_posns <- fishery_start +  seq(from = 8, to = (sample_size*(fsh_nobs[fishery] - 1)) + 8, by=sample_size)
    prop_pred <- wff[pred_posns]
    prop_pred <- as.numeric(unlist(strsplit(trim.leading(prop_pred), split="[[:blank:]]+")))
  
    # Put together into data.frame
    temp_df <- data.frame(fishery = fishery,
                            year = rep(year, each=length(wbins)),
                             month = rep(month, each=length(wbins)),
                             sample_size = rep(nsamples, each=length(wbins)),
                             weight = rep(wbins, fsh_nobs[fishery]),
                             obs = prop_obs,
                             pred = prop_pred)
    out[[fishery]] <- temp_df
  
    if(get_wgtage == TRUE){
      # The huge matrix of predicted props by age and length
      # Each matrix has nages rows, and length(lbins) columns
      # Each sample block has a matrix
      #browser()
      matrix_starts <- fishery_start + seq(from = 10, to=sample_size * fsh_nobs[fishery], by=sample_size)
      # From each start read in nages lines
      matrix_posns <- unlist(lapply(matrix_starts, function(x) return(x:(x+nages-1))))
      matdat <- wff[matrix_posns]
      matdat <- as.numeric(unlist(strsplit(trim.leading(matdat), split="[[:blank:]]+")))
    
      tempmatdf <- data.frame(fishery = fishery,
        year =rep(year, each=length(wbins)*nages),
        month =rep(month, each=length(wbins)*nages),
        weight = rep(rep(wbins, by=nages), fsh_nobs[fishery]),
        age  = rep(rep(ages, each=length(wbins)), fsh_nobs[fishery]),
        pred = matdat)
      
      out2[[fishery]] <- tempmatdf
    }
  }
  
  wgtdat <- do.call("rbind", out)
  agewgtdat <- do.call("rbind", out2)
  
  # Get range
  range <- c(ages[1], ages[length(ages)], NA, min(wgtdat$year), max(wgtdat$year))
  # Get length-at-age - in the fishery blocks but same for each fishery
  
  waa <- wff[grep(paste0("# fishery ", which(fsh_nobs>0)[1]), wff)[1] + 4]
  waa <- as.numeric(unlist(strsplit(trim.leading(waa), split="[[:blank:]]+")))
  waa <- FLQuant(waa, dimnames=list(age=as.character(ages), year="all", unit="unique", season="all", area="unique"))
  
  # Make new object and populate
  obj <- MFCLWgtFit()
  slot(obj, 'range')[] <- range
  slot(obj, 'waa') <- waa
  slot(obj, 'wgtfits') <- wgtdat
  slot(obj, 'wgtagefits') <- as.data.frame(agewgtdat) # Force as data frame in case it is NULL (get_lenage==FALSE)
  return(obj)
}


