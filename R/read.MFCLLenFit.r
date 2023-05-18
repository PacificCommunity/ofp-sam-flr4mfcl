#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLLenFit
#'
#' Reads information from the length.fit file and creates an MFCLLenFit object.
#'
#' @param inifile A character string giving the name and path of the length.fit file to be read.
#'
#' @return An object of class MFCLLenFit
#'
#' @examples
#' \dontrun{
#' read.MFCLLenFit("C:/R4MFCL/test_data/skj_ref_case/length.fit")
#' }
#'
#' @export

# lffile <- "Q:/skj/2016/assessment/RefCase/length.fit"

read.MFCLLenFit <- function(lffile) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 

  lff <- readLines(lffile)   # read file
  lff <- lff[nchar(lff)>=1]  # and remove all blank lines
  
  # First line is # FIT X, the version number
  version_number_line <- grep("# FIT", lff)
  # Length bins
  length_intervals <-  as.numeric(unlist(strsplit(trim.leading(lff[version_number_line + 2]), split="[[:blank:]]+")))
  names(length_intervals) <- c("no_intervals", "smallest_length", "interval")
  lbins <- seq(from=length_intervals[2], length=length_intervals[1], by=length_intervals[3])
  # Records per fishery
  fsh.obs <- rev(rev(as.numeric(unlist(strsplit(trim.leading(lff[version_number_line + 4]), split="[[:blank:]]+"))))[-1])
  # No. ages
  ages    <- 1:as.numeric(lff[version_number_line + 5])
  
  # After header stuff, rest of length.fit file is broken into no_fishery + 1 sections
  # All headed '# fishery X'. Final one is 'fishery totals'
  
  fsh.markers <- grep("# fishery", lff)
  # From email from ND 19/08/21
  # line 1 yr month wk
  # line 2 pmsd->fisc_lf(ir,ip,fi) else 1 added 17Mar2017 - index of fishery in respect of mult-spp
  # line 3 len_sample_size(ir,ip,fi) added 21Mar2017 - sample size
  # line 4 mean length at age
  # line 5 sum(len_frq)
  # line 6 proportions(len_frq)
  
  
  lf.obj <- MFCLLenFit()
  
  # Are these the same for all fisheries? Yes
  slot(lf.obj, 'laa') <- FLQuant(as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[1]+4]), split="[[:blank:]]+"))),
                                 dimnames=list(age=as.character(ages), year="all", unit="unique", season="all", area="unique"))
  
  df1 <- data.frame(fishery=NULL, year=NULL, month=NULL, length=NULL, obs=NULL, pred=NULL)
  df2 <- data.frame(fishery=NULL, year=NULL, month=NULL, length=NULL, age=NULL, pred=NULL)
  
  for(ii in 1:(length(fsh.markers)-1)){
    for(jj in (1:fsh.obs[ii]-1) * (7+length(ages))){
      df11 <- data.frame(fishery = ii, 
                         year    = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[1],
                         month   = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[2],
                         length  = lbins,
                         obs     = as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[ii]+jj+6]), split="[[:blank:]]+"))),
                         pred    = as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[ii]+jj+7]), split="[[:blank:]]+"))))
    
      df22 <- data.frame(fishery = ii, 
                         year    = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[1],
                         month   = as.numeric(unlist(strsplit(lff[fsh.markers[ii]+jj+1], split="[[:blank:]]+")))[2],
                         length  = lbins,
                         age     = rep(ages, each=length(lbins)),
                         pred    = as.numeric(unlist(strsplit(trim.leading(lff[fsh.markers[ii]+jj+8:(8+length(ages)-1)]), split="[[:blank:]]+"))))
    
      df1 <- rbind(df1, df11)
      df2 <- rbind(df2, df22)
    }
  }
  
  slot(lf.obj, 'lenfits') <- df1
  slot(lf.obj, 'lenagefits') <- df2
  
  slot(lf.obj, 'range')[] <- c(ages[1], ages[length(ages)], NA, min(df1$year), max(df1$year)) 
  
  return(lf.obj)
}  
  


#  kk <- read.MFCLLenFit2("/media/sf_assessments/bet/2023/model_runs/stepwise/02PreCatchCond/B06CatchCond4/length.fit")
  
# Speedier version of the original read.MFCLLenFit2
# With option not to get the predicted age-length data.frame (as it is huge)
read.MFCLLenFit2 <- function(lffile, get_lenage = FALSE) {

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
