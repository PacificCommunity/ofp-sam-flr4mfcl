#' read.fishmort
#' 
#' Reads the fishing mortality from the fishmort file that is output by Multifan-CL.
#' The fishing mortality in the fishmort file is by region, time period, fishing incident and age.
#' This function returns the fishing mortality by region, year, season, fishery and age.
#' As the fishmort file has values by fishing incident rather than fishery, the structure of realisations from the freq file
#' are necessary as function arguments so ensure the correct fishing mortality is allocated to the
#' correct fishery.
#' We also need a map of fishery to region - this can be generated from an MFCLFreq object.
#' The fishmort file provides fishing mortality on a log scale. The read.fishmort() function transforms them to
#' 'normal' scale (or whatever it's called).
#' Note that summing these fishing mortalities by region, time and age period yields the same values 
#' as reported in the 'fm' slot of an MFCLRep object. But summing by time and age does not yield the same values 
#' as in the 'fm_aggregated' slot which seems to be something to do with the yield analysis.
#'
#' @param file A character string giving the name and path of the fishmort file to be read in.
#' @param realisations A data.frame with columns of year, month and fishery where each row is a fishing incident. Probably easiest to get this by calling the realisations() function on an MFCLFrq object.
#' @param fishery_region A data.frame with columns of fishery and region that maps fishery to region. Probably easiest to get this using the region_fish() function on an MFCLFrq object (see example below).
#' @param nages An integer with the number of ages (expressed as time periods) in the stock, e.g. no. ages in years x no. seasons in the model.
#'
#' @return An FLQuant with fishing mortality by age, year, fishery and season.
#' 
#' @examples
#' \dontrun{
#' freq <- read.MFCLFrq("my_mfcl_op_path/bet.frq")
#' fishery_region_df <-  data.frame(fishery=1:length(c(region_fish(freq))), region= c(region_fish(freq)))
#' fish_mort <- read.fishmort(file = "my_mfcl_op_path/fishmort", realisations = realisations(freq), fishery_region = fishery_region_df, nages=40)
#' }
#' 
#' @export

read.fishmort <- function(file="fishmort", realisations=NULL, fishery_region=NULL, nages=NULL){
  if(is.null(realisations)){
    stop("You need to pass a realisations argument")
  }
  if(is.null(fishery_region)){
    stop("You need to pass a fishery_region argument")
  }
  if(is.null(nages)){
    stop("You need to pass a nages argument")
  }
  # Make a data.frame of fishing morts with the right structure
  fmort <- realisations[,c("year", "month", "fishery")]
  fmort <- merge(fmort, fishery_region)
  # Expand the fmort data.frame to include an age column
  fmort <- fmort[rep(seq_len(nrow(fmort)), each = nages), ]
  fmort$age <- 1:nages
  # Sort so order matches that in the fishmort file: region / time period / fishing incident
  fmort <- fmort[order(fmort$region, fmort$year, fmort$month, fmort$fishery, fmort$age),]
  
  # Read in raw values from fishmort and clean up a little
  ip <- readLines(con=file)
  trim.leading  <- function(x) sub("^\\s+", "", x)
  logf <- trim.leading(ip)
  # Turn into single, long vector
  logf <- as.numeric(unlist(strsplit(logf, "\\s+")))
  
  # Sanity check that everything has been set up OK
  if(nrow(fmort) != length(logf)){
    stop("Length of fishing mortality vector (logf) does not equal number of rows in fmort data.frame. Something bad has happened...")
  }
  
  # Add in the fishing mortality data
  fmort$data <- exp(logf)
  
  # Batter into an FLQuant - note that we no longer need the region column
  colnames(fmort)[colnames(fmort)=="fishery"] <- "unit"
  colnames(fmort)[colnames(fmort)=="month"] <- "season"
  fmort <- fmort[,c("age","year","season","unit","data")]
  # Sort again for convenience
  fmort <- fmort[order(fmort$age, fmort$year, fmort$season, fmort$unit),]
  out <- as.FLQuant(fmort)
  
  # Units are in a weird order because not always have all units in first time step
  out <- out[,,order(as.numeric(dimnames(out)$unit))]
  # Should season be 1:4?
  out <- out[,,,order(as.numeric(dimnames(out)$season))]
  dimnames(out)$season <- as.character(1:dim(out)[4])
  
  return(out)
}