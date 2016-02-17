#' read.MFCLTag
#'
#' Reads information from the tag file and creates  object
#'
#' @param tagfile:  A character string giving the name and path of the tag file to be read 
#' 
#'
#' @return An object of class character vector
#'
#' @examples
#' read.MFCLTag("C://R4MFCL//test_data//skj_ref_case//skj.tag")
#'
#' @export

read.MFCLTag <- function(tagfile) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  trim.trailing <- function(x) sub("\\s+$", "", x) # not used - maybe delete
  trim.hash     <- function(x) sub("#",     "", x) # not used - maybe delete
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)+ll]),split="[[:blank:]]+"))
  
  tagdat <- c(readLines(tagfile), "#", "#---")
  res    <- MFCLTag()
  
  topdat              <- as.numeric(splitter(tagdat, "# RELEASE GROUPS"))
  release_groups(res) <- topdat[1]
  release_lengths(res)<- seq(topdat[2], topdat[2]+topdat[4]*(topdat[3]-1), by=topdat[4])
  
  recoveries(res)     <- as.numeric(splitter(tagdat, "# TAG RECOVERIES", ll=2))
  
  release.marker     <- grep("#---", tagdat)
  recapture.marker   <- grep("# LENGTH RELEASE", tagdat)
  hash.marker        <- grep("#", tagdat)
  
  mm2 <- 1
  for(mm in 1:(length(release.marker)-1)){ 

    program      <- rev(unlist(strsplit(tagdat[release.marker[mm]+1], split="[[:blank:]]+")))[1]
    
    topdat.event <- as.numeric(unlist(strsplit(trim.leading(tagdat[release.marker[mm]+2]), split="[[:blank:]]+")))
    
    releases(res) <- rbind(releases(res), data.frame(rel.group  =mm,
                                                     region =topdat.event[1],
                                                     year   =topdat.event[2],
                                                     month  =topdat.event[3],
                                                     program=program,
                                                     length =release_lengths(res), 
                                                     lendist=as.numeric(unlist(strsplit(trim.leading(tagdat[release.marker[mm]+3]), split="[[:blank:]]+")))))
    
    if(recapture.marker[mm2]<release.marker[mm+1]) {    
      nrows   <- min(hash.marker[hash.marker>recapture.marker[mm2] & hash.marker<release.marker[mm+1]]) - recapture.marker[mm2] - 1

      tempdat <-t(array(as.numeric(unlist(strsplit(trim.leading(tagdat[recapture.marker[mm2]+1:nrows]), 
                                                 split="[[:blank:]]+"))), dim=c(5, nrows)))
      
      recaptures(res)<- rbind(recaptures(res), data.frame(rel.group    =mm,
                                                          region       =topdat.event[1],
                                                          year         =topdat.event[2],
                                                          month        =topdat.event[3],
                                                          program      =program,
                                                          rel.length   =tempdat[,1],
                                                          recap.fishery=tempdat[,2],
                                                          recap.year   =tempdat[,3],
                                                          recap.month  =tempdat[,4],
                                                          recap.number =tempdat[,5]))    
    mm2 <- mm2 + 1
    }
  }
  range(res) <- c(min=min(release_lengths(res)), max=max(release_lengths(res)), plusgroup=NA, 
                  minyear=min(releases(res)$year), maxyear=max(recaptures(res)$recap.year))
  
  return(res)
}
