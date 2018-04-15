


read.temporary_tag_report <- function(temp_tag_report="temporary_tag_report", year1=1972){
  
  get.number <- function(str){
    val <- as.numeric(unlist(strsplit(str, "[^0-9.]+")))    # reg expr to strip numbers from strings
    val[!is.na(val)]
  }
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  
  ttr <- readLines(temp_tag_report)
  
  rel.grp  <- c(grep("release group", ttr), length(ttr))
  rec.pred <- grep("predicted recapture in region", ttr)
  rel.grp  <- rel.grp[rel.grp >= min(rec.pred)-1]
  
  rec.fsh  <- grep("fishery", ttr)
  
  release.groups <- as.numeric(cut(rec.fsh,  rel.grp))
  recapture.dat  <- matrix(get.number(ttr[rec.fsh]), ncol=3, byrow=T)
  
  recaps.pred <- unlist(lapply(strsplit(trim.leading(ttr[rec.fsh+1]), split="[[:blank:]]+"), function(x){sum(as.numeric(x))}))
  recaps.obs  <- unlist(lapply(strsplit(trim.leading(ttr[rec.fsh+2]), split="[[:blank:]]+"), function(x){sum(as.numeric(x))}))  
  
  result <- data.frame(rel.group    =release.groups,
                       recap.fishery=recapture.dat[,1],
                       recap.year   =recapture.dat[,2]+year1-1,
                       recap.month  =recapture.dat[,3],
                       recap.pred   =recaps.pred,
                       recap.obs    =recaps.obs)
  
  return(result)
}




