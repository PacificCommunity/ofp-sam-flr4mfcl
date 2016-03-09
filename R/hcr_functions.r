


## write.simNumAge
## strips out the single iteration numbers at age and recruitments and writes simulated numbers at age file.

write.simNumAge <- function(simNumAge, filename, ctrl, nregions, iter){
  
  sna  <- simNumAge[c((((iter-1)*nregions+1)):(iter*nregions)+2)]
  srec <- simNumAge[c(((iter-1)*nregions+1):(iter*nregions)+(nsims(ctrl)*nregions)+3)]
  
  writeLines(c("# simulated_numbers_at_age", "1", sna, "# simulated_recruitments", srec), filename)
  
}



setSimYrs <- function(sna, from.nyrs, to.nyrs, nqtrs=4, nitns=200, nareas=5){
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))   
  
  srec <- t(array(as.numeric(splitter(sna, "recruitment", ll=1:(nitns*nareas))), dim=c(from.nyrs*nqtrs, nitns*nareas)))[,1:(to.nyrs*nqtrs)]
  
  res <- c(sna[1:grep("recruitment", sna)], paste(" ", apply(srec, 1, paste, collapse=" ")))
}
