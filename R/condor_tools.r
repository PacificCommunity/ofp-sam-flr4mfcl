

### A few tools for mucking about with condor files



#subfile <- '/media/penguin/MSE/skj_MSE/condor/pna_hcr1_em2phase/condor_full2.sub'

condor_submit_set_iters <- function(subfile, first.iter=1){
  
  ## WARNING - this function will overwrite your input file with the new settings !!!
  ##         - sets iteration numbers in submit file to increase successively from user defined start
  
  ss <- readLines(subfile)
  inc <- 0
  for(jobline in (grep("queue", ss)+1):(length(ss)-1)){
    txt    <- unlist(strsplit(ss[jobline], split=" "))
    txt[length(txt)] <- as.character(first.iter + inc) 
    ss[jobline] <- paste(txt, collapse=" ")
    inc    <- inc + 1
  }
  
  writeLines(ss, subfile)
}