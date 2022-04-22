#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


## write.simNumAge
## strips out the single iteration numbers at age and recruitments and writes simulated numbers at age file.

write.simNumAge <- function(simNumAge, filename, ctrl, nregions, iter){
  
  maxiter <- as.numeric(simNumAge[2])
  
  sna  <- simNumAge[c((((iter-1)*nregions+1)):(iter*nregions)+2)]
  srec <- simNumAge[c(((iter-1)*nregions+1):(iter*nregions)+(maxiter*nregions)+3)]
  
  writeLines(c("# simulated_numbers_at_age", "1", sna, "# simulated_recruitments", srec), filename)
  
}

write.simyears <- function(simyears, filename, iter){
  
  simyears[1] <- paste("1", paste(unlist(strsplit(simyears[1], split=" "))[c(2,3)], collapse=" "))
  simyears[2] <- simyears[iter+1]
  
  writeLines(simyears[1:2], filename)
}


setSimYrs <- function(sna, from.nyrs, to.nyrs, nqtrs=4, nitns=200, nareas=5){
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))   
  
  srec <- t(array(as.numeric(splitter(sna, "recruitment", ll=1:(nitns*nareas))), dim=c(from.nyrs*nqtrs, nitns*nareas)))[,1:(to.nyrs*nqtrs)]
  
  res <- c(sna[1:grep("recruitment", sna)], paste(" ", apply(srec, 1, paste, collapse=" ")))
}


# create a simyears object ready for writing to a file
simyears <- function(projpar, projCtrl){
  
  nsims  <- flagval(projpar, 2, 20)$value
  lyr    <- dimensions(projpar)["years"]/dimensions(projpar)["seasons"]
  fyr    <- lyr - nyears(projCtrl) +1
  
  recyrs        <- ceiling((flagval(projpar, 1, 232)$value : flagval(projpar, 1, 233)$value)/dimensions(projpar)['seasons'])
  recyrs_sample <- matrix(sample(recyrs, nyears(projCtrl)*nsims, replace = TRUE), nrow=nsims)
  
  return(list(header=c(nsims, fyr, lyr), matrix=recyrs_sample))
}




dummy_simulated_numbers_at_age <- function(){
  
  n <- popN(rep)[,'2015',,3]
  #z <- sweep(fm(rep)[,'2015',,3,], 1, c(aperm(slot(rep,'m_at_age'), c(4,1,2,3,5,6))), "+")
  z <- sweep(fm(rep)[,'2015',,3,], 1, c(m_at_age(rep)), "+")
  
  n[1,] %*% diff_coffs_age_period(par)[,,1,3] %*% c(exp(-z)[1,])

  
  age <- 3
  year<- 44
  ssn <- 1
  
  popN(rep)[age,year,1,ssn,,1] %*% diff_coffs_age_period(par)[,,age,ssn] * c(sweep(fm(rep)[age,year,1,ssn,,1], 1, c(m_at_age(rep))[age], "+"))
  
  c(popN(rep)[age+1,year,1,ssn+1,,1])
  
  aa <- aperm(popN(rep)[,44,1,,1,1], c(4,1,2,3,5,6))
  
  aa[2:4,2:16,,,,]/aa[1:3,1:15,,,,]
}


















