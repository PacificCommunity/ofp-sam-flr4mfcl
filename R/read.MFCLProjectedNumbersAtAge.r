#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


read.MFCLProjectedNatAge <- function(filename="projected_numbers_at_age", quarterly=T, fyear=1972){
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  
  ff   <- readLines(filename)
  
  comments <- grep("# ", ff)
  fff      <- ff[-comments]
  
  n_sims    <- length(grep("# Simulation", ff))
  n_regions <- length(grep("# Region", ff))/n_sims
  n_periods <- grep("# Region 2", ff)[1] - grep("# Region 1", ff)[1] - 2
  n_ages    <- length(unlist(strsplit(trim.leading(fff[1]), split="[[:blank:]]+")))
  
  if(quarterly)
    sns <- 1:4
  
  dimnames  <- list(age=1:n_ages, year=fyear:(fyear+n_periods/max(sns)-1), 
                    unit="unique", season=sns, area=1:n_regions, iter=1:n_sims)
  
  arr   <- array(unlist(strsplit(trim.leading(fff), split="[[:blank:]]+")), 
                 dim=c(n_ages, max(sns), n_periods/max(sns), n_regions, n_sims, 1))
  
  projN <- FLQuant(aperm(arr, c(1,3,6,2,4,5)), dimnames=dimnames)
  
  return(projN)
}


#kk <- readLines('/home/rob/MSE/ofp-sam-skipjack_MSE/reports/PseudoData/MFCL_refits/stoch_proj__1/projected_spawning_biomass')
#kk <- read.MFCLProjectedSpawningBiomass('/home/rob/MSE/ofp-sam-skipjack_MSE/reports/PseudoData/MFCL_refits/stoch_proj__1/projected_spawning_biomass')
#filename <- '/home/rob/MSE/ofp-sam-skipjack_MSE/reports/PseudoData/MFCL_refits/stoch_proj__1/projected_spawning_biomass'

read.MFCLProjectedSpawningBiomass <- function(filename="projected_spawning_biomass", quarterly=T, fyear=1972){

  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  ff   <- readLines(filename)
  
  comments <- grep("# ", ff)
  fff      <- ff[-comments]
  
  n_sims    <- length(grep("# Simulation", ff))
  n_regions <- length(unlist(strsplit(trim.leading(ff[3]), split="[[:blank:]]+")))
  n_periods <- grep("# Simulation number 2", ff)[1] - grep("# Simulation number 1", ff)[1] - 2
  
  if(quarterly)
    sns <- 1:4
  
  dimnames  <- list(age="all", year=fyear:(fyear+n_periods/max(sns)-1), 
                    unit="unique", season=sns, area=1:n_regions, iter=1:n_sims)
  
  arr   <- array(unlist(strsplit(trim.leading(fff), split="[[:blank:]]+")), 
                 dim=c(1, n_regions, max(sns), n_periods/max(sns), n_sims, 1))
  
  projB <- FLQuant(aperm(arr, c(1,4,6,3,2,5)), dimnames=dimnames)
  
}