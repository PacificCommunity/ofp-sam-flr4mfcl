#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLRep
#'
#' Reads information from the .rep file and creates an MFCLRep object
#'
#' @param inifile:  A character string giving the name and path of the .rep file to be read 
#' 
#'
#' @return An object of class MFCLRep
#'
#' @examples
#' read.MFCLIni("C://R4MFCL//test_data//skj_ref_case//plot-out.par.rep")
#' read.MFCLIni("/home/roberts/skj/HCR/run0/plot-out.par.rep")
#'
#' @export

read.MFCLRep <- function(repfile) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  
  res <- new("MFCLRep")
  
  pp    <- readLines(repfile)
  pp    <- pp[nchar(pp)>=1]                                          # remove blank lines
  if(any(grepl("# ", pp) & nchar(pp)<3))
    pp <- pp[-seq(1,length(pp))[grepl("# ", pp) & nchar(pp)<3]]   # remove single hashes with no text "# "
  
  dimensions(res)['agecls']    <- c(as.numeric(trim.leading(pp[grep("# Number of age classes", pp)+1])))
  dimensions(res)['years']     <- c(as.numeric(trim.leading(pp[grep("# Number of time periods", pp)+1])))
  dimensions(res)['seasons']   <- c(as.numeric(trim.leading(pp[grep("# Number of recruitments per year", pp)+1])))
  dimensions(res)['regions']   <- c(as.numeric(trim.leading(pp[grep("# Number of regions", pp)+1])))
  dimensions(res)['fisheries'] <- c(as.numeric(trim.leading(pp[grep("# Number of fisheries", pp)+1])))
  
  res@range['minyear'] <- as.numeric(trim.leading(pp[grep("# Year 1", pp)+1]))
  res@range['maxyear'] <- range(res)['minyear'] + (as.numeric(trim.leading(pp[grep("# Number of time periods", pp)+1]))/dimensions(res)['seasons'])-1
  res@range['min']     <- 0
  res@range['max']     <- (dimensions(res)['agecls']/dimensions(res)['seasons'])-1
  
  dnms1 <- list(age=0:range(res)['max'],year='all', unit='unique', season=1:dimensions(res)['seasons'])
  dnms2 <- list(age='all', year=range(res)['minyear']:range(res)["maxyear"], unit='unique', season=1:dimensions(res)['seasons'], area=1:dimensions(res)['regions'])
  dnms3 <- list(age=1:dimensions(res)['agecls'], year='all', unit=1:dimensions(res)['fisheries'], season='all', area='all')
  dnms4 <- list(age='all', year=range(res)['minyear']+c(1:((length(splitter(pp, "# Observed spawning Biomass"))+1)/dimensions(res)['seasons']))-1, unit='unique', season=1:dimensions(res)['seasons'], area='all')
  dnms4a<- list(age='all', year=range(res)['minyear']:range(res)['maxyear'], unit='unique', season='all', area='unique')
  dnms5 <- dnms2; dnms5$age <- 1:dimensions(res)['agecls']
  dnms5a<- dnms5; dnms5a$area <- "all"
  dnms6 <- list(age='all', year=range(res)['minyear']:range(res)["maxyear"], unit=1:dimensions(res)['fisheries'], season=1:dimensions(res)['seasons'], area='unique')
  dnms7 <- list(age='all', year=range(res)['minyear']:range(res)['maxyear'], unit=1:dimensions(res)['fisheries'], season='all', area='unique')
  
  # fishery realisations 
  temp2 <- lapply(1:dimensions(res)['fisheries'], function(x){as.numeric(splitter(pp,"# Time of each realization", ll=x))})
  
  fishery_realizations(res) <- as.FLQuant(data.frame(age   ='all', 
                                                     year  =floor(unlist(temp2)),
                                                     unit  =rep(1:length(temp2), unlist(lapply(temp2, length))), 
                                                     season=ceiling(((unlist(temp2))-floor(unlist(temp2)))*4), 
                                                     area  = 'unique', iter=1, data  = 1))

  # mean length at age
  mean_laa(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Mean lengths at age")),
                                       dim=c(dimensions(res)['seasons'], (range(res)['max']-range(res)['min']+1), 1,1,1)), c(2,3,4,1,5)), dimnames=dnms1 )
  # sd length at age
  sd_laa(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# SD of length at age")),
                                       dim=c(dimensions(res)['seasons'], (range(res)['max']-range(res)['min']+1), 1,1,1)), c(2,3,4,1,5)), dimnames=dnms1 )
  # mean weight at age
  mean_waa(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Mean weights at age")),
                                       dim=c(dimensions(res)['seasons'], (range(res)['max']-range(res)['min']+1), 1,1,1)), c(2,3,4,1,5)), dimnames=dnms1 )
  # m at age
  m_at_age(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Natural mortality at age")),
                                       dim=c(dimensions(res)['seasons'], (range(res)['max']-range(res)['min']+1), 1,1,1)), c(2,3,4,1,5)), dimnames=dnms1 )
  
  realizations.df <- data.frame(age   ="all", 
                                year  =floor(unlist(temp2)), 
                                unit  =rep(1:length(temp2), unlist(lapply(temp2, length))), 
                                season=ceiling(((unlist(temp2))-floor(unlist(temp2)))*4), 
                                area  = 'unique', iter=1)
  
  # q_fishery
  temp_dat       <- as.numeric(splitter(pp,"# Catchability by realization", 1:dimensions(res)['fisheries']))
  q_fishery(res) <- as.FLQuant(cbind(realizations.df, data=unlist(temp_dat)))

  # q_effdev
  temp_dat       <- as.numeric(splitter(pp,"dev.", 1:dimensions(res)['fisheries']))
  q_effdev(res)  <- as.FLQuant(cbind(realizations.df, data=unlist(temp_dat)))
  
  # catch_obs
  temp_dat       <- as.numeric(splitter(pp,"# Observed catch by fishery", 1:dimensions(res)['fisheries']))
  catch_obs(res) <- as.FLQuant(cbind(realizations.df, data=unlist(temp_dat)))
  
  # catch_pred
  temp_dat       <- as.numeric(splitter(pp,"# Predicted catch by fishery", 1:dimensions(res)['fisheries']))
  catch_pred(res) <- as.FLQuant(cbind(realizations.df, data=unlist(temp_dat)))  

  # cpue_obs
  temp_dat       <- as.numeric(splitter(pp,"# Observed CPUE by fishery", 1:dimensions(res)['fisheries']))
  cpue_obs(res) <- as.FLQuant(cbind(realizations.df, data=unlist(temp_dat)))
  
  # cpue_pred
  temp_dat       <- as.numeric(splitter(pp,"# Predicted CPUE by fishery", 1:dimensions(res)['fisheries']))
  cpue_pred(res) <- as.FLQuant(cbind(realizations.df, data=unlist(temp_dat)))    
    
  # total biomass  
  totalBiomass(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Total biomass", 1:dimensions(res)['years'])), 
                                           dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                                     c(4,3,5,2,1)), dimnames=dnms2)
  # Total biomass in absence of fishing
  totalBiomass_nofish(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Total biomass in absence of fishing", 1:dimensions(res)['years'])), 
                                           dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                                     c(4,3,5,2,1)), dimnames=dnms2)
  
  # adult biomass  
  adultBiomass(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Adult biomass", 1:dimensions(res)['years'])), 
                                     dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                               c(4,3,5,2,1)), dimnames=dnms2)
  # adult biomass_no_fish  
  adultBiomass_nofish(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Adult biomass in absence of fishing", 1:dimensions(res)['years'])), 
                                           dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                                     c(4,3,5,2,1)), dimnames=dnms2)
  # Selectivity by age class
  sel(res)  <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Selectivity by age class", 1:dimensions(res)['fisheries'])),
                             dim=c(dimensions(res)['agecls'], dimensions(res)['fisheries'],1,1,1)), c(1,3,2,4,5)),dimnames=dnms3)
  
  
  
  # Fishing mortality by age class (across), year (down) aggregated across regions
  dat     <- pp[(grep("# Fishing mortality by age class", pp)[1]+2) : (grep("# Fishing mortality by age class", pp)[1]+dimensions(res)['years']+1)]
  fm_aggregated(res) <- FLQuant(aperm(array(as.numeric(unlist(strsplit(trim.leading(dat),split="[[:blank:]]+"))), 
                                            dim=c(dimensions(res)['agecls'], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)['seasons'], 1,1)), 
                                      c(1,3,5,2,4)), dimnames=dnms5a)
  
  # Fishing mortality by age class (across), year (down) and region (block)
  dat     <- pp[(grep("# Fishing mortality by age class", pp)[2]+2) : (grep("# Fishing mortality by age class", pp)[2]+(dimensions(res)['years']*dimensions(res)['regions'])+dimensions(res)['regions'])]
  
  if(dimensions(res)['regions']>1)
    dat     <- dat[-grep("# Region", dat)] 
  
  fm(res) <- FLQuant(aperm(array(as.numeric(unlist(strsplit(trim.leading(dat),split="[[:blank:]]+"))), 
                                 dim=c(dimensions(res)['agecls'], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)['seasons'], dimensions(res)['regions'],1)), 
                           c(1,3,5,2,4)), dimnames=dnms5) 
  
    
  # rec_region  
  rec_region(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Recruitment", 1:dimensions(res)['years'])), 
                                         dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                                   c(4,3,5,2,1)), dimnames=dnms2)
  

  if(length(grep("# Observed spawning Biomass", pp))>0) {
    #yrs_orig  <- (length(splitter(pp, "# Observed spawning Biomass"))+1)/dimensions(res)['seasons']

# rds 14/02/20 - commenting out because not sure this is used in any .rep file ????? but not sure - changes made to work for swo projections   
#    if(length(splitter(pp, "# Observed spawning Biomass"))==dimensions(res)['years']){
#      ssb(res)  <- FLQuant(aperm(array(c(NA,as.numeric(splitter(pp, "# Observed spawning Biomass"))),c(dimensions(res)['seasons'],dimensions(res)['years'],1,1,1)), c(3,2,4,1,5)), dimnames=dnms4)
#      rec(res)  <- FLQuant(aperm(array(c(NA,as.numeric(splitter(pp, "# Observed recruitment"))),     c(dimensions(res)['seasons'],dimensions(res)['years'],1,1,1)), c(3,2,4,1,5)), dimnames=dnms4)
#    }
    if(length(splitter(pp, "# Observed spawning Biomass"))==dimensions(res)['years']/dimensions(res)['seasons']){
      ssb(res)  <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Observed spawning Biomass")),c(1,dimensions(res)['years']/dimensions(res)['seasons'],1,1,1)), c(3,2,4,1,5)), dimnames=dnms4a)
      rec(res)  <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Observed recruitment")),     c(1,dimensions(res)['years']/dimensions(res)['seasons'],1,1,1)), c(3,2,4,1,5)), dimnames=dnms4a)
    }
  
    srr(res)  <- FLPar(suppressWarnings(as.numeric(splitter(pp, "# Beverton-Holt")))[!is.na(suppressWarnings(as.numeric(splitter(pp, "# Beverton-Holt"))))],
                       params=c('a','b', 'steepness', 'sigma'))
  }
  
  # Vulnerable Biomass - assumes annual time step
  tempdat          <- aperm(array(as.numeric(splitter(pp, "# Exploitable population biomass by fishery", 1:dimensions(res)['fisheries'], inst=1)), 
                                  dim=c(dimensions(res)['seasons'],dimensions(res)['years']/dimensions(res)['seasons'], dimensions(res)['fisheries'],1,1)), 
                            c(5,2,3,1,4))#[,1:(dimensions(res)['years']/dimensions(res)['seasons']),,,]
  vulnBiomass(res) <- FLQuant(array(tempdat, dim=c(1,(dimensions(res)['years']/dimensions(res)['seasons']),dimensions(res)['fisheries'],dimensions(res)['seasons'],1)), dimnames=dnms6)
  

  dat_length <- length(splitter(pp, "# Equilibrium adult biomass"))
  # equilibrium biomass
  eq_biomass(res) <- FLQuant(array(as.numeric(splitter(pp, "# Equilibrium adult biomass")), dim=c(1,1,dat_length,1,1,1)))
  eq_yield(res)   <- FLQuant(array(as.numeric(splitter(pp, "# Equilibrium yield")), dim=c(1,1,dat_length,1,1,1)))
  
  #popN
  dat     <- pp[(grep("# Population Number by age", pp)+2) : (grep("# Population Number by age", pp)+(dimensions(res)['years']*dimensions(res)['regions'])+dimensions(res)['regions']+length(grep("Projected years",pp)))]
  
  if(dimensions(res)['regions']>1)
    dat     <- dat[-grep("# Region", dat)] 
  if(length(grep("#   Projected years", dat))>0)
    dat     <- dat[-grep("#   Projected years", dat)]
  
  popN(res) <- FLQuant(aperm(array(as.numeric(unlist(strsplit(trim.leading(dat),split="[[:blank:]]+"))), 
                                 dim=c(dimensions(res)['agecls'], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)['seasons'], dimensions(res)['regions'],1)), 
                           c(1,3,5,2,4)), dimnames=dnms5)   
  
  MSY(res) <- as.numeric(pp[grep("# MSY", pp)+1])
  FMSY(res)<- as.numeric(pp[grep("# F at MSY", pp)+1])
  BMSY(res)<- as.numeric(pp[grep("# Adult biomass at MSY", pp)+1])
  
  ABBMSY(res) <- as.numeric(pp[grep("# current Adult Biomass to Adult Biomass at MSY", pp)+1])
  TBBMSY(res) <- as.numeric(pp[grep("# current Total Biomass to Total biomass at MSY", pp)+1])  
  
  Fmult(res)<- as.numeric(pp[grep("# F multiplier at MSY", pp)+1])
  
  FFMSY_ts(res)<- FLQuant(aperm(array(as.numeric(splitter(pp, "# Aggregate F over F at MSY")), 
                                   dim=c(dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)['seasons'],1,1,1,1)), c(3,2,4,1,5,6)),
                       dimnames=list(age="all", year=as.character(range(res)['minyear']:range(res)["maxyear"])))
  
  ABBMSY_ts(res)<- FLQuant(aperm(array(as.numeric(splitter(pp, "# Adult biomass over adult biomass at MSY")), 
                                   dim=c(dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)['seasons'],1,1,1,1)), c(3,2,4,1,5,6)),
                       dimnames=list(age="all", year=as.character(range(res)['minyear']:range(res)["maxyear"])))
  
  AggregateF(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Aggregate F", inst=2)), 
                                         dim=c(dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)['seasons'],1,1,1,1)), c(3,2,4,1,5,6)),
                             dimnames=list(age="all", year=as.character(range(res)['minyear']:range(res)["maxyear"])))
  
  res <- checkUnitDimnames(res, nfisheries=dimensions(res)['fisheries'])
  return(res)
}











read.SBSBF0 <- function(repfile, sbsbf0 = 'latest') {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  
  res <- new("MFCLRep")
  
  pp    <- readLines(repfile)
  pp    <- pp[nchar(pp)>=1]                                          # remove blank lines
  if(any(grepl("# ", pp) & nchar(pp)<3))
    pp <- pp[-seq(1,length(pp))[grepl("# ", pp) & nchar(pp)<3]]   # remove single hashes with no text "# "
  
  dimensions(res)['agecls']    <- c(as.numeric(trim.leading(pp[grep("# Number of age classes", pp)+1])))
  dimensions(res)['years']     <- c(as.numeric(trim.leading(pp[grep("# Number of time periods", pp)+1])))
  dimensions(res)['seasons']   <- c(as.numeric(trim.leading(pp[grep("# Number of recruitments per year", pp)+1])))
  dimensions(res)['regions']   <- c(as.numeric(trim.leading(pp[grep("# Number of regions", pp)+1])))
  dimensions(res)['fisheries'] <- c(as.numeric(trim.leading(pp[grep("# Number of fisheries", pp)+1])))
  
  res@range['minyear'] <- as.numeric(trim.leading(pp[grep("# Year 1", pp)+1]))
  res@range['maxyear'] <- range(res)['minyear'] + (as.numeric(trim.leading(pp[grep("# Number of time periods", pp)+1]))/dimensions(res)['seasons'])-1
  res@range['min']     <- 0
  res@range['max']     <- (dimensions(res)['agecls']/dimensions(res)['seasons'])-1
  
  dnms2 <- list(age='all', year=range(res)['minyear']:range(res)["maxyear"], unit='unique', season=1:dimensions(res)['seasons'], area=1:dimensions(res)['regions'])

  # adult biomass  
  adultBiomass(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Adult biomass", 1:dimensions(res)['years'])), 
                                           dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                                     c(4,3,5,2,1)), dimnames=dnms2)
  # adult biomass_no_fish  
  adultBiomass_nofish(res) <- FLQuant(aperm(array(as.numeric(splitter(pp, "# Adult biomass in absence of fishing", 1:dimensions(res)['years'])), 
                                                  dim=c(dimensions(res)["regions"], dimensions(res)['seasons'], dimensions(res)['years']/dimensions(res)["seasons"],1,1)), 
                                            c(4,3,5,2,1)), dimnames=dnms2)
  
  returnval <- switch(sbsbf0,
                      latest = SBSBF0latest(res),
                      recent = SBSBF0recent(res),
                      instantaneous = SBSBF0(res))
  
  return(returnval)

}

