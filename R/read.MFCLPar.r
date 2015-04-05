
#' read.MFCLBiol
#'
#' Reads the Biol information from the par file and creates an MFCLBiol object
#' Unfortunately you still need to supply the first year of the time series.
#' Other dimensions are interpreted from the data blocks so care should be taken to check
#' that these have been reproduced correctly
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#' @param first.yr The first year of the time seires
#'
#' @return An object of class MFCLBiol
#'
#' @examples
#' read.MFCLBiol("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export


read.MFCLBiol <- function(parfile, first.yr=1972){
  
  res <- new("MFCLBiol")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  nseasons <- length(scan(parfile, skip=grep("# season_flags", par)+1, nline=1, quiet=quiet))
  nyears   <- length(scan(parfile, skip=grep("# Cohort specific growth deviations", par), nline=1, quiet=quiet)) / nseasons
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  nfish    <- grep("# tag flags", par) - grep("# fish flags", par) -1
  ntaggrps <- grep("# tag fish rep", par)[1] - grep("# tag flags", par) -1
  nregions <- length(scan(parfile, skip=grep("# region parameters", par)+1, nline=1, quiet=quiet))
  
  dims_age        <- dimnames(FLQuant(quant="age"))
  dims_age$age    <- as.character(0:((nagecls/nseasons)-1))
  dims_age$season <- as.character(1:nseasons)
  
  dims_all        <- dims_age
  dims_all$year   <- as.character(first.yr:(first.yr+nyears))
  dims_all$season <- as.character(1:nseasons)
  dims_all$area   <- as.character(1:nregions)
  
  dims_cohort        <- dimnames(FLCohort())
  dims_cohort$age    <- "0"
  dims_cohort$cohort <- as.character(first.yr:(first.yr+nyears))
  dims_cohort$season <- as.character(1:nseasons)
  
    
  age_class_pars <- t(array(scan(parfile, skip=grep("# age-class related parameters",  par)+1, n=nagecls*5, quiet=quiet), dim=c(nagecls,5)))
  
  slot(res, "m")                 <- as.numeric(par[grep("# natural mortality coefficient", par)+2])
  slot(res, "m_devs_age")        <- FLQuant(age_class_pars[2,], dimnames=dims_age)
  slot(res, "log_m")             <- FLQuant(age_class_pars[5,], dimnames=dims_age)
  slot(res, "growth_devs_age")   <- FLQuant(age_class_pars[3,], dimnames=dims_age)  
  slot(res, "growth_curve_devs") <- FLQuant(age_class_pars[4,], dimnames=dims_age)  
  
  slot(res, "mat")      <- FLQuant(aperm(array(scan(parfile, skip=grep("# percent maturity ",  par), n=nagecls, quiet=quiet),
                                         dim=c(nseasons,nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims_age)
  slot(res, "growth")   <- t(array(scan(parfile, skip=grep("# The von Bertalanffy parameters",  par), n=9, quiet=quiet), dim=c(3,3),
                                   dimnames=list(c("est","min","max"),c("Lmin","Lmax","k"))))
  slot(res, "richards") <- as.numeric(par[grep("# extra par for Richards",  par)+1])
  
  slot(res, "len_bias_pars")        <- scan(parfile, skip=grep("# First Length bias parameters",    par), n=nfish, quiet=quiet)
  slot(res, "common_len_bias_pars") <- scan(parfile, skip=grep("# Common first Length bias flags",  par), n=nfish, quiet=quiet)
  slot(res, "common_len_bias_coffs")<- scan(parfile, skip=grep("# Common first Length bias coffs",  par), n=nfish, quiet=quiet)
  slot(res, "season_growth_pars")   <- scan(parfile, skip=grep("# Seasonal growth parameters",      par), n=30,    quiet=quiet)
  
  slot(res, "growth_devs_cohort")   <- FLCohort(scan(parfile, skip=grep("# Cohort specific growth deviations",par), 
                                                     n=nyears*nseasons, quiet=quiet), dimnames=dims_cohort)
  
  slot(res, "range") <- c(min=min(as.numeric(dims_all$age)), max=max(as.numeric(dims_all$age)), plusgroup=NA,
                          minyear=min(as.numeric(dims_all$year)), maxyear=max(as.numeric(dims_all$year)))
  return(res)
}  

#kk <- read.MFCLBiol("C://R4MFCL//test_data//skj_ref_case//11.par")




#' read.MFCLFlags
#'
#' Reads the Flag information from the par file and creates an MFCLFlags object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLFlags
#'
#' @examples
#' read.MFCLFlags("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLFlags <- function(parfile) {
  
  res <- new("MFCLFlags")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  nfish    <- grep("# tag flags", par) - grep("# fish flags", par) -1
  ntaggrps <- grep("# tag fish rep", par)[1] - grep("# tag flags", par) -1
  nregions <- length(scan(parfile, skip=grep("# region parameters", par)+1, nline=1, quiet=quiet))
  
  parflags   <- scan(parfile, skip=grep("# The parest_flags", par), nlines=1, quiet=quiet) # 400 of them
  ageflags   <- scan(parfile, skip=grep("# age flags",        par), nlines=1, quiet=quiet) # 200 of them
  fishflags  <- scan(parfile, skip=grep("# fish flags",       par), nlines=nfish, quiet=quiet) # 100 for each fishery
  tagflags   <- scan(parfile, skip=grep("# tag flags",        par), nlines=ntaggrps, quiet=quiet) # 10 for each tag group
  yearflags  <- scan(parfile, skip=grep("# year_flags",       par), nlines=10, quiet=quiet)
  seasonflags<- scan(parfile, skip=grep("# season_flags",     par), nlines=10, quiet=quiet)
  regionflags<- scan(parfile, skip=grep("# region control flags", par), nlines=10, quiet=quiet) # 10 for each region
  
  parflagsdf <- data.frame(flagtype=1, flag=1:length(parflags), value=parflags)
  ageflagsdf <- data.frame(flagtype=2, flag=1:length(ageflags), value=ageflags)
  fishflagsdf<- data.frame(flagtype=rep(-1:-nfish, each=100), flag=1:100, value=fishflags)
  tagflagsdf <- data.frame(flagtype=rep(-1:-ntaggrps, each=10)-9999, flag=1:10, value=tagflags)
  regionflagsdf<- data.frame(flagtype=rep(-1:-10, each=nregions)-99999,flag=rep(1:nregions,10), value=regionflags)
  
  slot(res, 'flags') <- rbind(parflagsdf, ageflagsdf, fishflagsdf, tagflagsdf, regionflagsdf)
  slot(res, 'unused')<- list(yrflags  =array(yearflags,  dim=c(10, length(yearflags)/10)),
                             snflags  =array(seasonflags,dim=c(10, length(seasonflags)/10)))
  
  return(res)
}

#kk <- read.MFCLFlags("C://R4MFCL//test_data//skj_ref_case//11.par")






#' read.MFCLTagRep
#'
#' Reads the TAaRep information from the par file and creates an MFCLTagRep object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLTagRep
#'
#' @examples
#' read.MFCLTagRep("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLTagRep <- function(parfile) {
 
  res <- new("MFCLTagRep")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  nfish    <- grep("# tag flags", par) - grep("# fish flags", par) -1
  ntaggrps <- grep("# tag fish rep", par)[1] - grep("# tag flags", par) -1
  dims     <- c(nfish, ntaggrps+1)
  
  dev_coffs<- list(length=nfish)
  for(i in 1:nfish)
    dev_coffs[i] <- paste(scan(parfile, skip=grep("# Reporting rate dev coffs",par)+i,nlines=1, quiet=quiet), collapse=" ")
  
  lapply(lapply(lapply(dev_coffs, strsplit, split=" "), unlist), as.numeric)
  
  slot(res, 'tag_fish_rep_rate') <- t(array(scan(parfile, skip=grep("# tag fish rep",             par),nlines=ntaggrps+1, quiet=quiet), dim=dims))
  slot(res, 'tag_fish_rep_grp')  <- t(array(scan(parfile, skip=grep("# tag fish rep group flags", par),nlines=ntaggrps+1, quiet=quiet), dim=dims))
  slot(res, 'tag_fish_rep_flags')<- t(array(scan(parfile, skip=grep("# tag_fish_rep active flags",par),nlines=ntaggrps+1, quiet=quiet), dim=dims))
  slot(res, 'tag_fish_rep_target')<-t(array(scan(parfile, skip=grep("# tag_fish_rep target",      par),nlines=ntaggrps+1, quiet=quiet), dim=dims))
  slot(res, 'tag_fish_rep_pen')   <-t(array(scan(parfile, skip=grep("# tag_fish_rep penalty",     par),nlines=ntaggrps+1, quiet=quiet), dim=dims))
  slot(res, 'rep_rate_dev_coffs') <- dev_coffs
                                      
  return(res)
}

#kk <- read.MFCLTagRep("C://R4MFCL//test_data//skj_ref_case//11.par")





#' read.MFCLRec
#'
#' Reads the recruitment information from the par file and creates an MFCLec object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLRec
#'
#' @examples
#' read.MFCLRec("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLRec <- function(parfile, first.yr=1972) {
  
  res <- new("MFCLRec")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  nseasons <- length(scan(parfile, skip=grep("# season_flags", par)+1, nline=1, quiet=quiet))
  nyears   <- length(scan(parfile, skip=grep("# Cohort specific growth deviations", par), nline=1, quiet=quiet)) / nseasons 
  nregions <- length(scan(parfile, skip=grep("# region parameters", par)+1, nline=1, quiet=quiet))
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  
  dims        <- dimnames(FLQuant(quant="age"))
  dims$age    <- "0"
  dims$season <- as.character(1:nseasons)
  dims$year   <- as.character(first.yr:(first.yr+nyears-1))
  
  dims2       <- dims
  dims2$age   <- as.character(1:((nagecls/nseasons)-1))
  dims2$year  <- as.character(first.yr)
  dims2$area  <- as.character(1:nregions)
  
  rel_rec <- array(scan(parfile, skip=grep("# relative recruitment", par)+1, nlines=1, quiet=quiet),dim=c(nseasons, nyears, 1,1,1,1))
  rel_ini <- array(scan(parfile, skip=grep("# relative initial population", par)+1, nlines=nregions, quiet=quiet),dim=c(nseasons, (nagecls/nseasons)-1, nregions,1,1,1))
  
  slot(res, "rec_init_pop_diff") <- as.numeric(par[grep("# rec init pop level difference", par)+1])
  slot(res, "rec_times")         <- scan(parfile, skip=grep("# recruitment times", par), nlines=1, quiet=quiet)
  slot(res, "rel_rec")           <- FLQuant(aperm(rel_rec, c(3,2,4,1,5,6)), dimnames=dims)
  slot(res, "rel_ini_pop")       <- FLQuant(aperm(rel_ini, c(2,4,5,1,3,6)), dimnames=dims2)
  
  slot(res, "tot_pop")           <- as.numeric(par[grep("# total populations scaling parameter", par)+1])
  slot(res, "tot_pop_implicit")  <- as.numeric(par[grep("# implicit total populations scaling parameter ", par)+1])
  
  
  slot(res, "range") <- c(min=min(as.numeric(dims$age)), max=max(as.numeric(dims$age)), plusgroup=NA,
                          minyear=min(as.numeric(dims$year)), maxyear=max(as.numeric(dims$year)))
  
  return(res) 
}





#' read.MFCLRegion
#'
#' Reads the region specific information from the par file and creates an MFCLRegion object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLRec
#'
#' @examples
#' read.MFCLRegion("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLRegion <- function(parfile, first.yr=1972) {
  
  res <- new("MFCLRegion")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  nregions <- length(scan(parfile, skip=grep("# region parameters", par)+1, nline=1, quiet=quiet))
  nseasons <- length(scan(parfile, skip=grep("# season_flags", par)+1, nline=1, quiet=quiet))
  nyears   <- length(scan(parfile, skip=grep("# Cohort specific growth deviations", par), nline=1, quiet=quiet)) / nseasons
  
  dca <- par[(grep("# movement matrices", par)+2):grep("# age dependent movement coefficients",par)[1]]
  dca <- lapply(lapply(dca[-seq(nregions+1, length(dca), by=nregions+1)], strsplit, split="[[:blank:]]+"), unlist)
  dca <- t(matrix(as.numeric(unlist(dca)), nrow=nregions+1)[-1,])
  dca <- aperm(array(dca, dim=c(nregions, nagecls, nseasons, nregions), 
                     dimnames=list(from=as.character(1:nregions), age=as.character(1:nagecls), period=as.character(1:nseasons), to=as.character(1:nregions))),
               c(1,4,2,3))
  
  rrv <- aperm(array(scan(parfile, skip=grep("# regional recruitment variation ",par),nlines=nyears*nseasons, 
                     quiet=quiet), dim=c(nregions, nseasons, nyears, 1, 1)), 
               c(4,3,5,2,1)) 
  
  
  
  slot(res, 'control_flags') <- t(array(scan(parfile, skip=grep("# region control flags",par),nlines=10, quiet=quiet), 
                                        dim=c(nregions,10)))
  slot(res, 'move_map')      <- scan(parfile, skip=grep("# movement map",par),nlines=1, quiet=quiet)
  slot(res, 'diff_coffs')    <- matrix(scan(parfile, skip=grep("# movement coefficients",par),nlines=length(slot(res, 'move_map')), 
                                            quiet=quiet), nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_age_period') <- dca
  slot(res, 'diff_coffs_age') <- matrix(scan(parfile, skip=grep("# age dependent movement coefficients",par),nlines=length(slot(res, 'move_map')), 
                                             quiet=quiet), nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_nl')  <- matrix(scan(parfile, skip=grep("# nonlinear movement coefficients",par),nlines=length(slot(res, 'move_map')), 
                                             quiet=quiet), nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_priors')  <- matrix(scan(parfile, skip=grep("# Movement coefficients priors ",par),nlines=length(slot(res, 'move_map')), 
                                             quiet=quiet), nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_age_priors') <- matrix(scan(parfile, skip=grep("# age dependent movement coefficients priors",par),nlines=length(slot(res, 'move_map')), 
                                             quiet=quiet), nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_nl_priors')  <- matrix(scan(parfile, skip=grep("# nonlinear movement coefficients priors",par),nlines=length(slot(res, 'move_map')), 
                                             quiet=quiet), nrow=nseasons, byrow=T)
  slot(res, 'region_rec_var') <- FLQuant(rrv, dimnames=list(age="all", year=as.character(seq(first.yr, first.yr+nyears-1)),unit="unique", 
                                                            season=as.character(1:nseasons),area=as.character(1:nregions)))
  
  slot(res, 'region_pars') <- matrix(scan(parfile, skip=grep("# region parameters ",par),nlines=10, 
                                             quiet=quiet), ncol=nregions, byrow=T)
  
  slot(res, 'range') <- c(min=0, max=nagecls/nseasons, plusgroup=NA, minyear=first.yr, maxyear=max(as.numeric(dimnames(region_rec_var(res))$year)))
  
  return(res)
}
  




#' read.MFCLSel
#'
#' Reads the Selection information from the par file and creates an MFCLSel object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLSel
#'
#' @examples
#' read.MFCLSel("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLSel <- function(parfile) {
  
  res <- new("MFCLSel")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  nregions <- length(scan(parfile, skip=grep("# region parameters", par)+1, nline=1, quiet=quiet))
  nseasons <- length(scan(parfile, skip=grep("# season_flags", par)+1, nline=1, quiet=quiet))
  nfish    <- grep("# tag flags", par) - grep("# fish flags", par) -1
  
  dims1    <- list(age=as.character(seq(0,(nagecls/nseasons)-1)), year='all', unit='unique', season=c(as.character(1:nseasons)), area='all')
  dims2    <- list(age=as.character(seq(0,(nagecls/nseasons)-1)), year='all', unit=c(as.character(1:nfish)), season=c(as.character(1:nseasons)), area='all')
  dims3    <- list(age='all', year='all', unit=as.character(1:nfish), season="all", area="all")
  
  qdc_ptr  <- grep("# catchability deviation coefficients",par)+2
  qdc      <- lapply(strsplit(par[qdc_ptr:(qdc_ptr+nfish-1)], split="[[:blank:]]+"), as.numeric)
  qdc      <- lapply(qdc, function(x){x[!is.na(x)]})
  
  sdc_ptr  <- grep("# sel_dev_coffs ",par)
  sdc_ptrs <- c(sdc_ptr, sdc_ptr+cumsum(unlist(lapply(qdc, length))+1)+c(1:nfish))[-(nfish+1)]
  sdc      <- list(nfish)
  for(i in 1:nfish)
    sdc[[i]] <- matrix(scan(parfile, skip=sdc_ptrs[i], nlines=unlist(lapply(qdc, length))[i]+1, quiet=quiet), ncol=nagecls, byrow=T)
  
  slot(res, 'availability_coffs') <- FLQuant(aperm(array(scan(parfile, skip=grep("# availability coffs", par)+1,nlines=1, quiet=quiet), 
                                                   dim=c(nseasons, nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims1)
  slot(res, 'fishery_sel')  <- FLQuant(aperm(array(scan(parfile, skip=grep("# fishery selectivity", par),nlines=nfish, quiet=quiet), 
                                             dim=c(nseasons, nagecls/nseasons,nfish,1,1)),c(2,4,3,1,5)), dimnames=dims2)
  slot(res, 'av_q_coffs')   <- FLQuant(aperm(array(scan(parfile, skip=grep("# average catchability coefficients", par)+1,n=nfish, quiet=quiet), 
                                                   dim=c(nseasons, nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims1)
  slot(res, 'ini_q_coffs')  <- FLQuant(aperm(array(scan(parfile, skip=grep("# initial trend in catchability coefficients ", par)+1,n=nfish, quiet=quiet), 
                                                   dim=c(nseasons, nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims1)
  slot(res, 'q0_miss')      <- FLQuant(aperm(array(scan(parfile, skip=grep("# q0_miss", par)+1, n=nfish, quiet=quiet), 
                                                   dim=c(nseasons, nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims1)  
  slot(res, 'sel_dev_corr') <- FLQuant(aperm(array(scan(parfile, skip=grep("# correlation in selectivity deviations", par), n=nfish, quiet=quiet), 
                                                   dim=c(nfish, 1,1,1,1)),c(2,3,1,4,5)), dimnames=dims3)  
  slot(res, 'season_q_pars')<- matrix(scan(parfile, skip=grep("# seasonal_catchability_pars", par),nlines=nfish, quiet=quiet), ncol=12, byrow=T)
    
  slot(res, 'q_dev_coffs')  <- qdc
  # the big ones
  slot(res, 'sel_dev_coffs') <- matrix(scan(parfile, skip=grep("# selectivity deviation coefficients ", par)+2,
                                            nlines=sum(unlist(lapply(qdc, length)))-nfish, quiet=quiet), ncol=nagecls, byrow=T)
  slot(res, 'sel_dev_coffs2')<- sdc
    
  slot(res, 'range') <- c(min=0, max=nagecls/nseasons, plusgroup=NA, minyear=1, maxyear=1)
  
  return(res)
}




#' read.MFCLParBits
#'
#' Reads the remaining information from the par file and creates an MFCLParBits object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLParBit
#'
#' @examples
#' read.MFCLParBits("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLParBits <- function(parfile) {
  
  res <- new("MFCLParBits")
  quiet <- TRUE
  
  par    <- readLines(parfile)
  
  taglik <- unlist(strsplit(par[grep("# Likelihood component for tags",          par)], split="[[:blank:]]+"))
  av_f_i <- unlist(strsplit(par[grep("# Average fish mort per fishing incident", par)], split="[[:blank:]]+"))
  av_f_y <- unlist(strsplit(par[grep("# Average fish mort per year is",          par)], split="[[:blank:]]+"))
  av_f_a <- unlist(strsplit(par[grep("# Average fish mort per year by age class",par)+1], split="[[:blank:]]+"))
  
  slot(res, 'fm_level_devs') <- par[(grep("# fm_level_devs", par)+1):(grep("# movement map", par)-1)]
  slot(res, 'obj_fun')  <- scan(parfile, skip=grep("# Objective function value",par), n=1, quiet=quiet)
  slot(res, 'n_pars')   <- scan(parfile, skip=grep("# The number of parameters",par), n=1, quiet=quiet)
  slot(res, 'tag_lik')  <- as.numeric(taglik[length(taglik)])
  slot(res, 'max_grad') <- scan(parfile, skip=grep("# Maximum magnitude gradient value ",par), n=1, quiet=quiet)
  slot(res, 'av_fish_mort_inst') <- as.numeric(av_f_i[length(av_f_i)])
  slot(res, 'av_fish_mort_year') <- as.numeric(av_f_y[length(av_f_y)])
  slot(res, 'av_fish_mort_age')  <- as.numeric(av_f_a[-1])
  
  return(res)
}











#' read.MFCLPar
#'
#' Reads information from the par file and creates an MFCLPar object
#'
#' @param parfile A character string giving the name and path of the frq file to be read 
#'
#' @return An object of class MFCLPar
#'
#' @examples
#' read.MFCLPar("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLPar <- function(parfile) {
  
  res <- new("MFCLPar")
  
  slotcopy <- function(from, to){
    for(slotname in slotNames(from)){
      slot(to, slotname) <- slot(from, slotname)
    }
    return(to)
  }
  par    <- readLines(parfile)
  
  res <- slotcopy(read.MFCLBiol(parfile), res)
  res <- slotcopy(read.MFCLFlags(parfile), res)
  res <- slotcopy(read.MFCLTagRep(parfile), res)
  res <- slotcopy(read.MFCLRec(parfile), res)
  res <- slotcopy(read.MFCLRegion(parfile), res)
  res <- slotcopy(read.MFCLSel(parfile), res)
  res <- slotcopy(read.MFCLParBits(parfile), res)
  
  slot(res, 'range') <- c(min=0, max=max(as.numeric(unlist(dimnames(fishery_sel(res))['age']))), 
                          plusgroup=NA, 
                          minyear=min(as.numeric(unlist(dimnames(rel_rec(res))['year']))), 
                          maxyear=max(as.numeric(unlist(dimnames(rel_rec(res))['year']))))
  
  return(res)
}




  