
#' read.MFCLBiol
#'
#' Reads the Biol information from the par file and creates an MFCLBiol object
#' Unfortunately you still need to supply the first year of the time series.
#' Other dimensions are interpreted from the data blocks so care should be taken to check
#' that these have been reproduced correctly
#'
#' @param parfile:  A character string giving the name and path of the frq file to be read 
#' @param parobj:   A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#' @param first.yr: The first year of the input data time series (default values 1972)
#'
#' @return An object of class MFCLBiol
#'
#' @examples
#' read.MFCLBiol("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export


read.MFCLBiol <- function(parfile, parobj=NULL, first.yr=1972){
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  trim.trailing <- function(x) sub("\\s+$", "", x) # not used - maybe delete
  trim.hash     <- function(x) sub("#",     "", x) # not used - maybe delete
  
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)+ll]),split="[[:blank:]]+"))
  
  res <- new("MFCLBiol")
  
  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj
  
  nseasons <- length(splitter(par, "# season_flags"))
  nyears   <- length(splitter(par, "# Cohort specific growth deviations"))
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])
  
  nfish    <- grep("# tag flags", par) - grep("# fish flags", par)[1] -1
  ntaggrps <- grep("# tag fish rep", par)[1] - grep("# tag flags", par) -1
  nregions <- length(splitter(par, "# region parameters"))
  
  dims_age        <- dimnames(FLQuant(quant="age"))
  dims_age$age    <- as.character(0:((nagecls/nseasons)-1))
  dims_age$season <- as.character(1:nseasons)
  
  dims_all        <- dims_age
  dims_all$year   <- as.character(first.yr:(first.yr+(nyears/nseasons)))
  dims_all$season <- as.character(1:nseasons)
  dims_all$area   <- as.character(1:nregions)
  
  dims_cohort        <- dimnames(FLCohort())
  #dims_cohort$age    <- "0"
  dims_cohort$cohort <- as.character(first.yr:(first.yr+(nyears-1)/nseasons))
  dims_cohort$season <- as.character(1:nseasons)
  
  age_class_pars <- t(array(as.numeric(splitter(par, "# age-class related parameters", 1:5)), dim=c(nagecls, 5)))
  
  slot(res, "m")                 <- as.numeric(par[grep("# natural mortality coefficient", par)+1])
  slot(res, "m_devs_age")        <- FLQuant(age_class_pars[2,], dimnames=dims_age)
  slot(res, "log_m")             <- FLQuant(age_class_pars[5,], dimnames=dims_age)
  slot(res, "growth_devs_age")   <- FLQuant(age_class_pars[3,], dimnames=dims_age)  
  slot(res, "growth_curve_devs") <- FLQuant(age_class_pars[4,], dimnames=dims_age)  
  
  slot(res, "mat")      <- FLQuant(aperm(array(as.numeric(splitter(par, "# percent maturity")),
                                               dim=c(nseasons,nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims_age)

  slot(res, "growth")   <- t(array(as.numeric(splitter(par, "# The von Bertalanffy parameters", 1:3)),
                                   dim=c(3,3), dimnames=list(c("est","min","max"),c("Lmin","Lmax","k"))))
                                       
  slot(res, "richards") <- as.numeric(par[grep("# extra par for Richards",  par)+1])
  
  slot(res, "len_bias_pars")        <- as.numeric(splitter(par, "# First Length bias parameters"))
  slot(res, "common_len_bias_pars") <- as.numeric(splitter(par, "# Common first Length bias flags"))
  slot(res, "common_len_bias_coffs")<- as.numeric(splitter(par, "# Common first Length bias coffs"))
  slot(res, "season_growth_pars")   <- as.numeric(splitter(par, "# Seasonal growth parameters"))
  
  slot(res, "growth_devs_cohort")   <- FLCohort(as.numeric(splitter(par, "# Cohort specific growth deviations")), dimnames=dims_cohort)
  
  slot(res, "dimensions") <- c(agecls=nagecls, years=nyears, seasons=nseasons, regions=nregions, fisheries=nfish, taggrps=ntaggrps)
  
  slot(res, "range") <- c(min=min(as.numeric(dims_all$age)), max=max(as.numeric(dims_all$age)), plusgroup=NA,
                          minyear=min(as.numeric(dims_all$year)), maxyear=max(as.numeric(dims_all$year)))
  
  slot(res, "growth_var_pars") <- t(array(as.numeric(splitter(par,"# Variance parameters", 1:2)), dim=c(3,2),
                                          dimnames=list(c("ini","min","max"),c("Length","Size"))))
  
  slot(res, "n_mean_constraints") <- as.numeric(par[grep("# The number of mean constraints", par)+1])
  
  return(res)
}  

#kk <- read.MFCLBiol("C://R4MFCL//test_data//skj_ref_case//11.par")




#' read.MFCLFlags
#'
#' Reads the Flag information from the par file and creates an MFCLFlags object
#'
#' @param parfile: A character string giving the name and path of the frq file to be read 
#' @param parobj:  A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#'
#' @return An object of class MFCLFlags
#'
#' @examples
#' read.MFCLFlags("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLFlags <- function(parfile, parobj=NULL, first.yr=1972) {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[1]+ll]),split="[[:blank:]]+"))
  
  res    <- new("MFCLFlags")
  
  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj
  
  #nfish    <- grep("# tag flags", par) - grep("# fish flags", par)[1] -1
  nfish    <- length(splitter(par, "# q0_miss"))
  ntaggrps <- max(0,grep("# tag fish rep", par)[1] - grep("# tag flags", par) -1)
  nregions <- length(splitter(par,"# region parameters"))
  
  parflags   <- as.numeric(splitter(par,"# The parest_flags"))      # 400 of them
  ageflags   <- as.numeric(splitter(par,"# age flags"))             # 200 of them
  fishflags  <- as.numeric(splitter(par,"# fish flags", 1:nfish))   # 100 for each fishery
  tagflags   <- as.numeric(splitter(par,"# tag flags",  1:ntaggrps)) # 10 for each tag group
  yearflags  <- as.numeric(splitter(par,"# year_flags", 1:10))
  seasonflags<- as.numeric(splitter(par,"# season_flags",1:10))
  regionflags<- as.numeric(splitter(par,"# region control flags",1:10)) # 10 for each region
  
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
#' @param parfile: A character string giving the name and path of the frq file to be read 
#' @param parobj:  A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#'
#' @return An object of class MFCLTagRep
#'
#' @examples
#' read.MFCLTagRep("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLTagRep <- function(parfile, parobj=NULL, first.yr=1972) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[1]+ll]),split="[[:blank:]]+"))  
  nlines        <- function(ff, tt) grep('#',ff)[grep('#',ff)>grep(tt,ff)[1]][1]-grep(tt,ff)[1]-1
  
  res <- new("MFCLTagRep")
  
  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    if(any(grepl("# ", par) & nchar(par)<3))
      par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj
  
  nfish    <- length(splitter(par, '# tag fish rep', 1))
  
  slot(res, 'tag_fish_rep_rate') <- t(array(as.numeric(splitter(par, "# tag fish rep", 1:(nlines(par, "# tag fish rep")))), 
                                            dim=c(nfish, nlines(par, "# tag fish rep"))))
  slot(res, 'tag_fish_rep_grp')  <- t(array(as.numeric(splitter(par, "# tag fish rep group flags", 1:(nlines(par, "# tag fish rep group flags")))), 
                                            dim=c(nfish, nlines(par, "# tag fish rep group flags"))))
  slot(res, 'tag_fish_rep_flags')<- t(array(as.numeric(splitter(par, "# tag_fish_rep active flags",1:(nlines(par, "# tag_fish_rep active flags")))), 
                                            dim=c(nfish, nlines(par, "# tag_fish_rep active flags"))))
  slot(res, 'tag_fish_rep_target')<-t(array(as.numeric(splitter(par, "# tag_fish_rep target",      1:(nlines(par, "# tag_fish_rep target")))), 
                                            dim=c(nfish, nlines(par, "# tag_fish_rep target"))))
  slot(res, 'tag_fish_rep_pen')   <-t(array(as.numeric(splitter(par, "# tag_fish_rep penalty",     1:(nlines(par, "# tag_fish_rep penalty")))), 
                                            dim=c(nfish, nlines(par, "# tag_fish_rep penalty"))))
  slot(res, 'rep_rate_dev_coffs') <- lapply(seq(1:nfish), function(x) as.numeric(splitter(par, "# Reporting rate dev coffs",x)))
  
  return(res)
}

#kk <- read.MFCLTagRep("C://R4MFCL//test_data//skj_ref_case//11.par")





#' read.MFCLRec
#'
#' Reads the recruitment information from the par file and creates an MFCLec object
#'
#' @param parfile: A character string giving the name and path of the frq file to be read 
#' @param parobj:  A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#' @param first.yr: The first year of the input data time series (default value 1972)
#'
#' @return An object of class MFCLRec
#'
#' @examples
#' read.MFCLRec("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLRec <- function(parfile, parobj=NULL, first.yr=1972) {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[1]+ll]),split="[[:blank:]]+"))  

  res <- new("MFCLRec")
  
  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    par <- par[-seq(1,length(par))[grepl("#", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj
  
  nseasons <- length(splitter(par, "# season_flags"))
  nyears   <- length(splitter(par, "# Cohort specific growth deviations"))
  nregions <- length(splitter(par,"# region parameters"))
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  
  dims        <- dimnames(FLQuant(quant="age"))
  dims$age    <- "0"
  dims$season <- as.character(1:nseasons)
  dims$year   <- as.character(first.yr:(first.yr+(nyears/nseasons)-1))
  
  dims2       <- dims
  dims2$age   <- as.character(1:((nagecls/nseasons)-1))
  dims2$year  <- as.character(first.yr)
  dims2$area  <- as.character(1:nregions)
  
  rel_rec <- array(as.numeric(splitter(par, "# relative recruitment")),dim=c(nseasons, nyears/nseasons, 1,1,1,1))
  rel_ini <- array(as.numeric(splitter(par, "# relative initial population", 1:nregions)),dim=c(nregions, nagecls-1))
  
  slot(res, "rec_init_pop_diff") <- as.numeric(par[grep("# rec init pop level difference", par)+1])
  slot(res, "rec_times")         <- as.numeric(splitter(par, "# recruitment times"))
  slot(res, "rel_rec")           <- FLQuant(aperm(rel_rec, c(3,2,4,1,5,6)), dimnames=dims)
  slot(res, "rel_ini_pop")       <- rel_ini #FLQuant(aperm(rel_ini, c(2,4,5,1,3,6)), dimnames=dims2)
  
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
#' @param parfile: A character string giving the name and path of the frq file to be read 
#' @param parobj:  A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#' @param first.yr: The first year of the input data time series (default value 1972)
#'
#' @return An object of class MFCLRec
#'
#' @examples
#' read.MFCLRegion("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLRegion <- function(parfile, parobj=NULL, first.yr=1972, version='new') {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[1]+ll]),split="[[:blank:]]+"))    
  
  res <- new("MFCLRegion")
  
  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj
  
  nseasons <- length(splitter(par, "# season_flags"))
  nyears   <- length(splitter(par, "# Cohort specific growth deviations"))/nseasons
  nregions <- length(splitter(par,"# region parameters"))
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  
  dca <- par[(grep("# movement matrices", par)+2):grep("# age dependent movement coefficients",par)[1]]
  dca <- lapply(lapply(dca[-seq(nregions+1, length(dca), by=nregions+1)], strsplit, split="[[:blank:]]+"), unlist)
  dca <- t(matrix(as.numeric(unlist(dca)), nrow=nregions+1)[-1,])
  dca <- aperm(array(dca, dim=c(nregions, nagecls, nseasons, nregions), 
                     dimnames=list(from=as.character(1:nregions), age=as.character(1:nagecls), period=as.character(1:nseasons), to=as.character(1:nregions))),
               c(1,4,2,3))
  
  rrv <- aperm(array(as.numeric(splitter(par, "# regional recruitment variation ", 1:(nyears*nseasons))), 
                     dim=c(nregions, nseasons, nyears, 1, 1)), 
               c(4,3,5,2,1)) 
  
  
  slot(res, 'control_flags') <- t(array(as.numeric(splitter(par, "# region control flags",1:10)), 
                                        dim=c(nregions,10)))
  slot(res, 'move_map')      <- as.numeric(splitter(par, "# movement map"))
  slot(res, 'diff_coffs')    <- matrix(as.numeric(splitter(par,"# movement coefficients",1:length(slot(res, 'move_map')))), nrow=nseasons, byrow=T)  
  slot(res, 'diff_coffs_mat')<- matrix(as.numeric(splitter(par, "# The diffusion coefficients",1:nregions)), nrow=nregions, byrow=T)
  
  slot(res, 'diff_coffs_age_period') <- dca
  slot(res, 'diff_coffs_age') <- matrix(as.numeric(splitter(par, "# age dependent movement coefficients",1:length(slot(res, 'move_map')))), 
                                        nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_nl')  <- matrix(as.numeric(splitter(par, "# nonlinear movement coefficients",1:length(slot(res, 'move_map')))), 
                                        nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_priors')  <- matrix(as.numeric(splitter(par,"# Movement coefficients priors ",1:length(slot(res, 'move_map')))), 
                                        nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_age_priors') <- matrix(as.numeric(splitter(par, "# age dependent movement coefficients priors",1:length(slot(res, 'move_map')))), 
                                        nrow=nseasons, byrow=T)
  slot(res, 'diff_coffs_nl_priors')  <- matrix(as.numeric(splitter(par, "# nonlinear movement coefficients priors",1:length(slot(res, 'move_map')))), 
                                        nrow=nseasons, byrow=T)
  slot(res, 'region_rec_var') <- FLQuant(rrv, dimnames=list(age="all", year=as.character(seq(first.yr, first.yr+nyears-1)),unit="unique", 
                                                            season=as.character(1:nseasons),area=as.character(1:nregions)))
  
  if(version=='new')
    slot(res, 'region_pars') <- matrix(as.numeric(splitter(par, "# region parameters ",1:100)), ncol=nregions, byrow=T)
  if(version=='old')
    slot(res, 'region_pars') <- matrix(as.numeric(splitter(par, "# region parameters ",1:10)), ncol=nregions, byrow=T)
  
  slot(res, 'range') <- c(min=0, max=nagecls/nseasons, plusgroup=NA, minyear=first.yr, maxyear=max(as.numeric(dimnames(region_rec_var(res))$year)))
  
  return(res)
}
  




#' read.MFCLSel
#'
#' Reads the Selection information from the par file and creates an MFCLSel object
#'
#' @param parfile: A character string giving the name and path of the frq file to be read 
#' @param parobj:  A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#'
#' @return An object of class MFCLSel
#'
#' @examples
#' read.MFCLSel("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLSel <- function(parfile, parobj=NULL, first.yr=1972) {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))      
  
  res <- new("MFCLSel")
  
  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj

  parversion <- as.numeric(splitter(par, "# The parest_flags"))[200]
  nseasons <- length(splitter(par, "# season_flags"))
  nregions <- length(splitter(par,"# region parameters"))
  nagecls  <- as.numeric(par[grep("# The number of age classes", par)+1])  
  nfish    <- grep("# tag flags", par) - grep("# fish flags", par)[1] -1
  nqgroups <- max(array(as.numeric(splitter(par, "# fish flags",1:nfish)),dim=c(100,nfish))[29,], na.rm=T) #max value of fishf lag 29
  
  dims1    <- list(age=as.character(seq(0,(nagecls/nseasons)-1)), year='all', unit='unique', season=c(as.character(1:nseasons)), area='all')
  dims2    <- list(age=as.character(seq(0,(nagecls/nseasons)-1)), year='all', unit=c(as.character(1:nfish)), season=c(as.character(1:nseasons)), area='all')
  dims3    <- list(age='all', year='all', unit=as.character(1:nfish), season="all", area="all")

  qdc      <- lapply(1:nfish, function(x) as.numeric(splitter(par, "# catchability deviation coefficients",x)))
  edc      <- lapply(1:nfish, function(x) as.numeric(splitter(par, "# effort deviation coefficients",      x)))
  cdc      <- lapply(1:nqgroups, function(x) as.numeric(splitter(par, "# The grouped_catch_dev_coffs",     x, inst=2)))

  sdc_end  <- cumsum(lapply(qdc, length))+seq(length(qdc))
  sdc_start<- c(1, sdc_end[-length(sdc_end)]+1)
  sdc_lines<- lapply(seq(nfish), function(x) seq(sdc_start[x], sdc_end[x]))
  sdc      <- lapply(sdc_lines, function(x) matrix(as.numeric(splitter(par, "# sel_dev_coffs", x)),ncol=nagecls))
    
  slot(res, 'availability_coffs') <- FLQuant(aperm(array(as.numeric(splitter(par,"# availability coffs")), 
                                                   dim=c(nseasons, nagecls/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims1)
  slot(res, 'fishery_sel')        <- FLQuant(aperm(array(as.numeric(splitter(par,"# fishery selectivity",1:nfish)), 
                                                   dim=c(nseasons, nagecls/nseasons,nfish,1,1)),c(2,4,3,1,5)), dimnames=dims2)
  slot(res, 'fishery_sel_age_comp')<-FLQuant(aperm(array(as.numeric(splitter(par,"# age-dependent component of fishery selectivity", 1:nfish)), 
                                                   dim=c(nseasons, nagecls/nseasons,nfish,1,1)),c(2,4,3,1,5)), dimnames=dims2)
  
  slot(res, 'av_q_coffs')  <- FLQuant(aperm(array(as.numeric(splitter(par,"# average catchability coefficients")),
                                                    dim=c(1,1,1,nfish,1)),c(2,3,4,1,5)), dimnames=dims3)
  
  slot(res, 'ini_q_coffs')  <- FLQuant(aperm(array(as.numeric(splitter(par,"# initial trend in catchability coefficients ")),
                                                   dim=c(nfish, 1,1,1,1)),c(2,3,1,4,5)), dimnames=dims3)
  
  slot(res, 'q0_miss')      <- FLQuant(aperm(array(as.numeric(splitter(par,"# q0_miss")),
                                                   dim=c(nfish, 1,1,1,1)),c(2,3,1,4,5)), dimnames=dims3)
  
  slot(res, 'sel_dev_corr') <- FLQuant(aperm(array(as.numeric(splitter(par,"# correlation in selectivity deviations")), 
                                                   dim=c(nfish, 1,1,1,1)),c(2,3,1,4,5)), dimnames=dims3)  
  
  slot(res, 'season_q_pars')<- matrix(as.numeric(splitter(par,"# seasonal_catchability_pars", 1:nfish)), ncol=12, byrow=T)
  
  slot(res, 'q_dev_coffs')      <- qdc
  slot(res, 'effort_dev_coffs') <- edc
  slot(res, 'catch_dev_coffs')  <- cdc
  slot(res, 'catch_dev_coffs_flag') <- as.numeric(par[grep("# The grouped_catch_dev_coffs flag", par)+1])
  # the big ones
  slot(res, 'sel_dev_coffs') <- matrix(as.numeric(splitter(par,"# selectivity deviation coefficients ",
                                            1:sum(unlist(lapply(qdc, length))))), ncol=nagecls, byrow=T)
  slot(res, 'sel_dev_coffs2')<- sdc
  
  getfishparms <- function(xx, version){
    switch(as.character(version),
           '1049' = matrix(as.numeric(splitter(xx,"# extra fishery parameters", 1:20)), ncol=nfish, byrow=T),
           '1050' = matrix(as.numeric(splitter(xx,"# extra fishery parameters", 1:50)), ncol=nfish, byrow=T))
  }
  slot(res, 'fish_params')   <- getfishparms(par, parversion)
    
  slot(res, 'range') <- c(min=0, max=nagecls/nseasons, plusgroup=NA, minyear=1, maxyear=1)
  
  return(res)
}




#' read.MFCLParBits
#'
#' Reads the remaining information from the par file and creates an MFCLParBits object
#'
#' @param parfile: A character string giving the name and path of the frq file to be read 
#' @param parobj:  A character string containing the par file. If parobj is NULL the function uses parfile to read in the par file
#'
#' @return An object of class MFCLParBit
#'
#' @examples
#' read.MFCLParBits("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLParBits <- function(parfile, parobj=NULL, first.yr=1972, version='new') {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))    
  
  res <- new("MFCLParBits")

  if(is.null(parobj)){
    par <- readLines(parfile)
    par <- par[nchar(par)>=1]                                          # remove blank lines
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  }
  if(!is.null(parobj))
    par <- parobj
  
  taglik <- unlist(strsplit(par[grep("# Likelihood component for tags",          par)], split="[[:blank:]]+"))
  mn_l_p <- unlist(strsplit(par[grep("# Average fish mort per fishing incident", par)], split="[[:blank:]]+"))
  av_f_i <- unlist(strsplit(par[grep("# Average fish mort per fishing incident", par)], split="[[:blank:]]+"))
  av_f_y <- unlist(strsplit(par[grep("# Average fish mort per year is",          par)], split="[[:blank:]]+"))
  av_f_a <- unlist(strsplit(par[grep("# Average fish mort per year by age class",par)+1], split="[[:blank:]]+"))
  
  slot(res, 'fm_level_devs') <- par[(grep("# fm_level_devs", par)+1):(grep("# movement map", par)-1)]
  slot(res, 'obj_fun')  <- as.numeric(splitter(par, "# Objective function value"))
  slot(res, 'n_pars')   <- as.numeric(splitter(par, "# The number of parameters"))
  slot(res, 'tag_lik')  <- as.numeric(taglik[length(taglik)])
  slot(res, 'max_grad') <- as.numeric(splitter(par, "# Maximum magnitude gradient value"))
  slot(res, 'mn_len_pen')        <- as.numeric(mn_l_p[length(mn_l_p)])
  slot(res, 'av_fish_mort_inst') <- as.numeric(av_f_i[length(av_f_i)])
  slot(res, 'av_fish_mort_year') <- as.numeric(av_f_y[length(av_f_y)])
  slot(res, 'av_fish_mort_age')  <- as.numeric(av_f_a[-1])
  
  if(version=='new'){
    slot(res, 'logistic_normal_params') <- par[(grep("# The logistic normal parameters", par)+1):(grep("# The logistic normal parameters", par)+6)]
    slot(res, 'lagrangian') <- par[(grep("# Lambdas for augmented Lagrangian", par)+1):(grep("# Reporting rate dev coffs", par)-1)]
  }
  return(res)
}











#' read.MFCLPar
#'
#' Reads information from the par file and creates an MFCLPar object
#'
#' @param parfile:  A character string giving the name and path of the frq file to be read 
#' @param first.yr: The first year of the input data time series (default values 1972)
#'
#' @return An object of class MFCLPar
#'
#' @examples
#' read.MFCLPar("C://R4MFCL//test_data//skj_ref_case//11.par")
#'
#' @export

read.MFCLPar <- function(parfile, first.yr=1972, version='new') {
  
  res <- new("MFCLPar")
  
  slotcopy <- function(from, to){
    for(slotname in slotNames(from)){
      slot(to, slotname) <- slot(from, slotname)
    }
    return(to)
  }
  par    <- readLines(parfile)
  par <- par[nchar(par)>=1]                                          # remove blank lines
  par <- par[-seq(1,length(par))[grepl("#", par) & nchar(par)<3]]   # remove single hashes with no text "# "  
  
  res <- slotcopy(read.MFCLBiol(parfile,  par, first.yr), res)
  res <- slotcopy(read.MFCLFlags(parfile, par, first.yr), res)
  res <- slotcopy(read.MFCLTagRep(parfile,par, first.yr), res)
  res <- slotcopy(read.MFCLRec(parfile,   par, first.yr), res)
  res <- slotcopy(read.MFCLRegion(parfile,par, first.yr, version=version), res)
  res <- slotcopy(read.MFCLSel(parfile,   par, first.yr), res)
  res <- slotcopy(read.MFCLParBits(parfile,par, version=version), res)
  
  slot(res, 'range') <- c(min=0, max=max(as.numeric(unlist(dimnames(fishery_sel(res))['age']))), 
                          plusgroup=NA, 
                          minyear=min(as.numeric(unlist(dimnames(rel_rec(res))['year']))), 
                          maxyear=max(as.numeric(unlist(dimnames(rel_rec(res))['year']))))
  
  return(res)
}











  