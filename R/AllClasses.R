#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


######################################################################
###
###  Frq file
###
#######################################################################


###### CLASSS MFCLFrqStats  (from .frq file)

validMFCLFrqStats <- function(object){
  #Everything is fine
  return(TRUE)
}

#' An S4 class : Essential dimensions and ranges of the frq file.
#'
#' @slot n_regions Number of regions
#' @slot n_fisheries Number of fisheries
#' @slot n_tag_groups Number of tag groups
#' @slot n_recs_yr Number of recruitment events each year
#' @slot rec_month Month in which recruitment occurs - 0 means first month of each period
#' @slot generic_diffusion Logical
#' @slot frq_age_len Logical. Is the frq file age or length based
#' @slot frq_version The version of the frq file
#' @slot region_size Size of each region relative to first region
#' @slot region_fish Description
#' @slot move_matrix Description
#' @slot data_flags Description
#' @slot season_flags Description
#' @slot n_move_yr Description
#' @slot move_weeks Description
#' @slot range Description

setClass("MFCLFrqStats",
         representation(
           n_regions    = "numeric",
           n_fisheries  = "numeric",
           n_tag_groups = "numeric",
           n_recs_yr    = "numeric",
           rec_month    = "numeric",
           generic_diffusion = "logical",
           frq_age_len  = "logical",
           frq_version  = "numeric",
           region_size  = "FLQuant",
           region_fish  = "FLQuant",
           move_matrix  = "matrix",
           data_flags   = "matrix",
           season_flags = "matrix",
           n_move_yr    = "numeric",
           move_weeks   = "numeric",
           range        = "numeric"
         ),
         prototype=prototype(
           n_regions    = numeric(),
           n_fisheries  = numeric(),
           n_tag_groups = numeric(),
           n_recs_yr    = numeric(),
           rec_month    = numeric(),
           generic_diffusion = logical(),
           frq_age_len  = logical(),
           frq_version  = numeric(),
           region_size  = FLQuant(),
           region_fish  = FLQuant(),
           move_matrix  = matrix(),
           data_flags   = matrix(),
           season_flags = matrix(),
           n_move_yr    = numeric(),
           move_weeks   = numeric(),
           range        =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLFrqStats
)
setValidity("MFCLFrqStats", validMFCLFrqStats)
remove(validMFCLFrqStats)
#createMFCLAccesors("MFCLFrqStats")
#'MFCLFrqStats
#'
#'Basic constructor for MFCLFrqStats class
#'@export
MFCLFrqStats <- function() {return(new("MFCLFrqStats"))}



###### CLASSS MFCLLenFreq  (from .frq file)

validMFCLLenFreq <- function(object){
  #Everything is fine
  return(TRUE)
}
#' An S4 class : Length frequency information from the frq file.
#'
#' @slot lf_range Range information of the length frequencies
#' @slot age_nage I don't know what this is but it's in the frq file
#' @slot freq Data frame of length frequency information.
#'
setClass("MFCLLenFreq",
         representation(
           lf_range    ="numeric",
           age_nage    ="numeric",
           freq        ="data.frame"
         ),
         prototype=prototype(
           lf_range     =unlist(list(Datasets=0,LFIntervals=NA,LFFirst=NA,LFWidth=NA,LFFactor=NA,WFIntervals=NA,WFFirst=NA,WFWidth=NA,WFFactor=NA)),
           age_nage     =unlist(list(age_nage=0,age_age1=NA)),
           freq         =data.frame()
         ),
         validity=validMFCLLenFreq
)
setValidity("MFCLLenFreq", validMFCLLenFreq)
remove(validMFCLLenFreq)
#createMFCLAccesors("MFCLLenFreq")
#'MFCLLenFreq
#'
#'Basic constructor for MFCLLenFreq class
#'@export
MFCLLenFreq <- function() {return(new("MFCLLenFreq"))}



###### CLASSS MFCLLenFreq2  (from .frq file)

validMFCLLenFreq2 <- function(object){
  #Everything is fine
  return(TRUE)
}
#' An S4 class : Size frequency information from the frq file.
#'
#' @slot lf_range Range information of the length frequencies
#' @slot age_nage I don't know what this is but it's in the frq file
#' @slot cateffpen Data frame of catch effort and penalty information
#' @slot lnfrq Data frame of length frequency information.
#' @slot wtfrq Data frame of weight frequency information.
#'
setClass("MFCLLenFreq2",
         representation(
           "MFCLLenFreq",
           cateffpen   ="data.frame",
           lnfrq       ="data.frame",
           wtfrq       ="data.frame"
         ),
         prototype=prototype(
           cateffpen   =data.frame(),
           lnfrq       =data.frame(),
           wtfrq       =data.frame()
         ),
         validity=validMFCLLenFreq2
)
setValidity("MFCLLenFreq2", validMFCLLenFreq2)
remove(validMFCLLenFreq2)
#createMFCLAccesors("MFCLLenFreq2")
#'MFCLLenFreq2
#'
#'Basic constructor for MFCLLenFreq2 class
#'@export
MFCLLenFreq2 <- function() {return(new("MFCLLenFreq2"))}





###### CLASSS MFCLFrq

validMFCLFrq <- function(object){
  
  # Everything is fine
  return(TRUE)
}
#' An S4 class : Representation of a frq input file for MFCL
#'
#' A class comprising an MFCLFrqStats object and an MFCLLenFrq object
#'
setClass("MFCLFrq",
         representation(
           "MFCLFrqStats",
           "MFCLLenFreq"
         ),
         validity=validMFCLFrq
)
setValidity("MFCLFrq", validMFCLFrq)
remove(validMFCLFrq)

#'MFCLFrq
#'
#'Basic constructor for MFCLFrq class
#'
#'@export

MFCLFrq <- function() {return(new("MFCLFrq"))}




###### CLASSS MFCLFrq2

validMFCLFrq2 <- function(object){
  # Everything is fine
  return(TRUE)
}
#' An S4 class : Representation of a frq input file for MFCL (based on the MFCLLenFreq2 object)
#'
#' A class comprising an MFCLFrqStats object and an MFCLLenFrq2 object
#'
setClass("MFCLFrq2",
         representation(
           "MFCLFrqStats",
           "MFCLLenFreq2"
         ),
         validity=validMFCLFrq2
)
setValidity("MFCLFrq2", validMFCLFrq2)
remove(validMFCLFrq2)

#'MFCLFrq2
#'
#'Basic constructor for MFCLFrq2 class
#'
#'@export

MFCLFrq2 <- function() {return(new("MFCLFrq2"))}





######################################################################
###
###  Par file
###
#######################################################################


###### CLASSS MFCLBiol  (from .par file)

setClass("MFCLBase",
         representation(
           dimensions        ="numeric",
           range             ="numeric"
         ),
         prototype=prototype(
           dimensions        =unlist(list(agecls=as.numeric(NA), years=NA, seasons=NA, regions=NA, fisheries=NA, taggrps=NA)),
           range             =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ))
#'MFCLBase
#'
#'Basic constructor for MFCLBase class
#'@export
MFCLBase <- function() {return(new("MFCLBase"))}         



validMFCLBiol <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLBiol",
         representation(
           "MFCLBase",
           m                 ="numeric",
           m_devs_age        ="FLQuant",
           log_m             ="FLQuant",
           mat               ="FLQuant",
           mat_at_length     ="numeric",
           growth            ="array",
           richards          ="numeric",
           growth_var_pars   ="array",
           n_mean_constraints="numeric",
           growth_devs_age   ="FLQuant",
           growth_curve_devs ="FLQuant",
           growth_devs_cohort="FLCohort",
           season_growth_pars="numeric",
           len_bias_pars     ="numeric",
           common_len_bias_pars  ="numeric",
           common_len_bias_coffs ="numeric"
         ),
         prototype=prototype(
           m                 =numeric(),
           m_devs_age        =FLQuant(),
           log_m             =FLQuant(),
           mat               =FLQuant(),
           mat_at_length     =numeric(),
           growth            =array(),
           richards          =numeric(),
           growth_var_pars   =array(),
           n_mean_constraints=numeric(),
           growth_devs_age   =FLQuant(),
           growth_curve_devs =FLQuant(),
           growth_devs_cohort=FLCohort(),
           season_growth_pars=numeric(),
           len_bias_pars     =numeric(),
           common_len_bias_pars  =numeric(),
           common_len_bias_coffs =numeric()
         ),
         validity=validMFCLBiol
)
setValidity("MFCLBiol", validMFCLBiol)
remove(validMFCLBiol)
#'MFCLBiol
#'
#'Basic constructor for MFCLBiol class
#'@export
MFCLBiol <- function() {return(new("MFCLBiol"))}




###### CLASSS MFCLFlags

validMFCLFlags <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLFlags",
         representation(
           flags   = "data.frame",
           unused  ="list"
         ),
         prototype=prototype(
           flags   = data.frame(),
           unused  =list()
         ),
         validity=validMFCLFlags
)
setValidity("MFCLFlags", validMFCLFlags)
remove(validMFCLFlags)
#'MFCLFlags
#'
#'Basic consstructor for MFCLFlags class
#'@export
MFCLFlags <- function() {return(new("MFCLFlags"))}




###### CLASSS MFCLTagRep

validMFCLTagRep <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLTagRep",
         representation(
           tag_shed_rate      = "numeric",
           tag_fish_rep_rate  = "array",
           tag_fish_rep_grp   = "array",
           tag_fish_rep_flags = "array",
           tag_fish_rep_target= "array",
           tag_fish_rep_pen   = "array",
           rep_rate_dev_coffs = "list",
           range              = "numeric"
         ),
         prototype=prototype(
           tag_shed_rate      = numeric(),
           tag_fish_rep_rate  = array(),
           tag_fish_rep_grp   = array(),
           tag_fish_rep_flags = array(),
           tag_fish_rep_target= array(),
           tag_fish_rep_pen   = array(),
           rep_rate_dev_coffs = list(),
           range=unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLTagRep
)
setValidity("MFCLTagRep", validMFCLTagRep)
remove(validMFCLTagRep)
#'MFCLTagRep
#'
#'Basic constructor for MFCLTagRep class
#'@export
MFCLTagRep <- function() {return(new("MFCLTagRep"))}




###### CLASSS MFCLRec

validMFCLRec <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLRec",
         representation(
           rec_init_pop_diff   ="numeric",
           rec_times           ="numeric",
           rel_rec             ="FLQuant",
           tot_pop             ="numeric",
           tot_pop_implicit    ="numeric",
           rel_ini_pop         ="array",
           rec_standard_dim    ="numeric",
           rec_standard        ="FLQuant",
           rec_orthogonal      ="FLQuant",
           orth_coffs          ="numeric",
           new_orth_coffs      ="numeric",
           range               ="numeric"
         ),
         prototype=prototype(
           rec_init_pop_diff   =numeric(),
           rec_times           =numeric(),
           rel_rec             =FLQuant(),
           tot_pop             =numeric(),
           tot_pop_implicit    =numeric(),
           rel_ini_pop         =array(),
           rec_standard_dim    =numeric(),
           rec_standard        =FLQuant(),
           rec_orthogonal      =FLQuant(),
           orth_coffs          =numeric(),
           new_orth_coffs      =numeric(),
           range               =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLRec
)
setValidity("MFCLRec", validMFCLRec)
remove(validMFCLRec)
#'MFCLRec
#'
#'Basic constructor for MFCLRec class
#'@export
MFCLRec <- function() {return(new("MFCLRec"))}



###### CLASSS MFCLRegion

validMFCLRegion <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLRegion",
         representation(
           control_flags         = "matrix",
           move_map              = "numeric",
           diff_coffs            = "matrix",
           xdiff_coffs            = "matrix",
           y1diff_coffs          = "matrix",
           y2diff_coffs          = "matrix",
           zdiff_coffs          = "matrix",
           diff_coffs_mat        = "matrix",
           diff_coffs_age_ssn        = "array",
           diff_coffs_age_period = "array",
           diff_coffs_age        = "array",
           diff_coffs_nl         = "array",
           diff_coffs_priors     = "array",
           diff_coffs_age_priors = "array",
           diff_coffs_nl_priors  = "array",
           region_rec_var        = "FLQuant",
           region_pars           = "matrix",
           range                 = "numeric"
         ),
         prototype=prototype(
           control_flags         = matrix(),
           move_map              = numeric(),
           diff_coffs            = matrix(),
           xdiff_coffs            = matrix(),
           y1diff_coffs          = matrix(),
           y2diff_coffs          = matrix(),
           zdiff_coffs           = matrix(),
           diff_coffs_mat        = matrix(),
           diff_coffs_age_ssn    = array(),
           diff_coffs_age_period = array(),
           diff_coffs_age        = array(),
           diff_coffs_nl         = array(),
           diff_coffs_priors     = array(),
           diff_coffs_age_priors = array(),
           diff_coffs_nl_priors  = array(),
           region_rec_var        = FLQuant(),
           region_pars           = matrix(),
           range=unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLRegion
)
setValidity("MFCLRegion", validMFCLRegion)
remove(validMFCLRegion)
#'MFCLRegion
#'
#'Basic constructor for MFCLRegion class
#'@export
MFCLRegion <- function() {return(new("MFCLRegion"))}



###### CLASSS MFCLSel

validMFCLSel <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLSel",
         representation(
           availability_coffs   = "FLQuant",
           fishery_sel          = "FLQuant",
           fishery_sel_age_comp = "FLQuant",
           av_q_coffs           = "FLQuant",
           ini_q_coffs          = "FLQuant",
           q0_miss              = "FLQuant",
           q_dev_coffs          = "list",
           effort_dev_coffs     = "list",
           catch_dev_coffs      = "list",
           catch_dev_coffs_flag = "numeric",
           sel_dev_corr         = "FLQuant",
           sel_dev_coffs        = "matrix",
           sel_dev_coffs2       = "list",
           season_q_pars        = "matrix",
           fish_params          = "matrix",
           spp_params           = "matrix",
           range                = "numeric"
         ),
         prototype=prototype(
           availability_coffs   = FLQuant(),
           fishery_sel          = FLQuant(),
           fishery_sel_age_comp = FLQuant(),
           av_q_coffs           = FLQuant(),
           ini_q_coffs          = FLQuant(),
           q0_miss              = FLQuant(),
           q_dev_coffs          = list(),
           effort_dev_coffs     = list(),
           catch_dev_coffs      = list(),
           catch_dev_coffs_flag = numeric(),
           sel_dev_corr         = FLQuant(),
           sel_dev_coffs        = matrix(),
           sel_dev_coffs2       = list(),
           season_q_pars        = matrix(),
           fish_params          = matrix(),
           spp_params           = matrix(),
           range                = unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLSel
)
setValidity("MFCLSel", validMFCLSel)
remove(validMFCLSel)
#'MFCLSel
#'
#'Basic constructor for MFCLSel class
#'@export
MFCLSel <- function() {return(new("MFCLSel"))}


###### CLASSS MFCLParBits

validMFCLParBits <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLParBits",
         representation(
           fm_level_devs        = "character",
           fm_level_regression_pars = "matrix",
           obj_fun              = "numeric",
           n_pars               = "numeric",
           tag_lik              = "numeric",
           mn_len_pen           = "numeric",
           max_grad             = "numeric",
           av_fish_mort_inst    = "numeric",
           av_fish_mort_year    = "numeric",
           av_fish_mort_age     = "numeric",
           logistic_normal_params = "character",
           lagrangian             = "character",
           kludged_eq_coffs     = "array",
           kludged_eq_level_coffs = "numeric",
           range                = "numeric",
           historic_flags       = "character"
         ),
         prototype=prototype(
           fm_level_devs        = character(),
           fm_level_regression_pars = matrix(),
           obj_fun              = numeric(),
           n_pars               = numeric(),
           tag_lik              = numeric(),
           mn_len_pen           = numeric(),
           max_grad             = numeric(),
           av_fish_mort_inst    = numeric(),
           av_fish_mort_year    = numeric(),
           av_fish_mort_age     = numeric(),
           logistic_normal_params = character(),
           lagrangian             = character(),
           kludged_eq_coffs     = array(),
           kludged_eq_level_coffs = numeric(),
           range                = unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1)),
           historic_flags       = character()
         ),
         validity=validMFCLParBits
)
setValidity("MFCLParBits", validMFCLParBits)
remove(validMFCLParBits)
#'MFCLParBits
#'
#'Basic constructor for MFCLParBits class
#'@export
MFCLParBits <- function() {return(new("MFCLParBits"))}


###### CLASSS MFCLIniBits

validMFCLIniBits <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLIniBits",
         representation(
           ini_version         ="numeric",
           region_flags        ="matrix",
           age_pars            ="matrix",
           rec_dist            ="numeric",
           lw_params           ="numeric",
           sv                  ="numeric",
           sd_length_at_age    ="numeric",
           sd_length_dep       ='numeric'
         ),
         prototype=prototype(
           ini_version		       =numeric(),
           region_flags        =matrix(),
           age_pars            =matrix(),
           rec_dist            =numeric(),
           lw_params           =numeric(),
           sv                  =numeric(),
           sd_length_at_age    =numeric(),
           sd_length_dep       =numeric()
         ),
         validity=validMFCLIniBits
)
setValidity("MFCLIniBits", validMFCLIniBits)
remove(validMFCLIniBits)
#'MFCLIniBits
#'
#'Basic constructor for MFCLIniBits class
#'@export
MFCLIniBits <- function() {return(new("MFCLIniBits"))}


###### CLASSS MFCLIni

validMFCLIni <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLIni",
         representation(
           "MFCLBase",
           "MFCLTagRep",
           "MFCLBiol",
           "MFCLRegion",
           "MFCLIniBits"
         ),
         prototype=prototype(
         ),
         validity=validMFCLIni
)
setValidity("MFCLIni", validMFCLIni)
remove(validMFCLIni)
#'MFCLIni
#'
#'Basic constructor for MFCLIni class
#'@export
MFCLIni <- function() {return(new("MFCLIni"))}






###### CLASSS MFCLPar

validMFCLPar <- function(object){
  
  # Everything is fine
  return(TRUE)
}

#' An S4 class : Undocumented.

setClass("MFCLPar",
         representation(
           "MFCLBiol",
           "MFCLFlags",
           "MFCLTagRep",
           "MFCLRec",
           "MFCLRegion",
           "MFCLSel",
           "MFCLParBits",
           range="numeric"
         ),
         prototype=prototype(
         ),
         validity=validMFCLPar
)
setValidity("MFCLPar", validMFCLPar)
remove(validMFCLPar)
#'MFCLPar
#'
#'Basic constructor for MFCLPar class
#'@export
MFCLPar <- function() {return(new("MFCLPar"))}






###### CLASSS MFCLTagProj
validMFCLTagProj <- function(object){
  #Everything is fine
  return(TRUE)
}

#' An S4 class : Undocumented.

setClass("MFCLTagProj",
         representation(
           release_groups_proj = "numeric",
           releases_proj       = 'data.frame',
           rep_rate_proj       = "array",
           range               = "numeric"
         ),
         prototype=prototype(
           release_groups_proj = numeric(),
           releases_proj       = data.frame(region=NULL, year=NULL, month=NULL, fishery=NULL, n=NULL),
           rep_rate_proj       = array(),
           range               = unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLTagProj
)
setValidity("MFCLTagProj", validMFCLTagProj)
remove(validMFCLTagProj)
#'MFCLTagProj
#'
#'Basic constructor for MFCLTagProj class
#'@export
MFCLTagProj <- function(ptd=NULL, reprate=NULL) {
  res <- new("MFCLTagProj")
  if(!is.null(ptd)){
    releases_proj(res) <- ptd
    release_groups_proj(res) <- nrow(ptd)
    if(!is.null(reprate))
      slot(res, 'rep_rate_proj') <- reprate
    range(res)[c('min','max')] <- range(ptd$n)
    range(res)[c('minyear','maxyear')] <- range(ptd$year)
  }
  return(res)}





###### CLASSS MFCLTagProj
validMFCLTag <- function(object){
  #Everything is fine
  return(TRUE)
}

#' An S4 class : Undocumented.

setClass("MFCLTag",
         representation(
           "MFCLTagProj",
           release_groups  = "numeric",
           release_lengths = "numeric",
           recoveries      = "numeric",
           releases        = "data.frame",
           recaptures      = "data.frame",
           range           = "numeric"
         ),
         prototype=prototype(
           release_groups  = numeric(),
           release_groups_proj = numeric(),
           release_lengths  = numeric(),
           recoveries      = numeric(),
           releases        = data.frame(rel.group=NULL, region=NULL, year=NULL, month=NULL, program=NULL, length=NULL, lendist=NULL),
           releases_proj   = data.frame(region=NULL, year=NULL, month=NULL, fishery=NULL, n=NULL),
           recaptures      = data.frame(rel.group=NULL, region=NULL, year=NULL, month=NULL, program=NULL, rel.length=NULL, recap.fishery=NULL, recap.year=NULL, recap.month=NULL, recap.number=NULL),
           range           = unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLTag
)
setValidity("MFCLTag", validMFCLTag)
remove(validMFCLTag)
#'MFCLTag
#'
#'Basic constructor for MFCLTag class
#'@export
MFCLTag <- function() {return(new("MFCLTag"))}







###### CLASSS MFCLRep

validMFCLRep <- function(object){
  #Everything is fine
  return(TRUE)
}

setClass("MFCLRep",
         representation(
           "MFCLBase",
           fishery_realizations="FLQuant",
           mean_laa            ="FLQuant",
           mean_waa            ="FLQuant",
           sd_laa              ="FLQuant",
           m_at_age            ="FLQuant",
           sel                 ="FLQuant",
           q_fishery           ='FLQuant',
           q_effdev            ='FLQuant',
           fm                  ='FLQuant',
           fm_aggregated       ="FLQuant",
           popN                 ='FLQuant',
           rec_region          ='FLQuant',
           totalBiomass        ='FLQuant',
           totalBiomass_nofish ='FLQuant',
           adultBiomass        ='FLQuant',
           adultBiomass_nofish ='FLQuant',
           vulnBiomass         ='FLQuant',
           srr                 ='FLPar',
           eq_ssb                 ='FLQuant',
           eq_ssb_obs             ='FLQuant',
           eq_rec                 ='FLQuant',
           eq_rec_obs             ='FLQuant',
           catch_obs           ='FLQuant',
           catch_pred          ='FLQuant',
           cpue_obs            ='FLQuant',
           cpue_pred           ='FLQuant',
           eq_biomass          ='FLQuant',
           eq_yield            ='FLQuant',
           MSY                 ="numeric",
           FMSY                ='numeric',
           BMSY                ='numeric',
           ABBMSY_ts               ='FLQuant',
           FFMSY_ts               ='FLQuant',
           ABBMSY              ='numeric',
           TBBMSY              ='numeric',
           Fmult               ='numeric',
           AggregateF          ='FLQuant'
         ),
         prototype=prototype(
           fishery_realizations=FLQuant(),
           mean_laa            =FLQuant(),
           mean_waa            =FLQuant(),
           sd_laa              =FLQuant(),
           m_at_age            =FLQuant(),
           sel                 =FLQuant(),
           q_fishery           =FLQuant(),
           q_effdev            =FLQuant(),
           fm                  =FLQuant(),
           fm_aggregated       =FLQuant(),
           popN                =FLQuant(),
           rec_region          =FLQuant(),
           totalBiomass        =FLQuant(),
           totalBiomass_nofish =FLQuant(),
           adultBiomass        =FLQuant(),
           adultBiomass_nofish =FLQuant(),
           vulnBiomass         =FLQuant(),
           srr                 =FLPar(),
           ssb                 =FLQuant(),
           ssb_obs             =FLQuant(),
           rec                 =FLQuant(),
           rec_obs             =FLQuant(),
           catch_obs           =FLQuant(),
           catch_pred          =FLQuant(),
           cpue_obs            =FLQuant(),
           cpue_pred           =FLQuant(),
           eq_biomass          =FLQuant(),
           eq_yield            =FLQuant(),
           MSY                 =numeric(),
           FMSY                =numeric(),
           BMSY                =numeric(),
           BBMSY               =FLQuant(),
           FFMSY               =FLQuant(),
           Fmult               =numeric(),
           AggregateF          =FLQuant()
         ),
         validity=validMFCLRep
)
setValidity("MFCLRep", validMFCLRep)
remove(validMFCLRep)
#'MFCLRep
#'
#'Basic constructor for MFCLRep class
#'@export
MFCLRep <- function() {return(new("MFCLRep"))}




###### CLASSS projControl

validMFCLprojControl <- function(object){
  #Everything is fine
  return(TRUE)
}

#' An S4 class : Undocumented.

setClass("MFCLprojControl",
         representation(
           nyears              ="numeric",
           nsims               ="numeric",
           avyrs               ="character",
           fprojyr             ="numeric",
           controls            ="data.frame"
         ),
         prototype=prototype(
           nyears              =numeric(),
           nsims               =numeric(),
           avyrs               =character(),
           fprojyr             =numeric(),
           controls            =data.frame(name=NULL, region=NULL, caeff=NULL, scaler=NULL, ess_length=NULL, ess_weight=NULL) # RDS 24/02/2022
         ),                                                                                                                   # really hope this doesn't break everything!
         validity=validMFCLprojControl
)
setValidity("MFCLprojControl", validMFCLprojControl)
remove(validMFCLprojControl)
#'MFCLprojControl
#'
#'Basic constructor for MFCLprojControl class
#'@export
MFCLprojControl <- function(nyears=as.numeric(NULL), nsims=as.numeric(NULL), avyrs='', fprojyr=as.numeric(NULL), controls=data.frame(name=NULL, region=NULL, caeff=NULL, scaler=NULL, ess=NULL)) {
  
  res <- new("MFCLprojControl")
  slot(res, 'nyears') <- nyears
  slot(res, 'nsims')  <- nsims
  slot(res, 'avyrs')  <- avyrs
  slot(res, 'fprojyr') <- fprojyr
  slot(res, 'controls')  <- controls
  
  return(res)
}
#pp <- MFCLprojControl(nyears=3, nsims=200, avyrs='2012', caeff=1, scaler=1)





###### CLASSS MFCLCatch

validMFCLCatch <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLCatch",
         representation(
           total_catch         ="FLQuant",
           fishery_catch       ="FLQuant",
           range               ="numeric"
         ),
         prototype=prototype(
           total_catch         =FLQuant(),
           fishery_catch       =FLQuant(),
           range               =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLCatch
)
setValidity("MFCLCatch", validMFCLCatch)
remove(validMFCLCatch)
#'MFCLCatch
#'
#'Basic constructor for MFCLCatch class
#'@export
MFCLCatch <- function() {return(new("MFCLCatch"))}




########################
##
## MFCL diagnostics output classes
##
########################


###### CLASSS MFCLLenFit

validMFCLLenFit <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLLenFit",
         representation(
           laa                 ="FLQuant",
           lenfits             ="data.frame",
           lenagefits          ="data.frame",
           range               ="numeric"
         ),
         prototype=prototype(
           laa                 =FLQuant(),
           lenfits             =data.frame(fishery=NULL, year=NULL, month=NULL, length=NULL, obs=NULL, pred=NULL),
           lenagefits          =data.frame(fishery=NULL, year=NULL, month=NULL, length=NULL, age=NULL, pred=NULL), 
           range               =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLLenFit
)
setValidity("MFCLLenFit", validMFCLLenFit)
remove(validMFCLLenFit)
#'MFCLLenFit
#'
#'Basic constructor for MFCLLenFit class
#'@export
MFCLLenFit <- function() {return(new("MFCLLenFit"))}



###### CLASSS MFCLLikelihood

validMFCLLikelihood <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLLikelihood",
         representation(
           bh_steep_contrib    ="numeric",
           effort_dev_penalty  ="numeric",
           q_dev_pen_fish      ="numeric",
           q_dev_pen_fish_grp  ="numeric",
           total_length_fish   ="numeric",
           length_fish         ="list",
           total_weight_fish   ="numeric",
           weight_fish         ="list",
           total_catch_fish    ="numeric",
           catch_fish          ="list",
           tag_rel_fish        ="list",
           survey_index        ='numeric',
           dimensions          ="numeric"
         ),
         prototype=prototype(
           bh_steep_contrib    =numeric(),
           effort_dev_penalty  =numeric(),
           q_dev_pen_fish      =numeric(),
           q_dev_pen_fish_grp  =numeric(),
           total_length_fish   =numeric(),
           length_fish         =list(),
           total_weight_fish   =numeric(),
           weight_fish         =list(),
           total_catch_fish    =numeric(),
           catch_fish          =list(),
           tag_rel_fish        =list(), # or maybe a data.frame
           survey_index        =numeric(),
           dimensions          =unlist(list(agecls=as.numeric(NA), years=NA, seasons=NA, regions=NA, fisheries=NA, taggrps=NA))
         ),
         validity=validMFCLLikelihood
)
setValidity("MFCLLikelihood", validMFCLLikelihood)
remove(validMFCLLikelihood)
#'MFCLLikelihood
#'
#'Basic constructor for MFCLLikelihood class
#'@export
MFCLLikelihood <- function() {return(new("MFCLLikelihood"))}





########################
##
## MFCL pseudo data objects
##
########################


###### CLASSS MFCLPseudo

validMFCLPseudo <- function(object){
  #Everything is fine
  return(TRUE)
}

#' An S4 class : Undocumented.

setClass("MFCLPseudo",
         representation(
           "MFCLLenFreq",
           catcheff            ="data.frame",
           l_frq               ="data.frame",
           w_frq               ="data.frame",
           #           freq                ="data.frame",
           range               ="numeric"
         ),
         prototype=prototype(
           catcheff            =data.frame(year=NULL, month=NULL, fishery=NULL, iter=NULL, data=NULL),
           l_frq               =data.frame(year=NULL, month=NULL, fishery=NULL, length=NULL, iter=NULL, freq=NULL),
           w_frq               =data.frame(year=NULL, month=NULL, fishery=NULL, weight=NULL, iter=NULL, freq=NULL),
           #           freq                =data.frame(),
           range               =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
         ),
         validity=validMFCLPseudo
)
setValidity("MFCLPseudo", validMFCLPseudo)
remove(validMFCLPseudo)
#'MFCLPseudo
#'
#'Basic constructor for MFCLPseudo class
#'@export
MFCLPseudo <- function(catcheff =data.frame(year=NULL, month=NULL, fishery=NULL, iter=NULL, data=NULL),
                       l_frq    =data.frame(year=NULL, month=NULL, fishery=NULL, length=NULL, iter=NULL, freq=NULL),
                       w_frq    =data.frame(year=NULL, month=NULL, fishery=NULL, weight=NULL, iter=NULL, freq=NULL)) {
  res <- new("MFCLPseudo")
  slot(res, "catcheff") <- catcheff
  slot(res, "l_frq") <- l_frq
  slot(res, "w_frq") <- w_frq
  slot(res, "range") <- unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
  
  return(res)
}


validMFCLPseudoControl <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLPseudoControl",
         representation(
           "MFCLprojControl",
           catch_sd            ='numeric',
           effort_sd           ='numeric',
           tag_controls        ='data.frame',
           tag_fish_rep_rate   ='numeric',
           random_seeds        ='numeric'
         ),
         prototype=prototype(
           catch_sd            =numeric(),
           effort_sd           =numeric(),
           tag_controls        =data.frame(region=NULL, year=NULL, month=NULL, fishery=NULL, n=NULL),
           tag_fish_rep_rate   =numeric(),
           random_seeds        =unlist(list(catch=16001, effort=17001, length=18001, weight=19001, tag=20001))
         ),
         validity=validMFCLPseudoControl
)
setValidity("MFCLPseudoControl", validMFCLPseudoControl)
remove(validMFCLPseudoControl)
#'MFCLPseudoControl
#'
#'Basic constructor for MFCLPseudoControl class
#'@export
MFCLPseudoControl <- function(catch_sd=20, effort_sd=20, tag_fish_rep_rate=0.9, catch_seed=16001, effort_seed=17001, length_seed=18001, weight_seed=19001, tag_seed=20001) {
  
  pc <- new("MFCLPseudoControl")
  
  slot(pc, 'catch_sd') <- catch_sd
  slot(pc, 'effort_sd') <- effort_sd
  slot(pc, 'tag_fish_rep_rate') <- tag_fish_rep_rate
  slot(pc, 'random_seeds') <- unlist(list(catch=catch_seed, effort=effort_seed, length=length_seed, weight=weight_seed, tag=tag_seed))
  
  return(pc)
}



validMFCLEMControl <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLEMControl",
         representation(
           doitall             ='function',
           tag_fish_rep_rate   ='numeric'
         ),
         prototype=prototype(
           tag_fish_rep_rate   =numeric()
         ),
         validity=validMFCLEMControl
)
setValidity("MFCLEMControl", validMFCLEMControl)
remove(validMFCLEMControl)
#'MFCLEMControl
#'
#'Basic constructor for MFCLEMControl class
#'@export
MFCLEMControl <- function(doitall=function(){}, tag_fish_rep_rate=0.9,...) {
  
  emc <- new("MFCLEMControl")
  
  slot(emc, 'doitall') <- doitall
  slot(emc, 'tag_fish_rep_rate') <- tag_fish_rep_rate
  
  return(emc)
}



validMFCLMSEControl <- function(object){
  #Everything is fine - is it?
  return(TRUE)
}

#' An S4 class : Controls the MSE simulations
#'
#' @slot itn Undocumented.
#' @slot hcr The name of the HCR function as a character string. This function must exist and have the arguments: sbsbf0 and params. The params argument is taken from the params slot when evaluated.
#' @slot hcr_params A numeric vector of parameters for the HCR function.
#' @slot ess_scalar Undocumented.
#' @slot effort_creep Undocumented.
#' @slot effort_creep_fish Undocumented.
#' @slot avyrs Undocumented.
#' @slot catch_sd Undocumented.
#' @slot controls Undocumented.
#' @slot doitall Undocumented.
#' @slot effort_sd Undocumented.
#' @slot fprojyr Undocumented.
#' @slot nsims Undocumented.
#' @slot nyears Undocumented.
#' @slot random_seeds Undocumented.
#' @slot tag_controls Undocumented.
#' @slot tag_fish_rep_rate Undocumented.

setClass("MFCLMSEControl",
         representation(
           "MFCLPseudoControl",
           "MFCLEMControl",
           itn             = 'numeric',
           hcr             = 'character',
           hcr_params      = 'numeric',
           ess_scalar      = 'numeric',
           effort_creep    = 'numeric',
           effort_creep_fish = 'numeric'
         ),
         prototype=prototype(
           itn             = numeric(),
           hcr             = "hcr_threshold",
           hcr_params      = c(sbsbf0_min = 0.2, sbsbf0_max = 0.5, out_min = 0.2, out_max = 1.0),
           ess_scalar      = 1.0,
           effort_creep    = 0.0,
           effort_creep_fish = numeric()
         ),
         validity=validMFCLMSEControl
)
setValidity("MFCLMSEControl", validMFCLMSEControl)
remove(validMFCLMSEControl)


#' MFCLMSEControl
#'
#' Basic constructor for MFCLMSEControl class
#' @export
MFCLMSEControl <- function(hcr="hcr_threshold", hcr_params=c(sbsbf0_min = 0.2, sbsbf0_max = 0.5, out_min = 0.2, out_max = 1.0), itn=1, ess_scalar=1, ...) {
  
  msec <- new("MFCLMSEControl")
  
  slot(msec, 'hcr') <- hcr
  slot(msec, 'hcr_params') <- hcr_params
  slot(msec, 'itn') <- itn
  slot(msec, 'ess_scalar') <- ess_scalar
  
  return(msec)
}



###### CLASSS MFCLEquilibrium  (from .rep file)

validMFCLEquilibrium <- function(object){
  #Everything is fine
  return(TRUE)
}
#' An S4 class : Size frequency information from the frq file.
#'
#' @slot Eq_calcs Description
#' @slot YPR Description
#'
setClass("MFCLEquilibrium",
         representation(
           Eq_calcs   ="data.frame",
           YPR        ="data.frame"
         ),
         prototype=prototype(
           Eq_calcs   =data.frame(),
           YPR        =data.frame()
         ),
         validity=validMFCLEquilibrium
)
setValidity("MFCLEquilibrium", validMFCLEquilibrium)
remove(validMFCLEquilibrium)

#'MFCLEquilibrium
#'
#'Basic constructor for MFCLEquilibrium class
#'@export
MFCLEquilibrium <- function() {return(new("MFCLEquilibrium"))}




