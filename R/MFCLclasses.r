


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
#' An S4 class : Essesntial dimensions and ranges of the frq file.
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
#' @slot region_fish
#' @slot move_matrix
#' @slot data_flags
#' @slot season_flags
#' @slot n_move_yr
#' @slot move_weeks
#' @slot range
#' 
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




######################################################################
###
###  Par file 
###
#######################################################################


###### CLASSS MFCLBiol  (from .par file)

validMFCLBiol <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLBiol",
         representation(
           m                 ="FLQuant",
           mat               ="FLQuant",
           growth            ="FLPar",
           cohort_growth_devs="FLQuant",
           season_growth_pars="numeric",
           range             ="numeric"
           ),
         prototype=prototype(
           m                 =FLQuant(),
           mat               =FLQuant(),
           growth            =FLPar(),
           cohort_growth_devs=FLQuant(),
           season_growth_pars=numeric(),
           range             =unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
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
           flags   = "data.frame"
           ),
         prototype=prototype(
           flags   = data.frame()
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
           tag_fish_rep_rate  = "FLQuant",
           tag_fish_rep_grp   = "FLQuant",
           tag_fish_rep_flags = "FLQuant",
           tag_fish_rep_target= "FLQuant",
           tag_fish_rep_pen   = "FLQuant",
           rep_rate_dev_coffs = "FLQuant",
           range              = "numeric"
         ),
         prototype=prototype(
           tag_fish_rep_rate  = FLQuant(),
           tag_fish_rep_grp   = FLQuant(),
           tag_fish_rep_flags = FLQuant(),
           tag_fish_rep_target= FLQuant(),
           tag_fish_rep_pen   = FLQuant(),
           rep_rate_dev_coffs = FLQuant(),
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
MFCLTagRep <- function() {return(new("MFCLTagReg"))}


###### CLASSS MFCLRec

validMFCLRec <- function(object){
  #Everything is fine
  return(TRUE)
}
setClass("MFCLRec",
         representation(
           rec_init   ="numeric",
           rec_times  ="numeric",
           rel_rec    ="FLQuant",
           range      ="numeric"
         ),
         prototype=prototype(
           rec_init   =numeric(),
           rec_times  =numeric(),
           rel_rec    =FLQuant(),
           range=unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
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
           control_flags   = "matrix",
           diff_coffs      = "FLQuant",
           diff_coffs_age  = "FLQuant",
           diff_coffs_nl   = "FLQuant",
           reg_rec_var     = "FLQuant",
           reg_pars        = "matrix",
           range           = "numeric"
         ),
         prototype=prototype(
           control_flags   = matrix(),
           diff_coffs      = FLQuant(),
           diff_coffs_age  = FLQuant(),
           diff_coffs_nl   = FLQuant(),
           reg_rec_var     = FLQuant(),
           reg_pars        = matrix(),
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
           av_q_coffs           = "FLQuant",
           ini_q_coffs          = "FLQuant",
           q_dev_coffs          = "FLQuant",
           sel_dev_coffs        = "FLQuant",
           sel_dev_coffs2       = "FLQuant",
           season_q_pars        = "matrix",
           
           range                = "numeric"
         ),
         prototype=prototype(
           availability_coffs   = FLQuant(),
           fishery_sel          = FLQuant(),
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





###### CLASSS MFCLPar

validMFCLPar <- function(object){

   # Everything is fine
   return(TRUE)
}



setClass("MFCLPar",
         representation(
           "MFCLBiol",
           "MFCLFlags",
           "MFCLTagRep",
           "MFCLRec",
           range="numeric"
           ),
         prototype=prototype(
           range=unlist(list(min=NA,max=NA,plusgroup=NA,minyear=1,maxyear=1))
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

           
 








