#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' read.MFCLIni
#'
#' Reads information from the ini file and creates an MFCLIni object
#'
#' @param inifile:  A character string giving the name and path of the ini file to be read
#'
#'
#' @return An object of class MFCLIni
#'
#' @examples
#' read.MFCLIni("C://R4MFCL//test_data//skj_ref_case//skj.ini")
#' read.MFCLIni("/home/robertsc/skj/HCR/run0/skj.ini")
#'
#' @export

read.MFCLIni <- function(inifile, nseasons=4) {

  trim.leading  <- function(x) sub("^\\s+", "", x)
  trim.trailing <- function(x) sub("\\s+$", "", x)
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+"))

  slotcopy <- function(from, to){
    for(slotname in slotNames(from)){
      slot(to, slotname) <- slot(from, slotname)
    }
    return(to)
  }

  res <- new("MFCLIni")

  par <- readLines(inifile)
  par <- par[nchar(par)>=1]                                          # remove blank lines
  if(any(grepl("# ", par) & nchar(par)<3))
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "

  ## Read in the version number because this specifies if it is single species, mulitple species or includes maturity at length
  slot(res,'ini_version') <- as.numeric(splitter(par, "# ini version number"))
  if ( slot(res,'ini_version') == 1002) stop("Sorry FLR4MFCL is not compatable with multispecies models at the moment")
  nages    <- as.numeric(splitter(par, "# number of age classes"))
  nagestest <- length(splitter(par, "# maturity at age"))
  if (nages != nagestest) warning("The number of age classes and length of maturity at age don't match up")
  nregions <- length(splitter(par, "# recruitment distribution by region"))

  dims_age        <- dimnames(FLQuant(quant="age"))
  dims_age$age    <- as.character(0:((nages/nseasons)-1))
  dims_age$season <- as.character(1:nseasons)

  if(any(grep("# tag fish rep", par)))
    res <- slotcopy(read.MFCLTagRep(parfile, par), res)

  slot(res, 'm')          <- as.numeric(splitter(par, '# natural mortality'))
  slot(res, "mat")      <- FLQuant(aperm(array(as.numeric(splitter(par, "# maturity at age")),
                                               dim=c(nseasons,nages/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims_age)
  if ( slot(res,'ini_version') > 1002) slot(res, "mat_at_length") <- as.numeric(splitter(par,"# maturity at length"))
  slot(res, 'move_map')   <- as.numeric(splitter(par, '# movement map'))
#  slot(res, 'diff_coffs') <- as.array(matrix(as.numeric(splitter(par, '# diffusion coffs', 1:(max(c(nregions-1,1))))),nrow=max(c(nregions-1,1)), byrow=T))
#  slot(res, 'diff_coffs') <- as.array(matrix(as.numeric(splitter(par, '# diffusion coffs', 1:(max(c(length(slot(res,'move_map'))),1)))),nrow=max(c(nregions-1,1)), byrow=T))
  slot(res, 'diff_coffs') <- as.array(matrix(as.numeric(splitter(par, '# diffusion coffs', 1:(max(c(length(slot(res,'move_map'))),1)))),
                                             nrow=length(slot(res,"move_map")), byrow=T))

  if ( slot(res,'ini_version') > 1001) slot(res,"region_flags") <- matrix(as.numeric(splitter(par,"# region_flags")),ncol=nregions,nrow=10,byrow=TRUE)
  slot(res, 'age_pars')   <- as.array(matrix(as.numeric(splitter(par, '# age_pars', 1:10)), nrow=10, byrow=T))
  slot(res, 'rec_dist')   <- as.numeric(splitter(par, '# recruitment distribution'))
  slot(res, 'growth')     <- t(array(as.numeric(splitter(par, '# The von Bertalanffy', c(3,5,7))),
                                     dim=c(3,3), dimnames=list(c("est","min","max"),c("Lmin","Lmax","k"))))
  slot(res, 'lw_params')  <- as.numeric(splitter(par, '# Length-weight'))
  slot(res, 'sv')         <- as.numeric(splitter(par, '# sv'))
  slot(res, 'sd_length_at_age')   <- as.numeric(splitter(par, '# Generic SD'))
  slot(res, 'sd_length_dep')      <- as.numeric(splitter(par, '# Length-dependent SD'))
  slot(res, 'n_mean_constraints')   <- as.numeric(splitter(par, '# The number of mean constraints'))

  slot(res, 'dimensions') <- c(agecls   =nages,
                               years    =NA,
                               seasons   =dim(slot(res, 'mat'))[4],
                               regions  =nregions,
                               fisheries=dim(slot(res, 'tag_fish_rep_rate'))[2],
                               taggrps  =dim(slot(res, 'tag_fish_rep_rate'))[1]-1)
  return(res)
}
