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
#' read.MFCLIni("/home/roberts/skj/HCR/run0/skj.ini")
#'
#' @export

read.MFCLIni <- function(inifile, first.yr=1972, nseasons=4) {
  
  trim.leading  <- function(x) sub("^\\s+", "", x) 
  splitter      <- function(ff, tt, ll=1, inst=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[inst]+ll]),split="[[:blank:]]+")) 
  
  slotcopy <- function(from, to){
    for(slotname in slotNames(from)){
      slot(to, slotname) <- slot(from, slotname)
    }
    return(to)
  }
  
  res <- new("MFCLIni")
  
  par    <- readLines(inifile)
  par <- par[nchar(par)>=1]                                          # remove blank lines
  if(any(grepl("# ", par) & nchar(par)<3))
    par <- par[-seq(1,length(par))[grepl("# ", par) & nchar(par)<3]]   # remove single hashes with no text "# "
  
  nages    <- length(splitter(par, "# maturity at age"))
  nregions <- length(splitter(par, "# recruitment distribution by region"))

  dims_age        <- dimnames(FLQuant(quant="age"))
  dims_age$age    <- as.character(0:((nages/nseasons)-1))
  dims_age$season <- as.character(1:nseasons)
  
  res <- slotcopy(read.MFCLTagRep(parfile, par), res)
  
  slot(res, 'm')          <- as.numeric(splitter(par, '# natural mortality'))
  slot(res, "mat")      <- FLQuant(aperm(array(as.numeric(splitter(par, "# maturity at age")),
                                               dim=c(nseasons,nages/nseasons,1,1,1)),c(2,3,4,1,5)), dimnames=dims_age)
  slot(res, 'move_map')   <- as.numeric(splitter(par, '# movement map'))
  slot(res, 'diff_coffs') <- as.array(matrix(as.numeric(splitter(par, '# diffusion coffs', 1:(nregions-1))),nrow=nregions-1, byrow=T))
  slot(res, 'age_pars')   <- as.array(matrix(as.numeric(splitter(par, '# age_pars', 1:10)), nrow=10, byrow=T))
  slot(res, 'rec_dist')   <- as.numeric(splitter(par, '# recruitment distribution'))
  slot(res, 'growth')     <- t(array(as.numeric(splitter(par, '# The von Bertalanffy', c(3,5,7))), 
                                     dim=c(3,3), dimnames=list(c("est","min","max"),c("Lmin","Lmax","k"))))
  slot(res, 'lw_params')  <- as.numeric(splitter(par, '# Length-weight'))
  slot(res, 'sv')         <- as.numeric(splitter(par, '# sv'))  
  slot(res, 'sd_length_at_age')   <- as.numeric(splitter(par, '# Generic SD'))  
  slot(res, 'sd_length_dep')      <- as.numeric(splitter(par, '# Length-dependent SD'))
  slot(res, 'n_mean_constraints')   <- as.numeric(splitter(par, '# The number of mean constraints'))
  
  
  return(res)
}
