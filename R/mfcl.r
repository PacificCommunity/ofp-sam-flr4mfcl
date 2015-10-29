

#' mfcl
#'
#' Runs MFCL with defined inputs
#'
#' @param frq:    An object of class MFCLFrq.
#' @param par:    An object of class MFCLPar.
#' @param outpar: The name of the output par file
#' @param switch: Optional numeric vector of additional flag settings
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return Creates a text file at the specified location.
#' 
#' @seealso \code{\link{read.MFCLFrq}} and \code{\link{read.MFCLPar}}
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' write(MFCLFrqStats())

setGeneric('mfcl', function(frq, par, ...) standardGeneric('mfcl')) 




#' @rdname mfcl-methods
#' @aliases mfcl

setMethod("mfcl", signature(frq="MFCLFrq", par="MFCLPar"), 
  function(frq, par, outpar="out.par", tag=NULL, newflags=NULL, ...){
  
  path <- system.file("extdata",package="FLR4MFCL")
  
  write(frq, "xx.frq")
  write(par, "xx.par")
  
  if(!is.null(tag))
    writeLines(tag, "xx.tag")
  
  if(!is.null(newflags))
    flagsarg <- paste(" -switch", paste(newflags, collapse=" "))
  
  system(paste(path, "/mfclo64 xx.frq xx.par ", outpar, flagsarg, sep=""))  
  
  #file.remove("xx.frq","xx.par","xx.tag")
  #system(paste(path, "/mfclo64 --version", sep=""))  
})



#' setMFCLversion
#'
#' Sets the version of MFCL to be activated within the package
#' Currently only the 2015 development version of MFCL is available
#' The available versions can be obtained from the function availableMFCLversions
#'
#' @param version:  Character string specifying the MFCL version
#'
#' @return Returns Nothing: just sets the MFCL version to the one specified
#'
#' @examples
#' setMFCLversion("2015_devvsn_1.1.4.3_linux")
#'
#' @export
 
setMFCLversion <- function(version="2015_devvsn_1.1.4.3_linux"){
  
  #rev(unlist(strsplit(packageDescription("FLR4MFCL")$Built, split=" ")))[1]
  
  oldpath <- getwd()
  path <- system.file("extdata",package="FLR4MFCL")
  
  from <- paste(path, "/", version, sep="")
  to   <- paste(path, "/mfclo64", sep="")
  
  if(file.exists(to))
    file.remove(to)
  
  file.copy(from=from, to=to, overwrite=TRUE)
  system(paste("chmod 777 ", path, "/mfclo64", sep=""))
  
  setwd(oldpath)
}




#' availableMFCLversions
#'
#' Returns the available versions of MFCL within the package
#' Currently only the 2015 development version of MFCL is available
#'
#' 
#'
#' @return Returns a list of available MFCL versions
#'
#' @examples
#' availableMFCLversions()
#'
#' @export

availableMFCLversions <- function(){
  
  res <- dir(system.file("extdata",package="FLR4MFCL"))
  return(res[res!='mfclo64'])
    
}

