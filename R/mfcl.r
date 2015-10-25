#' mfcl
#'
#' Runs MFCL for specified .frq and .par files.
#' The version of MFCL to be run can be set using the function 'setMFCLversion'
#'
#' @param frq:  An object of class MFCLFrq
#' @param par:  An object of class MFCLPar
#' @param outpar: Character string specifying the name of the output par file.
#' @param switch: Numeric vector of additional flag settings (optional)
#'
#' @return Returns nothing: MFCL output files are written to the working directory
#'
#' @examples
#' mfcl(skj.frq, skj.par "out.par", switch=c(1, 1,1,1))
#'
#' @export


#' @rdname mfcl-methods
#' @aliases mfcl

setMethod("mfcl", signature(frq="MFCLFrq", par="MFCLPar"), 
  function(frq, par, outpar="out.par", switch=NULL){
  
  path <- system.file("extdata",package="FLR4MFCL")
  system(paste(path, "/mfclo64 --version", sep=""))  
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
  
  path <- system.file("extdata",package="FLR4MFCL")
  from <- paste(path, "/", version, sep="")
  to   <- paste(path, "/mfclo64", sep="")
  file.copy(from=from, to=to, overwrite=TRUE)
  system(paste("chmod 111 ", path, "/mfclo64", sep=""))
  
}




#' availableMFCLversions
#'
#' Returns the available versions of MFCL  within the package
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
  
  dir(system.file("extdata",package="FLR4MFCL"))
    
}

