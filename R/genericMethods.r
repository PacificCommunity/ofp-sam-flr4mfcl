

#' write
#'
#' Writes MFCL objects to a text file
#'
#' @param x An object of class MFCL eg. MFCLFrq, MFCLPar, etc.
#'
#' @param file The name and path of the file to be written
#'
#' @param append If True append to existing file, If False overwrite any existing file
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
#' @rdname write-methods
#'
#' @examples
#' write(MFCLFrqStats())

setGeneric('write', function(x, file, append=F, ...) standardGeneric('write')) 



#'@export version
setGeneric('version', function(x, ...) standardGeneric('version')) 
setMethod('version', signature(x='MFCLPar'),function(x) flagval(x, 1, 200)$value)  #flags(par)[flags(x)$flagtype==1 & flags(x)$flag==200,'value']) 

setMethod('version', signature(x='MFCLFrq'),function(x) return(slot(x,'frq_version'))) 

#'@export flagval
setGeneric('flagval', function(x, flagtype, flag, ...) standardGeneric('flagval'))
setMethod('flagval', signature(x='MFCLPar'), function(x, flagtype, flag) flags(x)[flags(x)$flagtype %in% flagtype & flags(x)$flag %in% flag,])

#'@export 
setGeneric('flagval<-', function(x, flagtype, flag, value) standardGeneric('flagval<-')) 
setReplaceMethod('flagval', signature(x='MFCLPar'),
                 function(x, flagtype, flag, value){flags(x)[flags(x)$flagtype %in% flagtype & flags(x)$flag %in% flag, 'value'] <- value; return(x)}) 

#'@export steepness
setGeneric('steepness', function(x) standardGeneric('steepness'))
setMethod('steepness', signature(x='MFCLIni'), function(x) return(slot(x, 'sv')))
setMethod('steepness', signature(x='MFCLPar'), function(x) return(slot(x, 'season_growth_pars')[29]))

setGeneric('steepness<-', function(x, value) standardGeneric('steepness<-'))
setReplaceMethod('steepness', signature(x='MFCLIni'), function(x, value){slot(x, 'sv') <- value; return(x)})
setReplaceMethod('steepness', signature(x='MFCLPar'), function(x, value){slot(x, 'season_growth_pars')[29] <- value; return(x)})

#'@export lw_params
setMethod('lw_params', signature(object='MFCLPar'), function(object) return(slot(object, 'season_growth_pars')[27:28]))
setReplaceMethod('lw_params', signature(object='MFCLPar'), function(object, value){slot(object, 'season_growth_pars')[27:28] <- value; return(x)})


