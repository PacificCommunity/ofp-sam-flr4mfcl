

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



#' generate
#'
#' generates modified input files for mfcl
#'
#' @param x:    An object of class MFCLFrq.
#' @param ctrl:    An object of class MFCLprojCtrl.
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return Modified input file in accordance with projection settings.
#' 
#' @seealso \code{\link{projCtrl}} 
#' 
#' @export
#' @docType methods
#' @rdname mfcl-methods
#'
#' @examples
#' generate(MFCLFrq(), MFCLprojControl())

setGeneric('generate', function(x, ctrl, ...) standardGeneric('mfcl')) 



