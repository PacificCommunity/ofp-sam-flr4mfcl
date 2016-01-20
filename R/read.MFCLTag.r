#' read.MFCLTag
#'
#' Reads information from the tag file and creates  object
#'
#' @param tagfile:  A character string giving the name and path of the tag file to be read 
#' 
#'
#' @return An object of class character vector
#'
#' @examples
#' read.MFCLTag("C://R4MFCL//test_data//skj_ref_case//skj.tag")
#'
#' @export

read.MFCLTag <- function(tagfile) {
  
  res <- readLines(tagfile)
  
  return(res)
}
