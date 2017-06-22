#' laa
#'
#' Calculates length at age from Von Bertalanffy parameters in the par file
#'
#' @param object:    An object of class MFCLPar
#'
#'
#' @return A vector of lengths at age.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'




#' @rdname par-methods
#' @aliases laa

setMethod("laa", signature(object="MFCLPar"), 
          function(object, ...){
            
            L1  <- growth(object)['Lmin','est']
            LA  <- growth(object)['Lmax','est']
            K   <- growth(object)['k',   'est']
            ages<- 1:dimensions(object)['agecls']
            
            return(L1+(LA-L1)*((1-exp(-K*(ages-1)))/(1-exp(-K*(max(ages)-1)))))

          })


#' waa
#'
#' Calculates weight at age from Von Bertalanffy parameters and length weight params in the par file
#'
#' @param object:    An object of class MFCLPar
#'
#'
#' @return A vector of weights at age.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('waa', function(object, ...) standardGeneric('waa')) 

#' @rdname par-methods
#' @aliases waa

setMethod("waa", signature(object="MFCLPar"), 
          function(object, ...){
            
            return(lw_params(object)[1]*laa(object)^lw_params(object)[2])
            
          })





