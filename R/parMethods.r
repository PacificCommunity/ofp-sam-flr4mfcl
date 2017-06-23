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





#' SPR0
#'
#' Calculates spawners per recruit at zero fishing
#'
#' @param par:    An object of class MFCLPar
#' @param rep:    An object of class MFCLRep  
#'
#' @return An FLQuant.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('SPR0', function(par, rep, ...) standardGeneric('SPR0')) 

#' @rdname par-methods
#' @aliases SPR0

setMethod("SPR0", signature(par="MFCLPar", rep="MFCLRep"), 
          function(par, rep, ...){
            
            age <- 1:dimensions(par)['agecls']
            wgt <- waa(par)
            m   <- c(aperm(m_at_age(rep), c(4,1,2,3,5,6)))
            mat <- c(aperm(mat(par), c(4,1,2,3,5,6)))
            

            spr0           <- c(1,exp(-cumsum(m)))[-41]*mat*wgt
            spr0[max(age)] <- spr0[max(age)]*(1/(1-exp(-m[max(age)])))  # add the plus group
            spr0           <- sum(spr0)
            
            return(spr0)
            
          })



