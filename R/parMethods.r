#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

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



setMethod("n_fisheries", signature(object="MFCLPar"),
          function(object, ...){
            nfish <- unique(flags(object)[flags(object)$flagtype <0 & flags(object)$flagtype>-1000,'flagtype'])
            return(length(nfish))
          })




#' @rdname par-methods
#' @aliases laa

setMethod("laa", signature(object="MFCLBiol"), 
          function(object, ...){
            
# test - L1 <- 38.4705; LA <- 152.8989; rho <- 0.94129; beta <- 0.5770; ages <- seq(0.5, 40.5)  - John' spreadsheet          
            
            L1  <- growth(object)['Lmin','est']
            LA  <- growth(object)['Lmax','est']
            K   <- growth(object)['k',   'est']
            rho <- exp(-K)
            beta<- richards(object)
            ages<- 1:dimensions(object)['agecls']
            
            if(beta != 0) # Richards
              return( (L1^(1/beta) + (LA^(1/beta) - L1^(1/beta)) * ((1-rho^(ages-1))/(1-rho^(max(ages-1)))))^beta   )
            
            if(beta == 0) # Von Bertalanffy
              return(L1+(LA-L1)*((1-exp(-K*(ages-1)))/(1-exp(-K*(max(ages)-1)))))

          })


setGeneric('aal', function(object, ...) standardGeneric('aal')) 

#' @rdname par-methods
#' @aliases aal

setMethod("aal", signature(object="MFCLBiol"), 
          function(object, lengths=seq(0,108,by=2)){
            
            L1  <- growth(object)['Lmin','est']
            LA  <- growth(object)['Lmax','est']
            K   <- growth(object)['k',   'est']
            ages<- 1:dimensions(object)['agecls']
            
            return(suppressWarnings((log(-(((lengths-L1)/(LA-L1))*(1-exp(-K*(max(ages)-1)))-1))/-K)+1))
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

setMethod("waa", signature(object="MFCLBiol"), 
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


setGeneric('SPR0', function(par, ...) standardGeneric('SPR0')) 

#' @rdname par-methods
#' @aliases SPR0

setMethod("SPR0", signature(par="MFCLBiol"), 
          function(par, ...){
            
            age <- 1:dimensions(par)['agecls']
            wgt <- waa(par)
            m   <- m(par)*exp(c(aperm(m_devs_age(par), c(4,1,2,3,5,6))))
            mat <- mat(par)  #c(aperm(mat(par), c(4,1,2,3,5,6)))
            

            spr0           <- c(1,exp(-cumsum(m)))[-max(age)]*mat*wgt
            spr0[max(age)] <- spr0[max(age)]*(1/(1-exp(-m[max(age)])))  # add the plus group
            spr0           <- sum(spr0)
            
            return(spr0)
            
          })




#' YPR
#'
#' Calculates yield per recruit and spawners per recruit - in kilos
#'
#' @param rep:    An object of class MFCLRep  
#' @param par:    An object of class MFCLPar
#'
#' @return A dataframe.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('YPR', function(rep, par, ...) standardGeneric('YPR')) 

#' @rdname par-methods
#' @aliases YPR

setMethod("YPR", signature(rep = "MFCLRep", par="MFCLPar"), 
          function(rep, par, fminyr=NULL, fmaxyr=NULL, scalars=NULL, ...){
            
            
            frange <- range(rep)['maxyear'] - flagval(par, 2, c(148,155))$value/flagval(par, 2, 57)$value 
            if(!is.null(fminyr))
              frange[1] <- fminyr
            if(!is.null(fmaxyr))
              frange[2] <- fmaxyr
            
            nm  <- m_at_age(rep)  
            mat <- mat(par)
            wt  <- c(aperm(mean_waa(rep), c(4,1,2,3,5,6)))  #waa(par)
            sel <- yearMeans(seasonMeans(fm_aggregated(rep)[,as.character(frange[1]:frange[2])]))
            
            if(is.null(scalars))
              scalars <- seq(0, 5, by=0.01)
            
            ypr <- spr <- NULL
            for(fmult in scalars){
              N            <- c(1, exp(-cumsum(nm + c(sel * fmult)))[-length(nm)])
              N[length(N)] <- N[length(N)-1] * exp(-nm[length(N)-1] - c(sel[length(N)-1] * fmult))/(1-exp(-nm[length(N)] - c(sel[length(N)] * fmult)))
              
              spr <- c(spr, sum(N * mat *wt))
              ypr <- c(ypr, sum(c(sel*fmult)/(nm+c(sel*fmult)) * N * (1-exp(-nm - c(sel*fmult))) * wt))
            }
            
            res <- data.frame(scalar = scalars,
                              ypr    = ypr,
                              spr    = spr)
            
            return(res)
          })







