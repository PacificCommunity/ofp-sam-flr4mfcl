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
          function(object, ages=NULL, ...){
            
# test - L1 <- 38.4705; LA <- 152.8989; rho <- 0.94129; beta <- 0.5770; ages <- seq(0.5, 40.5)  - John' spreadsheet          
            
            L1  <- growth(object)['Lmin','est']
            LA  <- growth(object)['Lmax','est']
            K   <- growth(object)['k',   'est']
            beta<- richards(object)
            maxage<- dimensions(object)['agecls']
            
            if(is.null(ages))
              ages<- 1:dimensions(object)['agecls']
            
            if(beta != 0) # Richards
              return( (L1^(1/exp(beta)) + (LA^(1/exp(beta)) - L1^(1/exp(beta))) * ((1-exp(-K*(ages-1)))/(1-exp(-K*(maxage)))))^exp(beta)   )
            
            if(beta == 0) # Von Bertalanffy
              return(L1+(LA-L1)*((1-exp(-K*(ages-1)))/(1-exp(-K*(maxage-1)))))

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


setGeneric('calc_diff_coffs_age_period', function(par, frq, ...) standardGeneric('calc_diff_coffs_age_period')) 

setMethod("calc_diff_coffs_age_period", signature(par="MFCLPar", frq="MFCLFrq"),
          function(par, frq, new_diff_coffs=NULL, ...){
            
            if(is.null(new_diff_coffs)) 
              new_diff_coffs <- diff_coffs(par) 
            
            # check the dimensinos of the new_diff_coffs
            if(!all(dim(diff_coffs(par))==dim(new_diff_coffs)))   
              stop("dimensions of new_diff_coffs not consistent with diff_coffs(par)")
            
            new_dcap <- array(NA, dim(diff_coffs_age_period(par))) # new diff_coffs_age_period container
            
            for(qq in 1:length(move_map(par))){                    # for each season (if seasonal movement)
              
              # get the move_matrix from the frq
              map2 <- move_matrix(frq)
              map2[lower.tri(map2)] <- t(map2)[lower.tri(map2)]    # complete the lower triangle of map2
              map2[is.na(map2)]     <- 0                           # and set the diagonals to zero
            
              map3 <- map2
              map3[map3!=0] <- cumsum(map3[map3!=0])               # map the diff_coff array sequence onto the move map

              dcoff <- matrix(0, nrow=nrow(map2), ncol=ncol(map2))
            
              for(dd in 1:length(diag(dcoff))){                    # set the diagonals to 1 + sum(diff_coff) by column
                vv <- unlist(apply(map3, 2, function(x){list(x)})[[dd]])
                diag(dcoff)[dd] <- 1+sum(new_diff_coffs[qq, vv[vv!=0]])
              }
            
              # feed the diff coffs into the movement matrix by columns
              nn <- 1
              for(mapcol in 1:ncol(map2)){
                for(maprow in 1:nrow(map2)){
                  if(map2[maprow, mapcol] == 1){
                    dcoff[maprow,mapcol] <- -new_diff_coffs[qq,nn]
                    nn <- nn+1
                  }
                }
              }
              new_dcap[,,,qq] <- solve(dcoff)
            }
            return(new_dcap)
          })






#' recYears
#'
#' Returns the year range over which the SRR is calculated given the flag settings from af199 and af200
#'
#' @param par:    An object of class MFCLPar
#'
#' @return A vector.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('recYears', function(par,...) standardGeneric('recYears'))

#' @rdname par-methods
#' @aliases recYears

setMethod("recYears", signature(par="MFCLPar"), 
          function(par, nrecsyr=4, ...){
            
            recyrs <- rev(unlist(dimnames(qts(rel_rec(par)))['year']))[recPeriod(par)[c(1,2)]+c(0,1)]
            return(recyrs)
          })
            
 
## Gets m at age from MFCLBiol - does not check flag settings
setMethod("m_at_age", signature(object='MFCLBiol'),
          function(object, ...){
            
            m <- CalcMSpline(MNodes=as.vector(aperm(log_m(object),c(4,1,2,3,5,6)))[1:5],ages=dimensions(object)['agecls'])
            return(m)
          })

## Gets m at age from MFCLBiol - additional checks of flag settings
setMethod("m_at_age", signature(object='MFCLPar'),
          function(object, ...){
            
            if(flagval(object, 1, 122)$value != 0)
              warning("parest flag 122 greater than 0 : natural mortality shared across older ages")
            
            m <- CalcMSpline(MNodes=as.vector(aperm(log_m(object),c(4,1,2,3,5,6)))[1:5],ages=dimensions(object)['agecls'])
            return(m)
          })


## Note at present the sel(par) method only returns selectivity values for the spline function
## logistic and double normal selectivity to be added later

setMethod("sel", signature(object="MFCLPar"),
          function(object, ...){
            
            nd    <- flagval(object, -(1:dimensions(object)['fisheries']), 3)$value     # first age class for commmon terminal sel
            nodes <- flagval(object, -(1:dimensions(object)['fisheries']), 61)$value    # n cubic spline nodes
            ff75  <- flagval(object, -(1:dimensions(object)['fisheries']), 75)$value    # youngest age classes assumed to be 0
            k     <- growth(object)['k','est']
            
            sel   <- FLQuant(NA, dimnames=list(age=1:dimensions(object)['agecls'], year='all', unit=1:dimensions(object)['fisheries'], season=1)) 
            
            nages <- dimensions(object)['agecls']
            fshsel<- aperm(fishery_sel(object), c(4,1,2,3,5,6))
            
            
            for(ff in 1:dimensions(object)['fisheries']){
              
              if(ff75[ff]>0)
                sel[1:ff75[ff],,ff,] <- 0
              
              if(flagval(object, -ff, 57)$value == 3){          # currently only implemented for cubic spline selectivity
                sel[(ff75[ff]+1):nd[ff],,ff,] <- CalcSelSpline(nd[ff], nodes[ff], ff75[ff], k, c(fshsel[,,,ff,,])[c(abs(fshsel[,,,ff,,]))>0])
                sel[(nd[ff]+1):nages,,ff,]    <- sel[nd[ff],,ff,]
              }
            }
            
            return(sel)
          })




