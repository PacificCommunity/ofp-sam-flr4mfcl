

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
#' @rdname generate-methods
#'
#' @examples
#' generate(MFCLFrq(), MFCLprojControl())

setGeneric('generate', function(x, ctrl, ...) standardGeneric('generate')) 



#' @rdname generate-methods
#' @aliases generate

setMethod("generate", signature(x="MFCLFrq", ctrl="MFCLprojControl"), 
         function(x, ctrl, ...){
            
            proj.yrs <- seq(range(x)['maxyear']+1, range(x)['maxyear']+nyears(ctrl))
            qtrs     <- sort(unique(freq(x)$month))
            
            week   <- rev(freq(x)$week)[1]
            if(!all(freq(x)$week==week))
              warning("Differences in week not accounted for in projection frq")
            
            if(length(caeff(ctrl))>1 & length(caeff(ctrl))!=n_fisheries(x))
              stop("Error: caeff values do not match number of fisheries")
            if(length(scaler(ctrl))>1 & length(scaler(ctrl))!=n_fisheries(x))
              stop("Error: scaler values do not match number of fisheries")
            
            sc_df <- data.frame(fishery=1:n_fisheries(x),
                                caeff  = ifelse(length(caeff(ctrl))==1,  rep(caeff(ctrl), n_fisheries(x)),  caeff(ctrl)),
                                scaler = ifelse(length(scaler(ctrl))==1,  rep(scaler(ctrl), n_fisheries(x)),  scaler(ctrl)))
            
            avdata <- freq(x)[is.element(freq(x)$year, avyrs(ctrl)) & is.na(freq(x)$length) & is.na(freq(x)$weight) ,]
            avdata <- rbind(avdata, freq(x)[is.element(freq(x)$year, avyrs(ctrl)) & freq(x)$length %in% lf_range(x)['LFFirst'] ,],
                                    freq(x)[is.element(freq(x)$year, avyrs(ctrl)) & freq(x)$weight %in% lf_range(x)['WFFirst'] ,])
            
            avcatch  <- sweep(tapply(avdata$catch,  list(avdata$month, avdata$fishery), sum, na.rm=T), 2, sc_df$scaler, "*")
            aveffort <- sweep(tapply(avdata$effort, list(avdata$month, avdata$fishery), sum, na.rm=T), 2, sc_df$scaler, "*")
            
            projdat  <- data.frame(year    = rep(proj.yrs, each=(n_fisheries(x)*length(qtrs))),
                                   month   = qtrs,
                                   week    = week,
                                   fishery = rep(rep(1:n_fisheries(x),each=length(qtrs)), nyears(ctrl)),
                                   catch   = c(avcatch), 
                                   effort  = c(aveffort),
                                   penalty = 1.0, length=NA, weight=NA, freq=-1)
            
            projdat <- projdat[!is.na(projdat$catch) & !is.na(projdat$effort),]
            
            projdat[is.element(projdat$fishery, sc_df[sc_df$caeff==1, 'fishery']), 'effort'] <- -1
            projdat[is.element(projdat$fishery, sc_df[sc_df$caeff==2, 'fishery']), 'catch']  <- -1
            
            freq(x) <- rbind(freq(x), projdat)
            
            data_flags(x)[2,] <- as.numeric(avyrs(ctrl))+1
            data_flags(x)[3,] <- as.numeric(qtrs[1])
            lf_range(x)['Datasets'] <- lf_range(x)['Datasets']+nrow(projdat)
            
            return(x)
          })

    