#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

## Unexported local functions

generate.ESS<- function(x, ctrl, projdat2, sc_df){
  
  # projdat stuff for which no ESS is specified
  projdat_noess <- projdat2[is.element(projdat2$fishery, sc_df$fishery[is.na(sc_df$ess)]),]
  # projdat stuff with an ESS to be applied to length comps
  projdat_ess_l <- projdat2[is.element(projdat2$fishery, sc_df$fishery[!is.na(sc_df$ess)]) & 
                              is.element(projdat2$fishery, sc_df$fishery[sc_df$length]),]
  # projdat stuff with an ESS to be applied to weight comps
  projdat_ess_w <- projdat2[is.element(projdat2$fishery, sc_df$fishery[!is.na(sc_df$ess)]) & 
                              is.element(projdat2$fishery, sc_df$fishery[sc_df$weight]),]
  # add length and weight frequency data to the frq
  lengths <- seq(lf_range(x)["LFFirst"], by=lf_range(x)["LFWidth"], length=lf_range(x)["LFIntervals"])
  weights <- seq(lf_range(x)["WFFirst"], by=lf_range(x)["WFWidth"], length=lf_range(x)["WFIntervals"])
  
  projdat_ess_ll <- as.data.frame(lapply(projdat_ess_l, rep, each=length(lengths)))
  projdat_ess_ww <- as.data.frame(lapply(projdat_ess_w, rep, each=length(weights)))
  projdat_ess_ll$length <- lengths
  projdat_ess_ww$weight <- weights
  
  projdat2      <- rbind(projdat_noess, projdat_ess_ll, projdat_ess_ww)
  projdat2$freq <- 0
  
  for(ff in 1:n_fisheries(x))
    projdat2[projdat2$length==lf_range(x)["LFFirst"] & projdat2$fishery==ff,'freq'] <- ess(ctrl)[ff]
  
  return(projdat2)
}

#' 'generate()' method for FLR4MFCL
#'
#' 'generate()' can generate a range of different MFCL objects including 'par' and 'freq' files.
#' The exact behaviour depends on the type of objects passed to 'generate()'.
#' It is used to (hopefully) improve the workflow when manipulating MFCL objects in R, particularly when preparing objects for 
#' running projections, but also when generating new stock assessment input files from pseudo data.
#'
#' There are currently five 'generate()' methods:
#' \itemize{
#'   \item Generate an expanded \linkS4class{MFCLFrq} object from an existing \linkS4class{MFCLFrq} object and a \linkS4class{MFCLprojControl} object. 
#'   \item Generate an expanded \linkS4class{MFCLPar} object from an existing \linkS4class{MFCLPar} object and a \linkS4class{MFCLFrq} object. This is typically used for taking a expanding an existing par and using a 00 par (generated using the MFCL executable).
#'   \item Generate an expanded \linkS4class{MFCLPar} object from an existing \linkS4class{MFCLFrq} object. This can be a useful way of avoiding making a 00 par with MFCL executable, reading it in, and blowing it up. It can be used for standard projection analyses that do not include additional tag data. Tests for this method can be found in the inst/mfcl_tests folder of the package source.
#'   \item Generate an expanded \linkS4class{MFCLPar} object from an existing \linkS4class{MFCLFrq} object and a \linkS4class{MFCLTagProj} object. This method is method can be used to set up par objects that include additional tag data.
#'   \item Generate new stock assessment input files \linkS4class{MFCLFrq}, \linkS4class{MFCLTag}, etc. objects from an \linkS4class{MFCLPseudo} object and a \linkS4class{MFCLTagProj} object.
#' }
#' @param x An \linkS4class{MFCLFrq} or \linkS4class{MFCLPar} object that will be expanded.
#' @param y If \code{x} is an \linkS4class{MFCLFrq} then \code{y} is an \linkS4class{MFCLprojControl}. If \code{x} is \linkS4class{MFCLPar}, \code{y} is either an \linkS4class{MFCLPar} or \linkS4class{MFCLFrq}. 
#' @param z If \code{x} and \code{y} are \linkS4class{MFCLPar}s then \code{z} is \linkS4class{MFCLFrq}. Alternatively if \code{x} and \code{y} are \linkS4class{MFCLPar} and \linkS4class{MFCLFrq} respectively then \code{z} is \linkS4class{MFCLTagProj}. Otherwise it is ignored.
#' @param ... Additional arguments (currently unused).
#' @return An object of the same type as the \code{x} argument, expanded and hopefully useable for running projections.
#' @export
#' @seealso \code{\link{MFCLprojContrl}} \code{\link{MFCLFrq}} \code{\link{MFCLPar}}
#' @examples
#' \dontrun{
#' # Expanding an MFCLFrq, e.g. that was used from in an assessment
#' frq <- read.MFCLFrq(initial_frq)
#' projCtrl <- MFCLprojControl(nyears=nyears, nsims=1, avyrs=avyrs, fprojyr=first_proj_yr, controls=proj_controls)
#' projfrq  <- generate(frq, projCtrl)
#' # Expanding an MFCLPar, e.g. that was generated as part of an assessment.
#' # After making a 00 par with the MFCL executable.
#' par <- read.MFCLPar(initial_par, first.yr=first_yr)
#' zero.par <- read.MFCLPar("00.par" first.yr=first_yr)
#' # Generate the new par file which has the right size for everything and has all the old information
#' projpar <- generate(par, zero.par, projfrq)
#' # Alternatively, expanding an MFCLPar without using the 00 par
#' projpar <- generate(par, projfrq)
#' }
setGeneric('generate', function(x, y, z, ...) standardGeneric('generate')) 

#' @rdname generate
setMethod("generate", signature(x="MFCLFrq", y="MFCLprojControl"), 
         function(x, y, ...){
#browser()            
            ctrl     <- y
            proj.yrs <- seq(fprojyr(ctrl), range(x)['maxyear']+nyears(ctrl))  #seq(range(x)['maxyear']+1, range(x)['maxyear']+nyears(ctrl))
            qtrs     <- sort(unique(freq(x)$month))
            
            week   <- rev(freq(x)$week)[1]
            if(!all(freq(x)$week==week))
              warning("Differences in week not accounted for in projection frq")
            
            if(length(caeff(ctrl))>1 & length(caeff(ctrl))!=n_fisheries(x))
              stop("Error: caeff values do not match number of fisheries")
            if(length(scaler(ctrl))>1 & length(scaler(ctrl))!=n_fisheries(x))
              stop("Error: scaler values do not match number of fisheries")
            
            #sc_df <- data.frame(fishery=1:n_fisheries(x), caeff  = caeff(ctrl), scaler = scaler(ctrl))
            sc_df <- data.frame(fishery=1:n_fisheries(x), caeff  = caeff(ctrl), scaler = scaler(ctrl), ess=ess(ctrl), 
                                length=tapply(freq(x)$length, freq(x)$fishery, function(tt){any(!is.na(tt))}), 
                                weight=tapply(freq(x)$weight, freq(x)$fishery, function(tt){any(!is.na(tt))}))
            
            avdata <- freq(x)[is.element(freq(x)$year, avyrs(ctrl)) & is.na(freq(x)$length) & is.na(freq(x)$weight) ,]
            avdata <- rbind(avdata, freq(x)[is.element(freq(x)$year, avyrs(ctrl)) & freq(x)$length %in% lf_range(x)['LFFirst'] ,],
                                    freq(x)[is.element(freq(x)$year, avyrs(ctrl)) & freq(x)$weight %in% lf_range(x)['WFFirst'] ,])
            avdata <- avdata[!duplicated(avdata[,1:7]),] # remove duplicates that can occur if you have both length and wgt freq data
            
            avdata$catch[avdata$catch == -1] <- NA
            avdata$effort[avdata$effort == -1] <- NA
            
            flts     <- as.numeric(colnames(tapply(avdata$catch,  list(avdata$month, avdata$fishery), sum, na.rm=T)))
            avcatch  <- sweep(tapply(avdata$catch,  list(avdata$month, avdata$fishery), mean, na.rm=T), 2, sc_df$scaler[flts], "*")
            aveffort <- sweep(tapply(avdata$effort, list(avdata$month, avdata$fishery), mean, na.rm=T), 2, sc_df$scaler[flts], "*")
            
            projdat  <- data.frame(year    = rep(proj.yrs, each=(length(flts)*length(qtrs))),
                                   month   = qtrs,
                                   week    = week,
                                   fishery = rep(rep(flts, each=length(qtrs)), nyears(ctrl)),
                                   catch   = c(avcatch), 
                                   effort  = c(aveffort),
                                   penalty = -1.0, length=NA, weight=NA, freq=-1)
            
            # remove records with missing values from projection years
            projdat2 <- rbind(projdat[is.element(projdat$fishery, sc_df[sc_df$caeff==1, 'fishery']) & !is.na(projdat$catch),],
                              projdat[is.element(projdat$fishery, sc_df[sc_df$caeff==2, 'fishery']) & !is.na(projdat$effort),])
            
            # set the penalty to 1.0 for those fisheries with standardised CPUE -- maybe ?
            std.fish <- seq(1:n_fisheries(x))[tapply(freq(x)$penalty, freq(x)$fishery, mean)>0]
            projdat2$penalty[is.element(projdat2$fishery, std.fish)] <- 1.0
            
            # set catch/effort to -1 for fisheries projected on effort/catch
            projdat2[is.element(projdat2$fishery, sc_df[sc_df$caeff==1, 'fishery']),'effort'] <- -1
            projdat2[is.element(projdat2$fishery, sc_df[sc_df$caeff==2, 'fishery']),'catch'] <- -1
            
            ## STOCHASTIC PROJECTIONS WITH ESS
            # if an ESS is specified - include length composition data and set first value to ESS
            if(!all(is.na(ess(ctrl)))){
              projdat2 <- generate.ESS(x, ctrl, projdat2, sc_df)
            }

            freq(x) <- rbind(freq(x), projdat2)
            
            data_flags(x)[2,] <- fprojyr(ctrl)  #range(x)['maxyear']+1  #as.numeric(max(avyrs(ctrl)))+1
            data_flags(x)[3,] <- as.numeric(qtrs[1])
            
            #original code
            #lf_range(x)['Datasets'] <- lf_range(x)['Datasets']+nrow(projdat2)
            
            # modified for pseudo obs - but doesn't work for bet and yft
            #lf_range(x)['Datasets'] <- nrow(freq(x)[is.element(freq(x)$length, c(NA,lf_range(x)['LFFirst'])),]) +
            #                           nrow(freq(x)[is.element(freq(x)$weight, c(   lf_range(x)['WFFirst'])),])
            
            # potential solution - see if this breaks anything
            #lf_range(x)['Datasets'] <- lf_range(x)['Datasets'] + nrow(unique(projdat2[,1:4]))
            # using realisations because I think it is safer RDS 15/04/2020
            lf_range(x)['Datasets'] <- nrow(realisations(x))
            
            slot(x,'range')['maxyear']     <- max(freq(x)$year)
            
            return(x)
          })

#' @rdname generate
setMethod("generate", signature(x="MFCLPar", y="MFCLPar", z="MFCLFrq"), 
          function(x, y, z, ...){
            
            # set stochastic recruitment flags
            if(flagval(x, 1, 232)$value == 0)
              flagval(x, 1, 232) <- recPeriod(x, af199=flagval(x, 2, 199)$value, af200=flagval(x, 2, 200)$value)['pf232']
            if(flagval(x, 1, 233)$value == 0)
              flagval(x, 1, 233) <- recPeriod(x, af199=flagval(x, 2, 199)$value, af200=flagval(x, 2, 200)$value)['pf233']
            
            #proj.yrs <- dimnames(rel_rec(y))[[2]][!is.element(dimnames(rel_rec(y))[[2]], dimnames(rel_rec(x))[[2]])]
            proj.yrs <- seq(range(x)['maxyear']+1, range(x)['maxyear']+(dimensions(y)[2]-dimensions(x)[2])/dimensions(x)[3])
            
            # zero filled objects that you can just copy across   
            rep_rate_dev_coffs(x) <- rep_rate_dev_coffs(y)
            fm_level_devs(x)      <- fm_level_devs(y)
            
            q_dev_coffs(x)        <- q_dev_coffs(y)
            sel_dev_coffs(x)      <- sel_dev_coffs(y)
            sel_dev_coffs2(x)     <- sel_dev_coffs2(y)
            growth_devs_cohort(x) <- growth_devs_cohort(y)
            unused(x)             <- unused(y)
            lagrangian(x)         <- lagrangian(y)
            
            if(any(dim(tag_fish_rep_rate(x)) != dim(tag_fish_rep_rate(y)))){
              flags(x) <- rbind(flags(x), flags(y)[!is.element(flags(y)$flagtype, flags(x)$flagtype),])
              
              for(ss in list("tag_fish_rep_rate", "tag_fish_rep_flags", "tag_fish_rep_grp", "tag_fish_rep_pen", "tag_fish_rep_target"))
                slot(x, ss) <- slot(y, ss)
            }
            
            # check that "other lambdas ..." are not specified for par files < 1053
            if(flagval(x, 1, 200)$value<1053 & length(grep("# Other lambdas", lagrangian(x)))>0)
              lagrangian(x) <- lagrangian(x)[1:(grep("Other lambdas", lagrangian(x))-1)]
            
            # non-zero filled objects that you need to append zeroes to 
            eff_dev_coff_incs     <- unlist(lapply(effort_dev_coffs(y),length)) - unlist(lapply(effort_dev_coffs(x),length))
            eff_dev_coff_vals     <- unlist(lapply(effort_dev_coffs(x), mean))

            #effort_dev_coffs(x)   <- lapply(1:dimensions(x)["fisheries"], function(g) c(effort_dev_coffs(x)[[g]], rep(eff_dev_coff_vals[g], eff_dev_coff_incs[g])))
            effort_dev_coffs(x)   <- lapply(1:dimensions(x)["fisheries"], function(g) c(effort_dev_coffs(x)[[g]], rep(0, eff_dev_coff_incs[g])))
            
            # catch_dev_coffs - gets really messy because you may have zero catch obs in some cases and fishery groupings to worry about.
#            catch_dev_coffs(x)    <- lapply(1:length(catch_dev_coffs(x)), 
#                                            function(g) c(catch_dev_coffs(x)[[g]], rep(0, length(proj.yrs)*dimensions(x)['seasons'])))
            
            ## YUKIO's CODE - hacked by RDS to remove dependencies on tidyr and magrittr
            ncgrp<-length(catch_dev_coffs(x))
            ffl29<-flagval(x,-(1:n_fisheries(x)),29)$value  # Obtain fish flags(29) determining the grouping of catchbility
            
            test1 <- unique(with(freq(z), paste(year,month,fishery, sep="_")))
            test2 <- as.data.frame(t(sapply(strsplit(test1, split="_"), "as.numeric",simplify = T)))
            test3 <- data.frame(V1=paste(test2$V1, test2$V2, sep="_"), V2=test2$V3)
            
            nElemByGrp<-vector(mode="numeric",length=length(unique(ffl29)))
            
            for(grp in sort(unique(ffl29))){
              nElemByGrp[grp] <- length(unique(test3[test3$V2 %in% which(ffl29==grp),'V1']))
              nElemFuture<- nElemByGrp[grp]-length(catch_dev_coffs(x)[[grp]])-1 #
              catch_dev_coffs(x)[[grp]]<-c(catch_dev_coffs(x)[[grp]],rep(0,nElemFuture))
            }
            
            # region_rec_var
            region_rec_var(x)     <- window(region_rec_var(x), start=range(x)['minyear'], end=range(y)['maxyear'])
            region_rec_var(x)[is.na(region_rec_var(x))] <- 0
            
            # set the rec vars to the average of the historical time series
            # I don't think this makes a difference for projections as these values are not used for projections 
            # but they may make a difference for re-estimating an extended model
            region_rec_var(x)[,as.character(range(x)['maxyear']:(range(y)['maxyear']-1))] <- apply(region_rec_var(x)[,as.character(range(x)['minyear']:(range(x)['maxyear']-1))], c(1,3,4,5,6), mean)
            
            rel_rec(x) <- window(rel_rec(x), start=range(x)['minyear'], end=range(y)['maxyear'])
            rel_rec(x)[,as.character(proj.yrs)] <- rel_rec(y)[,as.character(proj.yrs)] 
            
            dimensions(x)         <- dimensions(y)
            range(x)              <- range(y)
            
            return(x)
            
          })

#' @rdname generate
setMethod("generate", signature(x="MFCLPar", y="MFCLFrq"), 
  function(x, y, ...){
    # Add a check that we are going to extend the Par, not shorten it (which would be weird...)
    newx <- x
    
    # Handy stuff
    last_original_year <- range(x)["maxyear"]
    extra_years <- (last_original_year + 1):range(y)["maxyear"]
    nseasons <- dimensions(x)["seasons"]

    # Add timestep to the freq table - this is used in several places later on
    first_year <- min(freq(y)$year)
    first_month <- min(freq(y)[freq(y)$year==first_year,"month"])
    first_week <- min(freq(y)[freq(y)$year==first_year & freq(y)$month==first_month,"week"])
    months <- sort(unique(freq(y)$month))
    # get the season of each month in the freq table
    season <- match(freq(y)$month, months) 
    freq(y)$timestep <- ((freq(y)$year - first_year) * nseasons) + (season - which(first_month==months)) + 1

    # Go slot by slot and change those that we think need changing
    
    # range - easy - adjust maxyear using the Frq object
    range(newx)["maxyear"] <- range(y)["maxyear"]

    # dimensions - also easy, update the years element
    # this the number of timesteps, not years
    dimensions(newx)["years"] <- max(freq(y)$timestep)

    # growth_devs_cohort - bit trickier
    # In par file # Cohort specific growth deviations, lmul_io4.cpp
    # The data comes from the growth_dev member: It's a dvar_vector
    # pof <<  fsh.growth_dev << endl; # (see lmul_io4.cpp)
    # dimensions set in lmult.cpp
    # growth_dev.allocate(1,fsh.nyears); # a vector as long ntimesteps
    # In FLR4MFCL it's an FLCohort. We extend and fill it with ...? 0s? Is it always filled with 0s?
    # What if ntimesteps is not in whole years?
    # The values seem to be allocated in newmaux5.cpp
    # Leave as 0s for the moment
    new_growth_devs_cohort <- window(growth_devs_cohort(x), end=range(newx)["maxyear"])
    new_growth_devs_cohort[,as.character(extra_years)] <- 0.0
    growth_devs_cohort(newx) <- new_growth_devs_cohort

    # flags - we want to set flags 232 and 233 using flags 199 and 200
    # age flags 199 and 200 are specified in terms of time periods BEFORE the end of the analysis period
    # par flags 232 and 233 are specified in terms of time periods from the start.
    # Assume that flags 199 and 200 are anchored to last model / assessment timestep - they count backwards from here
    # Get the start of the projection period from the data flags
    first_projection_year <- data_flags(y)[2,1] 
    first_projection_month <- data_flags(y)[3,1]
    first_projection_timestep <- ((first_projection_year - first_year) * nseasons) + (which(first_projection_month==months) - which(first_month==months)) + 1
    last_model_timestep <- first_projection_timestep - 1
    # Translate into equivalent values counting forward
    if(flagval(newx, 1, 232)$value == 0){
      flagval(newx, 1, 232) <- last_model_timestep - flagval(newx,2,199)$value + 1
    }
    if(flagval(newx, 1, 233)$value == 0){
      flagval(newx, 1, 233) <- last_model_timestep - flagval(newx,2,200)$value + 1
    }

    # unused - quite weird
    # a list with 2 elements - yrflags and snflags - both matrices
    # snflags is untouched - probably - dim is the same anyway
    # yrflags has been expanded by nseasons * nyears and filled with 0s
    # will it always be filled with 0s? No idea...
    # written out in MFCL script newm_io3.cpp
    # pof << "# year_flags "<<endl;
    # pof << fsh.year_flags << endl; # an imatrix
    # The dimensions are set in newmult.cpp in the class declaration:
    # year_flags(1,10,1,nyrs),
    # nyrs is the number of timesteps - get from freq file
    # Make a new matrix of the right dims, fill it with 0s
    old_dims <- dim(unused(x)[["yrflags"]]) # Nrows should be 10
    new_yrflags <- matrix(0, nrow=old_dims[1], ncol=max(freq(y)$timestep))
    #new_yrflags <- matrix(0, nrow=old_dims[1], ncol=old_dims[2] + nseasons * length(extra_years))
    # Copy across the original values (probably still just 0s)
    new_yrflags[,1:old_dims[2]] <- unused(x)[["yrflags"]]
    unused(newx)[["yrflags"]] <- new_yrflags

    # Bring region into the freq object - used later on
    region_fishery <- data.frame(region=c(region_fish(y)), fishery=1:n_fisheries(y))
    freq(y) <- merge(freq(y), region_fishery[,c("fishery","region")], all.x=TRUE)

    # We want to get the number of unique fishing incidents - used later on
    # Drop -1s in both catch and effort - shouldn't be any
    freq(y) <- freq(y)[!((freq(y)$catch < -0.5) & (freq(y)$effort < -0.5)),]
    # One method is to use unique but it's quite slow (~ 1s)
    # ufreq <- unique(freq(y)[,c("year","month","week","fishery","catch","effort","region","timestep")])
    # This is quite slow (> 1s) - alternative to using unique?
    # Or use Rob's new method
    ufreq <- realisations(y)

    # Then we have these three:
    # * rep_rate_dev_coffs
    # * q_dev_coffs
    # * effort_dev_coffs
    # They are lists of nfishery vectors, but the dimensions are a bit tricky
    # Need to dig into MFCL source code to find out

    # rep_rate_dev_coffs
    # Written out as # Reporting rate dev coffs in newm_io3.cpp
    # pof << fsh.rep_dev_coffs << endl;
    # it's a dvarmatrix 
    # rep_dev_coffs.allocate(1,num_fisheries,2,num_fish_times); # see readtag.cpp
    # # so the first dim is 1: number of fisheries - not grouped!
    # Note that second dimension goes from 2 not 1

    # q_dev_coffs
    # Written out as # catchability deviation coefficients in newm_io3.cpp
    # pof << fsh.catch_dev_coffs << endl; $ a dvar_matrix
    # declared in newmult.cpp as part of class declaration
    # catch_dev_coffs(1,nfsh,2,nft) ,
    # Assumed to be number of fisheries - not grouped
    # Note that second dimension goes from 2 not 1

    # effort_dev_coffs
    # Written out as # effort deviation coefficients in newm_io3.cpp
    # pof << fsh.effort_dev_coffs << endl; $ a dvar_matrix
    # declared in newmult.cpp as part of class declaration
    # effort_dev_coffs(1,nfsh,1,nft) ,
    # Assumed to be number of fisheries - not grouped
    # Note that second dimension goes from 1 not 2

    number_of_fishing_incidents_per_fishery <- table(ufreq$fishery)
    # The values in this table will form the dimensions of the matrices above (minus 1, because of the 2:nofi, for some of them)
    # The matrices are ragged, so here represented as a list of vectors
    # Check original lengths match the assumptions above
    # unlist(lapply(rep_rate_dev_coffs(x), length))
    # unlist(lapply(q_dev_coffs(x), length))
    # unlist(lapply(effort_dev_coffs(x), length))
    # We expand the vector of each list by the required amount
    new_rep_rate_dev_coffs <- rep_rate_dev_coffs(x)
    new_q_dev_coffs <- q_dev_coffs(x)
    new_effort_dev_coffs <- effort_dev_coffs(x)
    # Go fishery by fishery and make each vector
    for (i in 1:n_fisheries(y)){
      # Extend the vector by the difference and fill with 0s
      extra_length <- (number_of_fishing_incidents_per_fishery[i] - 1) - length(new_rep_rate_dev_coffs[[i]])
      new_rep_rate_dev_coffs[[i]] <- c(new_rep_rate_dev_coffs[[i]], rep(0,extra_length))
      new_q_dev_coffs[[i]] <- c(new_q_dev_coffs[[i]], rep(0,extra_length))
      # effort dev coffs are filled with the mean - is that always the case?
      mean_effort_dev_coff <- mean(effort_dev_coffs(x)[[i]])
      #new_effort_dev_coffs[[i]] <- c(new_effort_dev_coffs[[i]], rep(0,extra_length)) # Not filled with 0s
      new_effort_dev_coffs[[i]] <- c(new_effort_dev_coffs[[i]], rep(mean_effort_dev_coff,extra_length))
    }
    rep_rate_dev_coffs(newx) <- new_rep_rate_dev_coffs
    q_dev_coffs(newx) <- new_q_dev_coffs
    effort_dev_coffs(newx) <- new_effort_dev_coffs

    # rel_rec - fairly straightforward
    # In the par file it's # relative recruitment, in newm_io3.cpp, depends age flag 52 (currently nothing in the manual?...)
    # pof <<  exp(fsh.recr) << endl;
    # fsh.recr.allocate(2,fsh.nyears);  //NMD_19May2016 $ dvar_vector
    # it's an FLQ - window to the new size and fill extra years with 1s - will it always be 1s - who knows? not I
    # Don't know how to fill with 1s using window()
    rel_rec(newx) <- window(rel_rec(newx), end=extra_years[length(extra_years)])
    rel_rec(newx)[,as.character(extra_years)] <- 1.0

    # region_rec_var - fairly straightfoward
    # it's an FLQ - window it to the new nyears and fill extra years
    # Fill with what?
    # In par file # regional recruitment variation
    # pof << fsh.region_rec_diff_coffs << endl; # newm_io3.cpp, dvar_matrix
    # region_rec_diff_coffs(1,nyrs,1,nregions),
    # Need to expand time dimension - year only, seasons kept as before
    region_rec_var(newx) <- window(region_rec_var(newx), end=extra_years[length(extra_years)])
    region_rec_var(newx)[,as.character(extra_years)] <- 0.0
    # Reproduce Rob's code in other generate() and fill up extra years with something
    region_rec_var(newx)[,as.character(range(x)['maxyear']:(range(newx)['maxyear']-1))] <- apply(region_rec_var(newx)[,as.character(range(x)['minyear']:(range(x)['maxyear']-1))], c(1,3,4,5,6), mean)

    # My attempt at reproducing Yukio's code
    # Catch dev coffs
    # Written to par file as # The grouped_catch_dev in lmul_io4.cpp
    # pof << "# The grouped_catch_dev_coffs" << endl;
    # pof << fsh.grouped_catch_dev_coffs << endl;
    # grouped_catch_dev_coffs is a dvar_matrix
    # grouped_catch_dev_coffs.allocate(1,ngroups,2,num_grouped_fish_times); (see grpcatch.cpp:)
    # dims are ngroups - fishing incidentes per group
    # ngroups is:
    # ngroups=max(grouping);
    # ivector grouping=column(fish_flags,29); # Fish flag 29!
    # A list of length groups
    # Each element is a vector of the number of incidents in each group
    # Groups given by fish flag 29
    ffl29 <- flagval(newx,-(1:n_fisheries(newx)),29)$value  # Obtain fish flags(29) determining the grouping of catchbility
    groups <- sort(unique(ffl29))
    no_grps <- length(groups) # should match length(catch_dev_coffs(newx))
    # Loop over group
    for(grp in groups){
      # ufreq has fishing incidents by fishery
      incidents_by_group <- length(unique(ufreq[ufreq$fishery %in% which(ffl29 == grp),"timestep"]))
      new_incidents_by_group <- incidents_by_group - length(catch_dev_coffs(newx)[[grp]]) - 1 # -1 as we go from 2:no_incidents
      catch_dev_coffs(newx)[[grp]] <- c(catch_dev_coffs(newx)[[grp]] , rep(0, new_incidents_by_group))
    }

    # sel_dev_coffs - a massive matrix
    # in Par # selectivity deviation coefficients
    # pof <<tarr << endl; # newm_io3.cpp, tarr is
    # d3_array tarr(1,fsh.num_fisheries,2,fsh.num_fish_times,1,fsh.nage); # newm_io3.cpp
    # In FLR4MFCL it's a matrix (sum(no_incidents per fishery - 1) x ages)
    # Need to increase the number of rows and set new data to 0
    # Dim of the matrix should be the number of fishing incidents
    # Subtract 1 from the number of fishing incidents again
    new_sel_dev_coffs <- matrix(0, nrow=sum(number_of_fishing_incidents_per_fishery - 1), ncol=ncol(sel_dev_coffs(x)))
    # Copy old matrix in
    new_sel_dev_coffs[1:nrow(sel_dev_coffs(x)),] <- sel_dev_coffs(x)
    sel_dev_coffs(newx) <- new_sel_dev_coffs

    # sel_dev_coffs2 - list length 23, each element is a matrix
    # Expand number of rows to include number of observations, fill extra rows with 0
    # In par # sel_dev_coffs
    # pof << fsh.sel_dev_coffs(i,j); # newm_io3.cpp, dvar3_array
    # In newmult.cpp
    # sel_dev_coffs(1,nfsh,1,nft,1,ng), # no fisheries, number of fishing times, nages
    for (i in 1:n_fisheries(y)){
      new_sel_dev_coffs2 <- matrix(0, nrow=number_of_fishing_incidents_per_fishery[i], ncol=ncol(sel_dev_coffs2(x)[[i]]))
      new_sel_dev_coffs2[1:nrow(sel_dev_coffs2(x)[[i]]),] <- sel_dev_coffs2(x)[[i]]
      sel_dev_coffs2(newx)[[i]] <- new_sel_dev_coffs2
    }

    # fm_level_devs - a vector of character strings - this one is weird.
    # in par # fm_level_devs, newm_io3.cpp
    # pof << setscientific()<< setprecision(14) << fsh.fm_level_devs << endl;
    # (see htotcafi.cpp: fm_level_devs.allocate(1,num_fisheries,2,missing_catch_by_fishery_flag);)
    # Each line is a fishery, but only fisheries which have a catch == < -0.5 in the freq table, i.e. missing catch by fishery
    # The length of each vector is the number of catch < -0.5 incidences in the freq file MINUS 1 
    # The MINUS 1 is important because for some reason in the MFCL code the dimension of the object is 2:something, not 1:something
    number_of_negcatch_incidents_by_fishery <- table(freq(y)[freq(y)$catch < -0.5,"fishery"])
    vector_length <- number_of_negcatch_incidents_by_fishery-1
    vector_length <- vector_length[vector_length > 0] # drop any length less than 1
    # Which fisheries have missing catches in original data
    orig_number_of_negcatch_incidents_by_fishery <- table(freq(y)[(freq(y)$catch < -0.5) & (freq(y)$year <= last_original_year ),"fishery"])
    orig_vector_length <- orig_number_of_negcatch_incidents_by_fishery-1
    orig_vector_length <- orig_vector_length[orig_vector_length > 0] # drop any length less than 1
    # Really horrible for loop to load the new vectors
    fm_level_devs(newx) <- vector(mode="character",length(vector_length))
    # Make a series of vectors of this length, character strings of 0.0x0e+00 , separated by 0
    # Bit of a pain in the arse
    zero_string <- " 0.00000000000000e+00"
    for (i in 1:length(vector_length)){
      fishery <- names(vector_length)[i]
      # Append or make new
      # If fishery already in fm_level_devs, add extra 0s to existing data
      if (fishery %in% names(orig_vector_length)){
        nzeros <- vector_length[fishery] - orig_vector_length[fishery]
        existing_data <- fm_level_devs(x)[which(names(orig_vector_length) == fishery)]
        fm_level_devs(newx)[i] <- paste(existing_data, paste(rep(zero_string, nzeros), collapse=""), sep="", collapse="")
          #paste(existing_data, paste(rep(zero_string, nzeros), collapse=" "), sep=" ", collapse="")
      }
      # Otherwise make new
      else {
        fm_level_devs(newx)[i] <- paste(rep(zero_string, vector_length[i]), collapse="")
      }
    }

    # langrangian - end of level boss - character
    # A 3d matrix printed in 2d form
    # 1:nregion
    # 1:timestep
    # 1:fishing incident
    # Each line is a region / timestep
    # Each row has the number of fisheries operating in that region in that timestep
    # What is the order? All region 1, all timesteps, regions 2 all timesteps, OR timestep 1 all regions, timestep2 all regions etc
    # The new lines are 0 (as far as I can tell) but there is no guarantee that the original ones are
    # Also, what if no fishing incident in that time step in that region? Empty line? Missing line?
    incidents_by_region_timestep <- table(ufreq[,c("region","timestep")])
    # Now go column by column to get the number of entries in a row (maybe just the new ones are 0)
    # Append new years onto existing lagrangian Tricky because the order is awkward
    # i.e. we have all region 1, then all region 2, so we need to insert into the right places
    # Go region by region
    #orig_years <- range(x)["minyear"]:range(x)["maxyear"]
    # Subset out the timesteps of original years - this will give you the original lagrangian
    # Then append the additional rows
    nolines_by_fishery <- apply(incidents_by_region_timestep > 0, 1, sum)
    orig_timesteps <- 1:dimensions(x)["years"]
    extra_timesteps <- (dimensions(x)["years"]+1):dimensions(newx)["years"]
    nolines_by_fishery_orig <- apply(incidents_by_region_timestep[,orig_timesteps] > 0, 1, sum)
    startlines_by_fishery_orig <- c(1, cumsum(nolines_by_fishery_orig)+1)
    startlines_by_fishery <- c(1, cumsum(nolines_by_fishery)+1)
    # Make the new lagrangian
    new_lagrangian <- character()
    for (i in 1:n_regions(y)){
      # Grab the original lines and put into the new object
      orig_lines <- startlines_by_fishery_orig[i]:(startlines_by_fishery_orig[i] + nolines_by_fishery_orig[i]-1)
      new_lagrangian <- c(new_lagrangian,lagrangian(x)[orig_lines])
      ol <- lagrangian(x)[orig_lines]
      # Make the extra lines and append
      extra_incidents <- c(incidents_by_region_timestep[i,extra_timesteps])
      # Drop any elements with a count of 0
      extra_incidents <- extra_incidents[extra_incidents > 0]
      # Each element is the number of times we have a 0 in the row
      #unname(unlist(lapply(extra_incidents, function(xx) paste("  ", rep("0", xx), collapse="   ", sep=""))))
      extra_lines <- unname(unlist(lapply(extra_incidents, function(xx) paste("  ",paste(rep("0", xx), collapse="   "),sep=""))))
      #all_lines <- c(orig_lines, extra_lines)
      new_lagrangian <- c(new_lagrangian, extra_lines)
    }
    lagrangian(newx) <- new_lagrangian
    # Check for # Other lambdas for augmented Lagrangian
    # check that "other lambdas ..." are not specified for par files < 1053
    #if(flagval(x, 1, 200)$value<1053 & length(grep("# Other lambdas", lagrangian(x)))>0)
    #  lagrangian(newx) <- lagrangian(x)[1:(grep("Other lambdas", lagrangian(x))-1)]
    # Check for Other lambdas and if it's there, include it, not sure if we really it.
    # Also no idea if I am supposed to edit the length of the vector
    if(any(grepl("# Other lambdas", lagrangian(x)))){
      # Grab the last two lines
      lagrangian(newx) <- c(lagrangian(newx), lagrangian(x)[(length(lagrangian(x))-1):length(lagrangian(x))])
    }
    return(newx)
  }
)


#' @rdname generate
setMethod("generate", signature(x="MFCLPar", y="MFCLPar", z="MFCLTagProj"), 
          function(x, y, z, ...){
            #browser()
            # does 'generate' as above but puts values into the tag reporting rate stuff rather than zeoes
            # x is the original par and y is the par generated from 00.par
            xdims <- dim(tag_fish_rep_rate(x))
            ydims <- dim(tag_fish_rep_rate(y))
            modrows <- (ydims[1]-(ydims[1]-xdims[1])):(ydims[1]-1)
            
            proj.yrs <- seq(range(x)['maxyear']+1, range(x)['maxyear']+(dimensions(y)[2]-dimensions(x)[2])/dimensions(x)[3])
            
            # fill the reporting rate priors with the reporting rate used for tag data generation (ie from the MFCLTagProj object)
            # remember to keep the final row for the pooled tag settings
            #tag_fish_rep_rate(y) <- rbind(tag_fish_rep_rate(x)[-xdims[1],], t(rep_rate_proj(z)), tag_fish_rep_rate(x)[xdims[1],])
            
            tag_fish_rep_rate(y)[1:(xdims[1]-1),] <- tag_fish_rep_rate(x)[1:(xdims[1]-1),]
            tag_fish_rep_rate(y)[modrows,]        <- t(rep_rate_proj(z)) 
            tag_fish_rep_rate(y)[ydims[1],]       <- tag_fish_rep_rate(x)[xdims[1],]
            
            # use the same reporting rate groupings as for previous recaptures but increment for "simulated data" programme
            #tag_fish_rep_grp(y)  <- rbind(tag_fish_rep_grp(x)[-xdims[1],], 
            #                                 matrix(max(tag_fish_rep_grp(x))+tag_fish_rep_grp(x)[1,], 
            #                                        ncol=xdims[2], nrow=release_groups_proj(z), byrow=T),
            #                                 tag_fish_rep_grp(x)[xdims[1],])
            tag_fish_rep_grp(y)[modrows,] <- matrix(max(tag_fish_rep_grp(x))+tag_fish_rep_grp(x)[1,], 
                                                    ncol=xdims[2], nrow=release_groups_proj(z), byrow=T)
            
            # for the rest - use the first row of the matrix as the basis for extending for simulated data
            for(ss in list("tag_fish_rep_flags", "tag_fish_rep_pen", "tag_fish_rep_target"))
              #slot(y, ss)[modrows,] <- matrix(slot(x, ss)[1,], ncol=xdims[2], nrow=release_groups_proj(z), byrow=T)
              slot(y, ss) <- rbind(slot(x, ss)[-xdims[1],], 
                                      matrix(slot(x, ss)[1,], ncol=xdims[2], nrow=release_groups_proj(z), byrow=T),
                                      slot(x, ss)[xdims[1],])
            
            # add new tag flags for the new tag release groups
            max_tags_orig <- max(abs(flags(x)[is.element(flags(x)$flagtype, -10000:-99999),'flagtype']))
            #flags(y)      <- rbind(flags(y), data.frame(flagtype=rep(-(max_tags_orig+1:(xdims[1]-(max_tags_orig-10000))),each=10), flag=1:10, value=c(1,rep(0,9))))
            flags(y)[is.element(flags(y)$flagtype, -max_tags_orig:-99999) & flags(y)$flag==1, 'value'] <- 1
            
            #dimensions(y)['taggrps'] <- xdims[1]-1
            
            # set future rec_standard and rec_orthogonal values to the mean of the historical values.
            if(version(x)>=1064){
              rec_standard(y) <- window(rec_standard(x), start=range(x)['minyear'], end=range(y)['maxyear'])
              rec_standard(y)[,as.character(proj.yrs)] <- apply(rec_standard(x), c(1,3,4,5,6), mean, na.rm=T)
              
              rec_orthogonal(y) <- window(rec_orthogonal(x), start=range(x)['minyear'], end=range(y)['maxyear'])
              rec_orthogonal(y)[,as.character(proj.yrs)] <- apply(rec_orthogonal(x), c(1,3,4,5,6), mean, na.rm=T)
              
              rec_standard_dim(y) <- c(dim(rec_standard(y))[5], prod(dim(rec_standard(y))[c(2,4)]))
            }
            
            
            return(y)
          }
)




#' @rdname generate
#' There's not much to this one - maybe not necessary.
#' 
setMethod("generate", signature(x="MFCLTag", y="MFCLTag", z="missing"), 
          function(x, y, z, ...){
            
           # seems so 
          }
)




#' @rdname generate
setMethod("generate", signature(x="MFCLFrq", y="MFCLPseudo", z="MFCLTag"), 
          function(x, y, z, eff_crp_mult, ...){
            
            newfrq  <- x + y 
            
            # Need to reorder newfrq by year, fishery, season, length so that eff_crp_mult works
            freq(newfrq) <- freq(newfrq)[order(freq(newfrq)$year, freq(newfrq)$fishery, freq(newfrq)$month, freq(newfrq)$length),]
            
            # Downscale effort so that effort has not been subject to effort creep so estimation model has perceived effort not effective effort
            freq(newfrq)[freq(newfrq)$year>=fprojyr(mseCtrl) & is.element(freq(newfrq)$fishery, effort_creep_fish(mseCtrl)), "effort"] <-
              freq(newfrq)[freq(newfrq)$year>=fprojyr(mseCtrl) & is.element(freq(newfrq)$fishery, effort_creep_fish(mseCtrl)), "effort"] / eff_crp_mult
            
            n_tag_groups(newfrq) <- release_groups(z)
            
            data_flags(newfrq)[c(2,3),] <- 0
            
            return(newfrq)
          }
)



#' @rdname generate
#' 
setMethod("generate", signature(x="MFCLIni", y="MFCLMSEControl", z="MFCLTag"), 
          function(x, y, z, z2="missing", ...){
            
            if(!is.element("MFCLTag", is(z2)))
              stop("argument z2 should be an object of class 'MFCLTag'. (z=original tag object; z2=new tag object)")
            
            newini  <- x
            mseCtrl <- y
            tag     <- z
            newtag  <- z2
            
            tagrepvals <- list(tag_fish_rep_rate  = tag_fish_rep_rate(mseCtrl),  
                               tag_fish_rep_grp   = c(max(tag_fish_rep_grp(newini)) + c(tag_fish_rep_grp(newini)[1,])), 
                               tag_fish_rep_flags = 1, 
                               tag_fish_rep_target= 50,
                               tag_fish_rep_pen   = 1)
            
            for(ss in names(tagrepvals))  {
              slot(newini, ss) <- rbind(slot(ini, ss)[1:release_groups(tag),], 
                                        t(matrix(tagrepvals[[ss]], ncol=release_groups(newtag)-release_groups(tag), nrow=ncol(slot(ini, ss)), byrow=F)), 
                                        slot(ini, ss)[release_groups(tag)+1,])
            }
          }
)








