#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

checkMissingRealisations <- function(frq, projcontrols, quantity, year.range, replace.val, penalty.val){
  # check the freq table for missing realisations and if exists fill them with appropriate replacement values
  # run seperately for either catch or effort - because the replacement values may differ
  
  if(quantity=="catch"){  
    catch_fisheries <- which(controls(projCtrl)$caeff == 1)
    catch_tab <- table(freq(frq)[(freq(frq)$year %in% year.range) & (freq(frq)$fishery %in% catch_fisheries),
                                 c("year","fishery","month")])
    # If missing values, fill them in
    if (any(catch_tab==0)){
      # Catch based fisheries should not have missing catches -  Look for 0s
      missing_catches <- which(catch_tab==0, arr.ind=TRUE)
      # For each row of missing_catches, insert a row into freq(frq)
      for (i in 1:nrow(missing_catches)){
        freq(frq) <- rbind(freq(frq),
                           data.frame( year = as.numeric(dimnames(catch_tab)$year[missing_catches[i,"year"]]),
                                       month = as.numeric(dimnames(catch_tab)$month[missing_catches[i,"month"]]),
                                       week = 1,
                                       fishery = as.numeric(dimnames(catch_tab)$fishery[missing_catches[i,"fishery"]]),
                                       catch = replace.val, # NULL value - needs to exist but be so small as to have no impact
                                       effort = -1,
                                       penalty = penalty.val, # use 0.05 - because... - tried with -1 but failed
                                       length = NA, weight = NA, freq = -1))
      }
    }
  }
  if(quantity=="effort"){
    # Do the same for effort
    effort_fisheries <- which(controls(projCtrl)$caeff == 2)
    effort_tab <- table(freq(frq)[(freq(frq)$year %in% year.range) & (freq(frq)$fishery %in% effort_fisheries),
                                  c("year","fishery","month")])
    if (any(effort_tab==0)){
      missing_effort <- which(effort_tab==0, arr.ind=TRUE)
      for (i in 1:nrow(missing_effort)){
        freq(frq) <- rbind(freq(frq),
                           data.frame(year = as.numeric(dimnames(effort_tab)$year[missing_effort[i,"year"]]),
                                      month = as.numeric(dimnames(effort_tab)$month[missing_effort[i,"month"]]),
                                      week = 1,
                                      fishery = as.numeric(dimnames(effort_tab)$fishery[missing_effort[i,"fishery"]]),
                                      catch = -1, # NULL value - needs to exist but be so small as to have no impact
                                      effort = replace.val,
                                      penalty = penalty.val,# use -1  # tried with 0.05 but failed - who knows?
                                      length = NA, weight = NA, freq = -1))
      }
    }
  }
  lf_range(frq)['Datasets'] <- nrow(realisations(frq))
  return(frq)
}
