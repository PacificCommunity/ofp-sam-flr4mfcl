

#####################################################
##
## Functions to produce - Skipjack single area MFCL input files for regions 2,3 and 5
## Uses tag data from the PTTP only
##
## 12 fleets, 1 area, 
## corresponding doitall - single_area_doitall_v1.skj
##
## 09/01/2017 - rds
##
##
######################################################


setGeneric('reduce', function(obj, ...) standardGeneric('reduce')) 


setMethod("reduce", signature(obj='MFCLFrq'),
          function(obj, fisheries=1:n_fisheries(obj), ...){
            
  frq2 <- obj
  n_regions(frq2)     <- 1
  n_fisheries(frq2)   <- length(fisheries)
  #n_tag_groups(frq2)  <- release_groups(tag2)
  region_size(frq2)   <- FLQuant(1)
  region_fish(frq2)[] <- 1
  region_fish(frq2)   <- trim(region_fish(frq2), unit=fisheries)
  data_flags(frq2)    <- data_flags(obj)[,fisheries]
  move_matrix(frq2)   <- matrix()
  season_flags(frq2)  <- t(season_flags(obj)[,1])
  move_weeks(frq2)    <- 1
  n_move_yr(frq2)      <- 1
  
  # chop out the fisheries in regions 1 and 4
  freq(frq2) <- freq(obj)[is.element(freq(obj)$fishery, fisheries),]
  
  # renumber the fisheries in the freq
  #freq(frq2)$fishery <- freq(frq2)$fishery-3   ### this is a hack - be careful
  freq(frq2)$fishery <- rep(1:length(fisheries), table(freq(obj)$fishery)[fisheries])
  
  lf_range(frq2)["Datasets"] <- nrow(freq(frq2)[freq(frq2)$length==range(frq2)["min"] | is.na(freq(frq2)$length),])
  
  return(frq2)
})


setMethod("reduce", signature(obj='MFCLTag'),
          function(obj, fisheries=4:15, regions=c(2,3,5), programs="PTTP", ...){
  
  # remove all releases from regions 1 and 4
  #releases.new <- releases(obj)[is.element(releases(obj)$region, regions & is.element(releases(obj)$program, programs)),]
  releases.new <- releases(obj)[is.element(releases(obj)$region, regions),]
  
  # remove all recaptures for releases from regions 1 and 4 and recaptures in 1 and 4
  recaptures.new <- recaptures(obj)[is.element(recaptures(obj)$region,          regions)   & 
                                      is.element(recaptures(obj)$recap.fishery, fisheries),] # &
                                      #is.element(recaptures(obj)$program,       "PTTP"),] 
  
  # Ideally release tags should be reduced to account for tags recaptured outside of assessment area (regions 1 and 4) - but only 1.5% so skipping for the time being.
  
  # mapping the release groups 
  rel.map <- cbind(unique(releases.new$rel.group), 1:length(unique(releases.new$rel.group)))
  
  # renumber the release groups (making sure to match the release group numbers for both releases and recaptures)
  releases.new$rel.group   <- match(releases.new$rel.group,   unique(releases.new$rel.group))
  
  new.rec.rel.grp <- rel.map[is.element(rel.map[,1],recaptures.new$rel.group),2]
  recaptures.new$rel.group <- rep(new.rec.rel.grp, table(recaptures.new$rel.group))
  
  # renumber the recapture fisheries
  recaptures.new$recap.fishery <- recaptures.new$recap.fishery-3    # this is a hack - be careful- may not work for different fishery selections
  
  releases.new$region <- 1    # since we only have one assessment region now
  recaptures.new$region <- 1
  
  # put revised stuff into new tag object
  tag2 <- obj
  release_groups(tag2) <- max(releases.new$rel.group)
  
  # re-calculate number of recapture strata for each tag group
  rr   <- rep(0, release_groups(tag2))
  rval <- tapply(recaptures.new$recap.number, recaptures.new$rel.group, length)
  rr[as.numeric(names(rval))] <- rval
  
  recoveries(tag2) <- rr
  
  releases(tag2)   <- releases.new
  recaptures(tag2) <- recaptures.new
  
  return(tag2)
})


setMethod("reduce", signature(obj='MFCLIni'),
          function(obj, tag, fisheries, regions, programs, ...){
  
  ini2 <- obj
  
  # identify the release groups you have removed.
  #release.grps.in <- rel.map[,1]  #unique(releases(tag)[is.element(releases(tag)$region, c(2,3,5)),]$rel.group)
  releases.new <- releases(tag)[is.element(releases(tag)$region, regions) & is.element(releases(tag)$program, programs),]
  rel.map <- cbind(unique(releases.new$rel.group), 1:length(unique(releases.new$rel.group)))
  release.grps.in  <- c(is.element(1:release_groups(tag), rel.map[,1]), TRUE)
  
  # chop out the tag reporting stuff for the release groups you have removed.
  tag_fish_rep_rate(ini2)   <- tag_fish_rep_rate(obj)[release.grps.in,  fisheries]
  tag_fish_rep_grp(ini2)    <- tag_fish_rep_grp(obj)[release.grps.in,   fisheries]
  tag_fish_rep_flags(ini2)  <- tag_fish_rep_flags(obj)[release.grps.in, fisheries]
  tag_fish_rep_target(ini2) <- tag_fish_rep_target(obj)[release.grps.in,fisheries]
  tag_fish_rep_pen(ini2)    <- tag_fish_rep_pen(obj)[release.grps.in,   fisheries]
  
  # re-order the tag rep group numbers to start from 1 and be continuous
  tag_fish_rep_grp(ini2) <- matrix(match(tag_fish_rep_grp(ini2), sort(unique(c(tag_fish_rep_grp(ini2))))), 
                                   nrow=nrow(tag_fish_rep_grp(ini2)))
  
  move_map(ini2)   <- 0
  diff_coffs(ini2) <- matrix(0)
  rec_dist(ini2)   <- 1
  
  return(ini2)
})





