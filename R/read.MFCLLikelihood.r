#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2020  Rob Scott


#setwd("/home/rob/MSE/ofp-sam-skipjack_MSE/OM_2019/A1B1C1D0E0_fit")


read.MFCLLikelihood <- function(tpofile="test_plot_output"){
  
  trim.leading  <- function(x) sub("^\\s+", "", x)
  splitter      <- function(ff, tt, ll=1) unlist(strsplit(trim.leading(ff[grep(tt, ff)[1]+ll]),split="[[:blank:]]+"))
  
  res <- MFCLLikelihood()
  tpo <- readLines(tpofile)
  
  n_len_samp_fsh_marker <- grep("length-sample components of likelihood for fishery", tpo)
  n_len_samp_fsh        <- lapply(strsplit(tpo[n_len_samp_fsh_marker+1], split="[[:blank:]]+"), as.numeric)

  n_wgt_samp_fsh_marker <- grep("weight-sample components of likelihood for fishery", tpo)
  n_wgt_samp_fsh        <- lapply(strsplit(tpo[n_wgt_samp_fsh_marker+1], split="[[:blank:]]+"), as.numeric)    

  n_catch_fsh_marker <- grep("total catch components of likelihood for fishery", tpo)
  n_catch_fsh        <- lapply(strsplit(tpo[n_catch_fsh_marker+1], split="[[:blank:]]+"), as.numeric)    
  
  n_tag_rel_marker <- grep("# tag release", tpo)
  # work out the number of tag fish groups based on the number of lines between report sections - skj has 31 fisheries but 27 tag groups
  n_tag_fsh_grps   <- (n_tag_rel_marker[2]-n_tag_rel_marker[1]-1)/2  
  inc              <- c(seq(2, length=27, by=2 ))
  
  tag_likelihood <- list()
  for(i in 1:length(n_tag_rel_marker)){
    p1 <- lapply(tpo[n_tag_rel_marker[i]+inc], trim.leading)
    tag_likelihood[[i]] <- lapply(strsplit(unlist(p1), split="[[:blank:]]+"), as.numeric)
  }
  
  slot(res, 'bh_steep_contrib')   <- as.numeric(splitter(tpo, "BH_steep"))
  slot(res, 'effort_dev_penalty') <- as.numeric(splitter(tpo, "Effort_dev_penalty_by_fishery"))
  slot(res, 'q_dev_pen_fish')     <- as.numeric(splitter(tpo, "catchability_dev_penalty_by_fishery"))
  slot(res, 'q_dev_pen_fish_grp') <- as.numeric(splitter(tpo, "catchability_dev_penalty_by_group"))
  slot(res, 'total_length_fish')  <- as.numeric(splitter(tpo, "total length component of likelihood for each fishery"))
  slot(res, 'length_fish')        <- n_len_samp_fsh
  slot(res, 'total_weight_fish')  <- as.numeric(splitter(tpo, "total weight component of likelihood for each fishery"))
  slot(res, 'weight_fish')        <- n_wgt_samp_fsh
  slot(res, 'total_catch_fish')   <- as.numeric(splitter(tpo, "total catch component of likelihood for each fishery"))
  slot(res, 'catch_fish')         <- n_catch_fsh
  slot(res, 'tag_rel_fish')       <- tag_likelihood
  
  slot(res, 'dimensions')          <- unlist(list(agecls=as.numeric(NA), years=NA, seasons=NA, regions=NA, 
                                                  fisheries=length(n_catch_fsh_marker), taggrps=length(tag_likelihood)))
  return(res)
}


