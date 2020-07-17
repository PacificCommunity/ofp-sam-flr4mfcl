#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott


# x    <- read.MFCLPar("C://temp//bet_2014_assessment//Model_runs//2014s//Run143//10.par", first.yr=1952)
# file <- "C://temp//par_crap"
# append <- FALSE
# source("C://R4MFCL//FLR4MFCL//R//write.MFCLPar.r")

# write.par(x, file=file)




##########################################################################
#
#  UNEXPORTED FUNCTIONS
#
##########################################################################

write.par <- function(x, file, append=F, ...){

  xfish    <- sum(flagval(x, -1:-dimensions(x)['fisheries'], 71)$value) # sum ff71 to check for selectivity blocks  # RDS 27/02/20
  
  float <- function(x,ZeroPrint="0.00000000000000e+00") formatC(x, digits=14, format='e',drop0trailing=FALSE,zero.print=ZeroPrint)

  cat("# The parest_flags \n",                file=file, append=append)
  cat(paste(formatC(flags(x)[flags(x)$flagtype==1,'value'], format='d'), collapse=' '), file=file, append=T)
  cat("\n \n# The number of age classes \n",  file=file, append=T)
  cat(dimensions(x)["agecls"],                file=file, append=T)
  cat("\n# age flags \n",                     file=file, append=T)
  cat(paste(formatC(flags(x)[flags(x)$flagtype==2,'value'], format='d'), collapse=' '), file=file, append=T)

  cat("\n \n# fish flags \n",                 file=file, append=T)
  write.table(t(matrix(formatC(flags(x)[is.element(flags(x)$flagtype,format='d'), -1:-dimensions(x)['fisheries']),'value'],
                       ncol=dimensions(x)['fisheries'])), row.names=F, col.names=F, file=file, append=T, quote=FALSE)
  if(!all(is.na(flags(x)[is.element(flags(x)$flagtype, -10000:-(10000+dimensions(x)["taggrps"]-1)),'value']))){
    cat("# tag flags\n",                       file=file, append=T)
    write.table(matrix(flags(x)[is.element(flags(x)$flagtype, -10000:-(10000+dimensions(x)["taggrps"]-1)),'value'],
                         ncol=10, byrow=TRUE), #dimensions(x)['taggrps'])),
                         row.names=F, col.names=F, file=file, append=T)

    cat("\n# tag fish rep\n\n",  file=file, append=T)
    write.table(tag_fish_rep_rate(x), row.names=F, col.names=F, file=file, append=T)

    cat("\n# tag fish rep group flags\n", file=file, append=T)
    write.table(tag_fish_rep_grp(x),  row.names=F, col.names=F, file=file, append=T)

    cat("# tag_fish_rep active flags \n", file=file, append=T)
    write.table(tag_fish_rep_flags(x), row.names=F, col.names=F, file=file, append=T)

    cat("# tag_fish_rep target\n", file=file, append=T)
    write.table(tag_fish_rep_target(x), row.names=F, col.names=F, file=file, append=T)

    cat("\n# tag_fish_rep penalty\n", file=file, append=T)
    write.table(tag_fish_rep_pen(x), row.names=F, col.names=F, file=file, append=T)
  }
  cat("\n# region control flags \n",file=file, append=T); #write.table(control_flags(x), row.names=F, col.names=F, file=file, append=T)
  write.table(t(array(flagval(x, -100000:-100009,1:dimensions(x)["regions"])$value, dim=c(dimensions(x)["regions"], 10))),
              col.names=F, row.names=F, file=file, append=T)

  #cat("# percent maturity \n ",   file=file, append=T); cat(float(as.vector(aperm(mat(x), c(4,1,2,3,5,6)))), file=file, append=T)
  cat("# percent maturity \n ",   file=file, append=T); cat(float(as.vector(mat(x))), file=file, append=T)  # RDS 28/04/2020
  
  cat(paste("\n# total populations scaling parameter   \n", float(tot_pop(x))),  file=file, append=T)
  cat(paste("\n# implicit total populations scaling parameter   \n", float(tot_pop_implicit(x))),  file=file, append=T)
  cat(paste("\n# rec init pop level difference    \n", float(rec_init_pop_diff(x))),  file=file, append=T)
  cat(paste("\n# recruitment times    \n", paste(rec_times(x), collapse=" ")),  file=file, append=T)

  cat("\n# relative recruitment \n#\n ",   file=file, append=T)
  #cat(formatC(as.vector(aperm(rel_rec(x), c(4,2,1,3,5,6))),format="e", digits=12), file=file, append=T)
  cat(float(as.vector(aperm(rel_rec(x), c(4,2,1,3,5,6)))[-1]), file=file, append=T) # drop the first because it is now an NA

  cat("\n\n# Lambdas for augmented Lagrangian \n#\n ", file=file, append=T)
  cat(unlist(lapply(lagrangian(x), paste, "\n")), file=file, append=T)

  cat("\n# Reporting rate dev coffs \n#\n ",   file=file, append=T)
  cat(unlist(lapply(lapply(rep_rate_dev_coffs(x), paste, collapse=" "), paste, "\n")), file=file, append=T)

  cat("\n# availability coffs \n# \n",   file=file, append=T)
  cat(as.vector(aperm(availability_coffs(x), c(4,1,2,3,5,6))), file=file, append=T)

  cat("\n \n# relative initial population \n \n",   file=file, append=T)
  write.table(float(rel_ini_pop(x)), file=file, append=T, col.names=F, row.names=F,quote=F)

  cat("# fishery selectivity \n",   file=file, append=T)
  write.table(float(t(array(aperm(fishery_sel(x), c(4,1,5,2,3,6)), dim=c(dimensions(x)['agecls'],dimensions(x)['fisheries']+xfish))),"0"),   # RDS 27/02/20
              file=file, append=T, col.names=F, row.names=F,quote=F)

  cat("# age-dependent component of fishery selectivity  \n",   file=file, append=T)
  write.table(t(array(aperm(fishery_sel_age_comp(x), c(4,1,5,2,3,6)), dim=c(dimensions(x)['agecls'],dimensions(x)['fisheries']))),
              file=file, append=T, col.names=F, row.names=F,quote=F)

  cat(paste("\n# natural mortality coefficient  \n# \n", paste(float(m(x)), collapse=" ")),  file=file, append=T)

  cat("\n# average catchability coefficients  \n# \n\n ",   file=file, append=T)
  cat(float(as.vector(av_q_coffs(x))), file=file, append=T)

  cat("\n \n# initial trend in catchability coefficients  \n# \n ",   file=file, append=T)
  cat(float(as.vector(ini_q_coffs(x))), file=file, append=T)

  cat("\n# q0_miss    \n# \n ",   file=file, append=T)
  cat(float(as.vector(q0_miss(x))), file=file, append=T)

  cat("\n# fm_level_devs     \n",   file=file, append=T)
  cat(unlist(lapply(fm_level_devs(x), paste, "\n")), file=file, append=T)

  cat(paste("# movement map \n", paste(move_map(x), collapse=" ")),  file=file, append=T)

  cat("\n# movement coefficients \n",   file=file, append=T)
  write.table(float(diff_coffs(x)), col.names=F, row.names=F, file=file, append=T,quote=F)

  if(version(x)>=1059){
    cat("# xmovement coefficients \n",   file=file, append=T)
    write.table(float(xdiff_coffs(x)), col.names=F, row.names=F, file=file, append=T,quote=F)
  }
  cat("# movement matrices \n",   file=file, append=T)

#  for(period in 1: dimensions(x)['seasons']){
  for(period in 1:dim(diff_coffs_age_period(x))[4]){       ## RDS 29/02/2020  
    for(age in 1:dimensions(x)['agecls']){
      cat(paste("# Movement period", period, " age class", age, "\n"), file=file, append=T)
      write.table(float(as.array(diff_coffs_age_period(x)[,,age,period], dim=rep(dimensions(x)['regions'],2))),
                  row.names=F, col.names=F, file=file, append=T,quote=F)
    }
  }

  cat("# age dependent movement coefficients \n",   file=file, append=T)
  write.table(float(diff_coffs_age(x)), col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("# nonlinear movement coefficients \n",   file=file, append=T)
  write.table(float(diff_coffs_nl(x)), col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("# Movement coefficients priors\n",   file=file, append=T)
  write.table(float(diff_coffs_priors(x)), col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("# age dependent movement coefficients priors \n",   file=file, append=T)
  write.table(float(diff_coffs_age_priors(x)), col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("# nonlinear movement coefficients priors \n",   file=file, append=T)
  write.table(float(diff_coffs_nl_priors(x)), col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("# regional recruitment variation  \n",   file=file, append=T)
  write.table(float(matrix(as.vector(aperm(region_rec_var(x), c(4,2,5,1,3,6))), ncol=dimensions(x)['regions']),ZeroPrint="0"),
              col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("\n# effort deviation coefficients   \n",   file=file, append=T)
  cat(unlist(lapply(lapply(lapply(effort_dev_coffs(x),float,ZeroPrint="0"), paste, collapse=" "), paste, "\n")),file=file, append=T)

  cat(paste("\n# correlation in selectivity deviations    \n", paste(as.vector(sel_dev_corr(x)), collapse=" ")),  file=file, append=T)

  cat("\n \n# extra fishery parameters   \n# \n \n",   file=file, append=T)
  write.table(float(fish_params(x),ZeroPrint="0"),  col.names=F, row.names=F, file=file, append=T,quote=F)

  if(version(x)>=1052){
    cat("\n \n# species parameters \n \n",   file=file, append=T)
    write.table(slot(x, 'spp_params'),  col.names=F, row.names=F, file=file, append=T,quote=F)
  }

  cat("\n \n# seasonal_catchability_pars     \n",   file=file, append=T)
  write.table(season_q_pars(x),  col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("\n \n# age-class related parameters (age_pars)   \n#  \n",   file=file, append=T)
  temp <- array(0, dim=c(10, dimensions(x)['agecls']))
  temp[2,] <- as.vector(aperm(m_devs_age(x),        c(4,1,2,3,5,6)))
  temp[3,] <- as.vector(aperm(growth_devs_age(x),   c(4,1,2,3,5,6)))
  temp[4,] <- as.vector(aperm(growth_curve_devs(x), c(4,1,2,3,5,6)))
  temp[5,] <- as.vector(aperm(log_m(x),             c(4,1,2,3,5,6)))
  write.table(float(temp), col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("\n \n# region parameters      \n",   file=file, append=T)
  write.table(float(region_pars(x),ZeroPrint="0"),  col.names=F, row.names=F, file=file, append=T,quote=F)

  cat("\n# catchability deviation coefficients  \n# \n",   file=file, append=T)
  cat(unlist(lapply(lapply(q_dev_coffs(x), paste, collapse=" "), paste, "\n")), file=file, append=T)

  cat("\n \n# selectivity deviation coefficients    \n#  \n",   file=file, append=T)
  write.table(sel_dev_coffs(x),  col.names=F, row.names=F, file=file, append=T)

  cat("\n# sel_dev_coffs \n",   file=file, append=T)
  for(mm in 1:length(sel_dev_coffs2(x))){
    write.table(sel_dev_coffs2(x)[[mm]], col.names=F, row.names=F, file=file, append=T)
    cat('\n', file=file, append=T)
  }

  cat("\n# year_flags      \n",   file=file, append=T)
  write.table(unused(x)$yrflags,  col.names=F, row.names=F, file=file, append=T)

  cat("\n# season_flags      \n",   file=file, append=T)
  write.table(unused(x)$snflags,  col.names=F, row.names=F, file=file, append=T)

  cat("# The logistic normal parameters \n", file=file, append=T)
  cat(unlist(lapply(logistic_normal_params(x), paste, "\n")), file=file, append=T)

  cat("\n# maturity at length   \n", file=file, append=T)
  cat(float(slot(x, "mat_at_length")), file=file, append=T)

  cat("\n# The von Bertalanffy parameters  \n",   file=file, append=T)
  write.table(float(growth(x)),  col.names=F, row.names=F, file=file, append=T,quote=F)

  #cat(paste("\n# extra par for Richards     \n", richards(x)),  file=file, append=T)
  cat(paste("\n# Extra par for Richards     \n", richards(x)),  file=file, append=T)   ## RDS 30/04/2020 
  
  cat(paste("\n \n# First Length bias parameters \n", paste(as.vector(len_bias_pars(x)), collapse=" ")),  file=file, append=T)
  cat(paste("\n \n# Common first Length bias flags \n", paste(as.vector(common_len_bias_pars(x)), collapse=" ")),  file=file, append=T)
  cat(paste("\n \n# Common first Length bias coffs \n", paste(as.vector(common_len_bias_coffs(x)), collapse=" ")),  file=file, append=T)

  cat(paste("\n \n# Seasonal growth parameters    \n"), file=file, append=T)
  cat(paste(float(as.vector(season_growth_pars(x)),ZeroPrint="0"), collaps=" "),  file=file, append=T)

  cat("\n \n# Cohort specific growth deviations  \n",   file=file, append=T)
  cat(as.vector(aperm(growth_devs_cohort(x), c(4,2,1,3,5,6))),  file=file, append=T)

  cat("\n \n# Variance parameters \n",   file=file, append=T)
  write.table(float(growth_var_pars(x),ZeroPrint="0"),  col.names=F, row.names=F, file=file, append=T,quote=F)

  if(version(x)>=1055)
    cat(paste("\n# new orthogonal coefficients     \n", orth_coffs(x)),  file=file, append=T)

  cat(paste("\n# The number of mean constraints     \n", n_mean_constraints(x)),  file=file, append=T)

  cat("\n# The diffusion coefficients  \n",   file=file, append=T)
  write.table(diff_coffs_mat(x),  col.names=F, row.names=F, file=file, append=T)

  cat(paste("\n# The grouped_catch_dev_coffs flag \n", catch_dev_coffs_flag(x)),  file=file, append=T)

  if(!all(is.na(catch_dev_coffs(x)))){
    cat("\n# The grouped_catch_dev_coffs \n",   file=file, append=T)
    cat(unlist(lapply(lapply(lapply(catch_dev_coffs(x),float,ZeroPrint="0"), paste, collapse=' '), paste, '\n')), file=file, append=T)
  }

  cat("\n ", file=file, append=T)

  cat("# Historical_flags  \n", file=file, append=T)

  cat(paste(slot(x, 'historic_flags'),"\n"), file=file, append=T)

  #writeLines(slot(x, 'historic_flags'), file=file, append=T)

#  cat(paste("\n \n# Objective function value \n", obj_fun(x)),  file=file, append=T)
#  cat(paste("\n# The number of parameters \n",    n_pars(x)),   file=file, append=T)
#  cat(paste("\n# Likelihood component for tags ", tag_lik(x)),  file=file, append=T)
#  cat(paste("\n# Penalty for mean length constraints \n ", mn_len_pen(x)),  file=file, append=T)
#  cat(paste("\n# Maximum magnitude gradient value \n ",    max_grad(x)),  file=file, append=T)
#  cat(paste("\n# Average fish mort per fishing incident is ", av_fish_mort_inst(x)),  file=file, append=T)
#  cat(paste("\n# Average fish mort per year is ",             av_fish_mort_year(x)),  file=file, append=T)
#  cat(paste("\n# Average fish mort per year by age class is \n# ", paste(av_fish_mort_age(x), collapse=" ")),  file=file, append=T)

}


##########################################################################
#
#  METHODS
#
##########################################################################


#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLPar"), function(x, file, append=F, ...){

  write.par(x=x, file=file, append=append, ...)
})

