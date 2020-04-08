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

write.ini <- function(x, file, append=F, ...){

  float <- function(x) formatC(x, digits=3, format='f')

  vers=slot(x,"ini_version")
  ## Do some checks and give some warnings if necessary to prevent mess ups
  ## Check for maturity at length and version number
  if (vers<1003 & length(mat_at_length(x))>0) warning("In order to use the maturity at length in the ini you need to set the version number to 1003. This will print out an ini of version 1001 that does not have the maturity at length.")
  if (vers==1003 & length(region_flags(x))==0) stop("You need to specify a region_flags matrix!! Otherwise MFCL will fail.")
  cat(c("# ini version number",vers),sep= "\n",file=file, append=append)
  if (slot(x,"dimensions")[1] != length(slot(x,"mat"))) warning("The number of age classes and length of maturity at age don't match up")

  cat("# number of age classes\n",  file=file, append=T)
  cat(slot(x,"dimensions")[1],file=file, append=T,sep='\n')
  if(!all(is.na(tag_fish_rep_rate(x)))){
    cat("# tag fish rep\n",         file=file, append=T)
    write.table(tag_fish_rep_rate(x), row.names=F, col.names=F, file=file, append=T)
    cat("# tag fish rep group flags\n", file=file, append=T)
    write.table(tag_fish_rep_grp(x),  row.names=F, col.names=F, file=file, append=T)
    cat("# tag_fish_rep active flags\n", file=file, append=T)
    write.table(tag_fish_rep_flags(x),  row.names=F, col.names=F, file=file, append=T)
    cat("# tag_fish_rep target\n", file=file, append=T)
    write.table(tag_fish_rep_target(x),  row.names=F, col.names=F, file=file, append=T)
    cat("# tag_fish_rep penalty\n", file=file, append=T)
    write.table(tag_fish_rep_pen(x),  row.names=F, col.names=F, file=file, append=T)
  }
  if( vers>1001){
    cat("# region_flags\n",file=file,append=T)
    write.table(region_flags(x),  row.names=F, col.names=F, file=file, append=T)
  }
  cat("# maturity at age\n", file=file, append=T)
  cat(as.character(aperm(mat(x))), file=file, append=T)
  cat("\n# natural mortality (per year)\n", file=file, append=T)
  cat(as.character(m(x)), file=file, append=T)
  cat("\n# movement map\n", file=file, append=T)
  cat(as.character(move_map(x)), file=file, append=T)
  cat("\n# diffusion coffs (per year)\n", file=file, append=T)
  write.table(diff_coffs(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# age_pars\n", file=file, append=T)
  write.table(age_pars(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# recruitment distribution by region\n", file=file, append=T)
  cat(as.character(rec_dist(x)), file=file, append=T,fill=TRUE)
  if (vers>1002) {
    cat(c("# maturity at length", paste(as.character(mat_at_length(x)),collapse=" ")), file=file, append=T,sep='\n')
  }
  cat("# The von Bertalanffy parameters\n# Initial  lower bound  upper bound\n# ML1\n", file=file, append=T)
  cat(as.character(growth(x)[1,]), file=file, append=T)
  cat("\n# ML2\n", file=file, append=T)
  cat(as.character(growth(x)[2,]), file=file, append=T)
  cat("\n# K (per year)\n", file=file, append=T)
  cat(as.character(growth(x)[3,]), file=file, append=T)
  cat("\n# Length-weight parameters\n", file=file, append=T)
  cat(as.character(slot(x, 'lw_params')), file=file, append=T)            ## different syntax because slot accessor for lw_params mysteriously fails.
  cat("\n# sv(29)\n", file=file, append=T)
  cat(as.character(sv(x)), file=file, append=T)
  cat("\n# Generic SD of length at age\n", file=file, append=T)
  cat(as.character(sd_length_at_age(x)), file=file, append=T)
  cat("\n# Length-dependent SD\n", file=file, append=T)
  cat(as.character(sd_length_dep(x)), file=file, append=T)
  cat("\n# The number of mean constraints\n", file=file, append=T)
  cat(as.character(n_mean_constraints(x)), file=file, append=T)
  cat("\n", file=file, append=T)
}


##########################################################################
#
#  METHODS
#
##########################################################################


#' @rdname write-methods
#' @aliases write
setMethod("write", signature("MFCLIni"), function(x, file, append=F, ...){

  write.ini(x=x, file=file, append=append, ...)
})

