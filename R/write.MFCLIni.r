
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
  
  cat("# ini version number\n1001 \n",                file=file, append=append)
  
  cat("# number of age classes\n",  file=file, append=T)
  cat(length(mat(x)),                file=file, append=T)
  cat("\n# tag fish rep \n",         file=file, append=T)
  write.table(tag_fish_rep_rate(x), row.names=F, col.names=F, file=file, append=T)
  cat("# tag fish rep group flags \n", file=file, append=T)
  write.table(tag_fish_rep_grp(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# tag_fish_rep active flags \n", file=file, append=T)
  write.table(tag_fish_rep_flags(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# tag_fish_rep target \n", file=file, append=T)
  write.table(tag_fish_rep_target(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# tag_fish_rep penalty \n", file=file, append=T)
  write.table(tag_fish_rep_pen(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# maturity at age \n", file=file, append=T)
  cat(c(aperm(mat(x), c(4,1,2,3,5,6))), file=file, append=T)
  cat("\n# natural mortality (per year)\n", file=file, append=T)
  cat(c(m(x)), file=file, append=T)
  cat("\n# movement map\n", file=file, append=T)
  cat(c(move_map(x)), file=file, append=T)
  cat("\n# diffusion coffs (per year)\n", file=file, append=T)
  write.table(diff_coffs(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# age_pars\n", file=file, append=T)
  write.table(age_pars(x),  row.names=F, col.names=F, file=file, append=T)
  cat("# recruitment distribution by region \n", file=file, append=T)
  cat(c(rec_dist(x)), file=file, append=T)
  cat("\n# The von Bertalanffy parameters \n# Initial  lower bound  upper bound \n# ML1\n", file=file, append=T)
  cat(c(growth(x)[1,]), file=file, append=T)
  cat("\n# ML2\n", file=file, append=T)
  cat(c(growth(x)[2,]), file=file, append=T)
  cat("\n# K (per year)\n", file=file, append=T)
  cat(c(growth(x)[3,]), file=file, append=T)
  cat("\n# Length-weight parameters\n", file=file, append=T)
  cat(c(lw_params(x)), file=file, append=T)
  cat("\n# sv(29)\n", file=file, append=T)
  cat(c(sv(x)), file=file, append=T)
  cat("\n# Generic SD of length at age\n", file=file, append=T)
  cat(c(sd_length_at_age(x)), file=file, append=T)
  cat("\n# Length-dependent SD\n", file=file, append=T)
  cat(c(sd_length_dep(x)), file=file, append=T)
  cat("\n# The number of mean constraints\n", file=file, append=T)
  cat(c(n_mean_constraints(x)), file=file, append=T)
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

