# FLR4MFCL - R4MFCL built with FLR classes
# Copyright (C) 2018  Rob Scott

#' Recruitment Periods
#'
#' Calculate recruitment periods for deterministic and stochastic projection settings.
#'
#' @param par an object of class MFCLPar.
#' @param af199 undocumented.
#' @param af200 undocumented.
#' @param pf232 undocumented.
#' @param pf233 undocumented.
#' @param show undocumented.
#'
#' @return An object of class numeric vector.
#'
#' @examples
#' data(par)
#' recPeriod(par)
#'
#' @export

recPeriod <- function(par, af199=NULL, af200=NULL, pf232=NULL, pf233=NULL, show=FALSE){

  mat_d <- matrix(rev(1:dimensions(par)["years"]), ncol=dimensions(par)["seasons"], byrow=TRUE)
  rownames(mat_d) <- as.character(range(par)["minyear"]:range(par)["maxyear"])

  mat_s <- matrix(1:dimensions(par)["years"], ncol=dimensions(par)["seasons"], byrow=TRUE)
  rownames(mat_s) <- as.character(range(par)["minyear"]:range(par)["maxyear"])

  if(show){
    print("Deterministic"); print(mat_d)
    print("Stochastic")   ; print(mat_s)
    print("Flags")
  }

  # If flag values given as args : calculate corresponding values
  if(!is.null(af199) && is.null(pf232))
    pf232 <- mat_s[mat_d==af199]
  if(!is.null(af200) && is.null(pf233))
    pf233 <- mat_s[mat_d==af200]

  if(!is.null(pf232) && is.null(af199))
    af199 <- mat_s[mat_d==pf232]
  if(!is.null(pf233) && is.null(af200))
    af200 <- mat_s[mat_d==pf233]

  # If no flagvals given as args : use values given in the par
  if(is.null(af199))
    af199 <- flagval(par, 2, 199)$value
  if(is.null(af200))
    af200 <- flagval(par, 2, 200)$value
  if(is.null(pf232))
    pf232 <- flagval(par, 1, 232)$value
  if(is.null(pf233))
    pf233 <- flagval(par, 1, 233)$value

  res <- c(af199, af200, pf232, pf233)
  names(res) <- c('af199', 'af200', 'pf232', 'pf233')

  return(res)
}


#' Flag Summary
#'
#' Flag settings summarised by MFCL User Guide sections.
#'
#' @param par An object of class MFCLPar.
#' @param type A character string specifying the MFCL User Guide section.
#'
#' @return A data frame of flag settings.
#'
#' @examples
#' data(par)
#' flagSummary(par, 'projection')
#'
#' @export

flagSummary <- function(par, type){

  options <- c('projection', 'impact_analysis', 'MSY')

  if(!is.element(type, options))
    stop(paste("arg 'type' must be one of the following:", paste0(options, collapse="; ")))

  ffrange <- -(1:dimensions(par)['fisheries'])
  switch(type,
    "projection"     = rbind(flagval(par, 1, c(142, 231:239)), flagval(par, 2, c(20, 190, 191, 195, 161, 199, 200))),
    "impact_analysis"= rbind(flagval(par, 2, c(170:176, 190, 191, 193)), flagval(par, ffrange, 55)),
    "MSY"            = rbind(flagval(par, 2, c(112, 140:141, 145:155, 161:163, 165:169, 194, 199:200)), flagval(par, ffrange, 70))
  )
}


#' Flag Diff
#'
#' Show flag differences between two par files or MFCL objects.
#'
#' @param par1 a filename or object of class \code{MFCLPar} or \code{MFCLFlags}.
#' @param par2 a filename or object of class \code{MFCLPar} or \code{MFCLFlags}.
#' @param all whether to compare all flags, including those that are not
#'        specified in both par files.
#'
#' @return A data frame of flag settings for par1 and par2.
#'
#' @note
#' The traditional way to read in a par file is using \code{read.MFCLPar} that
#' imports all parameter values, flags, and a variety of other information.
#'
#' For the purposes of comparing flags, \code{read.MFCLFLags} can be more
#' practical, as it takes around 20x less time to import only the flags.
#'
#' In both cases, the \code{flags()} method can be used to access the
#' \code{flags} data frame.
#'
#' @examples
#' data(par)
#' par1 <- par2 <- par
#'
#' # Different flag value
#' flags(par2)[20,"value"] <- 12
#' flagDiff(par1, par2)
#'
#' # Example where flag is specified in par1 but not in par2
#' flags(par1) <- rbind(flags(par1), c(-10269, 1, 1))
#' flagDiff(par1, par2)             # default is to show par2 as NA
#' flagDiff(par1, par2, all=FALSE)  # all=FALSE omits such comparisons
#'
#' @export

flagDiff <- function(par1, par2, all=TRUE) {

  # Extract flags
  if(is.character(par1) && file.exists(par1))
    par1 <- read.MFCLFlags(par1)
  if(is.character(par2) && file.exists(par2))
    par2 <- read.MFCLFlags(par2)
  flags1 <- flags(par1)
  flags2 <- flags(par2)

  # Combine
  flags <- merge(flags1, flags2, by=c("flagtype", "flag"), all=all)
  names(flags) <- c("flagtype", "flag", "par1", "par2")

  # Compare
  notsame <- is.na(flags$par1) | is.na(flags$par2) | flags$par1 != flags$par2
  diffs <- flags[notsame,]
  diffs <- diffs[order(-diffs$flagtype, diffs$flag),]
  rownames(diffs) <- NULL

  diffs
}
