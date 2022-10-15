# FLR4MFCL - R4MFCL built with FLR classes
# Copyright (C) 2018  Rob Scott

#' Recruitment Periods
#'
#' Calculate recruitment periods for deterministic and stochastic projection
#' settings.
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

recPeriod <- function(par, af199=NULL, af200=NULL, pf232=NULL, pf233=NULL,
                      show=FALSE){

  mat_d <- matrix(rev(1:dimensions(par)["years"]),
                  ncol=dimensions(par)["seasons"], byrow=TRUE)
  rownames(mat_d) <- as.character(range(par)["minyear"]:range(par)["maxyear"])

  mat_s <- matrix(1:dimensions(par)["years"], ncol=dimensions(par)["seasons"],
                  byrow=TRUE)
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
    stop(paste("arg 'type' must be one of the following:",
               paste0(options, collapse="; ")))

  ffrange <- -(1:dimensions(par)['fisheries'])
  switch(
    type,
    projection=rbind(flagval(par, 1, c(142, 231:239)),
                     flagval(par, 2, c(20, 190, 191, 195, 161, 199, 200))),
    impact_analysis=rbind(flagval(par, 2, c(170:176, 190, 191, 193)),
                          flagval(par, ffrange, 55)),
    MSY=rbind(
      flagval(par, 2, c(112, 140:141, 145:155, 161:163, 165:169, 194, 199:200)),
      flagval(par, ffrange, 70))
  )
}


#' Diff Flags
#'
#' Show differences in flag settings between two model runs.
#'
#' @param par1 MFCL flags from model run 1.
#' @param par2 MFCL flags from model run 2.
#' @param flaglist optional filename to use instead of the built-in
#'        \file{flaglist.csv} lookup table.
#' @param \dots passed to \code{diffFlags}.
#'
#' @details
#' The \code{par1} and \code{par2} objects can be any of the following:
#' \enumerate{
#' \item filename pointing to a par file
#' \item \code{MFCLPar} object
#' \item \code{MFCLFlags} object
#' \item \code{data.frame} containing flag settings
#' }
#'
#' @return
#' A data frame showing flag settings where par1 and par2 are different, along
#' with a column showing the meaning of each flag.
#'
#' @note
#' \code{flagDiff} is an older name of this function. To support legacy scripts,
#' a call to the old function is simply forwarded to \code{diffFlags}.
#'
#' @seealso
#' This function calls \code{\link{flagMeaning}} to add the column showing the
#' meaning of each flag.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' par1 <- par2 <- par
#'
#' # Different flag value
#' flags(par2)[20,"value"] <- 12
#' diffFlags(par1, par2)
#'
#' # When flag is specified in par1 but not in par2
#' flags(par1) <- rbind(flags(par1), c(-10269, 1, 1))
#' diffFlags(par1, par2)             # default is to show par2 as NA
#' diffFlags(par1, par2, all=FALSE)  # all=FALSE omits such comparisons
#'
#' @export

diffFlags <- function(par1, par2, all=TRUE, flaglist=NULL) {

  # Extract flags
  if(is.character(par1) && file.exists(par1))
    par1 <- read.MFCLFlags(par1)
  if(is.character(par2) && file.exists(par2))
    par2 <- read.MFCLFlags(par2)
  flags1 <- if(isS4(par1)) flags(par1) else par1
  flags2 <- if(isS4(par2)) flags(par2) else par2

  # Combine
  flags <- merge(flags1, flags2, by=c("flagtype", "flag"), all=all)
  names(flags) <- c("flagtype", "flag", "par1", "par2")

  # Compare
  notsame <- is.na(flags$par1) | is.na(flags$par2) | flags$par1 != flags$par2
  diffs <- flags[notsame,]
  diffs <- diffs[order(-diffs$flagtype, diffs$flag),]
  rownames(diffs) <- NULL

  # Add column with meaning
  diffs <- flagMeaning(diffs, flaglist=flaglist)

  diffs
}

#' @rdname diffFlags

flagDiff <- function(...) {
  diffFlags(...)
}


#' Flag Meaning
#'
#' Show the meaning of flags, based on a lookup table.
#'
#' @param flags MFCL flags from a model run.
#' @param flaglist optional filename to use instead of the built-in
#'        \file{flaglist.csv} lookup table.
#'
#' @details
#' The \code{flags} object can be any of the following:
#' \enumerate{
#' \item filename pointing to a par file
#' \item \code{MFCLPar} object
#' \item \code{MFCLFlags} object
#' \item \code{data.frame} containing flag settings
#' }
#'
#' @return
#' A data frame with the same columns as \code{flags} plus a column called
#' \code{meaning}.
#'
#' @seealso
#' \code{\link{diffFlags}} calls this function to show the meaning of flags that
#' are different between two model runs.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' flagMeaning(par)
#'
#' @export

flagMeaning <- function(flags, flaglist=NULL) {

  # Extract flags
  if(is.character(flags) && file.exists(flags))
    flags <- read.MFCLFlags(flags)
  if(isS4(flags))
    flags <- flags(flags)

  # Prepare flag list
  if(is.character(flaglist))
    flaglist <- read.csv(flaglist)
  if(is.null(flaglist))
    flaglist <- read.csv(system.file(package="FLR4MFCL",
                                     "flaglist/flaglist.csv"))

  # Look up flag meaning
  lookup <- function(flagtype, flag, flaglist)
  {
    flagtype <- as.integer(flagtype)
    flag <- as.integer(flag)
    if(flagtype == 1)
      flaglist[flag, "parest_flags"]
    else if(flagtype == 2)
      flaglist[flag, "age_flags"]
    else if(flagtype %in% -(1:999))
      flaglist[flag, "fish_flags"]
    else
      ""
  }

  # Add column with meaning
  flags$meaning <- ""
  for(i in seq_len(nrow(flags)))
    flags$meaning[i] <- lookup(flags$flagtype[i], flags$flag[i], flaglist)

  flags
}
