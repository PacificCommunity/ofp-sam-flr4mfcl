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
    pf232 <- mat_s[mat_d==af199]           # bug fix af199 cannot be zero - need to change it to 1
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
#' @param all whether to include flags that are only specified in one of the
#'        model runs.
#' @param flaglist optional filename to use instead of the built-in
#'        \file{flaglist.csv} lookup table.
#' @param \dots passed to \code{diffFlags}.
#'
#' @details
#' The \code{par1} and \code{par2} objects can be any of the following:
#' \enumerate{
#' \item folder containing a par file
#' \item filename of a par file
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
#' \code{\link{diffFlagsStepwise}} shows differences in flag settings between
#' stepwise model runs.
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

  flags1 <- flagExtract(par1)
  flags2 <- flagExtract(par2)

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
#' \item folder containing a par file
#' \item filename of a par file
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
  flags <- flagExtract(flags)

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
  flags$meaning <- character(nrow(flags))
  for(i in seq_len(nrow(flags)))
    flags$meaning[i] <- lookup(flags$flagtype[i], flags$flag[i], flaglist)

  flags
}


#' Diff Flags Stepwise
#'
#' Show differences in flag settings between stepwise model runs.
#'
#' @param stepdir a directory containing model runs in subdirectories.
#' @param models an optional vector of filenames to manually specify stepwise
#'        models to compare.
#' @param labels an optional vector of short labels to describe the stepwise
#'        models.
#' @param quiet whether to suppress the on-screen reporting of reading files.
#' @param \dots passed to \code{diffFlags}.
#'
#' @details
#' Generally, the user only needs to specify \code{stepdir}. If this top
#' directory contains stepwise model runs as subdirectories, then the default
#' values of \code{models} and \code{labels} will infer the correct paths and
#' model names.
#'
#' If the stepwise model runs are not organized in a straightforward way, the
#' \code{models} and \code{labels} arguments can be passed explicitly.
#'
#' @return
#' A list of data frames showing differences in flag settings between stepwise
#' model runs.
#'
#' @seealso
#' \code{\link{diffFlags}} shows differences in flag settings between two model
#' runs.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' \dontrun{
#' yft_dir <- "//penguin//assessments/yft/2020_review/analysis/stepwise"
#' yft_diffs <- diffFlagsStepwise(yft_dir)
#' lapply(yft_diffs, nrow)  # show number of flags changed in each step
#' lapply(yft_diffs, head)  # peek at the first 6 flags changes in each step
#'
#' # Unusual directory structure of BET 2020 stepwise models
#' bet_dir <- "//penguin/assessments/bet/2020/2020_stepwise"
#' bet_models <- file.path(dir(bet_dir, full.names=TRUE), "10N")
#' bet_labels <- dir(bet_dir)
#' bet_diffs <- diffFlagsStepwise(bet_dir, bet_models, bet_labels)
#' lapply(bet_diffs, nrow)
#' lapply(bet_diffs, head)
#' }
#'
#' @export

diffFlagsStepwise <- function(stepdir, models=dir(stepdir, full.names=TRUE),
                              labels=basename(models), quiet=FALSE, ...) {

  # Find models in stepwise folder
  models <- models[dir.exists(models)]  # only directories
  if(length(models) < 2)
    stop("fewer than 2 models in stepwise folder, nothing to diff")

  # Import each model once
  parobj <- list()
  for(i in seq_len(length(models))) {
    if(!quiet)
      cat("** Reading ", basename(models[i]), "/", sep="")
    parfile <- finalPar(models[i], quiet=quiet)
    parobj[[i]] <- read.MFCLFlags(parfile)
  }

  # Compare flags
  diffs <- list()
  for(i in seq_len(length(models)-1))
  {
    diffs[[i]] <- diffFlags(parobj[[i]], parobj[[i+1]], ...)
    names(diffs)[i] <- paste(labels[i], "vs.", labels[i+1])
  }

  diffs
}


# Get flags from anything: folder -> file -> flags -> data.frame

flagExtract <- function(flags) {

  if(is.character(flags) && dir.exists(flags))
    flags <- finalPar(flags, quiet=TRUE)
  if(is.character(flags) && file.exists(flags))
    flags <- read.MFCLFlags(flags)
  if(isS4(flags))
    flags <- flags(flags)
  flags
}


#' Flag Sort
#'
#' Sort flags by flagtype and flagnumber
#'
#' @param flags dataframe of MFCL flags from MFCLPar object.
#'
#' @return
#' A data frame of sorted flag settings
#'
#' @seealso
#' \code{\link{diffFlags}} calls this function to show the meaning of flags that
#' are different between two model runs.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' flagSort(flags(par))
#'
#' @export

flagSort <- function(flags){

  if(!is.data.frame(flags))
    stop("flags is not a data frame")

  # ftypes <- unique(flags$flagtype)
  sortedparestflags <- subset(flags, flagtype==1)[order(subset(flags, flagtype==1)$flag),]
  sortedageflags    <- subset(flags, flagtype==2)[order(subset(flags, flagtype==2)$flag),]
  sortedfishflags   <- subset(flags, flagtype%in%c(-1:-999))[order(abs(subset(flags, flagtype%in%c(-1:-999))$flagtype),
                                                                   subset(flags, flagtype%in%c(-1:-999))$flag),]
  sortedtagflags    <- subset(flags, flagtype%in%c(-10000:-99999))[order(abs(subset(flags, flagtype%in%c(-10000:-99999))$flagtype),
                                                                         subset(flags, flagtype%in%c(-10000:-99999))$flag),]
  sortedregionflags <- subset(flags, flagtype%in%c(-100000:-999999))[order(abs(subset(flags, flagtype%in%c(-100000:-999999))$flagtype),
                                                                           subset(flags, flagtype%in%c(-100000:-999999))$flag),]

  sortedflags <- rbind(sortedparestflags, sortedageflags, sortedfishflags, sortedtagflags, sortedregionflags)
  return(sortedflags)
}




#' parestflag
#'
#' get parest flag settings
#'
#' @param par object of class MFCLFlag()
#' @param flags numeric vector of parest flag number(s).
#'
#' @description 
#' A simple wrapper function for \code{\link{flagval}}
#' 
#' @return
#' A data frame of sorted flag settings
#'
#' @seealso
#' \code{\link{flagval}} calls this function to retrieve flag settings.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' parestflag(par, c(1,20))
#'
#' @export

setGeneric('parestflag', function(x, flags, ...) standardGeneric('parestflag')) 
setMethod('parestflag', signature(x='MFCLFlags'),function(x, flags){ flagval(x, 1, flags)})
          

#' ageflag
#'
#' get age flag settings
#'
#' @param par object of class MFCLFlag()
#' @param flags numeric vector of age flag number(s).
#'
#' @description 
#' A simple wrapper function for \code{\link{flagval}}
#' 
#' @return
#' A data frame of sorted flag settings
#'
#' @seealso
#' \code{\link{flagval}} calls this function to retrieve flag settings.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' ageflag(par, c(32,92))
#'
#' @export

setGeneric('ageflag', function(x, flags, ...) standardGeneric('ageflag')) 
setMethod('ageflag', signature(x='MFCLFlags'),function(x, flags){ flagval(x, 1, flags)})



#' fishflag
#'
#' get fish flag settings
#'
#' @param par object of class MFCLFlag()
#' @param flags numeric vector of fish flag number(s).
#'
#' @description 
#' A simple wrapper function for \code{\link{flagval}}
#' 
#' @return
#' A data frame of sorted flag settings
#'
#' @seealso
#' \code{\link{flagval}} calls this function to retrieve flag settings.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' fishflag(par, c(1,15))
#'
#' @export

setGeneric('fishflag', function(x, flags, ...) standardGeneric('fishflag')) 

setMethod('fishflag', signature(x='MFCLFlags'),
          function(x, flags){ 
            ff <- sort(unique(subset(flags(x), flagtype<0 & flagtype>-9999)$flagtype), decreasing = TRUE)
            flagval(x, ff, flags)
            })


#' tagflag
#'
#' get region flag settings
#'
#' @param par object of class MFCLFlag()
#' @param flags numeric vector of tag flag number(s).
#'
#' @description 
#' A simple wrapper function for \code{\link{flagval}}
#' 
#' @return
#' A data frame of sorted flag settings
#'
#' @seealso
#' \code{\link{flagval}} calls this function to retrieve flag settings.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' tagflag(par, c(1))
#'
#' @export

setGeneric('tagflag', function(x, flags, ...) standardGeneric('tagflag')) 

setMethod('tagflag', signature(x='MFCLFlags'),
          function(x, flags){ 
            ff <- sort(unique(subset(flags(x), flagtype<= -9999 & flagtype> -99999)$flagtype), decreasing = TRUE)
            flagval(x, ff, flags)
          })




#' regionflag
#'
#' get region flag settings
#'
#' @param par object of class MFCLFlag()
#' @param flags numeric vector of region flag number(s).
#'
#' @description 
#' A simple wrapper function for \code{\link{flagval}}
#' 
#' @return
#' A data frame of sorted flag settings
#'
#' @seealso
#' \code{\link{flagval}} calls this function to retrieve flag settings.
#'
#' \code{\link{read.MFCLFlags}} reads flag settings from a par file.
#'
#' @examples
#' data(par)
#' regionflag(par, c(1))
#'
#' @export

setGeneric('regionflag', function(x, flags, ...) standardGeneric('regionflag')) 

setMethod('regionflag', signature(x='MFCLFlags'),
          function(x, flags){ 
            ff <- sort(unique(subset(flags(x), flagtype<= -99999)$flagtype), decreasing = TRUE)
            flagval(x, ff, flags)
          })


