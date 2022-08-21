#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

#' recruitment periods
#'
#' Calculates recruitment periods for deterministic and stochastic projection settings
#'
#' @param parfile An object of class MFCLPar
#'
#' @return An object of class numeric vector
#'
#' @examples
#' data(par)
#' recPeriod(par)
#'
#' @export


recPeriod <- function(par, af199=NULL, af200=NULL, pf232=NULL, pf233=NULL, show=F){
 
  mat_d <- matrix(rev(1:dimensions(par)["years"]), ncol=dimensions(par)["seasons"], byrow=T)
  rownames(mat_d) <- as.character(range(par)["minyear"]:range(par)["maxyear"])
  
  mat_s <- matrix(1:dimensions(par)["years"], ncol=dimensions(par)["seasons"], byrow=T)
  rownames(mat_s) <- as.character(range(par)["minyear"]:range(par)["maxyear"])
  
  if(show==T){
    print("Deterministic"); print(mat_d)
    print("Stochastic")   ; print(mat_s)
    print("Flags")
  }
  # if flag values given as args : calculate corresponding values
  if(!is.null(af199) & is.null(pf232))
    pf232 <- mat_s[mat_d==af199]
  if(!is.null(af200) & is.null(pf233))
    pf233 <- mat_s[mat_d==af200]
  
  if(!is.null(pf232) & is.null(af199))
    af199 <- mat_s[mat_d==pf232]
  if(!is.null(pf233) & is.null(af200))
    af200 <- mat_s[mat_d==pf233]
  
  # if no flagvals given as args : use values given in the par
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
  


#' flag summary
#'
#' flag settings summarised by MFCL User Guide sections
#'
#' @param par An object of class MFCLPar
#' @param type A character string specifying the MFCL User Guide section
#'
#' @return A data frame of flag settings
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
  
  ffrange <- -1:-dimensions(par)['fisheries']
  switch(type,
    "projection"     = rbind(flagval(par, 1, c(142, 231:239)), flagval(par, 2, c(20, 190, 191, 195, 161, 199, 200))),
    "impact_analysis"= rbind(flagval(par, 2, c(170:176, 190, 191, 193)), flagval(par, ffrange, 55)),
    "MSY"            = rbind(flagval(par, 2, c(112, 140:141, 145:155, 161:163, 165:169, 194, 199:200)), flagval(par, ffrange, 70))
  )  
}



#' flagDiff
#'
#' flag differences between two par files
#'
#' @param par An object of class MFCLPar
#' @param par An object of class MFCLPar
#'
#' @return A data frame of flag settings for par1 and par2
#'
#' @examples
#' data(par)
#' par1 <- par2 <- par
#' flags(par2)[20,"value"] <- 12
#' flagDiff(par1, par2)
#'
#' @export

flagDiff <- function(par1, par2){
  
  res <- flags(par1)[flags(par1)$value!=flags(par2)$value,]
  res <- cbind(res, flags(par2)[flags(par1)$value!=flags(par2)$value, 'value'])
  colnames(res) <- c("flagtype","flag","par1","par2")
  
  return(res)
}
