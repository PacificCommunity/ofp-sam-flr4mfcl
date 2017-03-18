

#' recruitment peiods
#'
#' Calculates recruitment periods for deterministic and stochastic projection settings
#'
#' @param parfile A object of class MFCLPar 
#'
#' @return An object of class numeric vector
#'
#' @examples
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
 
  if(!is.null(af199) & is.null(pf232))
    pf232 <- mat_s[mat_d==af199]
  if(!is.null(af200) & is.null(pf233))
    pf233 <- mat_s[mat_d==af200]
  
  if(!is.null(pf232) & is.null(af199))
    af199 <- mat_s[mat_d==pf232]
  if(!is.null(pf233) & is.null(af200))
    af200 <- mat_s[mat_d==pf233]
  
  res <- c(af199, af200, pf232, pf233)
  names(res) <- c('af199', 'af200', 'pf232', 'pf233')
  
  return(res)
  
}
  