#' Adult and Juvenile F
#'
#' Calculate adult and juvenile fishing mortality.
#'
#' @param rep object of class MFCLRep.
#' @param par object of class MFCLPar.
#'
#' @return
#' Data frame containing four columns:
#' \item{year}{year}
#' \item{area}{region, including \code{"all"}}
#' \item{stage}{stage, \code{"adult"} and \code{"juvenile"}}
#' \item{f}{fishing mortality rate, annual}
#'
#' @section Warning:
#' The obsolete \pkg{diags4MFCL} package provided a similar function called
#' \code{plot.F.temporal}, but that function had errors and should not be used.
#' It was based on wrong calculations of fishing mortality:
#' \preformatted{
#' .[,dead.juv:=f*juv] \%>\%              # wrong
#' .[,dead.adult:=f*adult] \%>\%          # wrong
#' .[,F.juv:=(dead.juv/juv)] \%>\%        # wrong
#' .[,F.adult:=(dead.adult/adult)] \%>\%  # wrong
#' }
#' The \code{AdultJuvenileF} function in the \pkg{FLR4MFCL} package is the
#' correct function to use for calculating adult and juvenile fishing
#' mortalities.
#'
#' @note
#' Adult fishing mortality is calculated as the weighted average fishing
#' mortality, weighted by the maturity ogive.
#'
#' Juvenile fishing mortality is calculated as the weighted average fishing
#' mortality, weighted by the inverse \code{(1-p)} of the maturity ogive.
#'
#' @seealso
#' \code{\link{fm}} and \code{\link{fm_aggregated}} are used to access the
#' fishing mortalities.
#'
#' \code{\link{mat}} is used to access the maturity ogive.
#'
#' @examples
#' \dontrun{
#' AdultJuvenileF(rep, par)
#' }
#'
#' @export

AdultJuvenileF <- function(rep, par)
{
  # Check objects
  if(!inherits(rep, "MFCLRep"))
    stop("'rep' must be an object of class MFCLRep")
  if(!inherits(par, "MFCLPar"))
    stop("'par' must be an object of class MFCLPar")

  # Calculate annual F
  f.annual.all <- as.data.frame(seasonSums(fm_aggregated(rep)))
  f.annual.reg <- as.data.frame(seasonSums(fm(rep)))
  f.annual <- rbind(f.annual.reg, f.annual.all)
  names(f.annual)[names(f.annual) == "data"] <- "f"
  f.annual <- f.annual[c("year", "area", "age", "f")]

  # Calculate adult and juvenile F
  p.adult <- mat(par)
  p.adult[which.max(p.adult):length(p.adult)] <- 1  # once adult, stay adult
  p.juven <-  1 - p.adult
  f.adult <- aggregate(f~year+area, f.annual, weighted.mean, w=p.adult)
  f.juven <- aggregate(f~year+area, f.annual, weighted.mean, w=p.juven)
  f.adult$stage <- "adult"
  f.juven$stage <- "juvenile"
  f.stage <- rbind(f.adult, f.juven)[c("year", "area", "stage", "f")]

  # Use optimal data types
  f.stage$year <- as.integer(f.stage$year)

  f.stage
}
