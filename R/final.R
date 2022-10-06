#' Final Par File
#'
#' Find the final \code{par} file in a model run directory.
#'
#' @param folder model run directory name.
#' @param pattern pattern describing \code{par} filenames. The default pattern
#'        is two digits, followed by \code{.par}.
#' @param full whether to return the \code{par} filename with the full path.
#' @param quiet whether to suppress the on-screen reporting of the final
#'        \code{par} file found.
#'
#' @return The final \code{par} filename as a string.
#'
#' @note
#' If a folder contains \code{00.par}, \code{01.par}, \code{...}, \code{14.par},
#' then the final \code{par} file is \code{14.par}.
#'
#' @seealso
#' \code{\link{finalRep}} finds the final \code{rep} file.
#'
#' @examples
#' \dontrun{
#' finalPar("modelrun")
#' }
#'
#' @export

finalPar <- function(folder, pattern="^[0-9][0-9]\\.par$", full=TRUE,
                     quiet=FALSE)
{
  parfile <- dir(folder, pattern=pattern, full.names=full)
  parfile <- max(parfile)
  if(!quiet)
    cat(basename(parfile), fill=TRUE)
  parfile
}




#' Final Rep File
#'
#' Find the final \code{rep} file in a model run directory.
#'
#' @param folder model run directory name.
#' @param pattern pattern describing \code{rep} filenames. The default pattern
#'        is \code{plot-} and two digits, followed by \code{.rep}.
#' @param full whether to return the \code{rep} filename with the full path.
#' @param quiet whether to suppress the on-screen reporting of the final
#'        \code{rep} file found.
#'
#' @return The final \code{rep} filename as a string.
#'
#' @note
#' If a folder contains \code{plot-09.par.rep}, \code{plot-10.par.rep},
#' \code{...}, \code{plot-14.par.rep}, then the final \code{rep} file is
#' \code{plot-14.par.rep}.
#'
#' @seealso
#' \code{\link{finalPar}} finds the final \code{par} file.
#'
#' @examples
#' \dontrun{
#' finalRep("modelrun")
#' }
#'
#' @export

finalRep <- function(folder, pattern="^plot-[0-9][0-9]\\.par.rep$", full=TRUE,
                     quiet=FALSE)
{
  repfile <- dir(folder, pattern=pattern, full.names=full)
  repfile <- max(repfile)
  if(!quiet)
    cat(basename(repfile), fill=TRUE)
  repfile
}
