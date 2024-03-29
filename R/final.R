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
                     quiet=TRUE)
{
  # Find all par files matching pattern
  parfile <- dir(folder, pattern=pattern, full.names=full)
  if(length(parfile) == 0)
    stop("no par files found that match 'pattern'")

  # Find last one (in alphabetical order)
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
                     quiet=TRUE)
{
  # Find all rep files matching pattern
  repfile <- dir(folder, pattern=pattern, full.names=full)
  if(length(repfile) == 0)
    stop("no rep files found that match 'pattern'")

  # Find last one (in alphabetical order)
  repfile <- max(repfile)
  if(!quiet)
    cat(basename(repfile), fill=TRUE)

  repfile
}




#' First Year
#'
#' Read the first year in the model from the \verb{.rep} file.
#'
#' @param folder is a folder containing at least one \verb{.rep} file.
#'
#' @return First year as an integer.
#'
#' @note
#' Can be useful to find a suitable \code{first.yr} argument for
#' \code{read.MFCLPar}.
#'
#' @seealso
#' \code{\link{read.MFCLPar}}.
#'
#' @examples
#' \dontrun{
#' first.yr <- firstYear("modelrun")
#' parfile <- finalPar("modelrun")
#' par <- read.MFCLPar(parfile, first.yr)
#' }
#'
#' @export

firstYear <- function(folder)
{
  # Read rep file
  repfile <- finalRep(folder)
  txt <- readLines(repfile)

  # Look for label
  line <- which(txt == "# Year 1") + 1
  if(length(line) == 0)
    stop("label '# Year 1' not found in .rep file")
  if(length(line) > 1)
    stop("multiple labels '# Year 1' found in .rep file")

  # Read first year
  first.yr <- as.integer(txt[line])

  first.yr
}
