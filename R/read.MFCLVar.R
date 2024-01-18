#' Read Var File
#'
#' Read reference point estimates from the \verb{.var} file.
#'
#' @param varfile filename ending with \verb{.var}.
#'
#' @return Vector of reference point estimates, including standard errors.
#'
#' @note
#' The \verb{.var} file is produced by MFCL as a result of delta-method
#' analysis, following Hessian calculations.
#'
#' @section Warning:
#' The calculation of the Frecent/Fmsy in the \verb{.var} file is based on a
#' harvest rate approach, comparing catch in tonnes with biomass in tonnes.
#' Importantly, this is different from the calculation of Frecent/Fmsy in the
#' \verb{.rep} file, which uses an instantaneous mortality rate, comparing catch
#' in numbers with population size in numbers.
#'
#' The choice between reading Frecent/Fmsy from the \verb{.rep} file or the
#' \verb{.var} file should therefore be based on an informed decision,
#' understanding the differences between the two approaches.
#'
#' @seealso
#' \code{\link{read.MFCLRep}}.
#'
#' @examples
#' \dontrun{
#' read.MFCLVar("yft.var")
#' }
#'
#' @export

read.MFCLVar <- function(varfile)
{
  txt <- readLines(varfile)  # read file once, in case we're looping on network

  # F / Fmsy
  ffmsy <- scan(text=grep("F/Fmsy", txt, value=TRUE), n=3, quiet=TRUE)[3]
  ffmsy.se <- scan(text=grep("F/Fmsy", txt, value=TRUE), n=3, quiet=TRUE)[2]

  # SB / SBF0
  pattern <- "adult_rbio(recent) - average_adult_rbio_noeff(40_periods)"
  log.sbsbfo <- scan(text=grep(pattern, txt, fixed=TRUE, value=TRUE), n=3,
                     quiet=TRUE)[3]
  log.sbsbfo.se <- scan(text=grep(pattern, txt, fixed=TRUE, value=TRUE), n=3,
                        quiet=TRUE)[2]
  sbsbfo <- exp(log.sbsbfo)

  # SB / SBmsy
  sbsbmsy <- scan(text=grep("SB/SBmsy", txt, value=TRUE), n=3, quiet=TRUE)[3]
  sbsbmsy.se <- scan(text=grep("SB/SBmsy", txt, value=TRUE), n=3, quiet=TRUE)[2]

  out <- c(ffmsy=ffmsy, ffmsy.se=ffmsy.se,
           log.sbsbfo=log.sbsbfo, log.sbsbfo.se=log.sbsbfo.se, sbsbfo=sbsbfo,
           sbsbmsy=sbsbmsy, sbsbmsy.se=sbsbmsy.se)
  out
}
