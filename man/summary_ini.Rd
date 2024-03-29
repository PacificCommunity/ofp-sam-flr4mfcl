% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iniTools.R
\name{summary_ini}
\alias{summary_ini}
\title{Summarize Ini}
\usage{
summary_ini(inifile, ...)
}
\arguments{
\item{inifile}{\verb{ini} filename.}

\item{\dots}{passed to \code{read.MFCLIni}.}
}
\value{
A data frame summarizing the contents of the \verb{ini} file.
}
\description{
Summarize the contents of an \verb{ini} file.
}
\examples{
inifile <- system.file("extdata/skj.ini", package="FLR4MFCL")
summary_ini(inifile)

}
\seealso{
\code{\link{diffIniStepwise}} shows differences in the \verb{ini} file
between stepwise model runs.

\code{\link{read.MFCLIni}} reads an \verb{ini} file.
}
