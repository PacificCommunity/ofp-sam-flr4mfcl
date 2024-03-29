% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.MFCLPseudo.r
\name{read.MFCLPseudoSizeComp}
\alias{read.MFCLPseudoSizeComp}
\title{Read MFCLPseudo Size Comp}
\usage{
read.MFCLPseudoSizeComp(
  lw_sim = "test_lw_sim_alt",
  projfrq = projfrq,
  ctrl = "missing",
  historical = FALSE
)
}
\arguments{
\item{lw_sim}{A character string for the input file name.}

\item{projfrq}{An \linkS4class{MFCLFrq} object for the projection period over which the pseudo data have been generated.}

\item{ctrl}{An \linkS4class{MFCLMSEControl}.}

\item{historical}{Boolean TRUE or FALSE If the simulated data include the historical period or just the projecion period.}
}
\value{
An object of class \linkS4class{MFCLPseudo}.
}
\description{
Reads in simulated length and weight size composition data from the MFCL generated file 'test_lw_sim' (or 'test_lw_sim_alt') and
returns a partially complete object of MFCLPseudo. The 'catcheff' slot of the MFCLPseudo object is not filled.
}
\details{
There are now several functions for reading pseudo data :
\itemize{
  \item read.MFCLPseudo - reads both catch, effort and size comps. Works when you only have either pseudo length or weight composition data (e.g. skipjack). 
  \item read.MFCLCatchEffort - only reads in the simulated catch and effort data
  \item read.MFCLPseudoSizeComp - only reads the size comp data from test_lw_sim but works when you have both length and weight data.
}
}
\examples{
\dontrun{
# Expanding an MFCLFrq, e.g. that was used from in an assessment
pseudo <- read.MFCLPseudoSizeComp('test_lw_sim', projfrq, mseCtrl, historical=FALSE)
}
}
\seealso{
\code{\link{MFCLprojControl}} \code{\link{MFCLFrq}} \code{\link{MFCLPar}}
}
