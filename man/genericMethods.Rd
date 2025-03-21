% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericMethods.r, R/read.fishmort.R
\docType{methods}
\name{realisations}
\alias{realisations}
\alias{read.MFCLfishmort}
\title{realisations}
\usage{
realisations(object, ...)

read.MFCLfishmort(file, frq, rep, ...)
}
\arguments{
\item{object}{An object of class MFCLLenFreq.}

\item{\dots}{Currently ignored.}

\item{file}{A character string giving the name and path of the fishmort file to be read in.}

\item{frq}{An MFCLFrq object.}

\item{rep}{An MFCLRep object.}
}
\value{
An object of class data.frame.

An object of class data.frame.
}
\description{
Returns the unique fishery realisations. 
Essentially this is the contents of the \code{freq} slot of an \code{\link{MFCLFrq}} object 
without the replications for each length or weight category.

A wrapper for read.fishmort
Reads the fishing mortality from the fishmort file that is output by Multifan-CL.
The fishing mortality in the fishmort file is by region, time period, fishing incident and age.
This function returns the fishing mortality by region, year, season, fishery and age.
The fishmort file provides fishing mortality on a log scale. The read.MFCLfishmort() function transforms them to
'normal' scale (or whatever it's called).
Note that summing these fishing mortalities by region, time and age period yields the same values 
as reported in the 'fm' slot of an MFCLRep object. But summing by time and age does not yield the same values 
as in the 'fm_aggregated' slot which seems to be something to do with the yield analysis.
}
\examples{
\dontrun{
realisations(frq)
}
\dontrun{
read.MFCLfishmort('path/to/fishmort', frq, rep)
}
}
\seealso{
\code{\link{read.MFCLFrq}}

\code{\link{read.MFCLFrq}, \link{read.MFCLRep}}
}
